# ---- Packages ----
library(shiny)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(lubridate)
library(httr)
library(jsonlite)
library(xml2)

# ---- Configuration ----
data_path <- "/Users/amyfulton/Downloads/ff_data.csv"
sample_path <- "data/sample_fatalities.csv"
cache_path <- "data/fatalities.csv"
last_refresh_path <- "data/last_refresh.txt"
feed_url <- "https://apps.usfa.fema.gov/firefighter-fatalities/api/fatalityDatums/feed"
download_url <- "https://apps.usfa.fema.gov/firefighter-fatalities/api/csv"
default_model <- "gemini-2.5-flash"

# ---- Helpers: data normalization ----
normalize_names <- function(df) {
  names(df) <- names(df) |>
    tolower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("^_+|_+$", "")
  df
}

pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}

normalize_data <- function(df) {
  df <- normalize_names(df)

  col_year <- pick_col(df, c("year", "fatality_year", "incident_year", "incident_date", "date_of_death"))
  col_state <- pick_col(df, c("state", "state_abbrev", "state_abbr", "state_code", "dept_state"))
  col_incident <- pick_col(df, c("incident_type", "incidenttype", "incident_category", "incident", "activity", "property_type", "emergency"))
  col_cause <- pick_col(df, c("cause", "cause_of_death", "manner_of_death", "fatality_cause", "cause_of_fatal_injury"))
  col_narrative <- pick_col(df, c("narrative", "incident_narrative", "summary", "initial_summary"))
  col_dept_type <- pick_col(df, c("department_type", "dept_type", "dept_makeup", "classification"))
  col_dept_size <- pick_col(df, c("dept_size", "department_size", "staffing"))
  col_duty <- pick_col(df, c("duty"))
  col_emergency <- pick_col(df, c("emergency"))
  col_property <- pick_col(df, c("property_type"))

  out <- tibble(
    year = if (!is.na(col_year)) parse_year(df[[col_year]]) else NA_integer_,
    state = if (!is.na(col_state)) as.character(df[[col_state]]) else NA_character_,
    incident_type = if (!is.na(col_incident)) as.character(df[[col_incident]]) else NA_character_,
    cause = if (!is.na(col_cause)) as.character(df[[col_cause]]) else NA_character_,
    department_type = if (!is.na(col_dept_type)) as.character(df[[col_dept_type]]) else "Unknown",
    dept_size = if (!is.na(col_dept_size)) suppressWarnings(as.integer(df[[col_dept_size]])) else NA_integer_,
    narrative = if (!is.na(col_narrative)) as.character(df[[col_narrative]]) else NA_character_,
    duty = if (!is.na(col_duty)) as.character(df[[col_duty]]) else NA_character_,
    emergency = if (!is.na(col_emergency)) as.character(df[[col_emergency]]) else NA_character_,
    property_type = if (!is.na(col_property)) as.character(df[[col_property]]) else NA_character_
  )

  out$incident_category <- classify_incident(out)
  out$region <- state_to_region(out$state)
  out
}

# ---- Helpers: date parsing ----
parse_year <- function(x) {
  x <- as.character(x)
  y <- suppressWarnings(lubridate::year(lubridate::mdy(x)))
  if (all(is.na(y))) {
    y <- suppressWarnings(as.integer(str_extract(x, "[0-9]{4}")))
  }
  y
}

# ---- Helpers: refresh bookkeeping ----
read_last_refresh <- function() {
  if (!file.exists(last_refresh_path)) return(NA)
  txt <- tryCatch(readLines(last_refresh_path, warn = FALSE), error = function(e) NA)
  suppressWarnings(as.POSIXct(txt, tz = "UTC"))
}

write_last_refresh <- function(time = Sys.time()) {
  dir.create(dirname(last_refresh_path), showWarnings = FALSE, recursive = TRUE)
  writeLines(format(time, "%Y-%m-%d %H:%M:%S"), last_refresh_path)
}

should_refresh <- function(last_refresh, hours = 24) {
  if (is.na(last_refresh)) return(TRUE)
  difftime(Sys.time(), last_refresh, units = "hours") >= hours
}

# ---- Data refresh (RSS + CSV) ----
maybe_refresh_data <- function(force = FALSE) {
  last_refresh <- read_last_refresh()
  if (!force && file.exists(cache_path) && !should_refresh(last_refresh)) {
    return("cache")
  }

  latest_pub <- tryCatch({
    xml <- xml2::read_xml(feed_url)
    pub <- xml2::xml_text(xml2::xml_find_first(xml, "//channel/pubDate"))
    as.POSIXct(pub, format = "%a, %d %b %Y %H:%M:%S %Z", tz = "UTC")
  }, error = function(e) NA)

  if (!force && !is.na(latest_pub) && !is.na(last_refresh) && latest_pub <= last_refresh) {
    write_last_refresh(Sys.time())
    return("cache")
  }

  live <- tryCatch({
    read_csv(download_url, show_col_types = FALSE)
  }, error = function(e) NULL)

  if (!is.null(live)) {
    dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)
    write_csv(live, cache_path)
    write_last_refresh(Sys.time())
    return("refreshed")
  }

  write_last_refresh(Sys.time())
  "cache"
}

# ---- Data loader ----
get_data <- function(force_refresh = FALSE) {
  refresh_status <- maybe_refresh_data(force_refresh)

  if (file.exists(cache_path)) {
    df <- read_csv(cache_path, show_col_types = FALSE)
    df <- normalize_data(df)
    attr(df, "source") <- if (refresh_status == "refreshed") "refreshed" else "cache"
    return(df)
  }

  if (file.exists(data_path)) {
    df <- read_csv(data_path, show_col_types = FALSE)
    df <- normalize_data(df)
    attr(df, "source") <- "local"
    return(df)
  }

  sample <- read_csv(sample_path, show_col_types = FALSE)
  sample <- normalize_data(sample)
  attr(sample, "source") <- "sample"
  sample
}

# ---- LLM: Gemini API ----
extract_response_text <- function(resp_json) {
  if (is.null(resp_json$candidates) || length(resp_json$candidates) == 0) return(NA_character_)
  parts <- resp_json$candidates[[1]]$content$parts
  if (is.null(parts) || length(parts) == 0) return(NA_character_)
  parts[[1]]$text
}

call_gemini <- function(input_text, instructions, model = default_model) {
  key <- Sys.getenv("GEMINI_API_KEY")
  if (key == "") stop("GEMINI_API_KEY is not set.")

  prompt <- paste(instructions, "\n\n", input_text)

  body <- list(
    contents = list(
      list(
        parts = list(
          list(text = prompt)
        )
      )
    )
  )

  url <- sprintf("https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent", model)

  resp <- httr::POST(
    url,
    httr::add_headers(
      `Content-Type` = "application/json",
      `x-goog-api-key` = key
    ),
    body = jsonlite::toJSON(body, auto_unbox = TRUE)
  )

  if (httr::status_code(resp) >= 400) {
    stop("Gemini API error: ", httr::content(resp, as = "text", encoding = "UTF-8"))
  }

  resp_json <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
  extract_response_text(resp_json)
}

# ---- LLM: classification categories ----
llm_categories <- c(
  "Training Gaps",
  "Equipment Issues",
  "Operational Challenges",
  "Medical/Cardiac",
  "Vehicle/Traffic",
  "Collapse/Entrapment",
  "Hazmat/Exposure",
  "Wildland",
  "Other"
)

# ---- LLM: narrative classification ----
classify_narrative <- function(text, model = default_model) {
  if (is.na(text) || str_trim(text) == "" || str_detect(text, regex("^not available", ignore_case = TRUE))) {
    return(NA_character_)
  }

  instructions <- paste(
    "Classify the firefighter fatality narrative into exactly one category from this list:",
    paste(llm_categories, collapse = ", "),
    "Return JSON only: {\"category\":\"<one of the list>\"}. Do not add any extra text."
  )

  out <- call_gemini(text, instructions, model = model)

  # Try strict JSON parsing first
  parsed <- tryCatch(jsonlite::fromJSON(out), error = function(e) NULL)

  # If the model wrapped JSON in text, try to extract the JSON object
  if (is.null(parsed) && !is.na(out)) {
    json_match <- stringr::str_match(out, "\\\\{.*\\\\}")
    if (!is.na(json_match[1, 1])) {
      parsed <- tryCatch(jsonlite::fromJSON(json_match[1, 1]), error = function(e) NULL)
    }
  }

  if (is.null(parsed) || is.null(parsed$category)) {
    # Fallback: try to match any known category in the raw response
    hit <- llm_categories[str_detect(out, regex(paste(llm_categories, collapse = "|"), ignore_case = TRUE))]
    if (length(hit) > 0) {
      return(hit[[1]])
    }
    return(NA_character_)
  }

  category <- as.character(parsed$category)
  if (!category %in% llm_categories) return(NA_character_)
  category
}

# ---- LLM: guidance generation ----
generate_llm_guidance <- function(summary_text, model = default_model) {
  instructions <- paste(
    "You are generating concise prevention guidance for fire department leadership.",
    "Use clear, non-technical language.",
    "Return 3-5 sentences."
  )
  call_gemini(summary_text, instructions, model = model)
}

# ---- LLM: incident report analysis ----
generate_incident_analysis <- function(region, trends_text, reports_text, model = default_model) {
  instructions <- paste(
    "You are analyzing firefighter incident reports and highlighting fatality risks.",
    "Use the region context and historical trends provided.",
    "Return a short bulleted list of key risks and 2-3 targeted recommendations.",
    "Keep the tone clear and practical for fire department leadership."
  )

  input_text <- paste(
    "Region:", region,
    "\nHistorical trends:", trends_text,
    "\nIncident reports:", reports_text
  )

  call_gemini(input_text, instructions, model = model)
}

# ---- LLM: training plan generation ----
generate_training_plan <- function(region, trends_text, model = default_model) {
  instructions <- paste(
    "Create a 12-month firefighter training plan tailored to the region and trends provided.",
    "Return a numbered list with one monthly training activity per month.",
    "Each item should be 1-2 sentences and include the training focus and a brief objective."
  )

  input_text <- paste(
    "Region:", region,
    "\nHistorical trends:", trends_text
  )

  call_gemini(input_text, instructions, model = model)
}

# ---- Non-LLM fallback guidance ----
generate_guidance <- function(region, dept_type, dept_size, top_cause, top_incidents) {
  dept_size_text <- if (is.na(dept_size) || dept_size == "") "your department" else paste("a department of size", dept_size)
  incident_text <- if (length(top_incidents) == 0 || all(is.na(top_incidents))) "the incident types you handle" else paste(top_incidents, collapse = ", ")

  paste0(
    "In ", region, ", the most common fatality cause in the available data is ", top_cause, ". ",
    "For ", dept_size_text, " with a ", dept_type, " makeup, prioritize targeted training for ", incident_text, ". ",
    "Recommended interventions include routine health screening, scenario-based training, and clear operational checklists. ",
    "Consider equipment audits and after-action reviews to address the most common risks."
  )
}

# ---- Incident category heuristics ----
classify_incident <- function(df) {
  text <- paste(df$incident_type, df$property_type, df$duty, df$emergency, df$narrative, sep = " ")
  text <- tolower(text)

  case_when(
    str_detect(text, "wildland|brush|forest|woodland") ~ "Wildland",
    str_detect(text, "hazmat|hazardous|chemical|propane|gas|radiolog") ~ "Hazmat",
    str_detect(text, "ems|ambulance|patient|medical|cpr|emt") ~ "EMS",
    str_detect(text, "rescue|extrication|search|recovery|swiftwater|technical") ~ "Rescue",
    str_detect(text, "vehicle|apparatus|mva|mvc|traffic|road|highway|responding|en route|enroute|collision|crash") ~ "Vehicle Incident",
    str_detect(text, "structure|building|residential|commercial|store|office|apartment|house|fire attack") ~ "Structure Fire",
    TRUE ~ "Other"
  )
}

# ---- State-to-region mapping ----
state_to_region <- function(state_abbr) {
  state_abbr <- toupper(state_abbr)

  northeast <- c("CT","ME","MA","NH","RI","VT","NJ","NY","PA")
  midwest <- c("IL","IN","MI","OH","WI","IA","KS","MN","MO","NE","ND","SD")
  south <- c("DE","FL","GA","MD","NC","SC","VA","DC","WV","AL","KY","MS","TN","AR","LA","OK","TX")
  west <- c("AZ","CO","ID","MT","NV","NM","UT","WY","AK","CA","HI","OR","WA")

  case_when(
    state_abbr %in% northeast ~ "Northeast",
    state_abbr %in% midwest ~ "Midwest",
    state_abbr %in% south ~ "South",
    state_abbr %in% west ~ "West",
    TRUE ~ NA_character_
  )
}

# ---- UI ----
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Bebas+Neue&family=Source+Sans+3:wght@300;400;600;700&display=swap');
      :root{
        --bg: #0b0b0c;
        --panel: #151517;
        --panel-2: #1c1c1f;
        --text: #f7f3ef;
        --muted: #c7c0b8;
        --accent: #ff6a00;
        --accent-2: #d63230;
        --accent-3: #ffb347;
      }
      body { background: radial-gradient(1200px 600px at 10% -10%, #3a1b12 0%, #0b0b0c 55%, #0b0b0c 100%); color: var(--text); font-family: 'Source Sans 3', sans-serif; }
      .container-fluid { padding: 20px 24px 40px 24px; }
      .app-header { display: flex; align-items: center; gap: 16px; padding: 18px 20px; background: linear-gradient(90deg, #2b0f0a, #1a0f0b 45%, #0b0b0c); border: 1px solid #2c2c31; border-radius: 14px; box-shadow: 0 12px 30px rgba(0,0,0,0.35); }
      .app-title { font-family: 'Bebas Neue', sans-serif; font-size: 38px; letter-spacing: 1px; margin: 0; }
      .app-subtitle { color: var(--muted); margin: 0; font-size: 14px; }
      .helmet { width: 48px; height: 48px; }
      .sidebar { background: var(--panel); border: 1px solid #2a2a2f; border-radius: 14px; padding: 16px; }
      .main { background: var(--panel-2); border: 1px solid #2a2a2f; border-radius: 14px; padding: 16px; }
      h3 { font-family: 'Bebas Neue', sans-serif; letter-spacing: 0.5px; color: var(--accent-3); margin-top: 10px; }
      .control-label { color: var(--muted); font-weight: 600; }
      .btn { background: linear-gradient(90deg, var(--accent-2), var(--accent)); border: none; color: #fff; font-weight: 700; }
      .btn:hover { filter: brightness(1.05); }
      table { color: var(--text); }
      .shiny-output-error-validation { color: #ffb347; }
      .info-box { background: #101012; border: 1px dashed #333; padding: 10px; border-radius: 10px; margin-top: 10px; }
    "))
  ),

  tags$div(
    class = "app-header",
    tags$div(
      class = "helmet",
      HTML("<svg viewBox='0 0 64 64' xmlns='http://www.w3.org/2000/svg'><path d='M8 36c0-14 11-26 24-26s24 12 24 26v10H8V36z' fill='#ff6a00'/><path d='M14 36c0-10 8-18 18-18s18 8 18 18v4H14v-4z' fill='#d63230'/><path d='M6 46h52v8H6z' fill='#ffb347'/><circle cx='32' cy='30' r='6' fill='#0b0b0c'/></svg>")
    ),
    tags$div(
      tags$h1(class = "app-title", "Firefighter Fatality Risk Dashboard"),
      tags$p(class = "app-subtitle", "Project proposal - Amy Fulton • Auto refreshes daily"),
      tags$p(class = "app-subtitle", textOutput("last_refreshed"))
    )
  ),

  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      tags$div(textOutput("data_source")),
      tags$div(
        class = "info-box",
        tags$strong("What you need to run LLM"),
        tags$ol(
          tags$li("A Gemini API key."),
          tags$li("Set GEMINI_API_KEY in your environment.")
        )
      ),
      actionButton("refresh_data", "Refresh Data Now"),
      radioButtons(
        "geo_mode",
        "Location Input",
        choices = c("State" = "state", "Region" = "region"),
        selected = "state",
        inline = TRUE
      ),
      conditionalPanel(
        "input.geo_mode == 'state'",
        selectInput("state", "State", choices = c("All"))
      ),
      conditionalPanel(
        "input.geo_mode == 'region'",
        selectInput("region", "Region", choices = c("All", "Northeast", "Midwest", "South", "West"))
      ),
      selectInput("dept_type", "Department Makeup", choices = c("Mostly Volunteer", "Combination", "Career", "Unknown")),
      numericInput("dept_size", "Department Size", value = 50, min = 1),
      tags$div(
        tags$label("Department Challenges / Comments", `for` = "dept_comments"),
        tags$div(
          "Describe any specific challenges your department has faced or concerns you have.",
          style = "color: var(--muted); font-size: 12px; margin-top: 4px; margin-bottom: 6px;"
        ),
        textAreaInput(
          "dept_comments",
          NULL,
          placeholder = "e.g., training gaps, staffing shortages, budget constraints, aging equipment",
          width = "100%",
          height = "100px"
        )
      ),
      checkboxGroupInput(
        "incident_types",
        "Incident Types (Structure Fire and Wildland are always included)",
        choices = c("Hazmat", "EMS", "Vehicle Incident", "Rescue")
      ),
      actionButton("run_llm", "Generate Prevention Guidance")
    ),

    mainPanel(
      class = "main",
      tabsetPanel(
        tabPanel(
          "Risk Summary",
          h3("Fatality Risk Summary"),
          textOutput("risk_summary"),
          tableOutput("top_causes"),
          plotOutput("trend_plot", height = "250px"),
          h3("Incident Mix Over Time"),
          plotOutput("incident_mix_plot", height = "300px"),
          h3("Top Causes (Selected Filters)"),
          plotOutput("cause_plot", height = "260px"),
          h3("Prevention Guidance"),
          textOutput("guidance_status"),
          textOutput("guidance")
        ),
        tabPanel(
          "Incident Reports",
          h3("Incident Report Analysis"),
          tags$p(
            "Paste multiple incident reports below. The LLM will summarize key fatality risks and recommendations using regional trends.",
            style = "color: var(--muted);"
          ),
          textAreaInput(
            "incident_reports",
            NULL,
            placeholder = "Paste incident reports here (multiple reports are OK).",
            width = "100%",
            height = "180px"
          ),
          actionButton("analyze_reports", "Analyze Incident Reports"),
          textOutput("reports_status"),
          textOutput("reports_analysis")
        ),
        tabPanel(
          "Training Plan",
          h3("Monthly Training Plan"),
          tags$p(
            "Generates a 12-month training plan based on your region and historical fatality trends.",
            style = "color: var(--muted);"
          ),
          checkboxGroupInput(
            "training_equipment",
            "Available Training Equipment",
            choices = c(
              "Smoke Generators & Fluid",
              "Fire Simulation Props",
              "Rescue Mannequins",
              "Hose Lines & Nozzles",
              "Forcible Entry & Ventilation Tools",
              "Rescue Tools",
              "Fitness and Conditioning Equipment"
            )
          ),
          checkboxGroupInput(
            "training_incidents",
            "Incident Types Responded To",
            choices = c("Structure Fire", "Wildland", "Hazmat", "EMS", "Vehicle Incident", "Rescue")
          ),
          actionButton("generate_training", "Generate Training Plan"),
          textOutput("training_status"),
          textOutput("training_plan")
        )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  data_state <- reactiveVal(get_data())
  refresh_timer <- reactiveTimer(24 * 60 * 60 * 1000)
  llm_state <- reactiveVal(list(status = "LLM guidance not generated yet.", guidance = NULL))
  reports_state <- reactiveVal(list(status = "No reports analyzed yet.", analysis = NULL))
  training_state <- reactiveVal(list(status = "No training plan generated yet.", plan = NULL))

  observeEvent(data_state(), {
    df <- data_state()
    if (!all(is.na(df$state))) {
      states <- sort(unique(na.omit(df$state)))
      updateSelectInput(session, "state", choices = c("All", states))
      updateSelectInput(session, "state", selected = "All")
    }
  })

  filtered <- reactive({
    df <- data_state()

    if (input$geo_mode == "state") {
      if (!is.null(input$state) && input$state != "All" && !all(is.na(df$state))) {
        df <- df %>% filter(state == input$state)
      }
    } else {
      if (!is.null(input$region) && input$region != "All" && !all(is.na(df$region))) {
        df <- df %>% filter(region == input$region)
      }
    }

    if (!all(is.na(df$incident_category))) {
      allowed <- unique(c("Structure Fire", "Wildland", input$incident_types))
      df <- df %>% filter(incident_category %in% allowed)
    }

    df
  })

  observeEvent(refresh_timer(), {
    data_state(get_data(FALSE))
  })

  observeEvent(input$refresh_data, {
    data_state(get_data(TRUE))
  })

  output$data_source <- renderText({
    src <- attr(data_state(), "source")
    if (src == "refreshed") "Data source: refreshed from USFA API"
    else if (src == "cache") "Data source: cached USFA API download"
    else if (src == "local") "Data source: local file (ff_data.csv)"
    else "Data source: bundled sample data"
  })

  output$last_refreshed <- renderText({
    last <- read_last_refresh()
    if (is.na(last)) return("Last refreshed: not yet")
    paste("Last refreshed:", format(last, "%Y-%m-%d %H:%M %Z"))
  })

  output$data_source <- renderText({
    src <- attr(data_state(), "source")
    if (src == "refreshed") "Data source: refreshed from USFA API"
    else if (src == "cache") "Data source: cached USFA API download"
    else if (src == "local") "Data source: local file (ff_data.csv)"
    else "Data source: bundled sample data"
  })

  output$risk_summary <- renderText({
    df <- filtered()
    if (nrow(df) == 0) return("No records match the current filters.")

    top_cause <- df %>% count(cause, sort = TRUE) %>% slice(1) %>% pull(cause)
    top_incident <- df %>% count(incident_category, sort = TRUE) %>% slice(1) %>% pull(incident_category)

    region <- if (input$geo_mode == "state") {
      ifelse(input$state == "All", "all regions", input$state)
    } else {
      ifelse(input$region == "All", "all regions", input$region)
    }

    paste0(
      "For ", region, ", the most common fatality cause is ", top_cause, ", ",
      "and the most common incident type is ", top_incident, "."
    )
  })

  output$top_causes <- renderTable({
    df <- filtered()
    if (nrow(df) == 0) return(NULL)

    df %>%
      count(cause, sort = TRUE) %>%
      rename(`Cause of Death` = cause, `Count` = n) %>%
      head(5)
  })

  output$trend_plot <- renderPlot({
    df <- filtered()
    if (nrow(df) == 0 || all(is.na(df$year))) return(NULL)

    df %>%
      filter(!is.na(year)) %>%
      count(year) %>%
      ggplot(aes(x = year, y = n)) +
      geom_line(linewidth = 1.1, color = "#1b4f72") +
      geom_point(size = 2, color = "#1b4f72") +
      labs(x = "Year", y = "Fatalities") +
      theme_minimal(base_size = 12) +
      theme(
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        text = element_text(color = "#f7f3ef"),
        axis.text = element_text(color = "#c7c0b8"),
        axis.title = element_text(color = "#f7f3ef")
      )
  })

  output$incident_mix_plot <- renderPlot({
    df <- filtered()
    if (nrow(df) == 0 || all(is.na(df$year)) || all(is.na(df$incident_category))) return(NULL)

    palette <- c(
      "Structure Fire" = "#ff6a00",
      "Wildland" = "#d63230",
      "Hazmat" = "#ffb347",
      "EMS" = "#e0a800",
      "Vehicle Incident" = "#f05d23",
      "Rescue" = "#f2c94c",
      "Other" = "#6c757d"
    )

    df %>%
      filter(!is.na(year), !is.na(incident_category)) %>%
      count(year, incident_category) %>%
      ggplot(aes(x = year, y = n, fill = incident_category)) +
      geom_col() +
      scale_fill_manual(values = palette) +
      labs(x = "Year", y = "Fatalities", fill = "Incident Type") +
      theme_minimal(base_size = 12) +
      theme(
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        text = element_text(color = "#f7f3ef"),
        axis.text = element_text(color = "#c7c0b8"),
        axis.title = element_text(color = "#f7f3ef"),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "transparent", color = NA),
        legend.text = element_text(color = "#c7c0b8"),
        legend.title = element_text(color = "#f7f3ef")
      )
  })

  output$cause_plot <- renderPlot({
    df <- filtered()
    if (nrow(df) == 0 || all(is.na(df$cause))) return(NULL)

    top <- df %>%
      filter(!is.na(cause)) %>%
      count(cause, sort = TRUE) %>%
      slice_head(n = 6)

    ggplot(top, aes(x = reorder(cause, n), y = n)) +
      geom_col(fill = "#ff6a00") +
      coord_flip() +
      labs(x = NULL, y = "Fatalities") +
      theme_minimal(base_size = 12) +
      theme(
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        text = element_text(color = "#f7f3ef"),
        axis.text = element_text(color = "#c7c0b8"),
        axis.title = element_text(color = "#f7f3ef")
      )
  })

  output$guidance <- renderText({
    llm_guidance <- llm_state()$guidance
    if (!is.null(llm_guidance)) return(llm_guidance)
    "Click \"Generate Prevention Guidance\" to create tailored guidance with the LLM."
  })

  output$guidance_status <- renderText({
    llm_state()$status
  })

  observeEvent(input$analyze_reports, {
    df <- filtered()
    if (nrow(df) == 0) {
      reports_state(list(status = "No records match the current filters.", analysis = NULL))
      return()
    }

    key <- Sys.getenv("GEMINI_API_KEY")
    if (key == "") {
      reports_state(list(status = "Missing GEMINI_API_KEY. Set it in .Renviron or your session.", analysis = NULL))
      return()
    }

    reports_text <- str_trim(input$incident_reports)
    if (is.null(reports_text) || reports_text == "") {
      reports_state(list(status = "Please paste one or more incident reports.", analysis = NULL))
      return()
    }

    top_cause <- df %>% count(cause, sort = TRUE) %>% slice(1) %>% pull(cause)
    top_incidents <- df %>% count(incident_category, sort = TRUE) %>% slice(1:2) %>% pull(incident_category)
    trend <- df %>% count(year) %>% arrange(desc(year))

    trend_text <- paste0(
      "Top cause: ", top_cause, ". ",
      "Top incident types: ", paste(top_incidents, collapse = ", "), ". ",
      "Most recent year in data: ", ifelse(nrow(trend) > 0, trend$year[1], "unknown"), "."
    )

    region <- if (input$geo_mode == "state") {
      ifelse(input$state == "All", "all regions", input$state)
    } else {
      ifelse(input$region == "All", "all regions", input$region)
    }

    # Limit report length to keep requests reasonable
    if (nchar(reports_text) > 6000) {
      reports_text <- substr(reports_text, 1, 6000)
    }

    analysis <- tryCatch(
      generate_incident_analysis(region, trend_text, reports_text),
      error = function(e) NULL
    )

    if (is.null(analysis)) {
      reports_state(list(status = "LLM analysis failed. Try again.", analysis = NULL))
    } else {
      reports_state(list(status = "Analysis complete.", analysis = analysis))
    }
  })

  output$reports_status <- renderText({
    reports_state()$status
  })

  output$reports_analysis <- renderText({
    reports_state()$analysis
  })

  observeEvent(input$generate_training, {
    df <- filtered()
    if (nrow(df) == 0) {
      training_state(list(status = "No records match the current filters.", plan = NULL))
      return()
    }

    key <- Sys.getenv("GEMINI_API_KEY")
    if (key == "") {
      training_state(list(status = "Missing GEMINI_API_KEY. Set it in .Renviron or your session.", plan = NULL))
      return()
    }

    top_cause <- df %>% count(cause, sort = TRUE) %>% slice(1) %>% pull(cause)
    top_incidents <- df %>% count(incident_category, sort = TRUE) %>% slice(1:2) %>% pull(incident_category)
    trend <- df %>% count(year) %>% arrange(desc(year))

    trend_text <- paste0(
      "Top cause: ", top_cause, ". ",
      "Top incident types: ", paste(top_incidents, collapse = ", "), ". ",
      "Most recent year in data: ", ifelse(nrow(trend) > 0, trend$year[1], "unknown"), "."
    )

    region <- if (input$geo_mode == "state") {
      ifelse(input$state == "All", "all regions", input$state)
    } else {
      ifelse(input$region == "All", "all regions", input$region)
    }

    equipment_text <- if (!is.null(input$training_equipment) && length(input$training_equipment) > 0) {
      paste(input$training_equipment, collapse = ", ")
    } else {
      "No specific training equipment listed."
    }

    incident_response_text <- if (!is.null(input$training_incidents) && length(input$training_incidents) > 0) {
      paste(input$training_incidents, collapse = ", ")
    } else {
      "No incident response types listed."
    }

    trend_text <- paste0(
      trend_text,
      " Equipment available: ", equipment_text, ".",
      " Incident response types: ", incident_response_text, "."
    )

    plan <- tryCatch(
      generate_training_plan(region, trend_text),
      error = function(e) NULL
    )

    if (is.null(plan)) {
      training_state(list(status = "Training plan generation failed. Try again.", plan = NULL))
    } else {
      training_state(list(status = "Training plan generated.", plan = plan))
    }
  })

  output$training_status <- renderText({
    training_state()$status
  })

  output$training_plan <- renderText({
    training_state()$plan
  })

  observeEvent(input$run_llm, {
    df <- filtered()
    if (nrow(df) == 0) {
      llm_state(list(status = "No records to analyze.", guidance = NULL))
      return()
    }

    key <- Sys.getenv("GEMINI_API_KEY")
    if (key == "") {
      llm_state(list(status = "Missing GEMINI_API_KEY. Set it in .Renviron or your session.", guidance = NULL))
      return()
    }

    sample_df <- df %>%
      mutate(row_id = row_number()) %>%
      filter(!is.na(narrative) & !str_detect(narrative, regex("^not available", ignore_case = TRUE))) %>%
      slice_head(n = 30)

    if (nrow(sample_df) == 0) {
      llm_state(list(status = "No usable narratives found for LLM analysis.", guidance = NULL))
      return()
    }

    results <- sample_df %>% select(row_id, narrative)

    withProgress(message = "Classifying narratives...", value = 0, {
      results$llm_category <- NA_character_
      for (i in seq_len(nrow(results))) {
        incProgress(1 / nrow(results))
        results$llm_category[i] <- tryCatch(
          classify_narrative(results$narrative[i]),
          error = function(e) NA_character_
        )
      }
    })

    comment_text <- ifelse(
      is.null(input$dept_comments) || str_trim(input$dept_comments) == "",
      "No additional department comments provided.",
      paste("Department comments:", input$dept_comments)
    )

    region_text <- if (input$geo_mode == "state") {
      ifelse(input$state == "All", "All regions", input$state)
    } else {
      ifelse(input$region == "All", "All regions", input$region)
    }

    cause_counts <- df %>% count(cause, sort = TRUE) %>% slice_head(n = 3)
    incident_counts <- df %>% count(incident_category, sort = TRUE) %>% slice_head(n = 3)
    llm_counts <- results %>% count(llm_category, sort = TRUE)

    cause_text <- paste(paste0(cause_counts$cause, " (", cause_counts$n, ")"), collapse = ", ")
    incident_text <- paste(paste0(incident_counts$incident_category, " (", incident_counts$n, ")"), collapse = ", ")
    llm_text <- if (nrow(llm_counts) > 0) {
      paste(paste0(llm_counts$llm_category, " (", llm_counts$n, ")"), collapse = ", ")
    } else {
      "No LLM categories detected"
    }

    summary_text <- paste(
      "Region:", region_text,
      "Department type:", input$dept_type,
      "Department size:", input$dept_size,
      "Top causes:", cause_text,
      "Top incident types:", incident_text,
      "LLM narrative categories:", llm_text,
      comment_text
    )

    err_msg <- NULL
    guidance <- tryCatch(
      generate_llm_guidance(summary_text),
      error = function(e) {
        err_msg <<- e$message
        NULL
      }
    )

    if (is.null(guidance) || is.na(guidance) || guidance == "") {
      status_msg <- "LLM call failed. Check your GEMINI_API_KEY and try again."
      if (!is.null(err_msg)) {
        status_msg <- paste(status_msg, "Error:", err_msg)
      }
      llm_state(list(status = status_msg, guidance = NULL))
    } else {
      llm_state(list(status = "LLM guidance generated.", guidance = guidance))
    }
  })
}

shinyApp(ui, server)
