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

  out$cause <- str_replace_all(out$cause, regex("^struck\\s*by$", ignore_case = TRUE), "Struck")
  out$department_type <- normalize_dept_type(out$department_type)
  out$incident_category <- classify_incident(out)
  out$region <- state_to_region(out$state)
  out
}

# ---- Department type normalization ----
normalize_dept_type <- function(x) {
  x <- str_trim(as.character(x))
  x <- ifelse(is.na(x) | x == "", "Unknown", x)
  case_when(
    str_detect(x, regex("volunteer", ignore_case = TRUE)) ~ "Volunteer",
    str_detect(x, regex("career", ignore_case = TRUE)) ~ "Career",
    str_detect(x, regex("wildland.*contract|contract.*wildland", ignore_case = TRUE)) ~ "Wildland Contract",
    str_detect(x, regex("wildland.*full", ignore_case = TRUE)) ~ "Wildland Full-Time",
    str_detect(x, regex("wildland.*part", ignore_case = TRUE)) ~ "Wildland Part-Time",
    str_detect(x, regex("paid[- ]?on[- ]?call", ignore_case = TRUE)) ~ "Paid-on-Call",
    str_detect(x, regex("part[- ]?time", ignore_case = TRUE)) ~ "Part-Time (Paid)",
    str_detect(x, regex("industrial", ignore_case = TRUE)) ~ "Industrial",
    TRUE ~ "Unknown"
  )
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

## Narrative classification removed per request

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
    "You are analyzing firefighter incident reports.",
    "Extract and list specific hazards and risk factors mentioned or implied in the reports.",
    "Use the region context and historical trends provided to prioritize the risks.",
    "Return output in this format:",
    "Hazards & Risk Factors:",
    "- ...",
    "- ...",
    "Recommendations:",
    "- ...",
    "- ...",
    "Keep the tone clear and practical for fire department leadership."
  )

  input_text <- paste(
    "Region:", region,
    "\nHistorical trends:", trends_text,
    "\nIncident reports:", reports_text
  )

  call_gemini(input_text, instructions, model = model)
}

# ---- Formatting: incident analysis ----
format_incident_analysis <- function(text) {
  if (is.null(text) || is.na(text) || str_trim(text) == "") return(NULL)
  x <- str_replace_all(text, "\\r", "")
  parts <- str_split(x, "(?i)Recommendations\\s*:", n = 2, simplify = TRUE)
  hazards_raw <- str_trim(parts[1])
  recs_raw <- if (ncol(parts) > 1) str_trim(parts[2]) else ""

  extract_bullets <- function(block) {
    lines <- unlist(str_split(block, "\\n"))
    lines <- str_trim(lines)
    lines <- lines[lines != ""]
    lines <- str_replace(lines, "^[\\-*•]+\\s*", "")
    lines <- str_replace(lines, "^\\d+\\.\\s*", "")
    # Remove leading section header if present
    lines <- lines[!str_detect(lines, regex("^hazards?\\s*&?\\s*risk", ignore_case = TRUE))]
    lines
  }

  hazards <- extract_bullets(hazards_raw)
  recs <- extract_bullets(recs_raw)

  tags$div(
    tags$div(
      class = "card",
      tags$div(class = "card-title", "Hazards & Risk Factors"),
      tags$ul(lapply(hazards, tags$li))
    ),
    if (length(recs) > 0)
      tags$div(
        class = "card",
        tags$div(class = "card-title", "Recommendations"),
        tags$ul(lapply(recs, tags$li))
      ) else NULL
  )
}

# ---- LLM: training plan generation ----
generate_training_plan <- function(region, trends_text, model = default_model) {
  instructions <- paste(
    "Create a 12-month firefighter training plan tailored to the region and trends provided.",
    "Return a numbered list with one monthly training activity per month.",
    "Each item should be 1-2 sentences and include the training focus and a brief objective.",
    "Only include training activities that can be supported by the listed available equipment."
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

# ---- Training plan parsing ----
parse_training_plan <- function(text) {
  if (is.null(text) || is.na(text) || str_trim(text) == "") return(NULL)

  # Normalize whitespace
  x <- str_replace_all(text, "\\r", "")

  # Split on numbered items (1., 2., 3., etc.)
  parts <- str_split(x, "\\n?\\s*\\d+\\.\\s*", simplify = TRUE)
  parts <- parts[parts != ""]
  if (length(parts) == 0) return(NULL)

  rows <- lapply(parts, function(p) {
    p <- str_trim(p)
    # Extract Month title
    month_match <- str_match(p, "(?i)month\\s*\\d+\\s*:\\s*([^\\n\\*]+)")
    month_title <- if (!is.na(month_match[1, 2])) str_trim(month_match[1, 2]) else NA_character_

    focus_match <- str_match(p, "(?i)focus\\s*:\\s*([^\\n]+)")
    focus <- if (!is.na(focus_match[1, 2])) str_trim(focus_match[1, 2]) else NA_character_

    obj_match <- str_match(p, "(?i)objective\\s*:\\s*([^\\n]+)")
    objective <- if (!is.na(obj_match[1, 2])) str_trim(obj_match[1, 2]) else NA_character_

    list(
      Month = if (!is.na(month_title)) month_title else str_trim(str_extract(p, "^[^\\n]+")),
      Focus = focus,
      Objective = objective
    )
  })

  df <- do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
  if (nrow(df) == 0) return(NULL)
  df
}

# ---- Odds ratios (within fatality records) ----
compute_incident_odds <- function(df, top_n = 6) {
  if (nrow(df) == 0) return(NULL)

  df <- df %>%
    filter(!is.na(incident_category), !is.na(department_type)) %>%
    mutate(
      department_type = str_trim(department_type),
      incident_category = str_trim(incident_category)
    ) %>%
    filter(department_type != "Unknown")

  if (nrow(df) == 0) return(NULL)

  top_inc <- df %>% count(incident_category, sort = TRUE) %>% slice_head(n = top_n) %>% pull(incident_category)
  df <- df %>% filter(incident_category %in% top_inc)

  counts <- df %>%
    count(department_type, incident_category, name = "n")

  totals_by_type <- counts %>%
    group_by(department_type) %>%
    summarize(total = sum(n), .groups = "drop")

  overall_by_incident <- counts %>%
    group_by(incident_category) %>%
    summarize(overall_n = sum(n), .groups = "drop")

  overall_total <- sum(counts$n)

  out <- counts %>%
    left_join(totals_by_type, by = "department_type") %>%
    left_join(overall_by_incident, by = "incident_category") %>%
    mutate(
      other = total - n,
      odds = (n + 0.5) / (other + 0.5),
      overall_other = overall_total - overall_n,
      overall_odds = (overall_n + 0.5) / (overall_other + 0.5),
      or = odds / overall_odds
    )

  if (nrow(out) == 0) return(NULL)
  out
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
      .card { background: #141417; border: 1px solid #2a2a2f; border-radius: 12px; padding: 12px 14px; margin: 10px 0; box-shadow: 0 6px 16px rgba(0,0,0,0.25); }
      .card-title { font-family: 'Bebas Neue', sans-serif; letter-spacing: 0.5px; color: #ffb347; margin-bottom: 6px; }
      .card ul { margin: 0 0 0 16px; }
      .nav-tabs { border-bottom: 1px solid #2a2a2f; }
      .nav-tabs > li > a { color: var(--muted); background: #111114; border: 1px solid #2a2a2f; margin-right: 6px; border-radius: 8px 8px 0 0; }
      .nav-tabs > li > a:hover { color: var(--text); background: #1a1a1d; }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus { color: var(--text); background: #1c1c1f; border-bottom-color: transparent; }
      .global-spinner { display: none; position: fixed; top: 16px; right: 20px; width: 22px; height: 22px; border: 3px solid #2a2a2f; border-top-color: #ff6a00; border-radius: 50%; animation: spin 0.8s linear infinite; z-index: 9999; }
      .global-spinner.show { display: inline-block; }
      .btn-inline { display: inline-flex; align-items: center; gap: 8px; }
      .inline-spinner { width: 16px; height: 16px; border: 2px solid #2a2a2f; border-top-color: #ffb347; border-radius: 50%; animation: spin 0.8s linear infinite; }
      @keyframes spin { to { transform: rotate(360deg); } }
    "))
    ,
    tags$script(HTML("
      $(document).on('shiny:busy', function() { $('#global-spinner').addClass('show'); });
      $(document).on('shiny:idle', function() { $('#global-spinner').removeClass('show'); });
    "))
  ),
  tags$div(id = "global-spinner", class = "global-spinner"),

  tags$div(
    class = "app-header",
    tags$div(
      class = "helmet",
      HTML("<svg viewBox='0 0 64 64' xmlns='http://www.w3.org/2000/svg'><path d='M8 36c0-14 11-26 24-26s24 12 24 26v10H8V36z' fill='#ff6a00'/><path d='M14 36c0-10 8-18 18-18s18 8 18 18v4H14v-4z' fill='#d63230'/><path d='M6 46h52v8H6z' fill='#ffb347'/><circle cx='32' cy='30' r='6' fill='#0b0b0c'/></svg>")
    ),
    tags$div(
      tags$h1(class = "app-title", "Firefighter Fatality Risk Dashboard"),
      tags$p(class = "app-subtitle", "Auto refreshes daily"),
      tags$p(class = "app-subtitle", textOutput("last_refreshed"))
    )
  ),

  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      tags$div(textOutput("data_source")),
      
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
      checkboxGroupInput(
        "dept_type",
        "Department Makeup (Select All That Apply)",
        choices = c(
          "Volunteer",
          "Career",
          "Wildland Contract",
          "Wildland Full-Time",
          "Wildland Part-Time",
          "Paid-on-Call",
          "Part-Time (Paid)",
          "Industrial"
        )
      ),
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
        "Types of Incidents Responded To",
        choices = c("Structure Fire", "Wildland", "Hazmat", "EMS", "Vehicle Incident", "Rescue"),
        selected = c("Structure Fire")
      ),
      tags$div(style = "color: #ffb347; margin-top: 6px;", textOutput("incident_error")),

    ),

    mainPanel(
      class = "main",
      tabsetPanel(
        tabPanel(
          "Regional Risks",
          h3("Fatality Risk Summary"),
          textOutput("risk_summary"),
          tableOutput("top_causes"),
          plotOutput("trend_plot", height = "250px"),
          h3("Incident Mix Over Time"),
          plotOutput("incident_mix_plot", height = "300px"),
          h3("Top Causes (Selected Filters)"),
          plotOutput("cause_plot", height = "260px"),
          NULL
        ),
        tabPanel(
          "Personnel",
          h3("Incident Type Odds by Personnel Type (Your Region)"),
          tags$p(
            "Shows how incident types are over/under-represented in fatality records by personnel type in your selected region. Reference group: overall average. This is not a population risk estimate.",
            style = "color: var(--muted);"
          ),
          plotOutput("odds_plot", height = "320px"),
          tags$div(style = "margin-top: 12px;", uiOutput("odds_summary"))
        ),
        tabPanel(
          "Prevention Guidance",
          h3("Prevention Guidance"),
          tags$p(
            "Generate tailored prevention guidance based on your region, incident types, department makeup, and challenges.",
            style = "color: var(--muted);"
          ),
          tags$div(
            class = "btn-inline",
            actionButton("run_llm", "Generate Prevention Guidance"),
            conditionalPanel("output.guidance_busy == true", tags$span(class = "inline-spinner"))
          ),
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
          tags$div(
            class = "btn-inline",
            actionButton("analyze_reports", "Analyze Incident Reports"),
            conditionalPanel("output.reports_busy == true", tags$span(class = "inline-spinner"))
          ),
          textOutput("reports_status"),
          uiOutput("reports_analysis")
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
              "Forcible Entry Props",
              "Live-Fire Props",
              "Search and Rescue",
              "Hose/Nozzle Handling",
              "Ventilation Tools",
              "Physical Conditioning Equipment"
            )
          ),
          tags$div(
            class = "btn-inline",
            actionButton("generate_training", "Generate Training Plan"),
            conditionalPanel("output.training_busy == true", tags$span(class = "inline-spinner"))
          ),
          textOutput("training_status"),
          tableOutput("training_table"),
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
  guidance_busy <- reactiveVal(FALSE)
  reports_busy <- reactiveVal(FALSE)
  training_busy <- reactiveVal(FALSE)

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

    if (!is.null(input$incident_types) && length(input$incident_types) > 0 && !all(is.na(df$incident_category))) {
      df <- df %>% filter(incident_category %in% input$incident_types)
    }

    df
  })

  output$incident_error <- renderText({
    if (is.null(input$incident_types) || length(input$incident_types) == 0) {
      "Please select at least one incident type that your department responds to."
    } else {
      ""
    }
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
    if (is.null(input$incident_types) || length(input$incident_types) == 0) {
      return("Please select at least one incident type that your department responds to.")
    }
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
    if (is.null(input$incident_types) || length(input$incident_types) == 0) return(NULL)
    df <- filtered()
    if (nrow(df) == 0) return(NULL)

    df %>%
      count(cause, sort = TRUE) %>%
      rename(`Cause of Death` = cause, `Count` = n) %>%
      head(5)
  })

  output$trend_plot <- renderPlot({
    if (is.null(input$incident_types) || length(input$incident_types) == 0) return(NULL)
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
    if (is.null(input$incident_types) || length(input$incident_types) == 0) return(NULL)
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
    if (is.null(input$incident_types) || length(input$incident_types) == 0) return(NULL)
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

  output$odds_plot <- renderPlot({
    if (is.null(input$incident_types) || length(input$incident_types) == 0) return(NULL)
    df <- filtered()
    odds_df <- compute_incident_odds(df, top_n = 6)
    if (is.null(odds_df) || nrow(odds_df) == 0) return(NULL)

    selected_personnel <- if (!is.null(input$dept_type) && length(input$dept_type) > 0) {
      input$dept_type
    } else {
      character()
    }

    odds_df <- odds_df %>%
      mutate(
        incident_category = reorder(incident_category, or, FUN = median),
        legend_group = ifelse(department_type %in% selected_personnel, department_type, "Unselected"),
        is_selected = department_type %in% selected_personnel
      )

    selected_palette <- c(
      "Volunteer" = "#ffb347",
      "Career" = "#ff6a00",
      "Wildland Contract" = "#f05d23",
      "Wildland Full-Time" = "#d63230",
      "Wildland Part-Time" = "#ff8f1f",
      "Paid-on-Call" = "#f2c94c",
      "Part-Time (Paid)" = "#ffa07a",
      "Industrial" = "#ffd166"
    )
    palette <- c(selected_palette[selected_personnel], "Unselected" = "#6c757d")

    x_left <- min(odds_df$or, na.rm = TRUE) * 0.85
    x_right <- max(odds_df$or, na.rm = TRUE) * 1.15

    ggplot(odds_df, aes(x = or, y = incident_category, color = legend_group)) +
      geom_vline(xintercept = 1, linetype = "dashed", color = "#c7c0b8") +
      geom_segment(aes(x = 1, xend = or, yend = incident_category), linewidth = 0.7, alpha = 0.6) +
      geom_point(aes(size = is_selected, alpha = is_selected), position = position_jitter(height = 0.22, width = 0)) +
      annotate("text", x = x_left, y = -Inf, label = "Less represented in fatality data", vjust = -0.6, hjust = 0, color = "#c7c0b8", size = 3) +
      annotate("text", x = x_right, y = -Inf, label = "More represented in fatality data", vjust = -0.6, hjust = 1, color = "#c7c0b8", size = 3) +
      scale_color_manual(values = palette, breaks = selected_personnel, na.value = "#c7c0b8") +
      scale_size_manual(values = c(`TRUE` = 3.2, `FALSE` = 2.2), guide = "none") +
      scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.6), guide = "none") +
      scale_x_log10() +
      coord_cartesian(clip = "off") +
      labs(
        x = "Odds Ratio vs Overall Average (log scale)",
        y = "Incident Type",
        color = "Personnel Type",
        caption = "Reference group: overall average"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(10, 12, 28, 12),
        text = element_text(color = "#f7f3ef"),
        axis.text = element_text(color = "#c7c0b8"),
        axis.title = element_text(color = "#f7f3ef"),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "transparent", color = NA),
        legend.text = element_text(color = "#c7c0b8"),
        legend.title = element_text(color = "#f7f3ef"),
        plot.caption = element_text(color = "#c7c0b8", hjust = 0)
      )
  })

  output$odds_summary <- renderUI({
    if (is.null(input$incident_types) || length(input$incident_types) == 0) {
      return(tags$div("Select at least one incident type to see personnel insights."))
    }
    df <- filtered()
    odds_df <- compute_incident_odds(df, top_n = 6)
    if (is.null(odds_df) || nrow(odds_df) == 0) {
      return(tags$div("No personnel-level patterns available for the current filters."))
    }

    selected_personnel <- if (!is.null(input$dept_type) && length(input$dept_type) > 0) {
      input$dept_type
    } else {
      character()
    }

    if (length(selected_personnel) == 0) {
      return(tags$div("Select one or more personnel types to see tailored context for your department."))
    }

    summaries <- lapply(selected_personnel, function(p) {
      subset <- odds_df %>% filter(department_type == p)
      if (nrow(subset) == 0) return(NULL)

      if (nrow(subset) == 1) {
        row <- subset[1, ]
        direction <- if (!is.na(row$or) && row$or >= 1) "more" else "fewer"
        return(paste0(
          p, " firefighters have historically experienced ", direction,
          " fatalities in ", row$incident_category,
          " incidents on average (relative to the overall average)."
        ))
      }

      higher <- subset %>% arrange(desc(or)) %>% slice(1)
      lower <- subset %>% arrange(or) %>% slice(1)

      paste0(
        p, " firefighters have historically experienced more fatalities in ",
        higher$incident_category, " incidents on average, and fewer in ",
        lower$incident_category, " incidents (relative to the overall average)."
      )
    })

    summaries <- summaries[!vapply(summaries, is.null, logical(1))]
    if (length(summaries) == 0) {
      return(tags$div("No personnel-level patterns available for the current filters."))
    }

    tagList(lapply(summaries, tags$p))
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
    reports_busy(TRUE)
    on.exit(reports_busy(FALSE), add = TRUE)
    if (is.null(input$incident_types) || length(input$incident_types) == 0) {
      reports_state(list(status = "Please select at least one incident type that your department responds to.", analysis = NULL))
      return()
    }
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

  output$reports_analysis <- renderUI({
    format_incident_analysis(reports_state()$analysis)
  })

  output$guidance_busy <- reactive({ guidance_busy() })
  output$reports_busy <- reactive({ reports_busy() })
  output$training_busy <- reactive({ training_busy() })
  outputOptions(output, "guidance_busy", suspendWhenHidden = FALSE)
  outputOptions(output, "reports_busy", suspendWhenHidden = FALSE)
  outputOptions(output, "training_busy", suspendWhenHidden = FALSE)

  observeEvent(input$generate_training, {
    training_busy(TRUE)
    on.exit(training_busy(FALSE), add = TRUE)
    if (is.null(input$incident_types) || length(input$incident_types) == 0) {
      training_state(list(status = "Please select at least one incident type that your department responds to.", plan = NULL))
      return()
    }
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

    incident_response_text <- if (!is.null(input$incident_types) && length(input$incident_types) > 0) {
      paste(input$incident_types, collapse = ", ")
    } else {
      "No incident response types listed."
    }

    comment_text <- ifelse(
      is.null(input$dept_comments) || str_trim(input$dept_comments) == "",
      "No additional department comments provided.",
      paste("Department comments:", input$dept_comments)
    )

    trend_text <- paste0(
      trend_text,
      " Equipment available: ", equipment_text, ".",
      " Incident response types: ", incident_response_text, ".",
      " ", comment_text
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

  output$training_table <- renderTable({
    plan <- training_state()$plan
    parsed <- parse_training_plan(plan)
    if (is.null(parsed)) return(NULL)
    parsed
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  output$training_plan <- renderText({
    plan <- training_state()$plan
    parsed <- parse_training_plan(plan)
    if (!is.null(parsed)) return("")
    plan
  })

  observeEvent(input$run_llm, {
    guidance_busy(TRUE)
    on.exit(guidance_busy(FALSE), add = TRUE)
    if (is.null(input$incident_types) || length(input$incident_types) == 0) {
      llm_state(list(status = "Please select at least one incident type that your department responds to.", guidance = NULL))
      return()
    }
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

    cause_text <- paste(paste0(cause_counts$cause, " (", cause_counts$n, ")"), collapse = ", ")
    incident_text <- paste(paste0(incident_counts$incident_category, " (", incident_counts$n, ")"), collapse = ", ")

    dept_type_text <- if (!is.null(input$dept_type) && length(input$dept_type) > 0) {
      paste(input$dept_type, collapse = ", ")
    } else {
      "Not provided"
    }

    summary_text <- paste(
      "Region:", region_text,
      "Department makeup:", dept_type_text,
      "Department size:", input$dept_size,
      "Top causes:", cause_text,
      "Top incident types:", incident_text,
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
