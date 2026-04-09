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
library(plotly)

# ---- Configuration ----
data_path <- "/Users/amyfulton/Downloads/ff_data.csv"
sample_path <- "data/sample_fatalities.csv"
cache_path <- "data/fatalities.csv"
last_refresh_path <- "data/last_refresh.txt"
feed_url <- "https://apps.usfa.fema.gov/firefighter-fatalities/api/fatalityDatums/feed"
download_url <- "https://apps.usfa.fema.gov/firefighter-fatalities/api/csv"
default_model <- "gemini-2.5-flash"
census_year <- 2022

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
  col_age <- pick_col(df, c("age"))
  col_rank <- pick_col(df, c("rank", "position"))
  col_incident_date <- pick_col(df, c("incident_date", "date_of_death"))
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
    age = if (!is.na(col_age)) suppressWarnings(as.integer(df[[col_age]])) else NA_integer_,
    rank = if (!is.na(col_rank)) as.character(df[[col_rank]]) else NA_character_,
    incident_date = if (!is.na(col_incident_date)) parse_date(df[[col_incident_date]]) else as.Date(NA),
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
  out$rank_group <- rank_group(out$rank)
  out$state_name <- state_name_from_abbr(out$state)
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

parse_date <- function(x) {
  x <- as.character(x)
  suppressWarnings(
    coalesce(
      lubridate::mdy(x),
      lubridate::ymd(x),
      lubridate::dmy(x)
    )
  )
}

state_name_from_abbr <- function(x) {
  x <- toupper(str_trim(x))
  lookup <- setNames(state.name, state.abb)
  unname(lookup[x])
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
    "You are a fire service safety analyst writing guidance for fire department leadership.",
    "Interpret results as associations, not causation.",
    "Be practical, concise, and professional.",
    "Consider how disaster frequency may influence firefighter operational risk.",
    "Consider demographic vulnerability such as age structure and poverty.",
    "Consider evacuation challenges where vehicle access is limited and housing density is high.",
    "Return structured output with these sections:",
    "Risk Overview (2-3 sentences)",
    "Training Recommendations (bullet points)",
    "Health and Safety Interventions (bullet points)",
    "Equipment and Resource Needs (bullet points)",
    "Operational Policy Considerations (bullet points)",
    "Keep total length under 250 words."
  )
  call_gemini(summary_text, instructions, model = model)
}

# ---- LLM: incident report analysis ----
generate_incident_analysis <- function(region, trends_text, reports_text, model = default_model) {
  instructions <- paste(
    "You are analyzing firefighter incident reports.",
    "Extract and list specific hazards and risk factors mentioned or implied in the reports.",
    "Use the historical fatality trends (including common duties and activities) as explicit risk factors when interpreting the reports.",
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

# ---- LLM: profile recent report summary ----
generate_profile_summary <- function(profile_text, reports_text, model = default_model) {
  instructions <- paste(
    "You are summarizing the 10 most recent incident reports that match a firefighter profile.",
    "Identify key hazards, duties, and activities that appear across the reports.",
    "Provide 3-5 concise bullet points for risks and 3-5 bullet points for recommendations.",
    "Recommendations must be individual-level actions a firefighter can take (not department-wide policies).",
    "Return output in this format:",
    "Risks:",
    "- ...",
    "- ...",
    "Recommendations:",
    "- ...",
    "- ...",
    "Keep the tone clear and practical."
  )

  input_text <- paste(
    "Profile:", profile_text,
    "\nRecent incident reports:", reports_text
  )

  call_gemini(input_text, instructions, model = model)
}

# ---- LLM: disaster preparedness ----
generate_disaster_plan <- function(summary_text, model = default_model) {
  instructions <- paste(
    "You are a fire service disaster preparedness expert.",
    "Focus on operational readiness, be practical and specific.",
    "Do not overstate certainty.",
    "Consider how population density affects response complexity.",
    "Consider whether mitigation investment and fire station assistance suggest readiness gaps.",
    "Return sections:",
    "Risk Overview (2-3 sentences)",
    "Preparedness Priorities (bullet points)",
    "Training Needs (bullet points)",
    "Equipment and Resource Planning (bullet points)",
    "Mutual Aid and Surge Capacity Considerations (bullet points)",
    "Keep under 250 words."
  )
  call_gemini(summary_text, instructions, model = model)
}
# ---- Census + FEMA data ----
get_census_data <- function(year = census_year) {
  key <- Sys.getenv("CENSUS_API_KEY")
  if (key == "") return(NULL)

  elderly_vars <- c(
    "B01001_020E","B01001_021E","B01001_022E","B01001_023E","B01001_024E","B01001_025E",
    "B01001_044E","B01001_045E","B01001_046E","B01001_047E","B01001_048E","B01001_049E"
  )
  poverty_vars <- c("B17001_001E","B17001_002E")
  vehicle_vars <- c("B08201_001E","B08201_002E")
  housing_vars <- c("B25001_001E")

  url <- paste0(
    "https://api.census.gov/data/",
    year,
    "/acs/acs5?get=NAME,B01003_001E,B01002_001E,B19013_001E,",
    paste(c(elderly_vars, poverty_vars, vehicle_vars, housing_vars), collapse = ","),
    "&for=state:*&key=",
    key
  )

  resp <- tryCatch(httr::GET(url), error = function(e) NULL)
  if (is.null(resp) || httr::status_code(resp) >= 400) return(NULL)

  raw <- tryCatch(jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8")), error = function(e) NULL)
  if (is.null(raw) || length(raw) <= 1) return(NULL)

  df <- as.data.frame(raw, stringsAsFactors = FALSE)
  names(df) <- df[1, ]
  df <- df[-1, ]

  df <- df %>%
    transmute(
      NAME,
      population = as.numeric(B01003_001E),
      median_age = as.numeric(B01002_001E),
      median_income = as.numeric(B19013_001E),
      elderly_pop = rowSums(across(all_of(elderly_vars), as.numeric), na.rm = TRUE),
      poverty_total = as.numeric(B17001_001E),
      poverty_below = as.numeric(B17001_002E),
      households_total = as.numeric(B08201_001E),
      households_no_vehicle = as.numeric(B08201_002E),
      housing_units = as.numeric(B25001_001E),
      state = state.abb[match(NAME, state.name)]
    )

  df %>%
    mutate(
      pct_elderly = ifelse(population > 0, elderly_pop / population, NA_real_),
      poverty_rate = ifelse(poverty_total > 0, poverty_below / poverty_total, NA_real_),
      no_vehicle_rate = ifelse(households_total > 0, households_no_vehicle / households_total, NA_real_)
    )
}

get_fema_data <- function(max_pages = 5, page_size = 5000) {
  base <- "https://www.fema.gov/api/open/v2/DisasterDeclarationsSummaries"
  all_rows <- list()

  for (i in seq_len(max_pages)) {
    skip <- (i - 1) * page_size
    url <- paste0(
      base,
      "?$select=state,declarationDate",
      "&$top=", page_size,
      "&$skip=", skip
    )
    resp <- tryCatch(httr::GET(url), error = function(e) NULL)
    if (is.null(resp) || httr::status_code(resp) >= 400) break

    raw <- tryCatch(jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8")), error = function(e) NULL)
    if (is.null(raw$DisasterDeclarationsSummaries) || nrow(raw$DisasterDeclarationsSummaries) == 0) break
    all_rows[[length(all_rows) + 1]] <- raw$DisasterDeclarationsSummaries

    if (nrow(raw$DisasterDeclarationsSummaries) < page_size) break
  }

  if (length(all_rows) == 0) return(NULL)

  bind_rows(all_rows) %>%
    mutate(state = toupper(state)) %>%
    group_by(state) %>%
    summarize(disaster_count = n(), .groups = "drop")
}

get_fema_types <- function(max_pages = 5, page_size = 5000) {
  base <- "https://www.fema.gov/api/open/v2/DisasterDeclarationsSummaries"
  all_rows <- list()

  for (i in seq_len(max_pages)) {
    skip <- (i - 1) * page_size
    url <- paste0(
      base,
      "?$select=state,incidentType",
      "&$top=", page_size,
      "&$skip=", skip
    )
    resp <- tryCatch(httr::GET(url), error = function(e) NULL)
    if (is.null(resp) || httr::status_code(resp) >= 400) break

    raw <- tryCatch(jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8")), error = function(e) NULL)
    if (is.null(raw$DisasterDeclarationsSummaries) || nrow(raw$DisasterDeclarationsSummaries) == 0) break
    all_rows[[length(all_rows) + 1]] <- raw$DisasterDeclarationsSummaries

    if (nrow(raw$DisasterDeclarationsSummaries) < page_size) break
  }

  if (length(all_rows) == 0) return(NULL)

  bind_rows(all_rows) %>%
    mutate(state = toupper(state)) %>%
    filter(!is.na(incidentType), incidentType != "") %>%
    count(state, incidentType, name = "count")
}

get_fema_declarations <- function(max_pages = 5, page_size = 5000) {
  base <- "https://www.fema.gov/api/open/v2/DisasterDeclarationsSummaries"
  all_rows <- list()

  for (i in seq_len(max_pages)) {
    skip <- (i - 1) * page_size
    url <- paste0(
      base,
      "?$select=state,incidentType,declarationDate,ihProgramDeclared,paProgramDeclared,declaredCountyArea",
      "&$top=", page_size,
      "&$skip=", skip
    )
    resp <- tryCatch(httr::GET(url), error = function(e) NULL)
    if (is.null(resp) || httr::status_code(resp) >= 400) break

    raw <- tryCatch(jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8")), error = function(e) NULL)
    if (is.null(raw$DisasterDeclarationsSummaries) || nrow(raw$DisasterDeclarationsSummaries) == 0) break
    all_rows[[length(all_rows) + 1]] <- raw$DisasterDeclarationsSummaries

    if (nrow(raw$DisasterDeclarationsSummaries) < page_size) break
  }

  if (length(all_rows) == 0) return(NULL)

  bind_rows(all_rows) %>%
    mutate(
      state = toupper(state),
      declarationDate = as.Date(declarationDate),
      year = suppressWarnings(lubridate::year(declarationDate))
    )
}

get_openfema_fields <- function(dataset, version = 2) {
  url <- paste0(
    "https://www.fema.gov/api/open/v1/OpenFemaDataSetFields?$filter=",
    "openFemaDataSet%20eq%20%27", dataset, "%27%20and%20datasetVersion%20eq%20", version
  )
  resp <- tryCatch(httr::GET(url), error = function(e) NULL)
  if (is.null(resp) || httr::status_code(resp) >= 400) return(NULL)
  raw <- tryCatch(jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8")), error = function(e) NULL)
  if (is.null(raw$OpenFemaDataSetFields)) return(NULL)
  raw$OpenFemaDataSetFields$name
}

get_hma_grants <- function(max_pages = 5, page_size = 5000) {
  fields <- get_openfema_fields("HmaSubapplications", 2)
  if (is.null(fields)) return(NULL)

  state_field <- pick_col(data.frame(name = fields), c("state", "applicantState", "projectState", "recipientState")) %>%
    (`[[`)(1)
  amount_field <- pick_col(data.frame(name = fields), c("federalShareAmount", "totalFederalShareAmount", "managementFederalShareAmount", "federalShareObligated", "federalShare", "federalShareObligatedAmount")) %>%
    (`[[`)(1)

  if (is.na(state_field)) return(NULL)
  select_fields <- if (!is.na(amount_field)) paste0(state_field, ",", amount_field) else state_field

  base <- "https://www.fema.gov/api/open/v2/HmaSubapplications"
  all_rows <- list()
  for (i in seq_len(max_pages)) {
    skip <- (i - 1) * page_size
    url <- paste0(
      base,
      "?$select=", select_fields,
      "&$top=", page_size,
      "&$skip=", skip
    )
    resp <- tryCatch(httr::GET(url), error = function(e) NULL)
    if (is.null(resp) || httr::status_code(resp) >= 400) break

    raw <- tryCatch(jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8")), error = function(e) NULL)
    if (is.null(raw$HmaSubapplications) || nrow(raw$HmaSubapplications) == 0) break
    all_rows[[length(all_rows) + 1]] <- raw$HmaSubapplications
    if (nrow(raw$HmaSubapplications) < page_size) break
  }

  if (length(all_rows) == 0) return(NULL)
  df <- bind_rows(all_rows)
  df <- df %>% mutate(state = toupper(.data[[state_field]]))

  if (!is.na(amount_field) && amount_field %in% names(df)) {
    df %>%
      mutate(amount = suppressWarnings(as.numeric(.data[[amount_field]]))) %>%
      group_by(state) %>%
      summarize(mitigation_investment = sum(amount, na.rm = TRUE), .groups = "drop")
  } else {
    df %>%
      group_by(state) %>%
      summarize(mitigation_investment = n(), .groups = "drop")
  }
}

get_pa_fire_applicants <- function(max_pages = 5, page_size = 5000) {
  base <- "https://www.fema.gov/api/open/v1/PublicAssistanceApplicants"
  all_rows <- list()
  for (i in seq_len(max_pages)) {
    skip <- (i - 1) * page_size
    url <- paste0(
      base,
      "?$select=state,applicantName",
      "&$filter=contains(applicantName,'Fire')",
      "&$top=", page_size,
      "&$skip=", skip
    )
    resp <- tryCatch(httr::GET(url), error = function(e) NULL)
    if (is.null(resp) || httr::status_code(resp) >= 400) break
    raw <- tryCatch(jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8")), error = function(e) NULL)
    if (is.null(raw$PublicAssistanceApplicants) || nrow(raw$PublicAssistanceApplicants) == 0) break
    all_rows[[length(all_rows) + 1]] <- raw$PublicAssistanceApplicants
    if (nrow(raw$PublicAssistanceApplicants) < page_size) break
  }
  if (length(all_rows) == 0) return(NULL)
  bind_rows(all_rows) %>%
    mutate(state = toupper(state)) %>%
    group_by(state) %>%
    summarize(fire_applicant_count = n(), .groups = "drop")
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
    "Start with exactly two sentences:",
    "Sentence 1 must start with 'Top concerns for your department are likely' and end with a period.",
    "Sentence 2 must start with 'Top training priorities to combat these concerns are' and end with a period.",
    "Then add a blank line, then the 12-month plan.",
    "Return exactly 12 items formatted exactly like:",
    "Month 1: Title",
    "Focus: ...",
    "Objective: ...",
    "Month 2: Title",
    "Focus: ...",
    "Objective: ...",
    "Continue through Month 12 with the same format.",
    "Keep Focus and Objective to one sentence each.",
    "Do not add any extra headings or introductory text.",
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

format_guidance_cards <- function(text) {
  if (is.null(text) || !nzchar(text)) {
    return(tags$div(class = "card", "Click \"Generate Prevention Guidance\" to create tailored guidance with the LLM."))
  }
  raw <- gsub("\r", "", text)
  chunks <- strsplit(raw, "\\*\\*")[[1]]
  if (length(chunks) < 3) {
    return(tags$div(class = "guidance-card", tags$p(raw)))
  }

  cards <- list()
  idx <- 2
  while (idx < length(chunks)) {
    title <- str_trim(chunks[[idx]])
    body <- str_trim(chunks[[idx + 1]])
    lines <- unlist(strsplit(body, "\n"))
    bullets <- str_trim(gsub("^\\*\\s*", "", lines[str_detect(lines, "^\\s*\\*")]))
    paragraph <- str_trim(paste(lines[!str_detect(lines, "^\\s*\\*")], collapse = " "))

    cards[[length(cards) + 1]] <- tags$div(
      class = "guidance-card",
      tags$div(class = "guidance-title", title),
      if (nzchar(paragraph)) tags$p(paragraph) else NULL,
      if (length(bullets) > 0) tags$ul(lapply(bullets, tags$li)) else NULL
    )
    idx <- idx + 2
  }

  tags$div(class = "guidance-grid", cards)
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
    TRUE ~ "Other region"
  )
}

# ---- Rank grouping ----
rank_group <- function(rank) {
  rank <- tolower(str_trim(rank))
  case_when(
    is.na(rank) | rank == "" ~ NA_character_,
    str_detect(rank, "chief|deputy|assistant|commissioner|marshal") ~ "Chief/Command",
    str_detect(rank, "captain|lieutenant|officer|supervisor|battalion") ~ "Officer",
    str_detect(rank, "engineer|driver|apparatus|operator") ~ "Driver/Engineer",
    str_detect(rank, "firefighter|ff|emt|paramedic|responder") ~ "Firefighter/EMT",
    TRUE ~ "Specialist/Other"
  )
}

# ---- Training plan parsing ----
parse_training_plan <- function(text) {
  if (is.null(text) || is.na(text) || str_trim(text) == "") return(NULL)

  # Normalize whitespace
  x <- str_replace_all(text, "\\r", "")
  x <- str_replace(x, "^(?i)here is.*?\\n", "")

  # Split on Month blocks
  parts <- unlist(str_split(x, "(?i)(?=\\bmonth\\s*\\d+\\s*:)"))
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
  if (all(is.na(df$Focus)) && all(is.na(df$Objective))) return(NULL)
  df
}

extract_training_intro <- function(text) {
  if (is.null(text) || is.na(text) || str_trim(text) == "") return(NULL)
  lines <- unlist(str_split(str_replace_all(text, "\\r", ""), "\\n"))
  lines <- str_trim(lines)
  lines <- lines[lines != ""]
  if (length(lines) == 0) return(NULL)

  concerns <- lines[str_detect(lines, "^Top concerns for your department are likely")]
  priorities <- lines[str_detect(lines, "^Top training priorities to combat these concerns are")]

  if (length(concerns) == 0 && length(priorities) == 0) return(NULL)

  list(
    concerns = if (length(concerns) > 0) concerns[1] else NULL,
    priorities = if (length(priorities) > 0) priorities[1] else NULL
  )
}

write_training_pdf <- function(file, intro, training_table) {
  grDevices::pdf(file, width = 8.5, height = 11)
  on.exit(grDevices::dev.off(), add = TRUE)

  grid::grid.newpage()
  y <- 0.95

  grid::grid.text(
    "Fire Department Training Plan",
    x = 0.5,
    y = y,
    gp = grid::gpar(fontsize = 18, fontface = "bold")
  )
  y <- y - 0.05

  if (!is.null(intro)) {
    intro_lines <- c()
    if (!is.null(intro$concerns)) intro_lines <- c(intro_lines, intro$concerns)
    if (!is.null(intro$priorities)) intro_lines <- c(intro_lines, intro$priorities)
    intro_text <- paste(intro_lines, collapse = "\n")
    for (line in strwrap(intro_text, width = 90)) {
      grid::grid.text(line, x = 0.06, y = y, just = "left", gp = grid::gpar(fontsize = 11))
      y <- y - 0.03
    }
    y <- y - 0.01
  }

  if (!is.null(training_table) && nrow(training_table) > 0) {
    for (i in seq_len(nrow(training_table))) {
      row <- training_table[i, ]
      line <- paste0(
        row$Month, ": ",
        row$Focus, " — ",
        row$Objective
      )
      for (wrap_line in strwrap(line, width = 95)) {
        if (y < 0.06) {
          grid::grid.newpage()
          y <- 0.95
        }
        grid::grid.text(wrap_line, x = 0.06, y = y, just = "left", gp = grid::gpar(fontsize = 10))
        y <- y - 0.027
      }
      y <- y - 0.008
    }
  } else {
    grid::grid.text("No training plan available.", x = 0.06, y = y, just = "left", gp = grid::gpar(fontsize = 11))
  }

  grid::grid.text(
    paste0("Generated on ", format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
    x = 0.5,
    y = 0.02,
    gp = grid::gpar(fontsize = 9, col = "gray50")
  )
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

personnel_palette <- c(
  "Volunteer" = "#ffb347",
  "Career" = "#ff6a00",
  "Wildland Contract" = "#f05d23",
  "Wildland Full-Time" = "#d63230",
  "Wildland Part-Time" = "#ff8f1f",
  "Paid-on-Call" = "#f2c94c",
  "Part-Time (Paid)" = "#ffa07a",
  "Industrial" = "#ffd166"
)

summarize_duty_activity <- function(df, selected_personnel) {
  if (nrow(df) == 0) return(NULL)

  df <- df %>%
    mutate(
      department_type = str_trim(department_type),
      duty = str_trim(duty),
      activity = str_trim(coalesce(incident_type, incident_category, emergency, property_type))
    ) %>%
    filter(!is.na(department_type), department_type != "Unknown")

  if (!is.null(selected_personnel) && length(selected_personnel) > 0) {
    df <- df %>% filter(department_type %in% selected_personnel)
  }

  duty_counts <- df %>%
    filter(!is.na(duty), duty != "", !str_detect(duty, regex("^other$", ignore_case = TRUE))) %>%
    count(department_type, category = duty, name = "n")

  activity_counts <- df %>%
    filter(!is.na(activity), activity != "", !str_detect(activity, regex("^other$", ignore_case = TRUE))) %>%
    count(department_type, category = activity, name = "n")

  list(
    duty = if (nrow(duty_counts) > 0) duty_counts else NULL,
    activity = if (nrow(activity_counts) > 0) activity_counts else NULL
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
      .sidebar-title { font-family: 'Bebas Neue', sans-serif; font-size: 22px; letter-spacing: 0.6px; color: #ffb347; margin-bottom: 8px; }
      .main { background: var(--panel-2); border: 1px solid #2a2a2f; border-radius: 14px; padding: 16px; }
      h3 { font-family: 'Bebas Neue', sans-serif; letter-spacing: 0.5px; color: var(--accent-3); margin-top: 10px; }
      .control-label { color: var(--muted); font-weight: 600; }
      .btn { background: linear-gradient(90deg, var(--accent-2), var(--accent)); border: none; color: #fff; font-weight: 700; }
      .btn:hover { filter: brightness(1.05); }
      a { color: #ffb347; }
      a:hover { color: #ff6a00; }
      .link-fatality a, .link-fatality { color: #ffb347 !important; }
      .link-plans a, .link-plans { color: #ff6a00 !important; }
      .link-individual a, .link-individual { color: #d94b2b !important; }
      .link-disaster a, .link-disaster { color: #c77700 !important; }
      .link-fatality strong, .link-plans strong, .link-individual strong, .link-disaster strong { color: #ffffff; }
      .link-fatality a:hover, .link-plans a:hover, .link-individual a:hover, .link-disaster a:hover { filter: brightness(1.1); }
      .tab-fatality-data { color: #ffb347; }
      .tab-fatality-plans { color: #ff6a00; }
      .tab-fatality-individual { color: #d94b2b; }
      .tab-disaster { color: #c77700; }
      .tab-welcome { color: #ffffff; }
      .nav-tabs > li > a .tab-fatality-data { color: #ffb347; }
      .nav-tabs > li > a .tab-fatality-plans { color: #ff6a00; }
      .nav-tabs > li > a .tab-fatality-individual { color: #d94b2b; }
      .nav-tabs > li > a .tab-disaster { color: #c77700; }
      .nav-tabs > li > a .tab-welcome { color: #ffffff; }
      table { color: var(--text); }
      .shiny-output-error-validation { color: #ffb347; }
      .info-box { background: #101012; border: 1px dashed #333; padding: 10px; border-radius: 10px; margin-top: 10px; }
      .card { background: #141417; border: 1px solid #2a2a2f; border-radius: 12px; padding: 12px 14px; margin: 10px 0; box-shadow: 0 6px 16px rgba(0,0,0,0.25); }
      .card-title { font-family: 'Bebas Neue', sans-serif; letter-spacing: 0.5px; color: #ffb347; margin-bottom: 6px; }
      .card ul { margin: 0 0 0 16px; }
      .guidance-grid { display: grid; gap: 12px; margin-top: 8px; }
      .guidance-card { background: #121215; border: 1px solid #2a2a2f; border-radius: 12px; padding: 12px 14px; box-shadow: 0 6px 16px rgba(0,0,0,0.2); }
      .guidance-title { font-family: 'Bebas Neue', sans-serif; letter-spacing: 0.5px; color: #ffb347; font-size: 18px; margin-bottom: 6px; }
      .guidance-card p { margin: 0 0 6px 0; color: var(--text); }
      .guidance-card ul { margin: 0 0 0 18px; color: var(--text); }
      .nav-tabs { border-bottom: 1px solid #2a2a2f; display: flex; flex-wrap: wrap; gap: 6px; }
      .nav-tabs > li { float: none; }
      .nav-tabs > li > a { font-size: 13px; padding: 8px 10px; }
      .nav-tabs > li > a { color: var(--muted); background: #111114; border: 1px solid #2a2a2f; margin-right: 6px; border-radius: 8px 8px 0 0; }
      .nav-tabs > li > a:hover { color: var(--text); background: #1a1a1d; }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus { color: var(--text); background: #1c1c1f; border-bottom-color: transparent; }
      .tab-divider { pointer-events: none; }
      .tab-divider > a { color: #ffb347 !important; background: transparent !important; border: none !important; text-transform: uppercase; letter-spacing: 1px; font-size: 11px; padding: 6px 8px; }
      .tab-divider.disaster > a { color: #e18a0a !important; }
      .tab-fatality { color: #ffb347; }
      .tab-disaster { color: #e18a0a; }
      .tab-divider.disaster { flex-basis: 100%; margin-top: 8px; }
      .global-spinner { display: none; position: fixed; top: 16px; right: 20px; width: 22px; height: 22px; border: 3px solid #2a2a2f; border-top-color: #ff6a00; border-radius: 50%; animation: spin 0.8s linear infinite; z-index: 9999; }
      .global-spinner.show { display: inline-block; }
      .btn-inline { display: inline-flex; align-items: center; gap: 8px; }
      .inline-spinner { width: 16px; height: 16px; border: 2px solid #2a2a2f; border-top-color: #ffb347; border-radius: 50%; animation: spin 0.8s linear infinite; }
      .header-btn { margin-top: 8px; background: #ff6a00; border: none; color: #0b0b0c; font-weight: 600; }
      .header-btn:hover { background: #ff7e1f; }
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
      tags$h1(class = "app-title", "Fire Department Preparedness Dashboard"),
      tags$p(class = "app-subtitle", "Explore how your department can minimize potential for firefighter fatalities and prepare to respond to large-scale disasters."),
      tags$p(class = "app-subtitle", "Data auto refreshes daily"),
      tags$p(class = "app-subtitle", textOutput("last_refreshed")),
      tags$p(class = "app-subtitle", "Data sources: USFA Fatalities, Census ACS, FEMA Disasters, FEMA HMA, FEMA Public Assistance"),
      actionButton("refresh_data", "Refresh Data Now", class = "header-btn")
    )
  ),

  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      tags$div(class = "sidebar-title", "Enter your department details"),

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
      sliderInput("percent_volunteer", "Percent Volunteer", min = 0, max = 100, value = 50),
      selectInput(
        "incident_exposure",
        "Incident Exposure Level",
        choices = c("Low", "Medium", "High"),
        selected = "Medium"
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
      tags$div(style = "color: #ffb347; margin-top: 6px;", textOutput("incident_error"))

    ),

    mainPanel(
      class = "main",
      tabsetPanel(
        id = "main_tabs",
tabPanel(
          title = tags$span("Welcome", class = "tab-fatality tab-welcome"),
          value = "welcome",
          h3("Fire Department Preparedness Dashboard"),
          tags$p(
            "Most firefighter fatalities are preventable tragedies. Explore actionable guidance to improve training, safety, and incident planning on your department.",
            style = "color: var(--muted);"
          ),
          tags$p(
            "This decision-support dashboard for fire department leadership turns national fatality, disaster, and census data into practical prevention and preparedness guidance.",
            style = "color: var(--muted);"
          ),
          tags$div(
            class = "card",
            tags$div(class = "card-title", "Who This Is For"),
            tags$ul(
              tags$li("Fire chiefs and command staff"),
              tags$li("Training officers and safety committees"),
              tags$li("Volunteer and career department leadership")
            )
          ),
          tags$div(
            class = "card",
            tags$div(class = "card-title", "Core Capabilities"),
            tags$ul(
              tags$li("Regional fatality risk summaries and trends"),
              tags$li("Personnel-specific risk patterns"),
              tags$li("Department-tailored prevention guidance and training plans"),
              tags$li("Disaster preparedness guidance (historically a major source of preventable fatalities)")
            )
          ),
          tags$div(
            class = "card",
            tags$div(class = "card-title", "How To Use It"),
            tags$ul(
              tags$li("Select your region/state and department makeup."),
              tags$li("Choose incident types your department responds to."),
              tags$li("Review the risk summaries and benchmarking tabs."),
              tags$li("Generate prevention guidance and a 12‑month training plan.")
            )
          ),
          tags$div(
            class = "card",
            tags$div(class = "card-title", "I would like to …"),
            tags$div(
              class = "link-fatality",
              tags$strong("Explore fatality data:"),
              tags$ul(
                tags$li(actionLink("go_regional_profile", "Explore historical trends in my region")),
                tags$li(actionLink("go_personnel", "Understand trends for my personnel types")),
                tags$li(actionLink("go_fatality_gauge", "Understand how at-risk my department is")),
                tags$li(actionLink("go_geographic_trends", "Explore overall geographic trends"))
              )
            ),
            tags$div(
              class = "link-plans",
              tags$strong("Develop plans for my department:"),
              tags$ul(
                tags$li(actionLink("go_prevention", "Get tailored prevention guidance")),
                tags$li(actionLink("go_incident_reports", "Analyze my incident reports")),
                tags$li(actionLink("go_training_plan", "Get a training plan for my department"))
              )
            ),
            tags$div(
              class = "link-individual",
              tags$strong("Understand causes of fatalities for firefighters like me:"),
              tags$ul(
                tags$li(actionLink("go_individual", "Analyze fatality reports for firefighters matching my profile"))
              )
            ),
            tags$div(
              class = "link-disaster",
              tags$strong("Plan for disasters:"),
              tags$ul(
                tags$li(actionLink("go_disaster_gauge", "Understand my department’s disaster risk")),
                tags$li(actionLink("go_disaster_plan", "Generate a disaster preparedness plan"))
              )
            )
          )
        ),
tabPanel(
          title = tags$span("Regional Risk Profile", class = "tab-fatality tab-fatality-data"),
          value = "regional_profile",
          h3("Fatality Risk Summary"),
          textOutput("risk_summary"),
          tableOutput("top_causes"),
          h3("Fatalities Over Time"),
          plotOutput("trend_plot", height = "250px"),
          h3("Incident Mix Over Time"),
          plotOutput("incident_mix_plot", height = "300px"),
          h3("Top Causes (Selected Filters)"),
          plotOutput("cause_plot", height = "260px")
        ),
tabPanel(
          title = tags$span("Personnel", class = "tab-fatality tab-fatality-data"),
          value = "personnel",
          h3("Incident Type Odds by Personnel Type (Your Region)"),
          tags$p(
            "Shows how incident types are over/under-represented in fatality records by personnel type in your selected region. Reference group: overall average. This is not a population risk estimate.",
            style = "color: var(--muted);"
          ),
          tags$p(
            "Gray points represent other personnel types not selected.",
            style = "color: var(--muted); margin-top: -6px;"
          ),
          plotOutput("odds_plot", height = "320px"),
          tags$div(style = "margin-top: 12px;", uiOutput("odds_summary")),
          h3("Duty and Activity Patterns by Personnel Type"),
          tags$p(
            "Shows all duties and activities associated with fatalities for the selected personnel types in your region.",
            style = "color: var(--muted);"
          ),
          plotOutput("duty_plot", height = "320px"),
          plotOutput("activity_plot", height = "520px")
        ),
tabPanel(
          title = tags$span("Fatality Risk Gauge", class = "tab-fatality tab-fatality-data"),
          value = "fatality_gauge",
          h3("Fatality Risk Gauge"),
          tags$p(
            "Visual indicator of modeled firefighter fatality risk based on Census + FEMA context.",
            style = "color: var(--muted);"
          ),
          tags$p(
            "This gauge summarizes modeled relative risk (associations, not causation) and applies scenario adjustments for department makeup, incident exposure level, and incident types.",
            style = "color: var(--muted);"
          ),
          tags$ul(
            style = "color: var(--muted); margin-top: -6px;",
            tags$li("Department makeup, incident exposure level, and incident types (scenario adjustments)."),
            tags$li("State/region baseline adjustment using observed fatality rate vs national average."),
            tags$li("Population density (urban vs. rural operational complexity)."),
            tags$li("Housing density (structure fire exposure)."),
            tags$li("FEMA disaster count (operational surge exposure).")
          ),
          tags$p(
            "The gauge reports a relative risk score (0–2) derived from a Poisson model of line-of-duty fatalities. Values below 1 indicate lower-than-average modeled risk; values above 1 indicate higher-than-average modeled risk.",
            style = "color: var(--muted);"
          ),
          plotOutput("risk_gauge_plot", height = "220px"),
          textOutput("risk_label"),
          uiOutput("risk_overview")
        ),
tabPanel(
          title = tags$span("Geographic Trends", class = "tab-fatality tab-fatality-data"),
          value = "geographic_trends",
          h3("Geographic Trends"),
          tags$p(
            "Compare your selected geography with national averages and similarly populated states. Metrics use population density, housing density, percent elderly, poverty rate, no-vehicle rate, and FEMA disaster exposure.",
            style = "color: var(--muted);"
          ),
          tableOutput("benchmark_table"),
          h3("Annual FF Fatality Rates (Per 100K population)"),
          plotlyOutput("benchmark_map", height = "360px"),
          tags$div(
            class = "card",
            tags$div(class = "card-title", "Highest Annual FF Fatality Rates (Per 100K population)"),
            tableOutput("benchmark_top_states")
          ),
          tags$div(
            class = "card",
            tags$div(class = "card-title", "Highest Disaster Exposure (per 100k)"),
            tableOutput("benchmark_top_disasters")
          )
        ),
tabPanel(
          title = tags$span("Prevention Guidance", class = "tab-fatality tab-fatality-plans"),
          value = "prevention_guidance",
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
          uiOutput("guidance")
        ),
tabPanel(
          title = tags$span("Incident Report Analysis", class = "tab-fatality tab-fatality-plans"),
          value = "incident_reports",
          h3("Incident Report Analysis"),
          tags$p(
            "Paste multiple incident reports below. The LLM will summarize key fatality risks and recommendations using historical trends.",
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
          title = tags$span("Training Plan", class = "tab-fatality tab-fatality-plans"),
          value = "training_plan",
          h3("Monthly Training Plan"),
          tags$p(
            "Generates a 12-month training plan based on your department characteristics and historical fatality trends.",
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
            downloadButton("download_training_pdf", "Download PDF"),
            conditionalPanel("output.training_busy == true", tags$span(class = "inline-spinner"))
          ),
          textOutput("training_status"),
          uiOutput("training_intro"),
          tableOutput("training_table"),
          textOutput("training_plan")
        ),
tabPanel(
          title = tags$span("Individual Guidance", class = "tab-fatality tab-fatality-individual"),
          value = "individual_guidance",
          h3("Individualized Risk Profile"),
          tags$p(
            "Explore historical fatality patterns for firefighters who match your profile.",
            style = "color: var(--muted);"
          ),
          selectInput(
            "profile_age_range",
            "Age Range",
            choices = c("All", "20-29", "30-39", "40-49", "50-59", "60-69", "70+"),
            selected = "All"
          ),
          selectInput("profile_role", "Role / Classification", choices = c("All")),
          selectInput(
            "profile_rank",
            "Rank Category",
            choices = c("All", "Firefighter/EMT", "Driver/Engineer", "Officer", "Chief/Command", "Specialist/Other")
          ),
          h3("Fatalities Over Time"),
          plotOutput("profile_trend_plot", height = "260px"),
          h3("Top Causes"),
          plotOutput("profile_cause_plot", height = "260px"),
          h3("Top Incident Types"),
          plotOutput("profile_incident_plot", height = "260px"),
          h3("Recent Incident Summary"),
          tags$p(
            "Summarize up to 10 of the most recent incident reports that match your profile (fewer if fewer are available).",
            style = "color: var(--muted);"
          ),
          tags$div(
            class = "btn-inline",
            actionButton("profile_analyze", "Analyze Recent Reports"),
            conditionalPanel("output.profile_busy == true", tags$span(class = "inline-spinner"))
          ),
          textOutput("profile_status"),
          uiOutput("profile_analysis")
        ),
tabPanel(
          title = tags$span("Disaster Risk Gauge", class = "tab-disaster"),
          value = "disaster_gauge",
          h3("Disaster Response Likelihood Gauge"),
          tags$p(
            "Visual indicator of modeled disaster response likelihood based on FEMA + Census indicators.",
            style = "color: var(--muted);"
          ),
          tags$ul(
            style = "color: var(--muted); margin-top: -6px;",
            tags$li("FEMA disaster declarations per 100k (exposure)."),
            tags$li("Population density (response complexity)."),
            tags$li("Percent elderly (vulnerability)."),
            tags$li("Households without vehicles (evacuation burden)."),
            tags$li("Poverty rate (resource constraints).")
          ),
          tags$p(
            "This gauge summarizes a composite disaster-response likelihood score (0–2) using standardized Census + FEMA indicators. It reflects relative operational exposure, not certainty of future events.",
            style = "color: var(--muted);"
          ),
          plotOutput("disaster_gauge_plot", height = "220px"),
          textOutput("disaster_risk_label")
        ),
tabPanel(
          title = tags$span("Disaster Risk & Preparedness", class = "tab-disaster"),
          value = "disaster_preparedness",
          h3("Disaster Risk & Preparedness"),
          tags$p(
            "Estimate disaster exposure and generate preparedness guidance using Census + FEMA data.",
            style = "color: var(--muted);"
          ),
          uiOutput("disaster_summary"),
          h3("Top Disaster Types"),
          tableOutput("disaster_types_table"),
          h3("Preparedness Plan"),
          tags$div(
            class = "btn-inline",
            actionButton("run_disaster_plan", "Generate Preparedness Plan"),
            conditionalPanel("output.disaster_busy == true", tags$span(class = "inline-spinner"))
          ),
          textOutput("disaster_status"),
          uiOutput("disaster_plan")
        )
      )
    )
  )
)
# ---- Server ----
server <- function(input, output, session) {
  data_state <- reactiveVal(get_data())
  refresh_timer <- reactiveTimer(24 * 60 * 60 * 1000)
  last_refresh_state <- reactiveVal(read_last_refresh())
  census_state <- reactiveVal(get_census_data())
  fema_state <- reactiveVal(get_fema_data())
  fema_types_state <- reactiveVal(get_fema_types())
  fema_decl_state <- reactiveVal(get_fema_declarations())
  hma_state <- reactiveVal(get_hma_grants())
  pa_fire_state <- reactiveVal(get_pa_fire_applicants())
  llm_state <- reactiveVal(list(status = "LLM guidance not generated yet.", guidance = NULL))
  reports_state <- reactiveVal(list(status = "No reports analyzed yet.", analysis = NULL))
  training_state <- reactiveVal(list(status = "No training plan generated yet.", plan = NULL))
  profile_state <- reactiveVal(list(status = "No profile summary generated yet.", analysis = NULL))
  disaster_state <- reactiveVal(list(status = "No disaster plan generated yet.", plan = NULL))
  guidance_busy <- reactiveVal(FALSE)
  reports_busy <- reactiveVal(FALSE)
  training_busy <- reactiveVal(FALSE)
  profile_busy <- reactiveVal(FALSE)
  disaster_busy <- reactiveVal(FALSE)

  observeEvent(data_state(), {
    df <- data_state()
    last_refresh_state(read_last_refresh())
    if (!all(is.na(df$state))) {
      states <- sort(unique(na.omit(df$state)))
      states <- states[states %in% state.abb]
      updateSelectInput(session, "state", choices = c("All", states))
      updateSelectInput(session, "state", selected = "All")
    }

    if (!all(is.na(df$department_type))) {
      roles <- sort(unique(na.omit(df$department_type)))
      updateSelectInput(session, "profile_role", choices = c("All", roles), selected = "All")
    }

    if (!all(is.na(df$rank_group))) {
      groups <- sort(unique(na.omit(df$rank_group)))
      updateSelectInput(session, "profile_rank", choices = c("All", groups), selected = "All")
    }
  })

  observeEvent(input$go_regional_profile, {
    updateTabsetPanel(session, "main_tabs", selected = "regional_profile")
  })
  observeEvent(input$go_personnel, {
    updateTabsetPanel(session, "main_tabs", selected = "personnel")
  })
  observeEvent(input$go_fatality_gauge, {
    updateTabsetPanel(session, "main_tabs", selected = "fatality_gauge")
  })
  observeEvent(input$go_geographic_trends, {
    updateTabsetPanel(session, "main_tabs", selected = "geographic_trends")
  })
  observeEvent(input$go_prevention, {
    updateTabsetPanel(session, "main_tabs", selected = "prevention_guidance")
  })
  observeEvent(input$go_incident_reports, {
    updateTabsetPanel(session, "main_tabs", selected = "incident_reports")
  })
  observeEvent(input$go_training_plan, {
    updateTabsetPanel(session, "main_tabs", selected = "training_plan")
  })
  observeEvent(input$go_individual, {
    updateTabsetPanel(session, "main_tabs", selected = "individual_guidance")
  })
  observeEvent(input$go_disaster_gauge, {
    updateTabsetPanel(session, "main_tabs", selected = "disaster_gauge")
  })
  observeEvent(input$go_disaster_plan, {
    updateTabsetPanel(session, "main_tabs", selected = "disaster_preparedness")
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

  model_data <- reactive({
    df <- data_state()
    census <- census_state()
    fema <- fema_state()
    if (is.null(census) || nrow(df) == 0) return(NULL)

    fatalities <- df %>%
      filter(!is.na(state)) %>%
      group_by(state) %>%
      summarize(
        deaths = n(),
        years_observed = n_distinct(year[!is.na(year)]),
        .groups = "drop"
      )

    census <- census %>% filter(!is.na(state))
    combined <- fatalities %>%
      left_join(census, by = "state")

    if (!is.null(fema)) {
      combined <- combined %>%
        left_join(fema, by = "state") %>%
        mutate(disaster_count = ifelse(is.na(disaster_count), 0, disaster_count))
    } else {
      combined <- combined %>% mutate(disaster_count = 0)
    }

    combined %>%
      mutate(
        land_area_sq_mi = state.area[match(state, state.abb)],
        pop_density = ifelse(!is.na(land_area_sq_mi) & land_area_sq_mi > 0, population / land_area_sq_mi, NA_real_),
        log_density = log(pop_density + 1),
        housing_density = ifelse(!is.na(land_area_sq_mi) & land_area_sq_mi > 0, housing_units / land_area_sq_mi, NA_real_),
        log_housing_density = log(housing_density + 1)
      ) %>%
      mutate(
        annual_deaths = ifelse(!is.na(years_observed) & years_observed > 0, deaths / years_observed, NA_real_),
        deaths_per_100k = (annual_deaths / population) * 100000
      )
  })

  profile_filtered <- reactive({
    df <- data_state()
    if (nrow(df) == 0) return(df)

    if (!all(is.na(df$age)) && !is.null(input$profile_age_range) && input$profile_age_range != "All") {
      bounds <- str_split(input$profile_age_range, "-", simplify = TRUE)
      if (length(bounds) == 2) {
        low <- suppressWarnings(as.integer(bounds[1]))
        high <- suppressWarnings(as.integer(bounds[2]))
        df <- df %>% filter(!is.na(age), age >= low, age <= high)
      } else if (input$profile_age_range == "70+") {
        df <- df %>% filter(!is.na(age), age >= 70)
      }
    }

    if (!is.null(input$profile_role) && input$profile_role != "All") {
      df <- df %>% filter(department_type == input$profile_role)
    }

    if (!is.null(input$profile_rank) && input$profile_rank != "All") {
      if (!all(is.na(df$rank_group))) {
        df <- df %>% filter(rank_group == input$profile_rank)
      }
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
    last_refresh_state(read_last_refresh())
    census_state(get_census_data())
    fema_state(get_fema_data())
    fema_types_state(get_fema_types())
    fema_decl_state(get_fema_declarations())
    hma_state(get_hma_grants())
    pa_fire_state(get_pa_fire_applicants())
  })

  observeEvent(input$refresh_data, {
    data_state(get_data(TRUE))
    last_refresh_state(read_last_refresh())
    census_state(get_census_data())
    fema_state(get_fema_data())
    fema_types_state(get_fema_types())
    fema_decl_state(get_fema_declarations())
    hma_state(get_hma_grants())
    pa_fire_state(get_pa_fire_applicants())
  })

  observeEvent(data_state(), {
    if (is.null(census_state())) census_state(get_census_data())
    if (is.null(fema_state())) fema_state(get_fema_data())
    if (is.null(fema_types_state())) fema_types_state(get_fema_types())
    if (is.null(fema_decl_state())) fema_decl_state(get_fema_declarations())
    if (is.null(hma_state())) hma_state(get_hma_grants())
    if (is.null(pa_fire_state())) pa_fire_state(get_pa_fire_applicants())
  })

  output$data_source <- renderText({
    src <- attr(data_state(), "source")
    if (src == "refreshed") "Data source: refreshed from USFA API"
    else if (src == "cache") "Data source: cached USFA API download"
    else if (src == "local") "Data source: local file (ff_data.csv)"
    else "Data source: bundled sample data"
  })

  fatality_model_fit <- reactive({
    data <- model_data()
    if (is.null(data)) return(NULL)
    data <- data %>%
      filter(!is.na(annual_deaths), !is.na(population), population > 0, !is.na(log_density), !is.na(log_housing_density))
    if (nrow(data) < 5) return(NULL)

    tryCatch(
      glm(
        annual_deaths ~ log_density + log_housing_density + disaster_count,
        family = "poisson",
        offset = log(population),
        data = data
      ),
      error = function(e) NULL
    )
  })

  geo_states <- reactive({
    df <- data_state()
    if (input$geo_mode == "state") {
      if (is.null(input$state) || input$state == "All") {
        return(unique(na.omit(df$state)))
      }
      return(input$state)
    }

    if (is.null(input$region) || input$region == "All") {
      return(unique(na.omit(df$state)))
    }

    states <- df %>%
      filter(region == input$region, !is.na(state)) %>%
      pull(state) %>%
      unique()
    states
  })

  geo_summary <- reactive({
    data <- model_data()
    if (is.null(data) || nrow(data) == 0) return(NULL)
    states <- geo_states()
    data_geo <- data %>% filter(state %in% states)
    if (nrow(data_geo) == 0) return(NULL)

    deaths <- sum(data_geo$deaths, na.rm = TRUE)
    population <- sum(data_geo$population, na.rm = TRUE)
    median_age <- weighted.mean(data_geo$median_age, w = data_geo$population, na.rm = TRUE)
    median_income <- weighted.mean(data_geo$median_income, w = data_geo$population, na.rm = TRUE)
    pct_elderly <- weighted.mean(data_geo$pct_elderly, w = data_geo$population, na.rm = TRUE)
    poverty_rate <- weighted.mean(data_geo$poverty_rate, w = data_geo$population, na.rm = TRUE)
    no_vehicle_rate <- weighted.mean(data_geo$no_vehicle_rate, w = data_geo$households_total, na.rm = TRUE)
    disaster_count <- sum(data_geo$disaster_count, na.rm = TRUE)
    land_area <- sum(data_geo$land_area_sq_mi, na.rm = TRUE)
    pop_density <- ifelse(land_area > 0, population / land_area, NA_real_)

    tibble(
      deaths = deaths,
      population = population,
      median_age = median_age,
      median_income = median_income,
      pct_elderly = pct_elderly,
      poverty_rate = poverty_rate,
      no_vehicle_rate = no_vehicle_rate,
      disaster_count = disaster_count,
      land_area_sq_mi = land_area,
      pop_density = pop_density,
      log_density = log(pop_density + 1),
      housing_density = ifelse(land_area > 0, sum(data_geo$housing_units, na.rm = TRUE) / land_area, NA_real_),
      log_housing_density = log(ifelse(land_area > 0, sum(data_geo$housing_units, na.rm = TRUE) / land_area, 0) + 1),
      deaths_per_100k = ifelse(population > 0, (deaths / population) * 100000, NA_real_)
    )
  })

  output$risk_overview <- renderUI({
    data <- model_data()
    summary <- geo_summary()
    if (is.null(data) || nrow(data) == 0 || is.null(summary)) {
      return(tags$div(
        class = "card",
        tags$div(class = "card-title", "Risk-Adjusted Metrics"),
        tags$p("Census data unavailable. Set CENSUS_API_KEY to enable risk-adjusted metrics.")
      ))
    }

    rate <- summary$deaths_per_100k
    national_rate <- mean(data$deaths_per_100k, na.rm = TRUE)
    comparison <- ifelse(!is.na(rate) && !is.na(national_rate),
                         ifelse(rate >= national_rate, "above", "below"), "unknown")
    disaster_total <- summary$disaster_count

    risk_text <- "Select a state to view modeled risk."
    ratio <- fatality_risk_ratio_val()
    if (!is.na(ratio)) {
      risk_text <- paste0("Modeled relative risk (scenario-adjusted): ", round(ratio, 2), " (associations, not causation).")
    }

    tags$div(
      class = "card",
      tags$div(class = "card-title", "Risk-Adjusted Metrics"),
      tags$p(paste0("Fatalities per 100k: ", ifelse(is.na(rate), "NA", round(rate, 2)))),
      tags$p(paste0("Compared to national average: ", comparison)),
      tags$p(paste0("Total FEMA disaster declarations: ", ifelse(is.na(disaster_total), "NA", disaster_total))),
      tags$p(risk_text)
    )
  })

  fatality_risk_ratio_val <- reactive({
    data <- model_data()
    summary <- geo_summary()
    fit <- fatality_model_fit()
    if (is.null(data) || is.null(summary) || is.null(fit)) return(NA_real_)
    pred <- predict(fit, newdata = summary, type = "response")
    national_rate <- mean(data$deaths_per_100k, na.rm = TRUE)
    pred_rate <- ifelse(summary$population > 0, (pred / summary$population) * 100000, NA_real_)
    base <- as.numeric(ifelse(!is.na(pred_rate) && !is.na(national_rate) && national_rate > 0,
                              pred_rate / national_rate, NA_real_))

    geo_adjust <- 1

    exposure_adj <- switch(
      input$incident_exposure,
      "Low" = 0.9,
      "High" = 1.1,
      1.0
    )

    dept_adj <- 1.0
    if (!is.null(input$dept_type) && length(input$dept_type) > 0) {
      if ("Volunteer" %in% input$dept_type) dept_adj <- dept_adj + 0.05
      if ("Wildland Contract" %in% input$dept_type || "Wildland Full-Time" %in% input$dept_type || "Wildland Part-Time" %in% input$dept_type) {
        dept_adj <- dept_adj + 0.05
      }
    }

    incident_adj <- 1.0
    if (!is.null(input$incident_types) && length(input$incident_types) > 0) {
      if ("Wildland" %in% input$incident_types) incident_adj <- incident_adj + 0.05
      if ("Hazmat" %in% input$incident_types) incident_adj <- incident_adj + 0.03
      if ("Vehicle Incident" %in% input$incident_types) incident_adj <- incident_adj + 0.03
      if ("Rescue" %in% input$incident_types) incident_adj <- incident_adj + 0.02
    }

    as.numeric(base * geo_adjust * exposure_adj * dept_adj * incident_adj)
  })

  output$risk_gauge_plot <- renderPlot({
    ratio <- fatality_risk_ratio_val()
    if (is.na(ratio)) ratio <- 0
    safe_risk <- min(max(ratio, 0), 2)

    bands <- tibble(
      xmin = c(0, 0.8, 1.2),
      xmax = c(0.8, 1.2, 2),
      ymin = 0,
      ymax = 1,
      fill = c("Low", "Average", "High")
    )

    ggplot(bands) +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), color = NA, alpha = 0.8) +
      geom_segment(aes(x = safe_risk, xend = safe_risk, y = 0, yend = 1), color = "#0b0b0c", linewidth = 1.2) +
      geom_point(aes(x = safe_risk, y = 0.5), color = "#0b0b0c", size = 3) +
      scale_fill_manual(values = c("Low" = "#4caf50", "Average" = "#f2c94c", "High" = "#d63230")) +
      scale_x_continuous(limits = c(0, 2), breaks = c(0, 0.8, 1.2, 2)) +
      theme_void() +
      theme(
        legend.position = "none",
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA)
      )
  })

  output$risk_label <- renderText({
    ratio <- fatality_risk_ratio_val()
    if (is.na(ratio)) return("Risk unavailable")
    if (ratio < 0.8) {
      "Lower-than-average risk"
    } else if (ratio < 1.2) {
      "Average risk"
    } else {
      "Elevated risk"
    }
  })

  disaster_index_stats <- reactive({
    data <- model_data()
    if (is.null(data)) return(NULL)

    data <- data %>%
      mutate(
        disasters_per_100k = ifelse(population > 0, (disaster_count / population) * 100000, NA_real_)
      )

    vars <- list(
      log_density = data$log_density,
      pct_elderly = data$pct_elderly,
      no_vehicle_rate = data$no_vehicle_rate,
      poverty_rate = data$poverty_rate,
      disasters_per_100k = data$disasters_per_100k
    )

    means <- lapply(vars, function(x) mean(x, na.rm = TRUE))
    sds <- lapply(vars, function(x) sd(x, na.rm = TRUE))

    z_scores <- lapply(names(vars), function(nm) {
      x <- vars[[nm]]
      mu <- means[[nm]]
      s <- sds[[nm]]
      if (is.na(s) || s == 0) return(rep(0, length(x)))
      (x - mu) / s
    })

    index <- rowMeans(as.data.frame(z_scores), na.rm = TRUE)
    list(
      means = means,
      sds = sds,
      min_index = min(index, na.rm = TRUE),
      max_index = max(index, na.rm = TRUE)
    )
  })

  disaster_risk_score_val <- reactive({
    stats <- disaster_index_stats()
    summary <- geo_summary()
    if (is.null(stats) || is.null(summary)) return(NA_real_)

    disasters_per_100k <- ifelse(summary$population > 0, (summary$disaster_count / summary$population) * 100000, NA_real_)

    calc_z <- function(value, mu, s) {
      if (is.na(value) || is.na(s) || s == 0) return(0)
      (value - mu) / s
    }

    z_density <- calc_z(summary$log_density, stats$means$log_density, stats$sds$log_density)
    z_elderly <- calc_z(summary$pct_elderly, stats$means$pct_elderly, stats$sds$pct_elderly)
    z_vehicle <- calc_z(summary$no_vehicle_rate, stats$means$no_vehicle_rate, stats$sds$no_vehicle_rate)
    z_poverty <- calc_z(summary$poverty_rate, stats$means$poverty_rate, stats$sds$poverty_rate)
    z_disasters <- calc_z(disasters_per_100k, stats$means$disasters_per_100k, stats$sds$disasters_per_100k)

    index <- mean(c(z_density, z_elderly, z_vehicle, z_poverty, z_disasters), na.rm = TRUE)
    if (is.na(index) || is.na(stats$min_index) || is.na(stats$max_index) || stats$max_index == stats$min_index) {
      return(NA_real_)
    }
    scaled <- (index - stats$min_index) / (stats$max_index - stats$min_index) * 2
    min(max(scaled, 0), 2)
  })

  output$disaster_gauge_plot <- renderPlot({
    score <- disaster_risk_score_val()
    if (is.na(score)) score <- 0
    safe_risk <- min(max(score, 0), 2)

    bands <- tibble(
      xmin = c(0, 0.8, 1.2),
      xmax = c(0.8, 1.2, 2),
      ymin = 0,
      ymax = 1,
      fill = c("Low", "Average", "High")
    )

    ggplot(bands) +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), color = NA, alpha = 0.8) +
      geom_segment(aes(x = safe_risk, xend = safe_risk, y = 0, yend = 1), color = "#0b0b0c", linewidth = 1.2) +
      geom_point(aes(x = safe_risk, y = 0.5), color = "#0b0b0c", size = 3) +
      scale_fill_manual(values = c("Low" = "#4caf50", "Average" = "#f2c94c", "High" = "#d63230")) +
      scale_x_continuous(limits = c(0, 2), breaks = c(0, 0.8, 1.2, 2)) +
      theme_void() +
      theme(
        legend.position = "none",
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA)
      )
  })

  output$disaster_risk_label <- renderText({
    score <- disaster_risk_score_val()
    if (is.na(score)) return("Risk unavailable")
    if (score < 0.8) {
      "Lower-than-average disaster response likelihood"
    } else if (score < 1.2) {
      "Average disaster response likelihood"
    } else {
      "Elevated disaster response likelihood"
    }
  })

  disaster_summary_data <- reactive({
    summary <- geo_summary()
    data <- model_data()
    if (is.null(summary) || is.null(data)) return(NULL)

    decl <- fema_decl_state()
    states <- geo_states()
    last5_count <- NA
    if (!is.null(decl) && length(states) > 0) {
      recent <- decl %>%
        filter(state %in% states, !is.na(year)) %>%
        filter(year >= max(year, na.rm = TRUE) - 4)
      last5_count <- nrow(recent)
    }

    disasters_per_100k <- ifelse(summary$population > 0,
                                 (summary$disaster_count / summary$population) * 100000,
                                 NA_real_)
    max_disasters <- max(data$disaster_count, na.rm = TRUE)
    population_at_risk <- ifelse(is.finite(max_disasters) && max_disasters > 0,
                                 summary$population * (summary$disaster_count / max_disasters),
                                 NA_real_)

    density_label <- case_when(
      is.na(summary$pop_density) ~ "Unknown density",
      summary$pop_density < 50 ~ "Low density (rural)",
      summary$pop_density < 300 ~ "Moderate density",
      TRUE ~ "High density (urban)"
    )

    list(
      disasters_per_100k = disasters_per_100k,
      population_at_risk = population_at_risk,
      density = summary$pop_density,
      density_label = density_label,
      population = summary$population,
      disaster_count = summary$disaster_count,
      disasters_last5 = last5_count
    )
  })

  output$disaster_summary <- renderUI({
    info <- disaster_summary_data()
    if (is.null(info)) {
      return(tags$div(
        class = "card",
        tags$div(class = "card-title", "Disaster Risk Summary"),
        tags$p("Census or FEMA data unavailable. Set CENSUS_API_KEY to enable this summary.")
      ))
    }

    mitigation <- hma_state()
    fire_support <- pa_fire_state()
    states <- geo_states()

    mitigation_value <- if (!is.null(mitigation) && length(states) > 0) {
      mitigation %>%
        filter(state %in% states) %>%
        summarize(total = sum(mitigation_investment, na.rm = TRUE)) %>%
        pull(total)
    } else NA_real_

    fire_support_count <- if (!is.null(fire_support) && length(states) > 0) {
      fire_support %>%
        filter(state %in% states) %>%
        summarize(total = sum(fire_applicant_count, na.rm = TRUE)) %>%
        pull(total)
    } else NA_real_

    tags$div(
      class = "card",
      tags$div(class = "card-title", "Disaster Risk Summary"),
      tags$p(paste0("Population: ", format(round(info$population), big.mark = ","))),
      tags$p(paste0("Disaster declarations: ", info$disaster_count)),
      tags$p(paste0("Disasters in last 5 years: ", ifelse(is.na(info$disasters_last5), "NA", info$disasters_last5))),
      tags$p(paste0("Disasters per 100k: ", round(info$disasters_per_100k, 2))),
      tags$p(paste0("Estimated population at risk: ", format(round(info$population_at_risk), big.mark = ","))),
      tags$p(paste0("Population density: ", ifelse(is.na(info$density), "NA", round(info$density, 1)), " per sq. mile (", info$density_label, ")")),
      tags$p(paste0("Mitigation investment (HMA): ", ifelse(is.na(mitigation_value), "NA", format(round(mitigation_value), big.mark = ",")))),
      tags$p(paste0("Fire-related PA applicants: ", ifelse(is.na(fire_support_count), "NA", fire_support_count)))
    )
  })

  output$disaster_types_table <- renderTable({
    types <- fema_types_state()
    states <- geo_states()
    if (is.null(types) || length(states) == 0) return(NULL)

    types %>%
      filter(state %in% states) %>%
      group_by(state, incidentType) %>%
      summarize(count = sum(count), .groups = "drop") %>%
      group_by(state) %>%
      slice_max(order_by = count, n = 3, with_ties = FALSE) %>%
      ungroup() %>%
      arrange(desc(count)) %>%
      rename(`Disaster Type` = incidentType, `Count` = count)
  })

  output$disaster_status <- renderText({
    disaster_state()$status
  })

  output$disaster_plan <- renderUI({
    format_guidance_cards(disaster_state()$plan)
  })

  output$benchmark_table <- renderTable({
    data <- model_data()
    summary <- geo_summary()
    if (is.null(data) || is.null(summary)) return(NULL)

    national_rate <- mean(data$deaths_per_100k, na.rm = TRUE)
    geo_rate <- summary$deaths_per_100k

    table <- tibble(
      Comparison = c("Selected geography", "National average"),
      `Fatalities per 100k` = c(round(geo_rate, 2), round(national_rate, 2))
    )

    if (input$geo_mode == "state" && !is.null(input$state) && input$state != "All") {
      target <- data %>% filter(state == input$state) %>% slice(1)
      if (nrow(target) == 1) {
        pop <- target$population
        similar <- data %>%
          filter(population >= pop * 0.8, population <= pop * 1.2) %>%
          summarize(similar_rate = mean(deaths_per_100k, na.rm = TRUE))
        table <- bind_rows(
          table,
          tibble(Comparison = "Similar population states", `Fatalities per 100k` = round(similar$similar_rate, 2))
        )
      }
    }

    table
  })

  benchmark_map_data <- reactive({
    if (!requireNamespace("maps", quietly = TRUE)) return(NULL)
    data <- model_data()
    if (is.null(data)) return(NULL)

    state_lookup <- tibble(
      state = state.abb,
      state_name = tolower(state.name)
    )

    data <- data %>%
      left_join(state_lookup, by = "state") %>%
      mutate(state_name = ifelse(state == "DC", "district of columbia", state_name))

    map_df <- ggplot2::map_data("state")
    map_df %>% left_join(data, by = c("region" = "state_name"))
  })

  output$benchmark_map <- renderPlotly({
    df <- benchmark_map_data()
    if (is.null(df)) {
      return(
        plotly::plot_ly() %>%
          plotly::layout(
            annotations = list(list(
              text = "Map unavailable (install the maps package).",
              x = 0.5, y = 0.5, xref = "paper", yref = "paper",
              showarrow = FALSE, font = list(color = "#c7c0b8")
            ))
          )
      )
    }

    plot <- ggplot(
      df,
      aes(
        long, lat, group = group,
        fill = deaths_per_100k,
        text = paste0(
          "State: ", toupper(region), "<br>",
          "Annual FF fatality rate (per 100K population): ", ifelse(is.na(deaths_per_100k), "NA", round(deaths_per_100k, 2))
        )
      )
    ) +
      geom_polygon(color = "#ffffff", size = 0.35) +
      scale_fill_gradient(
        low = "#1c1c1f",
        high = "#ff6a00",
        na.value = "#2a2a2f"
      ) +
      coord_fixed(1.3) +
      theme_void(base_family = "Source Sans 3") +
      theme(
        legend.position = "right",
        legend.title = element_text(color = "#f7f3ef"),
        legend.text = element_text(color = "#c7c0b8"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA)
      ) +
      labs(fill = "Annual FF fatality rate (per 100K population)")

    plotly::ggplotly(plot, tooltip = "text") %>%
      plotly::layout(margin = list(l = 0, r = 0, t = 0, b = 0))
  })

  output$benchmark_top_states <- renderTable({
    data <- model_data()
    if (is.null(data)) return(NULL)

    data %>%
      filter(!is.na(deaths_per_100k)) %>%
      arrange(desc(deaths_per_100k)) %>%
      slice_head(n = 10) %>%
      transmute(
        State = state,
        `Annual FF fatality rate (per 100K population)` = round(deaths_per_100k, 2)
      )
  })

  output$benchmark_top_disasters <- renderTable({
    data <- model_data()
    if (is.null(data)) return(NULL)

    data %>%
      mutate(disasters_per_100k = ifelse(population > 0, (disaster_count / population) * 100000, NA_real_)) %>%
      filter(!is.na(disasters_per_100k)) %>%
      arrange(desc(disasters_per_100k)) %>%
      slice_head(n = 10) %>%
      transmute(
        State = state,
        `Disasters per 100k` = round(disasters_per_100k, 2)
      )
  })

  output$last_refreshed <- renderText({
    last <- last_refresh_state()
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
      group_by(year) %>%
      mutate(pct = n / sum(n)) %>%
      ungroup() %>%
      ggplot(aes(x = year, y = pct, fill = incident_category)) +
      geom_col() +
      scale_fill_manual(values = palette) +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Year", y = "Share of Fatalities", fill = "Incident Type") +
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

  output$profile_trend_plot <- renderPlot({
    df <- profile_filtered()
    if (nrow(df) == 0 || all(is.na(df$year))) return(NULL)

    df %>%
      filter(!is.na(year)) %>%
      count(year) %>%
      ggplot(aes(x = year, y = n)) +
      geom_area(fill = "#d63230", alpha = 0.4) +
      geom_line(linewidth = 1.1, color = "#ffb347") +
      geom_point(size = 2, color = "#ffb347") +
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

  output$profile_cause_plot <- renderPlot({
    df <- profile_filtered()
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

  output$profile_incident_plot <- renderPlot({
    df <- profile_filtered()
    if (nrow(df) == 0 || all(is.na(df$incident_category))) return(NULL)

    top <- df %>%
      filter(!is.na(incident_category)) %>%
      count(incident_category, sort = TRUE) %>%
      slice_head(n = 6)

    ggplot(top, aes(x = reorder(incident_category, n), y = n)) +
      geom_col(fill = "#ffb347") +
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

    palette <- c(personnel_palette[selected_personnel], "Unselected" = "#6c757d")

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

  output$duty_plot <- renderPlot({
    if (is.null(input$incident_types) || length(input$incident_types) == 0) return(NULL)
    df <- filtered()
    selected_personnel <- if (!is.null(input$dept_type) && length(input$dept_type) > 0) {
      input$dept_type
    } else {
      character()
    }
    if (length(selected_personnel) == 0) return(NULL)
    summary_list <- summarize_duty_activity(df, selected_personnel)
    summary_df <- summary_list$duty
    if (is.null(summary_df) || nrow(summary_df) == 0) return(NULL)

    summary_df <- summary_df %>%
      mutate(
        department_type = factor(department_type, levels = selected_personnel)
      ) %>%
      group_by(category) %>%
      mutate(total = sum(n, na.rm = TRUE)) %>%
      ungroup()

    ggplot(summary_df, aes(x = n, y = reorder(category, total), fill = department_type)) +
      geom_col(alpha = 0.9, position = "stack") +
      scale_fill_manual(values = personnel_palette[selected_personnel]) +
      scale_y_discrete(labels = function(x) str_wrap(x, width = 30)) +
      labs(x = "Fatality Count", y = "Duty", fill = "Personnel Type") +
      theme_minimal(base_size = 12) +
      theme(
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_text(color = "#c7c0b8"),
        axis.text.y = element_text(color = "#c7c0b8"),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "transparent", color = NA),
        legend.text = element_text(color = "#c7c0b8"),
        legend.title = element_text(color = "#f7f3ef")
      )
  })

  output$activity_plot <- renderPlot({
    if (is.null(input$incident_types) || length(input$incident_types) == 0) return(NULL)
    df <- filtered()
    selected_personnel <- if (!is.null(input$dept_type) && length(input$dept_type) > 0) {
      input$dept_type
    } else {
      character()
    }
    if (length(selected_personnel) == 0) return(NULL)
    summary_list <- summarize_duty_activity(df, selected_personnel)
    summary_df <- summary_list$activity
    if (is.null(summary_df) || nrow(summary_df) == 0) return(NULL)

    summary_df <- summary_df %>%
      mutate(
        department_type = factor(department_type, levels = selected_personnel)
      ) %>%
      group_by(category) %>%
      mutate(total = sum(n, na.rm = TRUE)) %>%
      ungroup()

    ggplot(summary_df, aes(x = n, y = reorder(category, total), fill = department_type)) +
      geom_col(alpha = 0.9, position = "stack") +
      scale_fill_manual(values = personnel_palette[selected_personnel]) +
      scale_y_discrete(labels = function(x) str_wrap(x, width = 30)) +
      labs(x = "Fatality Count", y = "Activity", fill = "Personnel Type") +
      theme_minimal(base_size = 12) +
      theme(
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_text(color = "#c7c0b8"),
        axis.text.y = element_text(color = "#c7c0b8"),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "transparent", color = NA),
        legend.text = element_text(color = "#c7c0b8"),
        legend.title = element_text(color = "#f7f3ef")
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

    single_incident <- length(unique(odds_df$incident_category)) == 1

    summaries <- lapply(selected_personnel, function(p) {
      subset <- odds_df %>% filter(department_type == p)
      if (nrow(subset) == 0) return(NULL)

      if (single_incident) {
        incident_label <- subset$incident_category[1]
        totals <- df %>%
          filter(!is.na(department_type)) %>%
          mutate(department_type = str_trim(department_type)) %>%
          filter(department_type != "Unknown") %>%
          count(department_type, name = "n")
        total_all <- sum(totals$n)
        share <- totals %>% filter(department_type == p) %>% pull(n)
        share <- ifelse(length(share) == 0 || total_all == 0, NA_real_, share / total_all)
        pct <- ifelse(is.na(share), "0%", paste0(round(share * 100, 1), "%"))
        return(paste0(
          p, " firefighters account for ", pct,
          " of fatalities for ", incident_label, " incidents in your region."
        ))
      }

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


  output$guidance <- renderUI({
    llm_guidance <- llm_state()$guidance
    format_guidance_cards(llm_guidance)
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
    df <- data_state()
    if (nrow(df) == 0) {
      reports_state(list(status = "No records available to summarize.", analysis = NULL))
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
    top_incidents <- df %>% count(incident_category, sort = TRUE) %>% slice(1:3) %>% pull(incident_category)
    top_duty <- df %>% filter(!is.na(duty)) %>% count(duty, sort = TRUE) %>% slice(1:3) %>% pull(duty)
    top_activity <- df %>%
      mutate(activity = coalesce(incident_type, incident_category, emergency, property_type)) %>%
      filter(!is.na(activity)) %>%
      count(activity, sort = TRUE) %>%
      slice(1:3) %>%
      pull(activity)
    trend <- df %>% count(year) %>% arrange(desc(year))

    trend_text <- paste0(
      "Top cause: ", top_cause, ". ",
      "Top incident types: ", paste(top_incidents, collapse = ", "), ". ",
      "Common duties in fatality data: ", paste(top_duty, collapse = ", "), ". ",
      "Common activities in fatality data: ", paste(top_activity, collapse = ", "), ". ",
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

  observeEvent(input$profile_analyze, {
    profile_busy(TRUE)
    on.exit(profile_busy(FALSE), add = TRUE)

    df <- profile_filtered()
    if (nrow(df) == 0) {
      profile_state(list(status = "No records match the current profile filters.", analysis = NULL))
      return()
    }

    key <- Sys.getenv("GEMINI_API_KEY")
    if (key == "") {
      profile_state(list(status = "Missing GEMINI_API_KEY. Set it in .Renviron or your session.", analysis = NULL))
      return()
    }

    reports <- df %>%
      mutate(order_date = ifelse(is.na(incident_date), as.Date(paste0(year, "-12-31")), incident_date)) %>%
      arrange(desc(order_date)) %>%
      filter(!is.na(narrative), narrative != "") %>%
      slice_head(n = 10) %>%
      pull(narrative)

    if (length(reports) == 0) {
      profile_state(list(status = "No incident narratives available for this profile.", analysis = NULL))
      return()
    }

    profile_text <- paste(
      "Age range:", input$profile_age_range,
      "| Role:", ifelse(is.null(input$profile_role), "All", input$profile_role),
      "| Rank:", ifelse(is.null(input$profile_rank), "All", input$profile_rank)
    )

    reports_text <- paste(reports, collapse = "\n---\n")
    if (nchar(reports_text) > 6000) {
      reports_text <- substr(reports_text, 1, 6000)
    }

    analysis <- tryCatch(
      generate_profile_summary(profile_text, reports_text),
      error = function(e) NULL
    )

    if (is.null(analysis)) {
      profile_state(list(status = "LLM analysis failed. Try again.", analysis = NULL))
    } else {
      profile_state(list(status = "Profile summary complete.", analysis = analysis))
    }
  })

  output$profile_status <- renderText({
    profile_state()$status
  })

  output$profile_analysis <- renderUI({
    format_incident_analysis(profile_state()$analysis)
  })

  output$guidance_busy <- reactive({ guidance_busy() })
  output$reports_busy <- reactive({ reports_busy() })
  output$training_busy <- reactive({ training_busy() })
  output$profile_busy <- reactive({ profile_busy() })
  output$disaster_busy <- reactive({ disaster_busy() })
  outputOptions(output, "guidance_busy", suspendWhenHidden = FALSE)
  outputOptions(output, "reports_busy", suspendWhenHidden = FALSE)
  outputOptions(output, "training_busy", suspendWhenHidden = FALSE)
  outputOptions(output, "profile_busy", suspendWhenHidden = FALSE)
  outputOptions(output, "disaster_busy", suspendWhenHidden = FALSE)

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

  output$training_intro <- renderUI({
    plan <- training_state()$plan
    intro <- extract_training_intro(plan)
    if (is.null(intro)) return(NULL)

    tags$div(
      class = "card",
      if (!is.null(intro$concerns)) tags$p(intro$concerns),
      if (!is.null(intro$priorities)) tags$p(intro$priorities)
    )
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

  output$download_training_pdf <- downloadHandler(
    filename = function() {
      paste0("training-plan-", Sys.Date(), ".pdf")
    },
    content = function(file) {
      plan <- training_state()$plan
      intro <- extract_training_intro(plan)
      table <- parse_training_plan(plan)
      write_training_pdf(file, intro, table)
    }
  )

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

    summary <- geo_summary()
    data <- model_data()
    risk_ratio <- fatality_risk_ratio_val()
    fatality_rate <- ifelse(!is.null(summary), summary$deaths_per_100k, NA)
    disaster_count <- ifelse(!is.null(summary), summary$disaster_count, NA)
    median_age <- ifelse(!is.null(summary), summary$median_age, NA)
    median_income <- ifelse(!is.null(summary), summary$median_income, NA)
    pct_elderly <- ifelse(!is.null(summary), summary$pct_elderly, NA)
    poverty_rate <- ifelse(!is.null(summary), summary$poverty_rate, NA)
    pop_density <- ifelse(!is.null(summary), summary$pop_density, NA)
    housing_density <- ifelse(!is.null(summary), summary$housing_density, NA)
    no_vehicle_rate <- ifelse(!is.null(summary), summary$no_vehicle_rate, NA)

    summary_text <- paste(
      "Region:", region_text,
      "Fatality rate per 100k:", ifelse(is.na(fatality_rate), "NA", round(fatality_rate, 2)),
      "Relative risk:", ifelse(is.na(risk_ratio), "NA", round(risk_ratio, 2)),
      "Median age:", ifelse(is.na(median_age), "NA", round(median_age, 1)),
      "Median income:", ifelse(is.na(median_income), "NA", round(median_income, 0)),
      "Percent elderly:", ifelse(is.na(pct_elderly), "NA", round(pct_elderly * 100, 1)),
      "Poverty rate:", ifelse(is.na(poverty_rate), "NA", round(poverty_rate * 100, 1)),
      "Households without vehicles:", ifelse(is.na(no_vehicle_rate), "NA", round(no_vehicle_rate * 100, 1)),
      "Population density:", ifelse(is.na(pop_density), "NA", round(pop_density, 1)),
      "Housing density:", ifelse(is.na(housing_density), "NA", round(housing_density, 1)),
      "Disaster count:", ifelse(is.na(disaster_count), "NA", disaster_count),
      "Department makeup:", dept_type_text,
      "Department size:", input$dept_size,
      "Percent volunteer:", input$percent_volunteer,
      "Incident exposure level:", input$incident_exposure,
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

  observeEvent(input$run_disaster_plan, {
    disaster_busy(TRUE)
    on.exit(disaster_busy(FALSE), add = TRUE)

    key <- Sys.getenv("GEMINI_API_KEY")
    if (key == "") {
      disaster_state(list(status = "Missing GEMINI_API_KEY. Set it in .Renviron or your session.", plan = NULL))
      return()
    }

    summary <- geo_summary()
    info <- disaster_summary_data()
    if (is.null(summary) || is.null(info)) {
      disaster_state(list(status = "Disaster data unavailable.", plan = NULL))
      return()
    }

    types <- fema_types_state()
    decl <- fema_decl_state()
    states <- geo_states()
    top_types <- character()
    type_summary <- character()
    if (!is.null(types) && length(states) > 0) {
      top_types <- types %>%
        filter(state %in% states) %>%
        group_by(incidentType) %>%
        summarize(count = sum(count), .groups = "drop") %>%
        slice_max(order_by = count, n = 3, with_ties = FALSE)
      type_summary <- paste(paste0(top_types$incidentType, " (", top_types$count, ")"), collapse = ", ")
      top_types <- top_types$incidentType
    }

    last5_summary <- ""
    if (!is.null(decl) && length(states) > 0) {
      last5 <- decl %>%
        filter(state %in% states, !is.na(year)) %>%
        filter(year >= max(year, na.rm = TRUE) - 4) %>%
        count(incidentType, sort = TRUE) %>%
        slice_head(n = 3)
      if (nrow(last5) > 0) {
        last5_summary <- paste(paste0(last5$incidentType, " (", last5$n, ")"), collapse = ", ")
      }
    }

    mitigation <- hma_state()
    fire_support <- pa_fire_state()
    mitigation_value <- if (!is.null(mitigation) && length(states) > 0) {
      mitigation %>%
        filter(state %in% states) %>%
        summarize(total = sum(mitigation_investment, na.rm = TRUE)) %>%
        pull(total)
    } else NA_real_

    fire_support_count <- if (!is.null(fire_support) && length(states) > 0) {
      fire_support %>%
        filter(state %in% states) %>%
        summarize(total = sum(fire_applicant_count, na.rm = TRUE)) %>%
        pull(total)
    } else NA_real_

    summary_text <- paste(
      "Region:", ifelse(input$geo_mode == "state", input$state, input$region),
      "Population:", round(info$population),
      "Estimated population at risk:", round(info$population_at_risk),
      "Disaster frequency per 100k:", round(info$disasters_per_100k, 2),
      "Population density per sq mile:", round(info$density, 1),
      "Top disaster types (all years):", ifelse(type_summary == "", "NA", type_summary),
      "Top disaster types (last 5 years):", ifelse(last5_summary == "", "NA", last5_summary),
      "Mitigation investment (HMA):", ifelse(is.na(mitigation_value), "NA", round(mitigation_value)),
      "Fire-related PA applicants:", ifelse(is.na(fire_support_count), "NA", fire_support_count),
      "Percent volunteer:", input$percent_volunteer,
      "Incident exposure level:", input$incident_exposure
    )

    plan <- tryCatch(
      generate_disaster_plan(summary_text),
      error = function(e) NULL
    )

    if (is.null(plan)) {
      disaster_state(list(status = "Preparedness plan generation failed. Try again.", plan = NULL))
    } else {
      disaster_state(list(status = "Preparedness plan generated.", plan = plan))
    }
  })
}

shinyApp(ui, server)
