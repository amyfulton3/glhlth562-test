# Firefighter Fatality Risk Dashboard (Shiny)

## Overview

This is a **data product** (not a static report). It accepts user input about region, department makeup, incident types, and department challenges, then returns **tailored fatality risk summaries** and **LLM‑generated prevention guidance**. It also includes a separate incident‑report analysis workflow and a monthly training plan generator.

## Capabilities (Requirement Checklist)

- **User input (required):** region/state, personnel makeup, incident types, equipment, and department comments.
- **API integration:** pulls firefighter fatality data from the USFA API feed and CSV endpoints.
- **GenAI in pipeline:** uses Gemini to generate prevention guidance, incident‑report risk summaries, and a monthly training plan.
- **Automation:** automatic daily refresh checks via the USFA RSS feed.

## Data Pipeline Documentation

### 1) Data Sources

- **USFA Firefighter Fatalities API (CSV + RSS feed)**  
  Used for historical fatality records and automated refresh checks.
- **US Census Bureau ACS 5-year estimates (via Census API)**  
  Population, median age, and median household income for risk normalization.
- **FEMA Disaster Declarations Summaries API**  
  Disaster counts by state as an operational exposure proxy.
- **FEMA Disaster Declarations (detail)**  
  Incident types, dates, and program flags for disaster trends.
- **FEMA Hazard Mitigation Assistance (HMA) subapplications**  
  Mitigation investment proxy by state.
- **FEMA Public Assistance Applicants (fire-related)**  
  Proxy for disaster response burden on fire services.

### 2) Ingestion

- **Packages:** `readr`, `httr`, `xml2`, `dplyr`  
- **Process:**  
  - On startup (and daily), the app checks the USFA RSS feed for updates.  
  - If a newer `pubDate` is found, it downloads the latest CSV and caches it locally in `data/fatalities.csv`.  
  - If the API is unavailable, it falls back to the cached file or local `ff_data.csv`.

### 3) Processing

- Column normalization, date parsing, and state→region mapping.
- Incident types are standardized via heuristic classification.
- Personnel type labels are normalized to fixed categories.
- Aggregations for:
  - top causes of death
  - incident mix over time
  - odds‑ratio–style comparisons by personnel type

### 4) Outputs

- **Interactive Shiny dashboard** with multiple tabs:
  - Regional risk summaries + visuals
  - Risk gauge + benchmarking
  - Disaster risk & preparedness (Census + FEMA + LLM)
  - Personnel risk visualization
  - Incident report analysis (LLM)
  - Monthly training plan (LLM + equipment constraints)

### 5) How to Run (Reproducibility)

```r
setwd("/Users/amyfulton/Desktop/glhlth562-test")
shiny::runApp()
```

Dependencies are captured in `manifest.json` for Connect Cloud.

## Gemini API key setup

1. Create a Gemini API key in Google AI Studio.
2. Add it to your environment as `GEMINI_API_KEY`.

**Local option**: create a `.Renviron` file in this folder using the template `.Renviron.example`:

```r
GEMINI_API_KEY=your_key_here
```

Restart R after setting the key.

## Census API key setup

The Census API key is required to pull ACS data.

Set it in your environment as `CENSUS_API_KEY`, for example in `.Renviron`:

```r
CENSUS_API_KEY=your_key_here
```

Restart R after setting the key.

### Posit Connect Cloud

To use the Census API in Connect Cloud, set an app-level environment variable:

1. Open your app in Posit Connect Cloud.
2. Click **Settings** (gear icon).
3. Add an **Environment Variable**:
   - Name: `CENSUS_API_KEY`
   - Value: your key
4. Save, then republish/restart the app.

## Daily data refresh (USFA feed + API)

The app checks the USFA RSS feed once every 24 hours. If a newer `pubDate` is found, it downloads the latest CSV and caches it at `data/fatalities.csv`. The app will fall back to your local `ff_data.csv` if the API is unavailable.

## Notes

- Never commit real API keys to GitHub.
- `.Renviron` is ignored via `.gitignore`.

## Project Structure

```
.
├── app.R
├── data/
│   ├── fatalities.csv
│   └── last_refresh.txt
├── manifest.json
├── .github/workflows/deploy-shinyapps.yml
└── README.md
```

## GitHub Actions deployment (Posit Connect)

This repo includes a workflow at `.github/workflows/deploy-shinyapps.yml`.

### Required GitHub secrets

Set these repository secrets:

- `CONNECT_SERVER` (e.g., `https://connect.your-org.com`)
- `CONNECT_ACCOUNT`
- `CONNECT_TOKEN`
- `CONNECT_SECRET`
- `CONNECT_APPNAME`

### Gemini API key in Posit Connect

Set `GEMINI_API_KEY` in the content environment variables for your deployed app. Posit Connect supports per-content environment variables via the content settings UI.

### GitHub Secrets (reproducible setup)

To keep the app reproducible without committing secrets:

1. Add a GitHub Actions secret named `GEMINI_API_KEY`:
   - Repo → Settings → Secrets and variables → Actions → New repository secret
2. Keep `.Renviron` out of the repo (already in `.gitignore`).
3. Use `.Renviron.example` as the template for local development.

Note: The runtime environment (Posit Connect) must also have `GEMINI_API_KEY` set, since GitHub Actions secrets are only available during deployment, not at app runtime.

## Posit Connect Cloud (Git-backed)

If you are deploying via **connect.posit.cloud** with a Git repository:

1. Repository: `amyfulton3/glhlth562-test`
2. Branch: `main`
3. Primary file: `app.R`
4. Root directory: repository root

Make sure `manifest.json` is present in the repo (it is committed).

## Republishing on Connect Cloud

If **Automatically publish on push** is enabled, any push to `main` will republish the app.

If it’s disabled:
1. Open your app on connect.posit.cloud.
2. Click **Publish** or **Republish**.
3. Choose the latest commit from `main`.
