# Fire Department Preparedness Dashboard (Shiny)

## Overview

This is a **data product** (not a static report). It accepts user input about region, department makeup, incident types, and department challenges, then returns **tailored fatality risk summaries** and **LLM‑generated guidance**. It also includes incident‑report analysis, a monthly training plan generator, and a disaster‑preparedness module.

## Capabilities (Requirement Checklist)

- **User input (required):** region/state, personnel makeup, incident types, equipment, and department comments.
- **API integration:** pulls firefighter fatality data from the USFA API feed and CSV endpoints, plus Census ACS + FEMA OpenFEMA APIs.
- **GenAI in pipeline:** uses Gemini to generate prevention guidance, incident‑report risk summaries, training plans, and disaster‑preparedness guidance.
- **Automation:** automatic daily refresh checks and API re‑pulls while the app is running (plus a manual refresh button).

## Data Pipeline Documentation

### 1) Data Sources

- **USFA Firefighter Fatalities API (CSV + RSS feed)**  
  Historical line‑of‑duty fatality records + auto‑refresh checks.
- **US Census Bureau ACS 5‑year estimates (API)**  
  Population, median age, median income, poverty rate, elderly share, housing units, and vehicle access.
- **FEMA OpenFEMA APIs**  
  Disaster declarations and related datasets used for exposure and preparedness context.

### 2) Ingestion

- **Packages:** `readr`, `httr`, `xml2`, `jsonlite`, `dplyr`, `plotly`  
  - Optional for static maps: `maps` (used for state polygons; installed on Connect via manifest)
- **Process:**  
  - On startup (and daily), the app checks the USFA RSS feed for updates.  
  - If a newer `pubDate` is found, it downloads the latest CSV and caches it locally in `data/fatalities.csv`.  
  - Census and FEMA data are pulled via API at startup and refreshed on the same daily timer (or via the **Refresh Data Now** button).  
  - If APIs are unavailable, the app falls back to cached fatality data and displays availability notes for Census/FEMA‑dependent panels.

### 3) Processing

- Column normalization, date parsing, and state→region mapping.
- Incident types are standardized via heuristic classification.
- Personnel type labels are normalized to fixed categories.
- Aggregations for:
  - top causes of death
  - incident mix over time
  - odds‑ratio–style comparisons by personnel type
- **Modeling (fatality risk gauge):** Poisson regression on fatalities by state using population offset and predictors:
  - log population density (urban vs. rural operational complexity)
  - log housing density (structure fire exposure)
  - FEMA disaster count (operational surge exposure)
  - plus scenario adjustments based on department makeup, incident exposure level, and incident types.
- **Disaster risk gauge:** standardized composite index using Census + FEMA indicators.

### 4) Outputs

- **Interactive Shiny dashboard** with multiple tabs:
  - Welcome (capabilities + navigation)
  - Regional Risk Profile
  - Personnel
  - Fatality Risk Gauge
  - Geographic Trends
  - Prevention Guidance (LLM)
  - Incident Report Analysis (LLM)
  - Training Plan (LLM + equipment constraints, PDF download)
  - Individual Guidance (LLM)
  - Disaster Risk Gauge
  - Disaster Risk & Preparedness (LLM)

### 5) How to Run (Reproducibility)

```r
setwd("/Users/amyfulton/Desktop/glhlth562-test")
shiny::runApp()
```

Dependencies are captured in `manifest.json` for Connect Cloud.  
For local runs, install the packages listed at the top of `app.R`.

## Code Organization

`app.R` is organized into clearly labeled sections:
- Packages and configuration
- Helpers (data normalization, refresh logic, modeling, LLM calls)
- UI definitions
- Server logic

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

## FEMA API access

FEMA OpenFEMA endpoints used in this project do **not** require an API key.

## Daily data refresh (USFA feed + API + Census/FEMA)

The app checks the USFA RSS feed once every 24 hours while running. If a newer `pubDate` is found, it downloads the latest CSV and caches it at `data/fatalities.csv`. Census and FEMA datasets are also re‑pulled on the same daily timer and on manual refresh. The app will fall back to your local `ff_data.csv` if the USFA API is unavailable.

## Notes

- Never commit real API keys to GitHub.
- `.Renviron` is ignored via `.gitignore`.

## Project Structure

```
.
├── app.R
├── R/
│   └── .gitkeep
├── deck/
│   ├── presentation.qmd
│   ├── presentation.html
│   └── presentation_files/
├── data/
│   ├── fatalities.csv
│   └── last_refresh.txt
├── manifest.json
├── .github/workflows/deploy-shinyapps.yml
├── .gitignore
└── README.md
```

Additional files used locally:
- `.Renviron.example` (template for API keys)
- `scripts/` (optional helper scripts)

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

## Rubric Coverage Summary (Quick Check)

- **User input:** yes (region/state, personnel makeup, incident types, equipment, comments, profile inputs).
- **API integration:** yes (USFA + Census + FEMA).
- **GenAI in pipeline:** yes (Gemini generates guidance, incident analysis, training plans, preparedness plan, profile summaries).
- **Automation:** yes (daily refresh timer + manual refresh).
- **Reproducibility:** documented env vars + `manifest.json` + deployment steps.

## Presentation Slides (Quarto)

The presentation deck lives at `deck/presentation.qmd`.

To render it locally:

```bash
quarto render deck/presentation.qmd
```
