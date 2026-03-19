# Final Project (Shiny)

## Run the app

```r
setwd("/Users/amyfulton/final-project")
shiny::runApp()
```

## Gemini API key setup

1. Create a Gemini API key in Google AI Studio.
2. Add it to your environment as `GEMINI_API_KEY`.

**Local option**: create a `.Renviron` file in this folder using the template `.Renviron.example`:

```r
GEMINI_API_KEY=your_key_here
```

Restart R after setting the key.

## Daily data refresh (USFA feed + API)

The app checks the USFA RSS feed once every 24 hours. If a newer `pubDate` is found, it downloads the latest CSV and caches it at `data/fatalities.csv`. The app will fall back to your local `ff_data.csv` if the API is unavailable.

## Notes

- Never commit real API keys to GitHub.
- `.Renviron` is ignored via `.gitignore`.

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
