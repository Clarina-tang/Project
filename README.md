# Project
Portfolio of data analytics projects in R and Python, including interactive Shiny dashboards, data cleaning and reproducible reporting.
## Dashboard
Five linked visualisations in three stages (Monitor & Locate, Diagnose & Prioritise, Business Impact):
1. Late-delivery rate over time (line chart with volume bars)
2. Region x shipping-mode heat-map (click a cell to focus all views)
3. Delivery promise vs reality (diverging bars)
4. Market > region > department treemap (drill-down)
5. Profit by discount band, late vs not late (grouped bars with 95% CIs)
## How to run
1. Get the dataset (`data.csv`, DataCo supply-chain data supplied by the course; not included here because of its size).
2. Create a folder named `data` next to `app.R` and put `data.csv` inside it.
3. Open `app.R` in RStudio and click **Run App**. Missing packages (shiny, bslib, plotly, data.table, lubridate, scales) install automatically.
## Tools
R, Shiny, bslib, plotly, data.table
