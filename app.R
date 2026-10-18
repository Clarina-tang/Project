# =============================================================================
# ETW2001 Assignment 3 - Regional E-Commerce Operations Command Centre
# End user : Regional Head of E-Commerce Operations
# Run      : open this file in RStudio and click "Run App"
#            (or shiny::runApp("path/to/this/folder"))
# Data     : data/data.csv (DataCo supply-chain dataset, read relative to app.R)
# =============================================================================

# ---- 0. Packages (auto-installs anything missing, so no manual editing) ------
.pkgs <- c("shiny", "bslib", "plotly", "data.table", "lubridate", "scales")
.miss <- .pkgs[!vapply(.pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(.miss)) install.packages(.miss, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(plotly)
  library(data.table); library(lubridate)
})

# ---- 1. Data loading & preparation -------------------------------------------
data_path <- c("data/data.csv", "data.csv")
data_path <- data_path[file.exists(data_path)][1]
if (is.na(data_path)) stop("data.csv not found: keep it in the 'data' folder next to app.R")

raw <- fread(
  data_path, encoding = "Latin-1",
  select = c("Days for shipping (real)", "Days for shipment (scheduled)",
             "Delivery Status", "Department Name", "Market", "Order Region",
             "order date (DateOrders)", "Order Item Discount Rate", "Sales",
             "Order Profit Per Order", "Shipping Mode", "Customer Segment")
)
setnames(raw, c("real", "sched", "status", "dept", "market", "region", "od_txt",
                "disc", "sales", "profit", "mode", "segment"))

# Analysis base: lines with a completed shipping outcome. 'Shipping canceled'
# lines are CANCELED / SUSPECTED_FRAUD orders that never shipped (7,754 lines).
dt <- raw[status != "Shipping canceled"]
rm(raw)
dt[, region  := trimws(gsub("\\s+", " ", region))]
dt[, od      := mdy_hm(od_txt)]
dt[, od_date := as.Date(od)]
dt[, month   := as.Date(floor_date(od, "month"))]
dt[, quarter := as.Date(floor_date(od, "quarter"))]
dt[, late    := as.integer(status == "Late delivery")]
dt[, gap     := real - sched]
dt[, loss    := as.integer(profit < 0)]
dt[, disc    := round(disc, 2)]
dt[, outcome := factor(fcase(status == "Late delivery", "Late",
                             status == "Shipping on time", "On time",
                             default = "Early"),
                       levels = c("Early", "On time", "Late"))]
dt[, gapcat := cut(gap, c(-Inf, -2, -1, 0, 1, 2, Inf), right = TRUE,
                   labels = c("2+ days early", "1 day early", "On time",
                              "1 day late", "2 days late", "3+ days late"))]
dt[, od_txt := NULL]

MODES   <- c("Same Day", "First Class", "Second Class", "Standard Class")
dt[, mode := factor(mode, levels = MODES)]
MARKETS <- sort(unique(dt$market))
REGMAP  <- unique(dt[, .(market, region)])[order(market, region)]
DEPTS   <- sort(unique(dt$dept))
SEGS    <- sort(unique(dt$segment))
DATE_MIN <- min(dt$od_date); DATE_MAX <- max(dt$od_date)
# Region order for the heat-map: by market, then by volume (largest first)
REG_ORDER <- dt[, .N, by = .(market, region)][order(market, -N)]$region

# Palettes (colour-blind safe: Okabe-Ito + diverging blue-grey-orange)
COL_MODE   <- c("Same Day" = "#0072B2", "First Class" = "#D55E00",
                "Second Class" = "#009E73", "Standard Class" = "#7F7F7F")
COL_MARKET <- c("Africa" = "#CC79A7", "Europe" = "#0072B2", "LATAM" = "#E69F00",
                "Pacific Asia" = "#009E73", "USCA" = "#56B4E9")
COL_GAP    <- c("2+ days early" = "#1b4f9c", "1 day early" = "#7fb3dd",
                "On time" = "#d3d8df", "1 day late" = "#fdae6b",
                "2 days late" = "#e6550d", "3+ days late" = "#7f2704")
COL_OUT    <- c("Early" = "#2b6cb0", "On time" = "#8b95a1", "Late" = "#d9480f")
FONT <- list(family = "Segoe UI, Helvetica Neue, Arial, sans-serif", size = 12,
             color = "#1f2a37")

pct  <- function(x, d = 1) sprintf(paste0("%.", d, "f%%"), 100 * x)
usd  <- function(x) paste0("$", formatC(x, format = "f", digits = 0, big.mark = ","))
num  <- function(x) formatC(x, format = "f", digits = 0, big.mark = ",")
cfg  <- function(p) config(p, displaylogo = FALSE,
                           modeBarButtonsToRemove = c("lasso2d", "select2d",
                                                      "autoScale2d", "toggleSpikelines"))

# ---- 2. UI -------------------------------------------------------------------
theme <- bs_theme(
  version = 5, bg = "#f4f6f9", fg = "#1f2a37", primary = "#1f3a5f",
  secondary = "#6b7280", success = "#2f855a", danger = "#c2410c",
  base_font = font_collection("Segoe UI", "Helvetica Neue", "Arial", "sans-serif"),
  heading_font = font_collection("Segoe UI", "Helvetica Neue", "Arial", "sans-serif"),
  "font-size-base" = "0.9rem"
)

css <- "
.card{border:1px solid #e3e7ee;box-shadow:0 1px 2px rgba(16,24,40,.04)}
.card-header{font-weight:600;background:#fff;border-bottom:1px solid #eef0f4;padding:.55rem .9rem}
.card-header .sub{font-weight:400;color:#6b7280;font-size:.78rem;margin-left:.4rem}
.card-footer{background:#fafbfc;font-size:.76rem;color:#5b6573;padding:.4rem .9rem}
.kpi{background:#fff;border:1px solid #e3e7ee;border-left:4px solid #1f3a5f;border-radius:6px;
     padding:.5rem .8rem;height:100%}
.kpi .t{font-size:.68rem;letter-spacing:.05em;text-transform:uppercase;color:#6b7280}
.kpi .v{font-size:1.3rem;font-weight:700;line-height:1.2;color:#111827}
.kpi .s{font-size:.72rem;color:#6b7280}
.kpi.bad{border-left-color:#c2410c}.kpi.bad .v{color:#c2410c}
.kpi.good{border-left-color:#2f855a}.kpi.good .v{color:#2f855a}
.focus-bar{background:#fff7ed;border:1px solid #fed7aa;border-radius:6px;padding:.35rem .7rem;
           font-size:.82rem;display:flex;align-items:center;gap:.6rem;flex-wrap:wrap}
.focus-bar.idle{background:#f1f5f9;border-color:#e2e8f0;color:#475569}
.chip{background:#1f3a5f;color:#fff;border-radius:999px;padding:.05rem .6rem;font-size:.76rem}
.ctl{display:flex;gap:1rem;align-items:flex-end;flex-wrap:wrap;margin-bottom:.2rem}
.ctl .form-group,.ctl .shiny-input-container{margin-bottom:0}
.ctl label.control-label{font-size:.72rem;color:#6b7280;margin-bottom:.1rem}
.notes li{margin-bottom:.45rem}
.sidebar .form-label,.sidebar label.control-label{font-size:.74rem;font-weight:600;color:#374151}
"

ctl_radio <- function(id, label, choices, selected = choices[1])
  radioButtons(id, label, choices, selected, inline = TRUE)

ui <- page_sidebar(
  title = tags$span(tags$b("E-Commerce Operations Command Centre"),
                    tags$span(" | Regional Head of E-Commerce Operations",
                              style = "font-weight:400;opacity:.8;font-size:.85rem")),
  theme = theme, fillable = FALSE,
  tags$head(tags$style(HTML(css))),
  sidebar = sidebar(
    width = 290, class = "sidebar",
    tags$div(style = "font-size:.72rem;color:#6b7280", "GLOBAL FILTERS - apply to every view"),
    sliderInput("dates", "Order period", min = DATE_MIN, max = DATE_MAX,
                value = c(DATE_MIN, DATE_MAX), timeFormat = "%b %Y", step = 30),
    selectizeInput("market", "Market", MARKETS, multiple = TRUE,
                   options = list(placeholder = "All markets",
                                  plugins = list("remove_button"))),
    selectizeInput("region", "Region", split(REGMAP$region, REGMAP$market),
                   multiple = TRUE,
                   options = list(placeholder = "All regions",
                                  plugins = list("remove_button"))),
    selectizeInput("mode", "Shipping mode", MODES, multiple = TRUE,
                   options = list(placeholder = "All modes",
                                  plugins = list("remove_button"))),
    selectizeInput("dept", "Product department", DEPTS, multiple = TRUE,
                   options = list(placeholder = "All departments",
                                  plugins = list("remove_button"))),
    selectizeInput("seg", "Customer segment", SEGS, multiple = TRUE,
                   options = list(placeholder = "All segments",
                                  plugins = list("remove_button"))),
    tags$hr(),
    sliderInput("target", "Late-delivery tolerance (target)", min = 0, max = 100,
                value = 20, step = 5, post = "%"),
    tags$div(style = "font-size:.7rem;color:#6b7280;margin-top:-.6rem;margin-bottom:.6rem",
             "Adjustable management target: drives KPI colour, the trend reference line and the heat-map highlight."),
    actionButton("reset", "Reset all", icon = icon("rotate-left"),
                 class = "btn-outline-secondary btn-sm w-100 mb-2"),
    downloadButton("dl", "Download filtered summary (CSV)",
                   class = "btn-outline-primary btn-sm w-100 mb-2"),
    actionLink("about", "Data notes & definitions", icon = icon("circle-info"))
  ),
  uiOutput("focus_ui"),
  uiOutput("kpis"),
  navset_card_tab(
    id = "stage",
    nav_panel(
      "1 | Monitor & Locate",
      layout_columns(
        col_widths = c(6, 6),
        card(full_screen = TRUE,
             card_header("Visual 1 - Late-delivery rate over time",
                         tags$span(class = "sub", "Is the problem episodic or structural?")),
             div(class = "ctl",
                 ctl_radio("gran", "Period", c("Month", "Quarter")),
                 ctl_radio("grp", "Compare by", c("Overall", "Market", "Shipping mode"))),
             plotlyOutput("p_trend", height = "480px"),
             card_footer("Dashed line = your tolerance target. Grey band = Oct 2017 onward, when volume drops by more than half. Bars = order-line volume. Note: each market is only reported in some periods (e.g. LATAM appears in 2015 H1 and 2017 H1), so lines for one market have gaps - see Data notes.")),
        card(full_screen = TRUE,
             card_header("Visual 2 - Hotspot heat-map: region x shipping mode",
                         tags$span(class = "sub", "Click a cell to focus the whole dashboard")),
             div(class = "ctl",
                 radioButtons("hm_metric", "Colour cells by",
                              c("Late-delivery rate", "Days late vs promise",
                                "Profit per order line", "Order-line volume"),
                              inline = TRUE)),
             plotlyOutput("p_heat", height = "560px"),
             card_footer("Cells below the tolerance target are greyed out (late-rate view). Click the same cell again to clear the focus."))
      )
    ),
    nav_panel(
      "2 | Diagnose & Prioritise",
      layout_columns(
        col_widths = c(6, 6),
        card(full_screen = TRUE,
             card_header("Visual 3 - Delivery promise vs reality",
                         tags$span(class = "sub", "Actual minus scheduled days, by shipping mode")),
             plotlyOutput("p_gap", height = "480px"),
             card_footer("Bars diverge from zero: blue = delivered earlier than promised, grey = on the promised day, orange/red = later. Axis labels show the promised vs average actual days.")),
        card(full_screen = TRUE,
             card_header("Visual 4 - Where the business sits: market > region > department",
                         tags$span(class = "sub", "Click a tile to drill down; click the path bar to go back")),
             div(class = "ctl",
                 selectInput("tm_size", "Tile size", c("Sales ($)", "Order lines", "Profit ($)"),
                             width = "150px"),
                 selectInput("tm_col", "Tile colour", c("Late-delivery rate", "Profit margin",
                                                        "Loss-making share"), width = "190px")),
             plotlyOutput("p_tree", height = "430px"),
             card_footer("Size = scale of the business, colour = performance. Colour range is set to the observed spread, so small differences look large - read the labelled values."))
      )
    ),
    nav_panel(
      "3 | Business Impact",
      layout_columns(
        col_widths = c(8, 4),
        card(full_screen = TRUE,
             card_header("Visual 5 - Profit by discount band: late vs not late",
                         tags$span(class = "sub", "Does late delivery cost margin? Does discounting?")),
             div(class = "ctl",
                 ctl_radio("p5_metric", "Measure",
                           c("Avg profit per order line ($)", "Share of loss-making lines",
                             "Avg profit ratio"))),
             plotlyOutput("p_profit", height = "470px"),
             card_footer("Bars = mean for each discount band, split by late vs not late; whiskers = 95% confidence interval. Similar bar heights mean delivery outcome makes no visible difference; bars falling to the right show the discount effect.")),
        card(card_header("Analyst notes for the current selection"),
             uiOutput("notes"),
             card_footer("Generated from the filtered data; wording updates with every filter or focus."))
      )
    )
  )
)

# ---- 3. Server ---------------------------------------------------------------
server <- function(input, output, session) {

  # ---- cascading filters ------------------------------------------------------
  observeEvent(input$market, ignoreNULL = FALSE, {
    rm <- if (length(input$market)) REGMAP[market %in% input$market] else REGMAP
    updateSelectizeInput(session, "region",
                         choices = split(rm$region, rm$market),
                         selected = intersect(input$region, rm$region))
  })

  # ---- focus (click on heat-map cell) ---------------------------------------
  focus <- reactiveVal(NULL)
  observeEvent(event_data("plotly_click", source = "heat", priority = "event"), {
    ev <- event_data("plotly_click", source = "heat", priority = "event")
    req(ev)
    reg <- as.character(ev$y)
    md  <- if (is.numeric(ev$x)) MODES[round(ev$x) + 1] else as.character(ev$x)
    new <- list(region = reg, mode = md)
    cur <- focus()
    if (!is.null(cur) && identical(cur, new)) focus(NULL) else focus(new)
  })
  observeEvent(input$clear_focus, focus(NULL))
  observeEvent(input$reset, {
    updateSliderInput(session, "dates", value = c(DATE_MIN, DATE_MAX))
    for (id in c("market", "region", "mode", "dept", "seg"))
      updateSelectizeInput(session, id, selected = character(0))
    updateSliderInput(session, "target", value = 20)
    focus(NULL)
  })

  # ---- reactive data ---------------------------------------------------------
  fd <- reactive({                       # global filters only
    d <- dt[od_date >= input$dates[1] & od_date <= input$dates[2]]
    if (length(input$market)) d <- d[market %in% input$market]
    if (length(input$region)) d <- d[region %in% input$region]
    if (length(input$mode))   d <- d[mode %in% input$mode]
    if (length(input$dept))   d <- d[dept %in% input$dept]
    if (length(input$seg))    d <- d[segment %in% input$seg]
    d
  })
  fdf <- reactive({                      # global filters + heat-map focus
    d <- fd(); f <- focus()
    if (!is.null(f)) d <- d[region == f$region & mode == f$mode]
    d
  })

  # ---- focus banner ----------------------------------------------------------
  output$focus_ui <- renderUI({
    f <- focus()
    if (is.null(f))
      div(class = "focus-bar idle", icon("hand-pointer"),
          "Tip: click a cell in the Visual 2 heat-map to focus Visuals 1, 3, 4, 5 and the KPIs on one region x shipping-mode combination.")
    else
      div(class = "focus-bar", icon("crosshairs"), tags$b("Focus:"),
          span(class = "chip", f$region), span(class = "chip", f$mode),
          actionLink("clear_focus", "clear focus", icon = icon("xmark")))
  })

  # ---- KPI strip -------------------------------------------------------------
  output$kpis <- renderUI({
    d <- fdf()
    validate(need(nrow(d) > 0, "No order lines match the current filters."))
    lr <- mean(d$late); tg <- input$target / 100
    kpi <- function(t, v, s, cls = "") div(class = paste("kpi", cls),
                                           div(class = "t", t), div(class = "v", v),
                                           div(class = "s", s))
    layout_columns(
      col_widths = c(2, 2, 2, 2, 2, 2), fill = FALSE, class = "mb-3",
      kpi("Order lines shipped", num(nrow(d)), "analysis base (excl. cancelled)"),
      kpi("Late-delivery rate", pct(lr),
          sprintf("target <= %s | %s pp %s target", pct(tg, 0),
                  formatC(abs(100 * (lr - tg)), format = "f", digits = 1),
                  if (lr > tg) "above" else "below"),
          if (lr > tg) "bad" else "good"),
      kpi("Promised vs actual", sprintf("%.1f vs %.1f d", mean(d$sched), mean(d$real)),
          "average days to deliver"),
      kpi("Sales", usd(sum(d$sales)), "sum of order-line sales"),
      kpi("Profit (margin)", sprintf("%s (%s)", usd(sum(d$profit)),
                                     pct(sum(d$profit) / sum(d$sales))), "benefit per order"),
      kpi("Loss-making lines", pct(mean(d$loss)), "profit below zero")
    )
  })

  # ---- Visual 1: trend -------------------------------------------------------
  output$p_trend <- renderPlotly({
    d <- fdf()
    validate(need(nrow(d) > 0, "No data for this selection."))
    per <- if (input$gran == "Quarter") "quarter" else "month"
    d[, period := get(per)]
    gcol <- switch(input$grp, "Overall" = NULL, "Market" = "market", "Shipping mode" = "mode")
    d[, grp := if (is.null(gcol)) "All selected" else as.character(get(gcol))]
    a <- d[, .(n = .N, late = mean(late)), by = .(period, grp)][order(period)]
    a[n < 30, late := NA_real_]
    v <- d[, .(n = .N), by = period][order(period)]
    pal <- switch(input$grp, "Overall" = c("All selected" = "#1f3a5f"),
                  "Market" = COL_MARKET, "Shipping mode" = COL_MODE)
    p1 <- plot_ly(source = "trend")
    for (g in unique(a$grp)) {
      s <- a[grp == g]
      p1 <- add_trace(p1, data = s, x = ~period, y = ~late, type = "scatter",
                      mode = "lines+markers", name = g, connectgaps = FALSE,
                      line = list(color = pal[[g]], width = 2.4),
                      marker = list(color = pal[[g]], size = 5),
                      text = ~sprintf("%s<br>%s<br>Late rate: %s<br>Order lines: %s",
                                      g, format(period, if (per == "month") "%b %Y" else "Q%q %Y"),
                                      pct(late), num(n)),
                      hoverinfo = "text")
    }
    rng <- range(a$period)
    if (max(a$period) >= as.Date("2017-10-01"))
      p1 <- add_trace(p1, x = as.Date(c("2017-10-01", format(max(a$period) + 20), format(max(a$period) + 20), "2017-10-01")),
                      y = c(0, 0, 1, 1), type = "scatter", mode = "lines", fill = "toself",
                      fillcolor = "rgba(120,120,120,.16)", line = list(width = 0),
                      name = "Reduced volume (Oct 2017+)", hoverinfo = "none")
    p1 <- add_trace(p1, x = rng, y = rep(input$target / 100, 2), type = "scatter",
                    mode = "lines", name = "Target", hoverinfo = "none",
                    line = list(color = "#111827", dash = "dash", width = 1.3))
    p2 <- plot_ly(v, source = "trend", x = ~period, y = ~n, type = "bar", name = "Order lines",
                  marker = list(color = "#b8c2d0"), showlegend = FALSE,
                  textposition = "none",
                  hovertext = ~sprintf("%s order lines", num(n)), hoverinfo = "text")
    p <- subplot(p1, p2, nrows = 2, shareX = TRUE, heights = c(.72, .28), titleY = TRUE)
    p %>% layout(font = FONT, hovermode = "closest",
                 margin = list(l = 60, r = 10, t = 10, b = 40),
                 legend = list(orientation = "h", y = 1.06, x = 0),
                 yaxis = list(title = "Late-delivery rate", tickformat = ".0%",
                              range = c(0, 1), gridcolor = "#eef0f4"),
                 yaxis2 = list(title = "Order lines", gridcolor = "#eef0f4"),
                 xaxis = list(title = "", gridcolor = "#eef0f4")) %>% cfg()
  })

  # ---- Visual 2: heat-map ----------------------------------------------------
  output$p_heat <- renderPlotly({
    d <- fd()
    validate(need(nrow(d) > 0, "No data for this selection."))
    a <- d[, .(n = .N, late = mean(late), gap = mean(gap), profit = mean(profit)),
           by = .(region, mode)]
    regs <- REG_ORDER[REG_ORDER %in% a$region]
    mods <- MODES[MODES %in% a$mode]
    mk <- function(col) {
      m <- matrix(NA_real_, length(regs), length(mods), dimnames = list(regs, mods))
      for (i in seq_len(nrow(a))) m[a$region[i], as.character(a$mode[i])] <-
          if (a$n[i] >= 30) a[[col]][i] else NA_real_
      m
    }
    nm <- matrix(0, length(regs), length(mods), dimnames = list(regs, mods))
    for (i in seq_len(nrow(a))) nm[a$region[i], as.character(a$mode[i])] <- a$n[i]
    met <- input$hm_metric
    col <- switch(met, "Late-delivery rate" = "late", "Days late vs promise" = "gap",
                  "Profit per order line" = "profit", "Order-line volume" = "n")
    z <- if (col == "n") { m <- nm; m[m < 30] <- NA; m } else mk(col)
    fmt <- function(x) switch(col, late = pct(x, 0), gap = sprintf("%+.1f", x),
                              profit = sprintf("$%.0f", x), n = num(x))
    txt <- matrix(ifelse(is.na(z), "", vapply(z, fmt, "")), nrow(z), dimnames = dimnames(z))
    hov <- matrix(sprintf("%s | %s<br>%s: %s<br>Order lines: %s",
                          rep(regs, times = length(mods)), rep(mods, each = length(regs)),
                          met, ifelse(is.na(z), "n/a (<30 lines)", txt), num(nm)),
                  nrow(z), dimnames = dimnames(z))
    scale <- switch(col,
      late = list(c(0, "#fff5eb"), c(.35, "#fdae6b"), c(.7, "#d94801"), c(1, "#7f2704")),
      gap = list(c(0, "#1b4f9c"), c(.5, "#f3f4f6"), c(1, "#d94801")),
      profit = list(c(0, "#f7fcf5"), c(.5, "#74c476"), c(1, "#00441b")),
      n = list(c(0, "#f7fbff"), c(.5, "#6baed6"), c(1, "#08306b")))
    tg <- input$target / 100
    p <- plot_ly(source = "heat")
    if (col == "late" && tg > 0) {
      below <- !is.na(z) & z < tg
      zlo <- ifelse(below, 1, NA_real_); zhi <- ifelse(below, NA_real_, z)
      p <- add_heatmap(p, x = mods, y = regs, z = zlo, colorscale = list(c(0, "#e5e7eb"), c(1, "#e5e7eb")),
                       showscale = FALSE, text = txt, texttemplate = "%{text}",
                       hoverinfo = "text", hovertext = hov, xgap = 2, ygap = 2,
                       textfont = list(color = "#6b7280", size = 10))
      p <- add_heatmap(p, x = mods, y = regs, z = zhi, colorscale = scale, zmin = 0, zmax = 1,
                       text = txt, texttemplate = "%{text}", hoverinfo = "text",
                       hovertext = hov, xgap = 2, ygap = 2, textfont = list(size = 10),
                       colorbar = list(title = "", tickformat = ".0%", len = .5, thickness = 10,
                                       y = .98, yanchor = "top"))
    } else {
      zmx <- max(abs(z), na.rm = TRUE)
      p <- add_heatmap(p, x = mods, y = regs, z = z, colorscale = scale,
                       zmid = if (col == "gap") 0 else NULL,
                       zmin = if (col == "gap") -zmx else NULL,
                       zmax = if (col == "gap") zmx else NULL,
                       text = txt, texttemplate = "%{text}", hoverinfo = "text",
                       hovertext = hov, xgap = 2, ygap = 2, textfont = list(size = 10),
                       colorbar = list(title = "", len = .5, thickness = 10,
                                       y = .98, yanchor = "top"))
    }
    shapes <- list(); f <- focus()
    if (!is.null(f) && f$region %in% regs && f$mode %in% mods) {
      xi <- match(f$mode, mods) - 1; yi <- match(f$region, regs) - 1
      shapes <- list(list(type = "rect", xref = "x", yref = "y",
                          x0 = xi - .5, x1 = xi + .5, y0 = yi - .5, y1 = yi + .5,
                          line = list(color = "#111827", width = 3)))
    }
    p %>% layout(font = FONT, shapes = shapes,
                 margin = list(l = 100, r = 10, t = 30, b = 10),
                 xaxis = list(side = "top", title = "", fixedrange = TRUE, tickangle = 0, tickfont = list(size = 10.5)),
                 yaxis = list(autorange = "reversed", categoryorder = "array",
                              categoryarray = regs, title = "", fixedrange = TRUE,
                              tickfont = list(size = 10.5))) %>%
      event_register("plotly_click") %>% cfg()
  })

  # ---- Visual 3: delivery promise vs reality ---------------------------------
  output$p_gap <- renderPlotly({
    d <- fd(); f <- focus()
    if (!is.null(f)) d <- d[region == f$region]
    validate(need(nrow(d) > 0, "No data for this selection."))
    s <- d[, .(n = .N, sched = mean(sched), real = mean(real), late = mean(late)),
           by = mode][order(mode)]
    g <- d[, .N, by = .(mode, gapcat)]
    g[, p := N / sum(N), by = mode]
    mods <- as.character(s$mode)
    lab <- sprintf("<b>%s</b><br>promised %.1f d | actual %.1f d", mods, s$sched, s$real)
    names(lab) <- mods
    val <- function(cat) vapply(mods, function(m) {
      x <- g[mode == m & gapcat == cat]; if (nrow(x)) x$p else 0 }, 0)
    cnt <- function(cat) vapply(mods, function(m) {
      x <- g[mode == m & gapcat == cat]; if (nrow(x)) x$N else 0L }, 0L)
    op <- if (!is.null(f) && f$mode %in% mods) ifelse(mods == f$mode, 1, .35) else rep(1, length(mods))
    mk_bar <- function(p, cat, sign, nm, show = TRUE, half = FALSE) {
      v <- val(cat) * (if (half) .5 else 1) * sign
      add_bars(p, x = v, y = lab[mods], orientation = "h", name = nm, legendgroup = cat,
               showlegend = show, marker = list(color = COL_GAP[[cat]], opacity = op,
                                                line = list(color = "white", width = .5)),
               textposition = "none",
               hovertext = sprintf("%s: %s of lines (n = %s)", cat, pct(val(cat)), num(cnt(cat))),
               hoverinfo = "text")
    }
    p <- plot_ly() %>% layout(barmode = "relative")
    p <- mk_bar(p, "On time", -1, "On time", FALSE, TRUE)
    p <- mk_bar(p, "1 day early", -1, "1 day early")
    p <- mk_bar(p, "2+ days early", -1, "2+ days early")
    p <- mk_bar(p, "On time", 1, "On time", TRUE, TRUE)
    p <- mk_bar(p, "1 day late", 1, "1 day late")
    p <- mk_bar(p, "2 days late", 1, "2 days late")
    p <- mk_bar(p, "3+ days late", 1, "3+ days late")
    neg <- -(val("On time") / 2 + val("1 day early") + val("2+ days early"))
    xl  <- min(neg, -.1) - .04
    ann <- lapply(seq_along(mods), function(i)
      list(x = 1.03, y = lab[mods[i]], xref = "x", yref = "y", showarrow = FALSE,
           xanchor = "left", text = sprintf("<b>%s late</b>", pct(s$late[i], 0)),
           font = list(color = "#c2410c", size = 12)))
    tv <- round(seq(-.6, 1, .2), 1)
    p %>% layout(font = FONT, annotations = ann,
                 margin = list(l = 10, r = 10, t = 20, b = 40),
                 legend = list(orientation = "h", y = -.12, x = 0),
                 xaxis = list(title = "Share of order lines", range = c(xl, 1.38),
                              tickvals = tv, ticktext = paste0(round(abs(tv) * 100), "%"),
                              zeroline = TRUE, zerolinewidth = 2, zerolinecolor = "#111827",
                              gridcolor = "#eef0f4"),
                 yaxis = list(title = "", categoryorder = "array",
                              categoryarray = rev(unname(lab[mods])), automargin = TRUE)) %>% cfg()
  })

  # ---- Visual 4: treemap -----------------------------------------------------
  output$p_tree <- renderPlotly({
    d <- fdf()
    validate(need(nrow(d) > 0, "No data for this selection."))
    sm <- input$tm_size; cm <- input$tm_col
    leaf <- d[, .(n = .N, sales = sum(sales), profit = sum(profit), late = sum(late),
                  loss = sum(loss)), by = .(market, region, dept)]
    leaf[, size := switch(sm, "Sales ($)" = sales, "Order lines" = as.numeric(n),
                          "Profit ($)" = pmax(profit, 0))]
    agg <- function(by) leaf[, .(n = sum(n), sales = sum(sales), profit = sum(profit),
                                 late = sum(late), loss = sum(loss), size = sum(size)), by = by]
    nodes <- rbindlist(list(
      agg("market")[, .(id = paste0("M|", market), label = market, parent = "", n, sales, profit, late, loss, size)],
      agg(c("market", "region"))[, .(id = paste0("R|", market, "|", region), label = region,
                                     parent = paste0("M|", market), n, sales, profit, late, loss, size)],
      leaf[, .(id = paste0("D|", market, "|", region, "|", dept), label = dept,
               parent = paste0("R|", market, "|", region), n, sales, profit, late, loss, size)]))
    nodes <- nodes[size > 0]
    nodes[, colv := switch(cm, "Late-delivery rate" = late / n, "Profit margin" = profit / sales,
                           "Loss-making share" = loss / n)]
    ok <- nodes[n >= 200]$colv; if (length(ok) < 3) ok <- nodes$colv
    lo <- as.numeric(quantile(ok, .05)); hi <- as.numeric(quantile(ok, .95))
    if (hi - lo < 1e-6) { lo <- lo - .01; hi <- hi + .01 }
    nodes[, cc := pmin(pmax(colv, lo), hi)]
    scale <- switch(cm,
      "Late-delivery rate" = list(c(0, "#fff5eb"), c(.5, "#fd8d3c"), c(1, "#7f2704")),
      "Profit margin" = list(c(0, "#f7fcf5"), c(.5, "#74c476"), c(1, "#00441b")),
      "Loss-making share" = list(c(0, "#fff7ec"), c(.5, "#fc8d59"), c(1, "#7f0000")))
    txt <- sprintf("%s %s", pct(nodes$colv), switch(cm, "Late-delivery rate" = "late",
                                                    "Profit margin" = "margin", "Loss-making share" = "loss lines"))
    hov <- sprintf("<b>%s</b><br>Sales: %s<br>Profit: %s (%s margin)<br>Order lines: %s<br>Late rate: %s<br>Loss-making lines: %s",
                   nodes$label, usd(nodes$sales), usd(nodes$profit), pct(nodes$profit / nodes$sales),
                   num(nodes$n), pct(nodes$late / nodes$n), pct(nodes$loss / nodes$n))
    plot_ly(type = "treemap", ids = nodes$id, labels = nodes$label, parents = nodes$parent,
            values = nodes$size, branchvalues = "total", maxdepth = 2,
            marker = list(colors = nodes$cc, colorscale = scale, cmin = lo, cmax = hi,
                          showscale = TRUE, line = list(width = 1.5, color = "white"),
                          colorbar = list(title = "", tickformat = if (cm == "Late-delivery rate") ".1%" else ".0%",
                                          thickness = 10, len = .6)),
            text = txt, texttemplate = "<b>%{label}</b><br>%{text}",
            hovertext = hov, hoverinfo = "text", textfont = list(size = 12),
            pathbar = list(visible = TRUE)) %>%
      layout(font = FONT, margin = list(l = 5, r = 5, t = 25, b = 5)) %>% cfg()
  })

  # ---- Visual 5: profitability by discount band, late vs not late ------------
  output$p_profit <- renderPlotly({
    d <- copy(fdf())
    validate(need(nrow(d) > 0, "No data for this selection."))
    met <- input$p5_metric
    d[, band := cut(disc * 100, c(-1, 4.5, 9.5, 14.5, 19.5, 100),
                    labels = c("0-4%", "5-9%", "10-14%", "15-19%", "20-25%"))]
    d[, grp := factor(ifelse(late == 1, "Late", "Not late (on time or early)"),
                      levels = c("Not late (on time or early)", "Late"))]
    a <- d[, {
      r <- sum(profit) / sum(sales); pl <- mean(loss)
      .(n = .N, m_profit = mean(profit), se_profit = sd(profit) / sqrt(.N),
        p_loss = pl, se_loss = sqrt(pl * (1 - pl) / .N),
        ratio = r, se_ratio = sd(profit - r * sales) / (mean(sales) * sqrt(.N)))
    }, by = .(band, grp)][n >= 30][order(band)]
    validate(need(nrow(a) > 0, "Too few order lines for this selection."))
    a[, `:=`(y = switch(met, "Avg profit per order line ($)" = m_profit,
                        "Share of loss-making lines" = p_loss, "Avg profit ratio" = ratio),
             se = switch(met, "Avg profit per order line ($)" = se_profit,
                         "Share of loss-making lines" = se_loss, "Avg profit ratio" = se_ratio))]
    yfmt <- function(x) switch(met, "Avg profit per order line ($)" = sprintf("$%.1f", x),
                               "Share of loss-making lines" = pct(x), "Avg profit ratio" = pct(x))
    cols <- c("Not late (on time or early)" = "#8b95a1", "Late" = "#d9480f")
    p <- plot_ly()
    for (g in levels(a$grp)) {
      s <- a[grp == g]; if (!nrow(s)) next
      p <- add_bars(p, data = s, x = ~band, y = ~y, name = g,
                    marker = list(color = cols[[g]]),
                    error_y = list(type = "data", array = 1.96 * s$se, color = "#374151",
                                   thickness = 1.2, width = 4),
                    text = yfmt(s$y), textposition = "inside", insidetextanchor = "start",
                    textfont = list(color = "white", size = 11),
                    hovertext = sprintf("%s<br>Discount band: %s<br>%s: %s (95%% CI +/- %s)<br>Order lines: %s",
                                        g, s$band, met, yfmt(s$y), yfmt(1.96 * s$se), num(s$n)),
                    hoverinfo = "text")
    }
    p %>% layout(font = FONT, barmode = "group", bargap = .25,
                 margin = list(l = 60, r = 10, t = 10, b = 45),
                 legend = list(orientation = "h", y = 1.08, x = 0),
                 xaxis = list(title = "Discount band on the order line", gridcolor = "#eef0f4"),
                 yaxis = list(title = met, gridcolor = "#eef0f4", rangemode = "tozero",
                              tickformat = if (met == "Avg profit per order line ($)") "$.0f" else ".0%")) %>%
      cfg()
  })

  # ---- Analyst notes ---------------------------------------------------------
  output$notes <- renderUI({
    d <- fdf()
    validate(need(nrow(d) > 0, "No data for this selection."))
    lr <- mean(d$late); tg <- input$target / 100
    items <- list(tags$li(HTML(sprintf(
      "<b>%s</b> order lines: <b>%s late</b> (%s pp %s your %s target). Promised %.1f days, actual %.1f days.",
      num(nrow(d)), pct(lr), formatC(abs(100 * (lr - tg)), format = "f", digits = 1),
      if (lr > tg) "above" else "below", pct(tg, 0), mean(d$sched), mean(d$real)))))
    m <- d[, .(n = .N, late = mean(late)), by = mode][n >= 100][order(-late)]
    if (nrow(m) >= 2)
      items <- c(items, list(tags$li(HTML(sprintf(
        "Shipping mode spread: <b>%s</b> is worst (%s late); <b>%s</b> is best (%s) - a gap of <b>%.0f pp</b>.",
        m$mode[1], pct(m$late[1], 0), m$mode[nrow(m)], pct(m$late[nrow(m)], 0),
        100 * (m$late[1] - m$late[nrow(m)]))))))
    r <- d[, .(n = .N, late = mean(late)), by = region][n >= 500][order(-late)]
    if (nrow(r) >= 2)
      items <- c(items, list(tags$li(HTML(sprintf(
        "Region spread (regions with 500+ lines): worst <b>%s</b> %s, best <b>%s</b> %s - only <b>%.1f pp</b> apart.",
        r$region[1], pct(r$late[1]), r$region[nrow(r)], pct(r$late[nrow(r)]),
        100 * (r$late[1] - r$late[nrow(r)]))))))
    lt <- d[outcome == "Late"]$profit; ot <- d[outcome == "On time"]$profit
    if (length(lt) >= 30 && length(ot) >= 30) {
      tt <- tryCatch(t.test(lt, ot)$p.value, error = function(e) NA_real_)
      items <- c(items, list(tags$li(HTML(sprintf(
        "Profit per line: late <b>%s</b> vs on-time <b>%s</b> (difference %s; Welch t-test p = %s, %s).",
        usd(mean(lt)), usd(mean(ot)), usd(mean(lt) - mean(ot)),
        if (is.na(tt)) "n/a" else formatC(tt, format = "f", digits = 3),
        if (!is.na(tt) && tt < .05) "statistically significant" else "not statistically significant")))))
    }
    lm <- d[, .(n = .N, l = mean(loss)), by = mode][n >= 100]
    items <- c(items, list(tags$li(HTML(sprintf(
      "<b>%s</b> of lines lose money%s.", pct(mean(d$loss)),
      if (nrow(lm) >= 2) sprintf(" (from %s to %s across shipping modes)", pct(min(lm$l)), pct(max(lm$l))) else "")))))
    tags$ul(class = "notes ps-3", items)
  })

  # ---- About modal -----------------------------------------------------------
  observeEvent(input$about, showModal(modalDialog(
    title = "Data notes & definitions", easyClose = TRUE, size = "l",
    tags$ul(
      tags$li(tags$b("Unit of analysis: "), "one order line (an item within an order), as recorded in data.csv."),
      tags$li(tags$b("Analysis base: "), sprintf("%s order lines with a completed shipping outcome. 7,754 lines with 'Shipping canceled' (all CANCELED or SUSPECTED_FRAUD orders) are excluded from every view because they never shipped.", num(nrow(dt)))),
      tags$li(tags$b("Late-delivery rate: "), "share of lines where actual shipping days exceed scheduled shipping days (Delivery Status = 'Late delivery')."),
      tags$li(tags$b("Days late vs promise: "), "actual minus scheduled shipping days (negative = early)."),
      tags$li(tags$b("Profit: "), "'Order Profit Per Order' (benefit per order). Profit ratio = profit / sales."),
      tags$li(tags$b("Coverage warning: "), "markets are not reported evenly over time. Africa appears only Jul 2016 - Mar 2017, USCA mainly Apr - Dec 2016, LATAM only in 2015 H1 and 2017 H1, and from Oct 2017 only Europe and Pacific Asia appear at about 40% of the earlier monthly volume. Market-level trends therefore mix real change with changing coverage; the overall rate is the safer trend signal."),
      tags$li(tags$b("Small cells: "), "heat-map cells and trend points with fewer than 30 lines are not coloured/plotted."),
      tags$li(tags$b("Target: "), "the late-delivery tolerance slider is a management assumption (default 20%), not a value taken from the data.")))))

  # ---- Download --------------------------------------------------------------
  output$dl <- downloadHandler(
    filename = function() paste0("filtered_delivery_summary_", Sys.Date(), ".csv"),
    content = function(file) {
      d <- fd()
      out <- d[, .(order_lines = .N, late_rate = round(mean(late), 4),
                   avg_scheduled_days = round(mean(sched), 2), avg_actual_days = round(mean(real), 2),
                   sales = round(sum(sales), 2), profit = round(sum(profit), 2),
                   loss_making_share = round(mean(loss), 4)), by = .(market, region, mode)]
      fwrite(out[order(market, region, mode)], file)
    })
}

shinyApp(ui, server)
