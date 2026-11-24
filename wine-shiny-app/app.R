# Australian Wine Sales Explorer & Forecaster
# ADS 506 - Week 5: Storytelling with Shiny

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(lubridate)
library(tsibble)
library(fable)
library(feasts)

# ---- Data -------------------------------------------------------------------
# Column names have trailing spaces and inconsistent case; Rose has "*" for
# Jul-94 and Aug-94. Read "*" as NA, then fill those two months by linear
# interpolation so ETS/ARIMA can be fit.
wine <- read_csv("AustralianWines.csv", na = c("", "*"), show_col_types = FALSE) |>
  rename_with(~ tools::toTitleCase(trimws(.x))) |>
  mutate(Month = yearmonth(my(Month))) |>
  pivot_longer(-Month, names_to = "Varietal", values_to = "Sales") |>
  group_by(Varietal) |>
  arrange(Month, .by_group = TRUE) |>
  mutate(Sales = approx(seq_along(Sales), Sales, seq_along(Sales), rule = 2)$y) |>
  ungroup() |>
  as_tsibble(key = Varietal, index = Month)

varietals  <- sort(unique(wine$Varietal))
data_start <- as.Date(min(wine$Month))
data_end   <- as.Date(max(wine$Month))

model_colors <- c(TSLM = "#1b9e77", ETS = "#d95f02", ARIMA = "#7570b3", SNAIVE = "#999999")
ylab_sales <- "Sales (thousands of liters)"

# Translate an ETS or ARIMA label into plain language for the spec table
describe_spec <- function(spec) {
  if (startsWith(spec, "ETS")) {
    p <- strsplit(gsub("ETS\\(|\\)", "", spec), ",")[[1]]
    err <- c(A = "additive", M = "multiplicative")[p[1]]
    trd <- c(N = "no trend", A = "additive trend", Ad = "damped trend")[p[2]]
    sea <- c(N = "no seasonality", A = "additive seasonality",
             M = "multiplicative seasonality")[p[3]]
    return(paste0(err, " errors, ", trd, ", ", sea))
  }
  if (startsWith(spec, "ARIMA")) {
    n <- as.integer(regmatches(spec, gregexpr("[0-9]+", spec))[[1]])
    term <- function(k, what) if (k > 0) sprintf("%d %s", k, what) else NULL
    parts <- c(term(n[1], "AR"), term(n[3], "MA"), term(n[2], "difference"))
    if (length(n) >= 6) {
      parts <- c(parts, term(n[4], "seasonal AR"), term(n[6], "seasonal MA"),
                 term(n[5], "seasonal difference"))
    }
    if (grepl("drift", spec)) parts <- c(parts, "drift (steady trend)")
    return(paste(parts, collapse = ", "))
  }
  ""
}

# ---- UI ---------------------------------------------------------------------
ui <- page_navbar(
  title = "Australian Wine Sales, 1980-1994",
  theme = bs_theme(bootswatch = "flatly"),
  fillable = FALSE,  # let cards grow to fit tables instead of clipping them
  # spinners on outputs while models fit (Shiny >= 1.8.1)
  header = if (exists("useBusyIndicators", asNamespace("shiny"))) shiny::useBusyIndicators() else NULL,

  # Tab 1: explore trends and the shifting varietal mix
  nav_panel(
    "Explore",
    layout_sidebar(
      sidebar = sidebar(
        checkboxGroupInput("ex_var", "Varietals", choices = varietals,
                           selected = varietals),
        sliderInput("ex_dates", "Date range",
                    min = data_start, max = data_end,
                    value = c(data_start, data_end), timeFormat = "%b %Y"),
        checkboxInput("ex_free", "Free y-axis scales", FALSE),
        checkboxInput("ex_trend", "Show 12-month moving average", TRUE)
      ),
      card(card_header("Monthly sales by varietal"),
           plotOutput("ex_facets", height = "380px")),
      card(card_header("Share of annual sales (selected varietals)"),
           plotOutput("ex_share", height = "260px"))
    )
  ),

  # Tab 2: seasonal pattern for one varietal
  nav_panel(
    "Seasonality",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("se_var", "Varietal", choices = varietals, selected = "Sparkling"),
        radioButtons("se_type", "View",
                     choices = c("Seasonal plot (one line per year)" = "season",
                                 "Average by month" = "avg"))
      ),
      card(card_header(textOutput("se_title")),
           plotOutput("se_plot", height = "420px"))
    )
  ),

  # Tab 3: fit models per varietal on a training window, score on validation
  nav_panel(
    "Forecast",
    layout_sidebar(
      sidebar = sidebar(
        width = 310,
        selectizeInput("fc_var", "Varietal(s)", choices = varietals,
                       selected = c("Fortified", "Red"), multiple = TRUE),
        sliderInput("fc_dates", tooltip(span("Date range used ", HTML("&#9432;")), "Months included in training, validation, and the future forecast."),
                    min = data_start, max = data_end,
                    value = c(data_start, data_end), timeFormat = "%b %Y"),
        sliderInput("fc_split", tooltip(span("Training ends ", HTML("&#9432;")), "Models are fit on data up to this month and scored on every later month in the date range."),
                    min = as.Date("1984-12-01"), max = as.Date("1994-06-01"),
                    value = as.Date("1992-12-01"), timeFormat = "%b %Y"),
        checkboxGroupInput("fc_models", tooltip(span("Models ", HTML("&#9432;")), "TSLM: linear trend + monthly effects. ETS: exponential smoothing. ARIMA: autoregressive model. SNAIVE: same month last year (a benchmark)."),
                           choices = names(model_colors),
                           selected = c("TSLM", "ETS", "ARIMA")),
        checkboxInput("fc_log", tooltip(span("Log transform ", HTML("&#9432;")), "Fits models to log(Sales) so seasonal swings can grow with the sales level. Forecasts are converted back to liters."), TRUE),
        sliderInput("fc_h", "Future horizon (months past the date range)",
                    min = 6, max = 36, value = 12, step = 6),
        checkboxInput("fc_pi", "Show 80% prediction intervals", TRUE),
        helpText("Fitting takes about 10-20 seconds for two varietals; more varietals take longer.")
      ),
      card(card_header("Validation: forecasts vs. actual (dashed line = training end)"),
           plotOutput("fc_val", height = "380px")),
      layout_columns(
        col_widths = c(12, 12),
        card(card_header("Accuracy (validation and training)"),
             tableOutput("fc_acc")),
        card(card_header("Selected model specifications"),
             tableOutput("fc_spec"))
      ),
      card(card_header("Future forecast (models refit on the full date range)"),
           plotOutput("fc_future", height = "340px"))
    )
  ),

  # Tab 4: documentation
  nav_panel(
    "About",
    card(
      card_body(
        markdown("
### Data
Monthly Australian wine sales (thousands of liters), January 1980 to December 1994,
for six varietals: Dry White, Fortified, Red, Rose, Sparkling, and Sweet White
(`AustralianWines.csv`). The file is read into a tsibble with `index = Month` and
`key = Varietal`. Column names are trimmed and title-cased, and the two Rose values
recorded as `*` (Jul and Aug 1994) are filled by linear interpolation.

### Modeling choices
- **Split:** the *Training ends* slider sets `train_end`. Training data is
  `filter_index(. ~ train_end)`; validation is every later month in the chosen date range.
- **Models (fit separately for each varietal):** `TSLM(Sales ~ trend() + season())`,
  `ETS(Sales)`, and `ARIMA(Sales)`, with ETS and ARIMA orders chosen automatically by AICc.
  `SNAIVE` (same month last year) is available as a benchmark.
- **Log transform (default on):** the models are fit to `log(Sales)`, which turns
  seasonal swings that grow with the level into constant ones. Forecasts are
  back-transformed to liters automatically. Uncheck it to fit the raw series.
- **Evaluation:** `accuracy()` reports RMSE, MAE, and MAPE on both the training fit and
  the validation window. Models are ranked by validation RMSE.
- **Future forecast:** the selected models are refit on the full date range and
  forecast *h* months ahead, with 80% prediction intervals.

### Reproducing the write-up figures
- **Figure 1:** Explore tab, all six varietals, full date range.
- **Figure 2:** Explore tab, Fortified and Red only, *Free y-axis scales* and
  *Show 12-month moving average* checked.
- **Figure 3 and Table 1:** Forecast tab, varietals Fortified and Red, full date range,
  training ends Dec 1992, TSLM/ETS/ARIMA checked, log transform on.

### Acknowledgment
Portions of this app's code were drafted with the help of Claude (Anthropic), an AI assistant,
then reviewed, tested, and adapted by the author.

Built with R: shiny, bslib, tsibble, fable, feasts, and ggplot2.
")
      )
    )
  )
)

# ---- Server -----------------------------------------------------------------
server <- function(input, output, session) {

  # ---------- Explore ----------
  ex_data <- reactive({
    req(input$ex_var)
    wine |>
      filter(Varietal %in% input$ex_var,
             Month >= yearmonth(input$ex_dates[1]),
             Month <= yearmonth(input$ex_dates[2]))
  })

  output$ex_facets <- renderPlot({
    d <- ex_data() |>
      group_by_key() |>
      # trailing 12-month average removes the seasonal swings, leaving the trend
      mutate(MA12 = slider::slide_dbl(Sales, mean, .before = 11, .complete = TRUE)) |>
      ungroup()
    p <- ggplot(d, aes(x = Month, y = Sales)) +
      geom_line(colour = "grey40") +
      facet_wrap(~Varietal, scales = if (input$ex_free) "free_y" else "fixed") +
      labs(x = NULL, y = ylab_sales) +
      theme_minimal(base_size = 13)
    if (input$ex_trend) {
      p <- p + geom_line(aes(y = MA12), colour = "firebrick", linewidth = 1, na.rm = TRUE)
    }
    p
  })

  output$ex_share <- renderPlot({
    ex_data() |>
      as_tibble() |>
      mutate(Year = year(Month)) |>
      group_by(Year, Varietal) |>
      summarise(Sales = sum(Sales), .groups = "drop_last") |>
      mutate(Share = Sales / sum(Sales)) |>
      ungroup() |>
      ggplot(aes(x = Year, y = Share, fill = Varietal)) +
      geom_area(alpha = 0.85, colour = "white") +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_brewer(palette = "Set2") +
      labs(x = NULL, y = "Share of selected sales") +
      theme_minimal(base_size = 13)
  })

  # ---------- Seasonality ----------
  output$se_title <- renderText(paste("Seasonal pattern:", input$se_var))

  output$se_plot <- renderPlot({
    d <- wine |>
      filter(Varietal == input$se_var) |>
      as_tibble() |>
      mutate(Year = year(Month),
             MonthName = factor(month.abb[month(Month)], levels = month.abb))
    if (input$se_type == "season") {
      ggplot(d, aes(x = MonthName, y = Sales, group = Year, colour = Year)) +
        geom_line() +
        scale_colour_viridis_c() +
        labs(x = NULL, y = ylab_sales) +
        theme_minimal(base_size = 13)
    } else {
      d |>
        group_by(MonthName) |>
        summarise(Mean = mean(Sales), SD = sd(Sales), .groups = "drop") |>
        ggplot(aes(x = MonthName, y = Mean)) +
        geom_col(fill = "steelblue") +
        geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.3) +
        labs(x = NULL, y = paste("Mean", ylab_sales, "(+/- 1 SD)")) +
        theme_minimal(base_size = 13)
    }
  })

  # ---------- Forecast ----------
  build_models <- function(data, models, use_log) {
    specs <- list(
      TSLM   = if (use_log) TSLM(log(Sales) ~ trend() + season()) else TSLM(Sales ~ trend() + season()),
      ETS    = if (use_log) ETS(log(Sales)) else ETS(Sales),
      ARIMA  = if (use_log) ARIMA(log(Sales)) else ARIMA(Sales),
      SNAIVE = if (use_log) SNAIVE(log(Sales)) else SNAIVE(Sales)
    )[models]
    do.call(model, c(list(data), specs))
  }

  fc_scales <- function() {
    list(
      scale_colour_manual(values = model_colors, aesthetics = c("colour", "fill")),
      labs(x = NULL, y = ylab_sales, colour = "Model", fill = "Model"),
      theme_minimal(base_size = 13),
      theme(legend.position = "bottom")
    )
  }

  # Selected varietals within the chosen date range
  series <- reactive({
    req(input$fc_var)
    wine |>
      filter(Varietal %in% input$fc_var) |>
      filter_index(format(yearmonth(input$fc_dates[1])) ~ format(yearmonth(input$fc_dates[2])))
  })

  val_fit <- reactive({
    req(input$fc_models)
    train_end <- yearmonth(input$fc_split)
    validate(
      need(train_end < yearmonth(input$fc_dates[2]),
           "Training must end before the end of the date range."),
      need(train_end - yearmonth(input$fc_dates[1]) >= 36,
           "Use at least 3 years of training data.")
    )
    training <- series() |> filter_index(. ~ format(train_end))
    list(fit = build_models(training, input$fc_models, input$fc_log),
         train_end = train_end)
  })

  val_fc <- reactive({
    vf <- val_fit()
    validation <- series() |> filter(Month > vf$train_end)
    forecast(vf$fit, new_data = validation)
  })

  output$fc_val <- renderPlot({
    vf <- val_fit()
    val_fc() |>
      autoplot(series() |> filter(Month >= vf$train_end - 36),
               level = if (input$fc_pi) 80 else NULL, alpha = 0.6) +
      geom_vline(xintercept = as.Date(vf$train_end), linetype = "dashed") +
      facet_wrap(~Varietal, scales = "free_y") +
      fc_scales()
  })

  output$fc_acc <- renderTable({
    bind_rows(
      accuracy(val_fc(), series()) |> mutate(Set = "Validation"),
      accuracy(val_fit()$fit) |> mutate(Set = "Training")
    ) |>
      select(Varietal, Model = .model, Set, RMSE, MAE, MAPE) |>
      arrange(Varietal, desc(Set), RMSE)
  }, digits = 1)

  output$fc_spec <- renderTable({
    val_fit()$fit |>
      as_tibble() |>
      pivot_longer(-Varietal, names_to = "Model", values_to = "mdl") |>
      filter(Model %in% c("ETS", "ARIMA")) |>
      mutate(Specification = vapply(mdl, \(m) format(m), character(1)),
             `In plain terms` = vapply(Specification, describe_spec, character(1))) |>
      select(Varietal, Model, Specification, `In plain terms`)
  })

  output$fc_future <- renderPlot({
    req(input$fc_models)
    full_fit <- build_models(series(), input$fc_models, input$fc_log)
    forecast(full_fit, h = input$fc_h) |>
      autoplot(series() |> filter(Month >= max(Month) - 59),
               level = if (input$fc_pi) 80 else NULL, alpha = 0.6) +
      facet_wrap(~Varietal, scales = "free_y") +
      fc_scales()
  })
}

shinyApp(ui, server)
