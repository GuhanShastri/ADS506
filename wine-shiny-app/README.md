# Australian Wine Sales: Explore and Forecast (ADS 506, Week 5)

A Shiny app for exploring monthly Australian wine sales (1980–1994) and comparing
TSLM, ETS, and ARIMA forecasts for each varietal.

- **Deployed app:** <APP URL>
- **Author:** Guhan Shastri

## Run locally

```r
install.packages(c("shiny", "bslib", "dplyr", "tidyr", "readr", "ggplot2",
                   "lubridate", "tsibble", "fable", "feasts", "slider"))
shiny::runApp()   # from this folder
```

`AustralianWines.csv` must be in the same folder as `app.R`. All paths are relative.

## Tabs

- **Explore:** sales by varietal, a 12-month moving-average trend line, and each varietal's share of annual sales.
- **Seasonality:** seasonal plot or average-by-month view for one varietal.
- **Forecast:** fits TSLM, ETS, and ARIMA (plus an optional seasonal naive benchmark) to each selected
  varietal. Shows validation forecasts with 80% intervals, training and validation accuracy,
  model specifications in plain language, and a future forecast.
- **About:** data, modeling choices, and the inputs for each figure in the write-up.

## Package versions tested

R 4.3, shiny 1.8.0, bslib 0.6.1, dplyr 1.1.4, tidyr 1.3.1, readr 2.1.5, ggplot2 3.4.4,
lubridate 1.9.3, tsibble 1.2.0, fable 0.4.1, feasts 0.4.2, slider 0.3.1.
Newer versions should also work.

## Acknowledgment

Portions of this app's code were drafted with the help of Claude (Anthropic), an AI assistant,
then reviewed, tested, and adapted by the author.
