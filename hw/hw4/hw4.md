# Homework 4 - Due 10/28/2026

Please complete the following and upload **reproducible code** to the [Github HW4 submissions](https://github.com/jlivsey/UB-fall26-time-series/tree/main/hw/hw4/submissions) repository.

Your code must be **reproducible**, and the results must be **visible without downloading or running anything**. The easiest way to do this is to use the [`reprex`](https://reprex.tidyverse.org/) R package.

This assignment follows the workflow from **11A-applications-of-methods-so-far** (the forecasting bake-off), but goes deeper. In 11A we fit *one or two versions* of each model class and compared them. Here you will examine *several choices within each model class* and then compare the classes. You will do this on two datasets that behave very differently:

* **Part 1:** a **shorter** series (about 200 months) where we expect simple models to do well.
* **Part 2:** a **longer, more structured** series (about 1,800 days) where we expect the more flexible models to do well.

Part 3 asks you to compare what you found. Your job is to test our expectations, not to confirm them. If the data disagree with what we expect, say so.

## The ground rules (the same as 11A)

* **Train once, score one step ahead.** Fit every model on the training period only. Then walk through the test period, giving each model the data up to time $t-1$ to predict time $t$, with the parameters held fixed. Compare with RMSE and MAE, and RMSE *relative to the benchmark*.
* **All predictors are lagged**, so everything a model uses would be known at forecast time.
* **Every model is compared to a benchmark** (naive or seasonal naive). A model that cannot beat the benchmark is not earning its complexity.
* **Do not choose a model by its test-set score.** Any choice *within* a model class (an order, a lag length, a network size, a set of predictors) must be made using the **training data only**: AICc, BIC, or a validation block carved from the end of the training period. The test period is used once, at the end, to score the choices you already made. (If you tune on the test set, you are reporting the best of many tries, not a forecast.)
* The helper functions from 11A are in [`bakeoff-helpers.R`](https://github.com/jlivsey/UB-fall26-time-series/blob/main/slides/11-path-forward/bakeoff-helpers.R): `fit_fable_models()`, `var_onestep()`, `nnar_seeds()`, `score_table()`, and `plot_onestep()`. You may `source()` the file or paste what you need into your code. You will need to **copy and modify** these functions to explore the choices below. They fix several things (for example, `var_onestep()` always selects the lag order by AIC) that you will want to vary.

## Part 1: Housing starts (the shorter series)

* **Target:** Housing Starts, new privately-owned units, **not seasonally adjusted**, monthly, thousands of units. FRED: [`HOUSTNSA`](https://fred.stlouisfed.org/series/HOUSTNSA)
* **Predictors:**
  * New Privately-Owned Housing Units **Authorized by Building Permits**, not seasonally adjusted, monthly: [`PERMITNSA`](https://fred.stlouisfed.org/series/PERMITNSA). A permit comes before a start, so permits should be a *leading* indicator.
  * **30-Year Fixed Rate Mortgage Average**, weekly, percent: [`MORTGAGE30US`](https://fred.stlouisfed.org/series/MORTGAGE30US). Average it to monthly.
* **Sample:** January 2010 to the most current month (about 200 months). Use the most current vintage when you run your code.

Code to load the data (this uses `download.file(method = "curl")` because `read_csv()` directly from the FRED URL can fail on some machines; `fredr` works too if you have an API key):

```r
library(fpp3)

fred <- function(id) {
  f <- tempfile(fileext = ".csv")
  download.file(paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", id),
                f, method = "curl", quiet = TRUE)
  readr::read_csv(f, show_col_types = FALSE, na = ".") |>
    setNames(c("date", "value")) |>
    mutate(Month = yearmonth(date)) |>
    group_by(Month) |>
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop")  # monthly average
}

housing <- fred("HOUSTNSA") |> rename(Starts = value) |>
  inner_join(fred("PERMITNSA") |> rename(Permits = value), by = "Month") |>
  inner_join(fred("MORTGAGE30US") |> rename(Mortgage = value), by = "Month") |>
  filter(Month >= yearmonth("2010 Jan")) |>
  as_tsibble(index = Month)
```

### 1A. Look at the data and state your expectation

1. Plot the three series. Describe the trend, the seasonality, the variance, and the relationship between the series. Is a log transformation of `Starts` and `Permits` warranted? Justify with a plot.
1. Using a cross-correlation plot (`CCF()`), decide which lags of the predictors are worth using. Create the lagged predictors you will use.
1. Before fitting anything, write down **which model class you expect to do best, and which worst, and why**. (You will return to this in Part 3.)
1. Set up the split. **Train through December 2021, test on the remaining months.** Report both sample sizes.

### 1B. Reproduce the 11A baseline

1. Run the 11A bake-off on this dataset with its default settings: ETS, ARIMA, NNAR, NNAR_small, lag regression, VAR, and a seasonal-naive benchmark. Use `SNAIVE` as the benchmark since the data are monthly.
1. Report the `score_table()` and a `plot_onestep()` figure, and print the fitted models (as in the "What each model chose" slides). This is your **baseline**.

### 1C. Go deeper: explore choices within each model class

For **each** of the five model classes below, fit the variants described, compare them **using the training data only**, and name the one you would select. Show the comparison in a table. Then fit the selected variant's one-step-ahead forecasts on the test period, and compare it to the baseline version from 1B.

1. **ETS.** Compare at least four specifications, for example the automatically selected model, an additive vs. multiplicative error/seasonal, a damped trend vs. an undamped trend vs. no trend, and ETS on the log scale vs. the raw scale. Compare by AICc (or by a validation block, if the scales differ and AICc is not comparable).
1. **ARIMA.** Compare at least four specifications, for example the default `ARIMA()` search, a more thorough search (`stepwise = FALSE, approximation = FALSE`), a hand-chosen model read off the ACF/PACF of the differenced series (as in 05a), and a variant that changes the differencing (e.g. $d = 1, D = 1$ vs. $D = 1$ only, with or without drift). Compare by AICc where the models share the same differencing (AICc is not comparable across different orders of differencing), and by a validation block otherwise.
1. **NNAR.** Vary the number of lagged inputs $p$, the seasonal lags $P$, and the number of hidden nodes $k$ over a grid of at least six combinations. Choose by a **validation block** (e.g. fit on 2010 to 2018, score 2019 to 2021, then refit the chosen setting on all of the training data). Also report the spread of the test RMSE across at least 8 random seeds (use `nnar_seeds()` or adapt it) for the baseline and for your selected network. Does the choice of $(p, P, k)$ matter more or less than the seed?
1. **VAR.** Vary (a) the information criterion used to select the lag order (AIC, BIC/SC, HQ, FPE), (b) the variables included (target + permits; target + mortgage; all three), and (c) whether you add monthly dummy variables as exogenous regressors (`exog_cols`) to handle seasonality. Choose by the criterion or a validation block. Does the VAR need the seasonal dummies to compete?
1. **Lag regression** (`ARIMA(y ~ x_lag)`). Vary (a) which lags of the predictors enter (lag 1 only; lags 1 and 2; lags 1 to 3), (b) which predictors enter (permits only, mortgage only, both), and (c) whether the errors are chosen automatically or fixed to a simple form. Choose by AICc (same errors structure and same response) or by a validation block.

For each class, say in one or two sentences: **did tuning help on the test set relative to the baseline, and was the improvement larger than the noise?** (For example, compare the gain to the seed spread of NNAR.)

### 1D. Compare the classes

1. Make a single table of test RMSE, MAE, and RMSE-vs-benchmark with one row per **tuned** model class (plus the benchmark and, for reference, the baseline versions from 1B).
1. Make one figure of the test period showing the actual series and the one-step forecasts from the best two or three models.
1. **How fragile is the ranking?** Repeat the comparison for a **second train/test split** of your choosing (for example, train through December 2019 and test on the remainder, or train through December 2023). Does the ranking of model classes change? Do not re-tune the choices from 1C; use the same specifications and re-fit them on the new training period.
1. Write a short paragraph: which class won, was it what you expected in 1A, and how much would you trust the ranking given the size of the test set?

## Part 2: Beijing air quality (the longer, more structured series)

* **Target:** daily average **PM2.5** concentration ($\mu g/m^3$) in Beijing, January 2010 to December 2014 (about 1,800 days), built from the hourly [Beijing PM2.5 Data Set](https://archive.ics.uci.edu/dataset/381/beijing+pm2+5+data) (UCI Machine Learning Repository, U.S. Embassy monitor in Beijing).
* **Predictors** (all in the same file, averaged to daily): **temperature**, **dew point**, **air pressure**, and **wind speed**. PM2.5 is driven by the weather. Pollution builds up on calm, humid days and is blown away by wind from the north. These relationships are strongly nonlinear, and the yearly heating season adds seasonality.
* **Model `log(PM2.5 + 1)`.** The raw series is right-skewed with large spikes.

Code to load the data:

```r
library(fpp3)

dir <- tempfile(); dir.create(dir)
zipf <- file.path(dir, "beijing.zip")
download.file("https://archive.ics.uci.edu/static/public/381/beijing+pm2+5+data.zip",
              zipf, method = "curl", quiet = TRUE)
unzip(zipf, exdir = dir)

beijing <- readr::read_csv(file.path(dir, "PRSA_data_2010.1.1-2014.12.31.csv"),
                           show_col_types = FALSE, na = "NA") |>
  mutate(Date = as.Date(paste(year, month, day, sep = "-"))) |>
  group_by(Date) |>
  summarise(PM25 = mean(`pm2.5`, na.rm = TRUE),   # NaN when all 24 hours are missing
            Temp = mean(TEMP), Dew = mean(DEWP), Pres = mean(PRES),
            Wind = mean(Iws), .groups = "drop") |>
  as_tsibble(index = Date) |>
  fill_gaps() |>
  mutate(PM25 = ifelse(is.nan(PM25), NA, PM25)) |>
  tidyr::fill(PM25, .direction = "down") |>        # a few fully-missing days: carry forward
  filter(!is.na(PM25)) |>
  mutate(lPM = log(PM25 + 1), lWind = log(Wind + 1))
```

(Note what this code does to the missing days: the target is carried forward on a few days. Say in your write-up whether you think that matters for a comparison of one-step forecasts.)

### 2A. Look at the data and state your expectation

1. Plot `lPM` and the four predictors. Describe the seasonality (weekly? yearly?), the variance, the spikes, and any trend. Use `gg_season()` and/or an ACF to back up what you claim.
1. Plot `lPM` against each predictor (scatterplots) and comment on linearity. Use `CCF()` to decide which lags of each predictor to use. Create the lagged predictors.
1. Before fitting anything, write down **which model class you expect to do best and worst, and why**.
1. **Train through December 31, 2013 and test on all of 2014.** Report both sample sizes. Use `SNAIVE` with a weekly period, or `NAIVE`, as the benchmark, and say which and why.

### 2B. Reproduce the 11A baseline

Same as 1B: run the default bake-off (ETS, ARIMA, NNAR, NNAR_small, lag regression, VAR, benchmark), and report the `score_table()`, a `plot_onestep()` figure of a window of the test period of your choosing (e.g. the 2014 heating season), and the fitted models. Note anything surprising about what the automatic procedures chose (for example, the default NNAR has a very large number of lags on a series this long; the default ETS has no seasonal component).

### 2C. Go deeper: explore choices within each model class

Follow the same rules as 1C (choose using training data only) for the five classes. Because the sample is larger and the structure richer, the choices to explore are richer too:

1. **ETS.** ETS in `fable` cannot handle a period of 365. Compare ETS on `lPM` with and without a trend, with and without damping, and with a weekly seasonal component. Then try **ETS errors fitted to the residuals of a regression on Fourier terms** for the yearly cycle (`fourier(period = "year", K = ...)`, with $K$ chosen by AICc). Say what you learned about the limits of ETS for this series.
1. **ARIMA.** Compare the default search with (a) a dynamic harmonic regression: `ARIMA(lPM ~ fourier(period = "year", K = k) + PDQ(0,0,0))` for several $K$ chosen by AICc, and (b) a version with weekly seasonal terms. Which seasonal structure matters?
1. **NNAR.** Vary $p$, $P$ (weekly seasonal lags), and $k$ over a grid of at least eight combinations, with and without the weather predictors as external regressors (e.g. `NNETAR(lPM ~ AR(p = 7) + T_l1 + Dew_l1 + Pres_l1 + W_l1, n_nodes = k)`). Choose by a validation block (e.g. train on 2010 to 2012, validate on 2013, then refit the chosen setting on all of the training data). Report the spread of the test RMSE across at least 8 seeds for the baseline and for your selected network. With this much data, does the seed matter less than it did for Part 1?
1. **VAR.** Vary the information criterion, the variables included (adding each of temperature, dew point, pressure, wind), the maximum lag, and exogenous calendar terms (day-of-week dummies, Fourier terms for the yearly cycle). Choose by criterion or by a validation block.
1. **Lag regression.** Vary which weather predictors enter, how many lags of each, whether you add **nonlinear terms** (squares of temperature or wind, an interaction between dew point and wind), and whether you add Fourier terms for the yearly cycle. Choose by AICc (same response and errors structure) or by a validation block.

For each class, say whether tuning helped relative to the baseline and whether the gain is larger than the noise.

### 2D. Compare the classes

Do the same four steps as 1D (tuned-class table, figure, a second split, and a paragraph). For the second split, use training through June 30, 2013 and test on the second half of 2013 and all of 2014, or a split of your own choosing with a justification.

In addition, **evaluate the models on the days when it matters.** Pollution spikes are what people care about. Compute the test RMSE (or MAE) separately for the "high pollution" days (say the top 10% of `lPM` values in the test set) and for the other days. Does the ranking of model classes change on the hard days?

## Part 3: What did we learn?

1. Make one combined table with the **tuned** model classes as rows and the two datasets as columns, with the RMSE-vs-benchmark entries (the analogue of the "Everything side by side" slide from 11A). Bold the best in each column.
1. Return to the expectations you wrote down in 1A and 2A. Were they right? For each dataset, explain the result in terms of the features of the data (sample size, noise, nonlinearity, seasonality, the usefulness of the predictors).
1. Which of the choices you explored (ETS form, ARIMA differencing, NNAR size, VAR lag criterion, lag-regression predictors, seasonal terms) mattered **most**, and which hardly mattered at all? Give numbers.
1. If someone gave you a **new** monthly series with about 150 observations and a **new** daily series with about 2,000 observations, what would be your first three models to try for each, and why?

## Notes

* Make sure each of your comparison tables (1B, 1C, 1D, 2B, 2C, 2D, Part 3) is among the visible outputs of your code and is clearly labeled.
* Every choice should be backed by a table or plot **computed on the training data**. State clearly, for every class, which criterion you used to choose.
* Some of these fits (NNAR over a grid and across seeds, `stepwise = FALSE` ARIMA) take a few minutes. Start early, set a seed (`set.seed()`) so your results are reproducible, and consider caching or reducing your grid if it is very slow.
* If an automatic procedure fails or behaves oddly on a variant, say what happened and move on. That is part of the answer.
