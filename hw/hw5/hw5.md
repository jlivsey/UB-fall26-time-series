# Homework 5 - Due 11/18/2026

Please complete the following and upload **reproducible code** to the [Github HW5 submissions](https://github.com/jlivsey/UB-fall26-time-series/tree/main/hw/hw5/submissions) repository.

Your code must be **reproducible**, and the results must be **visible without downloading or running anything**. The easiest way to do this is to use the [`reprex`](https://reprex.tidyverse.org/) R package.

This assignment applies the tree-based methods from **12-ML** (Machine Learning for Forecasting) to a new daily series. You will build the feature table yourself, fit a random forest and XGBoost, and decide whether they are good enough to use.

## The problem

You work for Capital Bikeshare in Washington, DC. Bikes are pulled from service for maintenance, and a repair takes two days. To have bikes back on the street before a high-demand day, the operations team needs a forecast of daily rentals **three days ahead**: at the end of day $t$, using data through day $t$, forecast the number of rentals on day $t+3$.

Your job is to build tree-based forecasts for this purpose and to judge whether they are good enough to hand to the operations team.

To keep results comparable across the class, **train through June 30, 2012 and test on July 1 to December 31, 2012.** Use the test period once, at the end, to score the choices you have already made.

Choose all model settings on a **validation period taken from the end of the training period**, never from the test period. You may use one validation block or several. For each block, you may use either an **expanding** training window (all days before the block) or a **rolling** one (a fixed number of the most recent days before the block). Say which you chose and why.

## The data

* **Target:** daily bike rentals from Capital Bikeshare, 2011 to 2012 (731 days), from the [Bike Sharing Dataset](https://archive.ics.uci.edu/dataset/275/bike+sharing+dataset) (UCI Machine Learning Repository).
* **Other variables:** temperature (Celsius), humidity (percent), wind speed, a weather category (1 = clear, 2 = mist or cloudy, 3 = light rain or snow, 4 = heavy rain), and holiday and working-day indicators.

Code to load the data:

```r
library(fpp3)

dir <- tempfile(); dir.create(dir)
zipf <- file.path(dir, "bike.zip")
download.file("https://archive.ics.uci.edu/static/public/275/bike+sharing+dataset.zip",
              zipf, method = "curl", quiet = TRUE)
unzip(zipf, exdir = dir)

bike <- readr::read_csv(file.path(dir, "day.csv"), show_col_types = FALSE) |>
  transmute(Date = as.Date(dteday),
            Rentals = cnt,
            Temp = temp * 41,          # the file stores these divided by their maximum
            Hum = hum * 100,
            Wind = windspeed * 67,
            Weather = weathersit,
            Holiday = holiday,
            Workday = workingday) |>
  as_tsibble(index = Date)
```

## Part 1: Make a plan

1. Plot `Rentals` and the weather variables. Describe the trend, the weekly pattern, the yearly seasonality, and any unusual days.
1. Before fitting anything, answer the following in a few sentences each. 
   * **What is known at forecast time?** At the end of day $t$, which values of `Rentals` and of the weather are available for forecasting day $t+3$? Which calendar information about day $t+3$ is known in advance?
   * **How will you score the forecasts?** RMSE, MAE, or a score aimed at the operations goal (for example, error on high-demand days only, or a heavier penalty for under-forecasting than for over-forecasting). Justify your choice.
   * **What validation period and training window will you use** to choose model settings? Justify the length and timing of your validation block(s), and your choice of an expanding or rolling window.
   * **What counts as a good forecast?** Choose one or more benchmark forecasts that use only information available three days ahead. Say how much better than the benchmark a model must be to be worth using.

## Part 2: Build the feature table

1. Build a table with one row per day and `Rentals` as the target. Include lags of the target, at least one rolling statistic of the target, calendar features, and weather features.
1. For each feature, state which day's data it uses and why it is known three days ahead.
1. Make the train/test split, report both sample sizes, and compute your benchmark forecasts on the validation and test periods.

## Part 3: Random forest

1. Fit a random forest with `ranger` using the default settings.
1. Fit at least one other setting, changing at least one of `mtry`, `min.node.size`, `max.depth` or `num.trees`. Choose between your settings on your validation period, using your score from Part 1. Show the comparison in a table.
1. Refit the chosen setting on all of the training data and score it on the test period. Also report its out-of-bag (OOB) error. Is it close to your validation error? Explain why or why not.
1. Make a permutation importance plot. Which features matter most?

## Part 4: XGBoost

1. Fit XGBoost (`xgb.train()` with `objective = "reg:squarederror"`) with a starting setting of your choice.
1. Fit at least one other setting, changing at least one of `max_depth`, `eta`, `subsample` or `colsample_bytree`. For each setting, choose the number of rounds on your validation period. Show the settings, the chosen rounds and the validation score in a table.
1. Refit the chosen setting on all of the training data and score it on the test period.

## Part 5: Is it good enough?

1. Make one table of test scores (your score from Part 1, plus RMSE and MAE) for each benchmark and each model setting you fit, with the score relative to your main benchmark.
1. Plot the test period: the actual series, your main benchmark, and your best model's forecasts.
1. Look at the **high-demand days** in the test period. Are your forecasts on those days too high or too low? Explain why, using how trees make predictions (see "Trees and trends" in 12-ML). Try one fix from those slides, for example predicting the change from a recent level and adding it back, and report whether it helps.
1. **Weather on day $t+3$.** Refit your best model using the weather on the day being forecast. In practice that would come from a three-day weather forecast. How much does the test score change? Explain why this result is optimistic compared with using a real weather forecast.
1. Write a short recommendation to the operations team: would you use your model, the benchmark, or neither? Support your answer with your test scores and your criterion from Part 1.

## Notes

* Set seeds so your results are reproducible: `seed = ` in `ranger()`, and `seed = ` inside the `params` list of `xgb.train()`.
* Make sure each table and figure is among the visible outputs of your code and is clearly labeled.
