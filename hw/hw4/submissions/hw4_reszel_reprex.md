# Problem 1

## Part 1a

First, we load in the data for the housing series. Then we plot the data.

``` r
set.seed(12345)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("bakeoff-helpers.R")

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

housing |> pivot_longer(-Month) |>
  ggplot(aes(Month, value)) + geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(x = NULL, y = "Value")
```

![](https://i.imgur.com/40ilPJx.png)<!-- -->

``` r

housing$Mortgage %>% ts(start=c(2010,1), frequency=12) %>%
  as_tsibble() %>% gg_season(y=value) + 
  labs(title = "Seasonal Plot of Mortgage Rates",
    x = "Month",
    y = "Mortgage Rate")
```

![](https://i.imgur.com/WODqY2W.png)<!-- -->

``` r

housing$Permits %>% ts(start=c(2010,1), frequency=12) %>%
  as_tsibble() %>% gg_season(y=value) +
  labs(title = "Seasonal Plot of New Permits",
       x = "Month",
       y = "New Permits")
```

![](https://i.imgur.com/dRvU3MJ.png)<!-- -->

``` r

housing$Starts %>% ts(start=c(2010,1), frequency=12) %>%
  as_tsibble() %>% gg_season(y=value) +
  labs(title = "Seasonal Plot of Housing Starts",
       x = "Month",
       y = "Housing Starts")
```

![](https://i.imgur.com/suRxN2N.png)<!-- -->
There is a clear increasing trend in housing permits and starts, although it appears to level out instead of increasing linearly. Mortgage rate does not appear to have a constant trend, instead showing decreasing trend (with significant noise) from 2010 to 2020, then showing a period of significant decrease followed by a sharp increase.

Mortgage rates do not show any clear seasonality, but the seasonal plot shows mortgage rates are higher in more recent years. Housing starts and new permits appear to be higher in the summer months, but this seasonality is relatively weak.

Changes in permits appears to be strongly correlated with changes in starts. It seems that a decrease in mortgage rate leads to a corresponding increase in starts/permits, while an increase in mortgage rate could correspond to a decrease in the acceleration of starts/permits.

The variance is housing permits/starts does not seem to be constant over time. We can look at the
scale-location plot from a simple regression on time to see if variance changes over time.

``` r
housing$date <- as.Date(housing$Month)
housing$time <- housing$date - min(housing$date)

plot(lm(Starts~time, data=housing), which=3)
```

![](https://i.imgur.com/R8WFvnC.png)<!-- -->

``` r
plot(lm(Permits~time, data=housing), which=3)
```

![](https://i.imgur.com/qPLGN50.png)<!-- -->

``` r

housing$logStarts <- log(housing$Starts)
housing$logPermits <- log(housing$Permits)
```

While not drastic, both scale location plots do not exhibit completely straight line behavior. This evidence for non-constant variance, as well as a desire to get mortgages and housing starts/permits on the same scale causes us to take the log of these two variables.

Now, we can look at the cross-correlation plots to see which lags to include. We use the prewhiten trend to remove autocorrelation, and take the difference to remove the trend:

``` r
prewhiten(diff(housing$logStarts), diff(housing$logPermits))
```

![](https://i.imgur.com/M2DZE6q.png)<!-- -->

``` r
prewhiten(diff(housing$logStarts), diff(housing$Mortgage))
```

![](https://i.imgur.com/SIHjLUk.png)<!-- -->

``` r

acf(housing$logStarts)
```

![](https://i.imgur.com/QMsQAYq.png)<!-- -->

``` r

housing$lag1_logPermits <- lag(housing$logPermits, 1)
housing$lag2_logPermits <- lag(housing$logPermits, 2)
housing$lag3_logPermits <- lag(housing$logPermits, 3)

housing$lag1_Mortgage <- lag(housing$Mortgage, 1)
housing$lag2_Mortgage <- lag(housing$Mortgage, 2)
housing$lag3_Mortgage <- lag(housing$Mortgage, 3)

housing_upd <- housing[housing$Month >= yearmonth(as.Date("2010-04-01")),]
housing_train_end <- yearmonth(as.Date('2021-12-01'))
c(train = sum(housing_upd$Month <= housing_train_end), 
  test = sum(housing_upd$Month > housing_train_end))
#> train  test 
#>   141    57
```

Based off the acf plots, we will consider lag 1 of log housing permits and lag 2 of mortgage rate. We create variables for lag 1,2, and 3 of each variable since those are needed for later. We will exclude the first 3 observations so none of these lagged variables have missing data, and all the fitted models are fit on the same training data.

For this data set, since it expresses seasonality and is a relatively short series with only a few covariates, we might expect classic, simpler models such as ARIMA and exponential smoothing to perform the best. We might expect NNAR to do the worst due to the small data set.

## Part 1b

Now, we use the helper functions to produce the baseline.

``` r
housing_fable <- fit_fable_models(housing_upd, "logStarts", c("lag1_logPermits", "lag2_Mortgage"),
                 housing_train_end, naive="SNAIVE")
housing_var <- var_onestep(housing_upd, c("logStarts","logPermits", "Mortgage"), "logStarts",
                           housing_train_end)
housing_result <- bind_rows(housing_fable, housing_var)

score_table(housing_result)
#> # A tibble: 7 × 4
#>   model       RMSE   MAE `RMSE vs benchmark`
#>   <chr>      <dbl> <dbl>               <dbl>
#> 1 ETS        0.075 0.059               0.671
#> 2 ARIMA      0.085 0.071               0.761
#> 3 NNAR       0.087 0.072               0.779
#> 4 LagReg     0.096 0.077               0.861
#> 5 NNAR_small 0.099 0.077               0.885
#> 6 VAR(4)     0.1   0.08                0.898
#> 7 Benchmark  0.111 0.085               1

plot_onestep(housing_result, title = "Log Housing Starts (1/2022-9/2026)")
```

![](https://i.imgur.com/Nrs5FJm.png)<!-- -->

``` r

attr(housing_result, "fits")
#> # A mable: 1 x 6
#>            ETS                     ARIMA              NNAR  NNAR_small
#>        <model>                   <model>           <model>     <model>
#> 1 <ETS(M,N,A)> <ARIMA(1,1,1)(0,1,1)[12]> <NNAR(2,1,2)[12]> <NNAR(2,2)>
#> # ℹ 2 more variables: LagReg <model>, Benchmark <model>
```

From the baseline, simpler models such as ETS and ARIMA did best, while VAR and NNAR with small p/P/k did the worst. It is expected that NNAR selected a relatively simple model since we have a relatively short series that appears to have strong autocorrelation and seasonality. Each model did better than the seasonal benchmark.

## Part 1c

First, we consider the manual tuning for the ETS. We include the auto-chosen model from 1b, additive and multiplicative seasonal models (which is possibly not appropriated since we took the log of the outcome), regular Holt’s and a dampened Holt’s. We compare these models by Aicc. We also obtain the RMSE for the naive benchmark, since that is needed for comparison.

``` r
housing_train <- housing_upd[housing_upd$date <= '2021-12-01',]
housing_test <- housing_upd[housing_upd$date > "2021-12-01",]

################################
## Seasonal naive predictions ##
################################
train_end <- yearmonth("2021 Dec")

seasonal_naive <- housing_train |>
  model(
    SNAIVE(logStarts)
  )

seasonal_naive_preds <- refit(
  seasonal_naive,
  housing_upd,
  reestimate = FALSE
) |>
  augment() |>
  as_tibble() |>
  filter(Month > train_end) |>
  transmute(
    model = .model,
    date = Month,
    actual = logStarts,
    pred = .fitted
  )

housing_bench <- seasonal_naive_preds |>
  summarise(mse = sqrt(mean((actual - pred)^2))) |> pull(mse)

housing_bench_results <- seasonal_naive_preds |>
  summarise(
    RMSE = sqrt(mean((actual - pred)^2, na.rm = TRUE)),
    MAE = mean(abs(actual - pred), na.rm = TRUE)
  ) |>
  mutate(`RMSE vs benchmark` = RMSE / housing_bench,
         model = "Baseline") |>
  arrange(RMSE)

print(housing_bench_results)
#> # A tibble: 1 × 4
#>    RMSE    MAE `RMSE vs benchmark` model   
#>   <dbl>  <dbl>               <dbl> <chr>   
#> 1 0.111 0.0849                   1 Baseline

############
##  ETS   ##
############
ets_fits <- housing_train |>
  model(
    auto_choice = ETS(logStarts ~ error("M") + trend("N") +
                        season("A")),
    additive_seas = ETS(logStarts ~ error("A") + trend("A") +
                season("A")),
    multiplicative_seas = ETS(logStarts ~ error("M")+ trend("A") +
                           season("M")),
    holts_reg = ETS(logStarts ~ error("A")+ trend("A")+
                      season("N")),
    damp_holts = ETS(logStarts ~ error("A") +
                       trend("Ad", phi = 0.9) + season("N"))
  )

ets_fits |>
  glance() |>
  dplyr::select(.model, AICc)
#> # A tibble: 5 × 2
#>   .model               AICc
#>   <chr>               <dbl>
#> 1 auto_choice          39.3
#> 2 additive_seas        40.7
#> 3 multiplicative_seas  47.2
#> 4 holts_reg           102. 
#> 5 damp_holts          102.
```

The auto-chosen ETS model was found to be the best in terms of AICC. The regular and dampened Holt linear trend models performed extremely poorly, likely since the seasonality in the data is ignored. Now, fit the model’s one-step-ahead forecasts on the test period, similar to what is done inside the helper functions.

So, we can conclude that tuning the ETS model does not provide any benefit for this series.

``` r
ets_final_models <- housing_train |> 
  model(ETS_auto_choice = ETS(logStarts ~ error("M") + trend("N") +
                        season("A")))

ets_predictions <- refit(
  ets_final_models,
  housing_upd,
  reestimate = FALSE
) |>
  augment() |>
  as_tibble() |>
  filter(Month > train_end) |>
  transmute(
    model = .model,
    date = Month,
    actual = logStarts,
    pred = .fitted
  )
  
ets_results <- ets_predictions |>
  group_by(model) |>
  summarise(
    RMSE = sqrt(mean((actual - pred)^2, na.rm = TRUE)),
    MAE = mean(abs(actual - pred), na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(`RMSE vs benchmark` = RMSE / housing_bench) |>
  arrange(RMSE)

print(ets_results)
#> # A tibble: 1 × 4
#>   model             RMSE    MAE `RMSE vs benchmark`
#>   <chr>            <dbl>  <dbl>               <dbl>
#> 1 ETS_auto_choice 0.0748 0.0587               0.671
```

Now, we perform the same process for an ARIMA model. We fit the models on data through 2018, choose the candidate models with the same levels of differencing using Aicc, validate those choices on data from 2019-2020, and then refit the best model by RMSE on the full training set.

``` r
housing_train_fit <- housing_train[housing_train$date <= '2018-12-01',]
housing_train_val <- housing_train[housing_train$date > '2018-12-01',]

log_starts_train_fit_ts <- ts(housing_train_fit$logStarts, start=c(2010,4), frequency=12)
log_starts_all_train_ts <- ts(housing_train$logStarts, start=c(2010,4), frequency=12)

acf(log_starts_train_fit_ts, lag.max=48)
```

![](https://i.imgur.com/cZs461V.png)<!-- -->

``` r
acf(diff(log_starts_train_fit_ts, 1), lag.max=48)
```

![](https://i.imgur.com/S6kAuWf.png)<!-- -->

``` r
pacf(diff(log_starts_train_fit_ts, 1), lag.max=48)
```

![](https://i.imgur.com/0N7hPML.png)<!-- -->

``` r

acf(diff(log_starts_train_fit_ts, 12), lag.max=48)
```

![](https://i.imgur.com/RcxKxn7.png)<!-- -->

``` r
pacf(diff(log_starts_train_fit_ts, 12), lag.max=48)
```

![](https://i.imgur.com/45TzNqn.png)<!-- -->

``` r

acf(diff(diff(log_starts_train_fit_ts, 12)), lag.max=48)
```

![](https://i.imgur.com/MAcuSHg.png)<!-- -->

``` r
pacf(diff(diff(log_starts_train_fit_ts, 12)), lag.max=48)
```

![](https://i.imgur.com/TTpCXh6.png)<!-- -->

Since the ACF on the non-differenced series decays very slowly, this indicates some level of differencing is needed. If we take the first difference, the first several lags are all non-significant, possibly indicating regular differencing on its own is not appropriate. For the seasonaly differenced series, the ACF and PACF plot direct us to consider p=3, q=0, P=1, and D=0. For the series with both regular and seasonal differencing, the ACF and PACF plot direct us to consider p=2, q=1, P=2, Q=0. In both plots here, the ACF and PACF are negative at lag 1, indicating we may have over-differenced.

We also include models selected by auto arima, auto arima without a stepwise search, auto arima with seasonal differencing only, and auto arima with both kinds of differencing.

``` r
housing_autoarima_regular <- housing_train_fit |> model(ARIMA(logStarts))
housing_autoarima_regular
#> # A mable: 1 x 1
#>          `ARIMA(logStarts)`
#>                     <model>
#> 1 <ARIMA(0,1,2)(2,1,0)[12]>
housing_autoarima_large <- housing_train_fit |> model(ARIMA(logStarts, stepwise=FALSE))
housing_autoarima_large
#> # A mable: 1 x 1
#>   `ARIMA(logStarts, stepwise = FALSE)`
#>                                <model>
#> 1            <ARIMA(0,1,2)(2,1,0)[12]>
housing_arima_auto_D <- housing_train_fit |> model(ARIMA(logStarts ~ pdq(d = 0) + PDQ(D = 1)))
housing_arima_auto_D
#> # A mable: 1 x 1
#>   `ARIMA(logStarts ~ pdq(d = 0) + PDQ(D = 1))`
#>                                        <model>
#> 1                    <ARIMA(1,0,2)(2,1,0)[12]>
housing_arima_auto_dD <- housing_train_fit |> model(ARIMA(logStarts ~ pdq(d = 1) + PDQ(D = 1)))
housing_arima_auto_dD
#> # A mable: 1 x 1
#>   `ARIMA(logStarts ~ pdq(d = 1) + PDQ(D = 1))`
#>                                        <model>
#> 1                    <ARIMA(0,1,2)(2,1,0)[12]>

arima_fits <- housing_train_fit |>
  model(
    housing_autoarima = ARIMA(logStarts ~ pdq(0,1,2) + PDQ(2,1,0)),
    housing_arima_manual_D = ARIMA(logStarts ~ pdq(3, 0, 0) + PDQ(1, 1, 0)),
    housing_arima_manual_dD = ARIMA(logStarts ~ pdq(2, 1, 1) + PDQ(2, 1, 1)),
    housing_arima_auto_D = ARIMA(logStarts ~ pdq(1,0,2) + PDQ(2,1,0))
  )

arima_fits |> glance() |>
  dplyr::select(.model, AIC, AICc, BIC)
#> # A tibble: 4 × 4
#>   .model                    AIC  AICc   BIC
#>   <chr>                   <dbl> <dbl> <dbl>
#> 1 housing_autoarima       -165. -164. -152.
#> 2 housing_arima_manual_D  -160. -160. -148.
#> 3 housing_arima_manual_dD -175. -174. -158.
#> 4 housing_arima_auto_D    -166. -165. -151.
```

We select the best model in each category of differencing by AICc, and then check the model residual diagnostics, which seem acceptable for both models. We then compare the one-step ahead predictions on the validation period to select the final model. We then compare this model to the one produced by the helper function by using the testing data.

``` r
arima_options <- housing_train_fit |>
  model(
    housing_autoarima = ARIMA(logStarts ~ pdq(0,1,2) + PDQ(2,1,0)),
    housing_arima_manual_D = ARIMA(logStarts ~ pdq(3, 0, 0) + PDQ(1, 1, 0))
  )

arima_options |>
  dplyr::select(housing_autoarima) |>
  gg_tsresiduals()
```

![](https://i.imgur.com/2PFnrCF.png)<!-- -->

``` r

arima_options |>
  dplyr::select(housing_arima_manual_D) |>
  gg_tsresiduals()
```

![](https://i.imgur.com/gn2bWP0.png)<!-- -->

``` r

arima_predictions <- refit(
  arima_options,
  housing_train,
  reestimate = FALSE
) |>
  augment() |>
  as_tibble() |>
  filter(Month >  yearmonth(as.Date('2018-12-01'))) |>
  transmute(
    model = .model,
    date = Month,
    actual = logStarts,
    pred = .fitted
  )

arima_results <- arima_predictions |>
  group_by(model) |>
  summarise(
    RMSE = sqrt(mean((actual - pred)^2, na.rm = TRUE)),
    MAE = mean(abs(actual - pred), na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(RMSE)

print(arima_results)
#> # A tibble: 2 × 3
#>   model                   RMSE   MAE
#>   <chr>                  <dbl> <dbl>
#> 1 housing_autoarima      0.143 0.105
#> 2 housing_arima_manual_D 0.152 0.107

arima_test_check <- housing_train |>
  model(
    housing_autoarima = ARIMA(logStarts ~ pdq(0,1,2) + PDQ(2,1,0)),
    housing_autopicked_arima = ARIMA(logStarts ~ pdq(1, 1, 1) + PDQ(0, 1, 1))
  )

arima_test_predictions <- refit(
  arima_test_check,
  housing_upd,
  reestimate = FALSE
) |>
  augment() |>
  as_tibble() |>
  filter(Month > train_end) |>
  transmute(
    model = .model,
    date = Month,
    actual = logStarts,
    pred = .fitted
  )

arima_test_results <- arima_test_predictions |>
  group_by(model) |>
  summarise(
    RMSE = sqrt(mean((actual - pred)^2, na.rm = TRUE)),
    MAE = mean(abs(actual - pred), na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(`RMSE vs benchmark` = RMSE / housing_bench) |>
  arrange(RMSE)

print(arima_test_results)
#> # A tibble: 2 × 4
#>   model                      RMSE    MAE `RMSE vs benchmark`
#>   <chr>                     <dbl>  <dbl>               <dbl>
#> 1 housing_autopicked_arima 0.0848 0.0705               0.761
#> 2 housing_autoarima        0.0967 0.0760               0.868
```

The tuned arima model does not appear to show any benefit over the baseline arima model.

Now, we fit the NNAR models for a variety of small p,k. We fit the models on the training fit set and then compare the RMSE for the training validation set.:

``` r
nnar_pPks <- expand.grid(
  p = c(1,2,3,5,7),
  P = c(0,1),
  k = c(1,2,3,5,7)
)

nnar_train_results <- data.frame(
  p = integer(),
  P = integer(),
  k = integer(),
  RMSE = numeric()
)

set.seed(12345)
for (i in seq_len(nrow(nnar_pPks))) {
  nnar_fit <- housing_train_fit |>
    model(
      nnar = NNETAR(
        logStarts ~ AR(
          p = nnar_pPks$p[i],
          P = nnar_pPks$P[i],
          period = 12
        ),
        n_nodes = nnar_pPks$k[i]
      )
    )
  val_preds_nnar <- refit(
    nnar_fit,
    housing_train,
    reestimate = FALSE
  ) |>
    augment() |>
    as_tibble() |>
    filter(.data[[index_var(housing_train)]] %in%
             housing_train_val[[index_var(housing_train_val)]])
  rmse <- sqrt(mean(
    (val_preds_nnar$logStarts - val_preds_nnar$.fitted)^2,
    na.rm = TRUE
  ))
  nnar_train_results <- rbind(
    nnar_train_results,
    data.frame(
      p = nnar_pPks$p[i],
      P = nnar_pPks$P[i],
      k = nnar_pPks$k[i],
      RMSE = rmse
    )
  )
}
nnar_train_results %>% arrange(RMSE)
#>    p P k      RMSE
#> 1  5 1 1 0.1233739
#> 2  2 1 1 0.1261149
#> 3  1 1 1 0.1261375
#> 4  3 1 1 0.1268013
#> 5  7 1 1 0.1291226
#> 6  7 0 1 0.1339366
#> 7  5 0 1 0.1390501
#> 8  7 0 3 0.1408367
#> 9  1 0 7 0.1409153
#> 10 7 0 2 0.1414007
#> 11 1 0 3 0.1435669
#> 12 1 0 1 0.1437271
#> 13 3 0 1 0.1448360
#> 14 1 0 5 0.1464202
#> 15 2 0 1 0.1465221
#> 16 5 0 2 0.1468646
#> 17 7 0 5 0.1492823
#> 18 1 0 2 0.1505772
#> 19 3 0 2 0.1530018
#> 20 5 0 3 0.1539426
#> 21 7 0 7 0.1543244
#> 22 2 0 2 0.1553509
#> 23 5 1 2 0.1559181
#> 24 5 0 7 0.1570181
#> 25 7 1 2 0.1584148
#> 26 3 0 7 0.1589097
#> 27 2 0 5 0.1599346
#> 28 2 0 3 0.1619015
#> 29 5 0 5 0.1641897
#> 30 2 1 5 0.1642013
#> 31 3 1 2 0.1650695
#> 32 2 1 2 0.1653605
#> 33 3 0 5 0.1653914
#> 34 1 1 2 0.1658090
#> 35 2 0 7 0.1680866
#> 36 2 1 3 0.1697742
#> 37 3 1 3 0.1720895
#> 38 3 0 3 0.1725730
#> 39 3 1 7 0.1764299
#> 40 3 1 5 0.1797906
#> 41 2 1 7 0.1811542
#> 42 5 1 3 0.1828923
#> 43 1 1 3 0.1842427
#> 44 5 1 7 0.1911039
#> 45 5 1 5 0.1931524
#> 46 1 1 7 0.1942595
#> 47 7 1 5 0.1976700
#> 48 7 1 3 0.1981648
#> 49 7 1 7 0.2020813
#> 50 1 1 5 0.2174453
```

We can see the best choice is p=5, p=1,k=1. So, we consider 5 lags, 1 seasonal lag, and 1 node in the hidden layer. This is a relative simple neural network, but it uses a moderate number of lags. Now, we consider changing the seeds to see how this choice compares with the baseline model.

``` r
set.seed(2026)
nnar_models <- housing_train |>
  model(nnar_manual = NNETAR(logStarts ~ AR(p = 5 ,P = 1,
                                     period = 12),
                      n_nodes = 1),
        nnar_auto = NNETAR(logStarts ~ AR(p = 2 ,P = 1,
                                     period = 12),
                      n_nodes = 2)
  )


nnar_specs <- list(
  Manual = list(p = 5, P = 1, k = 1),
  Auto   = list(p = 2, P = 1, k = 2)
)

nnar_test_results_list <- vector("list", 8 * length(nnar_specs))
result_idx <- 1

for (model_name in names(nnar_specs)) {
  spec <- nnar_specs[[model_name]]
  for (seed in 1:8) {
    set.seed(seed)
    nnar_fit <- housing_train |>
      model(
        nnar = NNETAR(
          logStarts ~ AR(
            p = spec$p,
            P = spec$P,
            period = 12
          ),
          n_nodes = spec$k,
          repeats = 1
        )
      )
    nnar_preds <- refit(
      nnar_fit,
      housing_upd,
      reestimate = FALSE
    ) |>
      augment() |>
      as_tibble() |>
      filter(
        .data[[index_var(housing_upd)]] %in%
          housing_test[[index_var(housing_test)]]
      )
    rmse <- sqrt(mean(
      (nnar_preds$logStarts - nnar_preds$.fitted)^2,
      na.rm = TRUE
    ))
    mae <- mean(abs(nnar_preds$logStarts - nnar_preds$.fitted))
    nnar_test_results_list[[result_idx]] <- data.frame(
      seed = seed,
      model = model_name,
      RMSE = rmse
    )
    result_idx <- result_idx + 1
  }
}

nnar_test_results <- bind_rows(nnar_test_results_list)

nnar_test_results
#>    seed  model       RMSE
#> 1     1 Manual 0.09288663
#> 2     2 Manual 0.09284807
#> 3     3 Manual 0.09276883
#> 4     4 Manual 0.09282278
#> 5     5 Manual 0.09274822
#> 6     6 Manual 0.09282585
#> 7     7 Manual 0.09293037
#> 8     8 Manual 0.09276719
#> 9     1   Auto 0.08761525
#> 10    2   Auto 0.08699638
#> 11    3   Auto 0.08699152
#> 12    4   Auto 0.08807436
#> 13    5   Auto 0.08825398
#> 14    6   Auto 0.08788365
#> 15    7   Auto 0.08804930
#> 16    8   Auto 0.08877765

nnar_test_results |>
  group_by(model) |>
  summarize(
    mean_rmse = mean(RMSE, na.rm = TRUE),
    sd_rmse = sd(RMSE, na.rm = TRUE),
    .groups = "drop"
  )
#> # A tibble: 2 × 3
#>   model  mean_rmse   sd_rmse
#>   <chr>      <dbl>     <dbl>
#> 1 Auto      0.0878 0.000613 
#> 2 Manual    0.0928 0.0000631
```

There appears to be slightly more variation depending on seed in the automatic model, while the manually tuned model shows more stability. This shows that depending on the specific NNAR fit, seed can have a impact, but its small in our scenario. Regardless, this demonstrates the importance of fitting multiple models and taking averages to have more stable estimates, which R generally does automatically. However, our manually tuned model seems to show a worsening in terms of RMSE. It could be overfit given the size/complexity of the data. Here are the final models for comparison:

``` r
nnar_test_preds <- refit(
  nnar_models,
  housing_upd,
  reestimate = FALSE
) |>
  augment() |>
  as_tibble() |>
  filter(
    .data[[index_var(housing_upd)]] >
      max(housing_train_fit[[index_var(housing_train)]])
  ) |>
  transmute(
    model = .model,
    date = .data[[index_var(housing_upd)]],
    actual = logStarts,
    pred = .fitted
  )

nnar_test_metrics <- nnar_test_preds |>
  group_by(model) |>
  summarise(
    RMSE = sqrt(mean((actual - pred)^2, na.rm = TRUE)),
    MAE = mean(abs(actual - pred), na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(`RMSE vs benchmark` = RMSE / housing_bench) |>
  arrange(RMSE)

nnar_test_metrics
#> # A tibble: 2 × 4
#>   model         RMSE    MAE `RMSE vs benchmark`
#>   <chr>        <dbl>  <dbl>               <dbl>
#> 1 nnar_auto   0.0880 0.0674               0.790
#> 2 nnar_manual 0.0955 0.0763               0.858
```

The manually tuned model performs worse here in terms of RMSE.

Now, we tune the VAR models. We will use both AicC and BIC to select the value of p. We will also include different combinations of lags for parameters and including dummy monthly variables to attempt to explain some of the seasonality:

``` r
housing_train$month_factor <- factor(month(housing_train$Month),levels = 1:12)

log_starts_train_var_ts <- ts(housing_train_fit$logStarts, start=c(2010,4), frequency=12)
log_permits_train_var_ts <- ts(housing_train_fit$logPermits, start=c(2010,4), frequency=12)
mortgage_train_var_ts <- ts(housing_train_fit$Mortgage, start=c(2010,4), frequency=12)

## naming models is p/m/b for permits,mortgage both, m/n for months/no months, a/b for aic/bic 
make_month_dummies <- function(data) {
  m <- month(data$Month)
  out <- sapply(1:11, function(j) as.numeric(m == j))
  out <- as.matrix(out)
  colnames(out) <- paste0("month_", 1:11)
  out
}

month_fit <- make_month_dummies(housing_train_fit)
month_all_train <- make_month_dummies(housing_train)
month_test <- make_month_dummies(housing_test)

VARselect(cbind(log_starts_train_var_ts, log_permits_train_var_ts), lag.max=8, type='const')$selection
#> AIC(n)  HQ(n)  SC(n) FPE(n) 
#>      8      3      1      7

VARselect(cbind(log_starts_train_var_ts, mortgage_train_var_ts), lag.max=8, type='const')$selection
#> AIC(n)  HQ(n)  SC(n) FPE(n) 
#>      2      2      1      2

VARselect(cbind(log_starts_train_var_ts, log_permits_train_var_ts, mortgage_train_var_ts),
          lag.max=8, type='const')$selection
#> AIC(n)  HQ(n)  SC(n) FPE(n) 
#>      5      1      1      5


VARselect(cbind(log_starts_train_var_ts, log_permits_train_var_ts), lag.max=8, type='const',
          exogen= month_fit)$selection
#> AIC(n)  HQ(n)  SC(n) FPE(n) 
#>      3      3      3      3

VARselect(cbind(log_starts_train_var_ts, mortgage_train_var_ts), lag.max=8, type='const',
          exogen=month_fit)$selection
#> AIC(n)  HQ(n)  SC(n) FPE(n) 
#>      4      3      2      4

VARselect(cbind(log_starts_train_var_ts, log_permits_train_var_ts, mortgage_train_var_ts),
          lag.max=8, type='const', exogen= month_fit)$selection
#> AIC(n)  HQ(n)  SC(n) FPE(n) 
#>      3      3      2      3


specs <- tribble(
  ~name, ~vars, ~months, ~p,
  "pna", c("logStarts", "logPermits"), FALSE, 8,
  "pnb", c("logStarts", "logPermits"), FALSE, 1,
  "mna", c("logStarts", "Mortgage"), FALSE, 2,
  "mnb", c("logStarts", "Mortgage"), FALSE, 1,
  "bna", c("logStarts", "logPermits", "Mortgage"), FALSE, 5,
  "bnb", c("logStarts", "logPermits", "Mortgage"), FALSE, 1,
  
  "pya", c("logStarts", "logPermits"), TRUE, 3,
  "pyb", c("logStarts", "logPermits"), TRUE, 3,
  "mya", c("logStarts", "Mortgage"), TRUE, 4,
  "myb", c("logStarts", "Mortgage"), TRUE, 2,
  "bya", c("logStarts", "logPermits", "Mortgage"), TRUE, 3,
  "byb", c("logStarts", "logPermits", "Mortgage"), TRUE, 2
)

run_var_models <- function(train_data, test_data, specs) {
  make_month_dummies <- function(data) {
    m <- lubridate::month(data$Month)
    out <- vapply(
      1:11,
      function(j) as.numeric(m == j),
      FUN.VALUE = numeric(nrow(data))
    )
    out <- matrix(out, nrow = nrow(data), ncol = 11)
    colnames(out) <- paste0("month_", 1:11)
    out
  }
  purrr::map_dfr(seq_len(nrow(specs)), function(i) {
    cur_specs <- specs[i, ]
    response_vars <- cur_specs$vars[[1]]
    use_months <- cur_specs$months[[1]]
    p <- cur_specs$p[[1]]
    all_data <- dplyr::bind_rows(train_data, test_data)
    Y_train <- as.matrix(
      train_data[, response_vars, drop = FALSE]
    )
    Y_all <- as.matrix(
      all_data[, response_vars, drop = FALSE]
    )
    if (use_months) {
      X_train <- make_month_dummies(train_data)
      X_all <- make_month_dummies(all_data)

      fit <- vars::VAR(
        y = Y_train,
        p = p,
        type = "const",
        exogen = X_train
      )
    } else {
      fit <- vars::VAR(
        y = Y_train,
        p = p,
        type = "const"
      )
    }
    B <- vars::Bcoef(fit)
    eq <- B["logStarts", ]
    n_train <- nrow(train_data)
    validation_indices <- (n_train + 1):nrow(Y_all)
    predicted <- purrr::map_dbl(validation_indices, function(t) {
      sum(purrr::map_dbl(names(eq), function(nm) {
        if (grepl("\\.l[0-9]+$", nm)) {
          v <- sub("\\.l[0-9]+$", "", nm)
          l <- as.integer(sub(".*\\.l", "", nm))
          eq[nm] * Y_all[t - l, v]
        } else if (nm == "const") {
          eq[nm]
        } else if (use_months && nm %in% colnames(X_all)) {
          eq[nm] * X_all[t, nm]
        } else {
          0
        }
      }))
    })
    actual <- test_data$logStarts
    tibble::tibble(
      model = cur_specs$name,
      lag = p,
      months = use_months,
      Month = test_data$Month,
      actual = actual,
      predicted = predicted
    )
  })
}

var_1step_predictions <- run_var_models(
  housing_train_fit,
  housing_train_val,
  specs
)

var_1step_results <- var_1step_predictions |>
  group_by(model, lag, months) |>
  summarise(
    RMSE = sqrt(mean((actual - predicted)^2, na.rm = TRUE)),
    MAE = mean(abs(actual - predicted), na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(`RMSE vs benchmark` = RMSE / housing_bench) |>
  arrange(RMSE)

var_1step_results
#> # A tibble: 12 × 6
#>    model   lag months   RMSE    MAE `RMSE vs benchmark`
#>    <chr> <dbl> <lgl>   <dbl>  <dbl>               <dbl>
#>  1 pna       8 FALSE  0.0977 0.0781               0.877
#>  2 bnb       1 FALSE  0.100  0.0769               0.901
#>  3 pnb       1 FALSE  0.101  0.0775               0.906
#>  4 bna       5 FALSE  0.103  0.0782               0.927
#>  5 mnb       1 FALSE  0.108  0.0786               0.966
#>  6 mna       2 FALSE  0.110  0.0821               0.992
#>  7 byb       2 TRUE   0.119  0.0880               1.06 
#>  8 pya       3 TRUE   0.126  0.0906               1.13 
#>  9 pyb       3 TRUE   0.126  0.0906               1.13 
#> 10 myb       2 TRUE   0.126  0.0863               1.13 
#> 11 bya       3 TRUE   0.130  0.0899               1.17 
#> 12 mya       4 TRUE   0.141  0.0962               1.27
```

According to the RMSE criterion, the best model choice is only including log permits and log starts, without the monthly seasonal dummies and using a large value of p=8. Each of the models that did not use the seasonal dummy variables had a lower RMSE, indicating they are not needed for VAR when considering 1-step ahead validation. In fact, they performed worse than the naive benchmark.

Now, we fit the selected model and baseline model on all of the training data, and then compare the two using the testing data.

``` r
final_housing_var_specs <- tribble(
  ~name, ~vars, ~months, ~p,
  "VAR_permits_noseasonal_p8", c("logStarts", "logPermits"), FALSE, 8,
  "VAR_Auto_selected", c("logStarts", "logPermits", "Mortgage"), FALSE, 4
)

var_1step_final <- run_var_models(
  housing_train,
  housing_test,
  final_housing_var_specs
)

var_1step_info <- var_1step_final |>
  group_by(model, lag, months) |>
  summarise(
    RMSE = sqrt(mean((actual - predicted)^2, na.rm = TRUE)),
    MAE = mean(abs(actual - predicted), na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(`RMSE vs benchmark` = RMSE / housing_bench) |>
  arrange(RMSE)

var_1step_info
#> # A tibble: 2 × 6
#>   model                       lag months   RMSE    MAE `RMSE vs benchmark`
#>   <chr>                     <dbl> <lgl>   <dbl>  <dbl>               <dbl>
#> 1 VAR_permits_noseasonal_p8     8 FALSE  0.0880 0.0707               0.790
#> 2 VAR_Auto_selected             4 FALSE  0.1000 0.0800               0.898
```

Here, the manually tuned model showed a significant improvement over the auto selected model in terms of RMSE. The improvement is quite large, and noise is likely removed by having fewer variables in the model.

Perhaps removing the mortgage information improved out of sample testing accuracy, and it is possible this model was over fit initially.

Finally, we will fit the lagged regression models. We include various combinations of lag 1, lag 2, and lag 3 predictors. For each option, we also include a simple version which constrains the sum of p,P,q,Q to be less than 4.

``` r
housing_lr_formulas <- list(
  all_lag1 = logStarts ~ lag1_Mortgage + lag1_logPermits,
  mort_lag1 = logStarts ~ lag1_Mortgage,
  perm_lag1 = logStarts ~ lag1_logPermits,
  all_lag1_simple = logStarts ~ lag1_Mortgage + lag1_logPermits,

  all_lag2 = logStarts ~ lag2_Mortgage + lag2_logPermits +
    lag1_Mortgage + lag1_logPermits,
  mort_lag2 = logStarts ~ lag2_Mortgage + lag1_Mortgage,
  perm_lag2 = logStarts ~ lag2_logPermits + lag1_logPermits,
  all_lag2_simple = logStarts ~ lag2_Mortgage + lag2_logPermits +
    lag1_Mortgage + lag1_logPermits,

  all_lag3 = logStarts ~ lag3_Mortgage + lag3_logPermits +
    lag2_Mortgage + lag2_logPermits +
    lag1_Mortgage + lag1_logPermits,
  mort_lag3 = logStarts ~ lag3_Mortgage + lag2_Mortgage +
    lag1_Mortgage,
  perm_lag3 = logStarts ~ lag3_logPermits + lag2_logPermits +
    lag1_logPermits,
  all_lag3_simple = logStarts ~ lag3_Mortgage + lag3_logPermits +
    lag2_Mortgage + lag2_logPermits +
    lag1_Mortgage + lag1_logPermits
)

housing_lr_models <- imap(
  housing_lr_formulas,
  function(formula, name) {
    if (grepl("_simple$", name)) {
      housing_train_fit |>
        model(
          fit = ARIMA(
            formula,
            order_constraint = p + q + P + Q <= 3
          )
        )
    } else {
      housing_train_fit |>
        model(
          fit = ARIMA(formula)
        )
    }
  }
)

housing_lr_train_rmse_results <- imap_dfr(housing_lr_models, function(fit, model_name) {
  forecast(fit, new_data = housing_train_val) |>
    accuracy(housing_train_val) |>
    filter(.type == "Test") |>
    transmute(
      model = model_name,
      RMSE
    )
}) |>
  arrange(RMSE)

housing_lr_train_rmse_results
#> # A tibble: 12 × 2
#>    model            RMSE
#>    <chr>           <dbl>
#>  1 all_lag1_simple 0.102
#>  2 all_lag1        0.105
#>  3 perm_lag2       0.105
#>  4 perm_lag3       0.106
#>  5 perm_lag1       0.106
#>  6 all_lag2        0.108
#>  7 all_lag2_simple 0.108
#>  8 all_lag3        0.110
#>  9 all_lag3_simple 0.110
#> 10 mort_lag2       0.178
#> 11 mort_lag3       0.184
#> 12 mort_lag1       0.216

housing_lr_models$all_lag1_simple
#> # A mable: 1 x 1
#>                                      fit
#>                                  <model>
#> 1 <LM w/ ARIMA(2,0,0)(1,0,0)[12] errors>
```

Here, we see that simple models containing lag 1 and lag 2 were the best performers in terms of RMSE on the validation set. The models containing mortgage rates only performed extremely poorly. Models with higher levels of p/P/q/Q may be overfit for the data.

We fit our final choice, which includes both lag 1 covariates and has the sum constraint for p,P,q, and Q on the full training data and produce the RMSE for the test data.

``` r
housing_lr_formulas_final <- housing_train %>% 
  model(
    all_lag1_simple = ARIMA(
      logStarts ~ lag1_Mortgage + lag1_logPermits +
        pdq(2,0,0) + PDQ(1,0,0)
    ),
    auto_picked_lr = ARIMA(
      logStarts ~ lag2_Mortgage + lag1_logPermits +
        pdq(0,0,0) + PDQ(2,0,0)
    )
  )

housing_lr_final_predictions <- refit(
  housing_lr_formulas_final,
  housing_upd,
  reestimate = FALSE
) |>
  augment() |>
  as_tibble() |>
  filter(Month > train_end) |>
  transmute(
    model = .model,
    date = Month,
    actual = logStarts,
    pred = .fitted
  )

housing_lr_test_metrics <- housing_lr_final_predictions |>
  group_by(model) |>
  summarise(
    RMSE = sqrt(mean((actual - pred)^2, na.rm = TRUE)),
    MAE = mean(abs(actual - pred), na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(`RMSE vs benchmark` = RMSE / housing_bench) |>
  arrange(RMSE)

housing_lr_test_metrics
#> # A tibble: 2 × 4
#>   model             RMSE    MAE `RMSE vs benchmark`
#>   <chr>            <dbl>  <dbl>               <dbl>
#> 1 all_lag1_simple 0.0896 0.0697               0.805
#> 2 auto_picked_lr  0.0959 0.0771               0.861
```

For the lagged regression, we showed a significant improvement in RMSE, which is clearly an improvement over the auto-choice in face of any random noise from the choice of training/test window. This shows the benefit of considering a wide variety of lagged values and simpler models instead of manually choosing specific lagged predictors and only considering those.

## Part 1d

Now, we print the total table of all our results and a plot of predicted values for the testing set for the top 2 models in terms of RMSE, alongside the true data.

``` r
all_part1_results <- bind_rows(housing_bench_results, ets_results, arima_test_results,
                           nnar_test_metrics, var_1step_info %>% 
                             dplyr::select(model, RMSE, `RMSE vs benchmark`, MAE), 
                          housing_lr_test_metrics )

all_part1_results %>% arrange(RMSE) %>% relocate(model, .before = everything())
#> # A tibble: 10 × 4
#>    model                       RMSE    MAE `RMSE vs benchmark`
#>    <chr>                      <dbl>  <dbl>               <dbl>
#>  1 ETS_auto_choice           0.0748 0.0587               0.671
#>  2 housing_autopicked_arima  0.0848 0.0705               0.761
#>  3 VAR_permits_noseasonal_p8 0.0880 0.0707               0.790
#>  4 nnar_auto                 0.0880 0.0674               0.790
#>  5 all_lag1_simple           0.0896 0.0697               0.805
#>  6 nnar_manual               0.0955 0.0763               0.858
#>  7 auto_picked_lr            0.0959 0.0771               0.861
#>  8 housing_autoarima         0.0967 0.0760               0.868
#>  9 VAR_Auto_selected         0.1000 0.0800               0.898
#> 10 Baseline                  0.111  0.0849               1

part1_plot_data <- bind_rows(
  housing_test %>%
     as_tibble() %>%
    transmute(Month, value = logStarts, series = "Actual"),

  ets_predictions %>%
    as_tibble() %>%
    transmute(Month= date, value = pred, series = "ETS auto"),

  arima_test_predictions %>%
    as_tibble() %>%
    filter(model == "housing_autopicked_arima") %>%
    transmute(Month = date, value = pred, series = "ARIMA auto")
)

ggplot(part1_plot_data, aes(x = Month, y = value, color = series)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Actual vs Predicted Housing Starts",
    x = "Month",
    y = "Log Housing Starts",
    color = "Series"
  ) +
  theme_minimal()
```

![](https://i.imgur.com/n7dI8XA.png)<!-- -->

Now, we will create a new test/training period, and we will now train through 2023. We will see if this changes how the different models performed.

``` r
housing_train2 <- housing_upd[housing_upd$date <= '2023-12-01',]
housing_test2 <- housing_upd[housing_upd$date > "2023-12-01",]

housing_fits_newperiod <- housing_train2 |>
  model(
    ets_auto_choice = ETS(logStarts ~ error("M") + trend("N") +
                        season("A")),
    housing_autopicked_arima = ARIMA(logStarts ~ pdq(1, 1, 1) + PDQ(0, 1, 1)),
    nnar_auto = NNETAR(logStarts ~ AR(p = 2 ,P = 1,
                                     period = 12),
                      n_nodes = 2),
    all_lag1_simple = ARIMA(
      logStarts ~ lag1_Mortgage + lag1_logPermits +
        pdq(2,0,0) + PDQ(1,0,0)
    )
  )

housing_new_period_predictions <- refit(
  housing_fits_newperiod,
  housing_upd,
  reestimate = FALSE
) |>
  augment() |>
  as_tibble() |>
  filter(Month > yearmonth(as.Date("2023-12-01"))) |>
  transmute(
    model = .model,
    date = Month,
    actual = logStarts,
    pred = .fitted
  )

housing_newperiod_test_metrics <- housing_new_period_predictions |>
  group_by(model) |>
  summarise(
    RMSE = sqrt(mean((actual - pred)^2, na.rm = TRUE)),
    MAE = mean(abs(actual - pred), na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(RMSE)


var_new_period <- tribble(
  ~name, ~vars, ~months, ~p,
  "pna", c("logStarts", "logPermits"), FALSE, 8
)

var_newperiod_preds <- run_var_models(housing_train2, housing_test2, var_new_period)

var_newperiod_info <- var_newperiod_preds |>
  group_by(model, lag, months) |>
  summarise(
    RMSE = sqrt(mean((actual - predicted)^2, na.rm = TRUE)),
    MAE = mean(abs(actual - predicted), na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(RMSE)

housing_newperiod_test_metrics
#> # A tibble: 4 × 3
#>   model                      RMSE    MAE
#>   <chr>                     <dbl>  <dbl>
#> 1 ets_auto_choice          0.0756 0.0600
#> 2 all_lag1_simple          0.0792 0.0672
#> 3 housing_autopicked_arima 0.0828 0.0677
#> 4 nnar_auto                0.0885 0.0735
var_newperiod_info
#> # A tibble: 1 × 5
#>   model   lag months   RMSE    MAE
#>   <chr> <dbl> <lgl>   <dbl>  <dbl>
#> 1 pna       8 FALSE  0.0959 0.0797
```

When we changed the size of the training/test period to increase the amount of training data, the exponential smoothing model still performed the best. There was some change in the ordering otherwise, with VAR performing worse and lagged regression performing better.

In general, we can say that exponential smoothing methods performed the best in terms of one-step ahead predictions. The rest of the list did change somewhat depending on the training/testing windows, which likely shows the model performance may be somewhat dependent on this choice given the relatively small number of observations. Regardless, the models that performed well were generally those which were simpler, such as exponential smoothing and ARIMA/lagged regression. Give the small data set, NNAR was not expected to perform well, and the results agreed with this prediction.

This specific ranking being dependent on the windows does lead us to question how reliable the window is. With the small number of observations, we expect more variation in predictive ability between models when even small changes are made, such as the size of the training/testing window. However, since ETS performed the best in both scenarios and it makes sense it would perform well for one-step ahead predictions given the data, it may be trustworthy to choose this as the best model.

# Problem 2

Here, we load in the Beijing data set.

``` r
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

beijing <- beijing %>%
  mutate(
    day_of_week = factor(
      wday(Date, week_start = 1),
      levels = 1:7,
      labels = c("Mon", "Tue", "Wed", "Thu",
                 "Fri", "Sat", "Sun")
    )
  )
```

When data are missing, we carry forward the target. This may affect one-step comparisons, since some methods such as ETS and ARIMA are heavily dependent on the prior value. It also will increase the RMSE for the naive method, which will be 0 when predicting a data point that was missing.

Here are some simple plots of the data:

``` r
lPM_full_ts <- beijing$lPM |>
  ts(start=c(2010,2), frequency=365)
temp_full_ts <- beijing$Temp |>
  ts(start=c(2010,2), frequency=365)
dew_full_ts <- beijing$Dew |>
  ts(start=c(2010,2), frequency=365)
lWind_full_ts <- beijing$lWind |>
  ts(start=c(2010,2), frequency=365)
pres_full_ts <- beijing$Pres |>
  ts(start=c(2010,2), frequency=365)


beijing |> 
  dplyr::select(Date, lPM, Dew, Pres) |>
  tidyr::pivot_longer(-Date) |>
  ggplot(aes(Date, value)) + geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_grid(name ~ ., scales = "free_y") +
  labs(x = NULL, y = "value")
#> `geom_smooth()` using formula = 'y ~ x'
```

![](https://i.imgur.com/Xh80TYG.png)<!-- -->

``` r

beijing |> 
  dplyr::select(Date, lPM, Temp, lWind) |>
  tidyr::pivot_longer(-Date) |>
  ggplot(aes(Date, value)) + geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_grid(name ~ ., scales = "free_y") +
  labs(x = NULL, y = "value")
#> `geom_smooth()` using formula = 'y ~ x'
```

![](https://i.imgur.com/QjyYGFX.png)<!-- -->
Dew point, air pressure, and temperature show clear cyclical behavior, while the behavior of pm concentration and wind speed is less clear from these plots. Temperature and dew point appear to show a very minor increasing trend, and wind speed appears to show a very minor decreasing trend. There is no clear trend in the other series. Wind seems to oscillate around some low value, with periods of more spikes/peaks. PM25 concentration appears to be a noisy series.

We can look at some seasonal plots:

``` r
lPM_tsibble <- tibble(
  date = seq.Date(
    from = as.Date("2010-01-02"),
    by = "day",
    length.out = length(lPM_full_ts)
  ),
  value = as.numeric(lPM_full_ts)
) %>%
  as_tsibble(index = date) %>%
  fill_gaps()

temp_tsibble <- tibble(
  date = seq.Date(
    from = as.Date("2010-01-02"),
    by = "day",
    length.out = length(temp_full_ts)
  ),
  value = as.numeric(temp_full_ts)
) %>%
  as_tsibble(index = date) %>%
  fill_gaps()

dew_tsibble <- tibble(
  date = seq.Date(
    from = as.Date("2010-01-02"),
    by = "day",
    length.out = length(dew_full_ts)
  ),
  value = as.numeric(dew_full_ts)
) %>%
  as_tsibble(index = date) %>%
  fill_gaps()

lPM_tsibble %>%
  gg_season(value, period = "year") +
  labs(
    title = "Yearly Seasonal Plot of log(PM2.5+1)",
    x = "Day of Year",
    y = "log(PM2.5+1)"
  ) +
  theme_minimal()
```

![](https://i.imgur.com/zlk5sqQ.png)<!-- -->

``` r

temp_tsibble %>%
  gg_season(value, period = "year") +
  labs(
    title = "Yearly Seasonal Plot of Temperature",
    x = "Day of Year",
    y = "Temperature"
  ) +
  theme_minimal()
```

![](https://i.imgur.com/B4NsObp.png)<!-- -->

``` r

dew_tsibble %>%
  gg_season(value, period = "year") +
  labs(
    title = "Yearly Seasonal Plot of Dew point",
    x = "Day of Year",
    y = "Dew Point"
  ) +
  theme_minimal()
```

![](https://i.imgur.com/JwDxiBT.png)<!-- -->

``` r

lPM_weekdays <- lPM_tsibble %>%
  as_tibble() %>%
  mutate(
    year = year(date),
    weekday = wday(date, label = TRUE, abbr = FALSE, week_start = 7)
  ) %>%
  group_by(year, weekday) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(
  lPM_weekdays,
  aes(x = weekday, y = mean_value, color = factor(year), group = year)
) +
  geom_line(linewidth = 0.8) +
  geom_point() +
  labs(
    title = "Day-of-Week Pollution Patterns by Year",
    x = "Day of Week",
    y = "Mean log(PM2.5+1)",
    color = "Year"
  ) +
  theme_minimal()
```

![](https://i.imgur.com/P6pE3W9.png)<!-- -->

In terms of PM2.5, there appears to be higher concentrations in the winter months and low concentrations in the summer months, as well as high variation in the winter months. There may also be a slightly higher concentration on weekends, but the plot is somewhat unclear. This seasonality does not appear to be drastic, as the series is very noisy.

Here are some ACF plots of the data:

``` r
acf(lPM_full_ts, lag.max=365)
```

![](https://i.imgur.com/EwEvuLq.png)<!-- -->

``` r
acf(temp_full_ts, lag.max=365)
```

![](https://i.imgur.com/oQQifuk.png)<!-- -->

``` r
acf(dew_full_ts, lag.max=365)
```

![](https://i.imgur.com/drMgB3w.png)<!-- -->

``` r
acf(pres_full_ts, lag.max=365)
```

![](https://i.imgur.com/XWz7Z5r.png)<!-- -->

``` r
acf(lWind_full_ts, lag.max=365)
```

![](https://i.imgur.com/yZspi7N.png)<!-- -->

PM25 concentration does not appear to be heavily autocorrelated, since it appears very noisy and already looks stationary. In all of the other series, there is obvious seasonality/oscilation.

Now, we can look at scatter plots of PM25 with the other variables:

``` r
ggplot(beijing, aes(x = lPM, y = Temp)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue")
#> `geom_smooth()` using formula = 'y ~ x'
```

![](https://i.imgur.com/hOnumti.png)<!-- -->

``` r

ggplot(beijing, aes(x = lPM, y = Dew)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue")
#> `geom_smooth()` using formula = 'y ~ x'
```

![](https://i.imgur.com/h1gCoBy.png)<!-- -->

``` r

ggplot(beijing, aes(x = lPM, y = Wind)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue")
#> `geom_smooth()` using formula = 'y ~ x'
```

![](https://i.imgur.com/SEatid2.png)<!-- -->

``` r

ggplot(beijing, aes(x = lPM, y = Pres)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue")
#> `geom_smooth()` using formula = 'y ~ x'
```

![](https://i.imgur.com/ZETLTb9.png)<!-- -->

There appears to be very moderate linear relationships between PM2.5 concentration and dew point (positive) and between PM2.5 concentration and air pressure (negative). The relationship between wind and PM2.5 and wind does not appear to be linear. Regularly, this may prompt us to consider a transformation of wind speed, depending on which kind of model we choose to use.

Now we can look at ccf plots to consider which lags to use in the models. We consider differenced values if there is seasonality or trend:

``` r
ccf(lPM_full_ts, diff(diff(temp_full_ts,365)))
```

![](https://i.imgur.com/zzL8rBN.png)<!-- -->

``` r
ccf(lPM_full_ts, diff(diff(dew_full_ts,365)))
```

![](https://i.imgur.com/wj9CWVb.png)<!-- -->

``` r
ccf(lPM_full_ts, diff(pres_full_ts,365))
```

![](https://i.imgur.com/AUBK3qz.png)<!-- -->

``` r
ccf(lPM_full_ts, diff(diff(lWind_full_ts,365)))
```

![](https://i.imgur.com/YkAZMFw.png)<!-- -->

``` r

beijing$lag1_temp <- lag(beijing$Temp, 1)
beijing$lag1_dew <- lag(beijing$Dew, 1)
beijing$lag2_pres <- lag(beijing$Pres, 2)
beijing$lag1_lwind <- lag(beijing$lWind, 1)
```

These plots suggest using lag 1 for dew point, lag 2 for air pressure, and lag 1 for wind speed. Lags for temperature do not appear significant, so we will use lag 1 out of necessity.

Given we have more complex seasonality, potential non-linear relationships between the covariates, and large series with a lot of noise, we would expect more complex models such as NNAR to perform well. We would expect simple models such as ETS and ARIMA to perform poorly, especially since the primary series of interest does not have strong autocorrelation and has a long seasonal period and complex seasonality that ETS/ARIMA likely cannot model well.

We will use the regular naive benchmark, since the seasonality is complex, containing elements of both yearly and weekly seasonality depending on each series. PM25 also had significant autocorrelation at lag 1, so the regular naive benchmark might perform better.

## Part 2b

Here, we fit the baseline models using the helper functions:

``` r
beijing_cutoff_date <- as.Date("2013-12-31")
beijing_fit_date <- as.Date("2012-12-31")
beijing_upd <- beijing[beijing$Date >= as.Date("2010-01-04"),]
c(train = sum(beijing_upd$Date <= beijing_cutoff_date), 
  test = sum(beijing_upd$Date > beijing_cutoff_date))
#> train  test 
#>  1458   365

beijing_fable <- fit_fable_models(beijing_upd, "lPM", c("lag1_temp", "lag1_dew", 
                                                        "lag2_pres", "lag1_lwind"),
                                  beijing_cutoff_date, naive="NAIVE")
beijing_var <- var_onestep(beijing_upd, c("lPM", "Temp", "Dew", 
                                          "Pres", "lWind"), "lPM",
                           beijing_cutoff_date)
beijing_result <- bind_rows(beijing_fable, beijing_var)

score_table(beijing_result)
#> # A tibble: 7 × 4
#>   model       RMSE   MAE `RMSE vs benchmark`
#>   <chr>      <dbl> <dbl>               <dbl>
#> 1 VAR(4)     0.698 0.546               0.82 
#> 2 NNAR_small 0.706 0.548               0.83 
#> 3 LagReg     0.711 0.552               0.836
#> 4 ARIMA      0.718 0.555               0.844
#> 5 NNAR       0.752 0.584               0.884
#> 6 ETS        0.842 0.646               0.989
#> 7 Benchmark  0.851 0.65                1

plot_onestep(beijing_result, title = "Log daily average PM2.5 concentration (2014)")
```

![](https://i.imgur.com/TINsmS5.png)<!-- -->

``` r

attr(beijing_fable, "fits")
#> # A mable: 1 x 6
#>            ETS                  ARIMA               NNAR  NNAR_small
#>        <model>                <model>            <model>     <model>
#> 1 <ETS(A,N,N)> <ARIMA(1,0,1) w/ mean> <NNAR(29,1,15)[7]> <NNAR(2,2)>
#> # ℹ 2 more variables: LagReg <model>, Benchmark <model>
```

ETS performs very poorly, as expected, and does not include a seasonal part. This suggests ETS cannot handle the long seasonal period of the data, and does a poor job making predictions. This is likely because this method largely averages recent observations, and the PM25 series is not strongly auto-correlated for lags beyond 1. The NNAR without restriction also chooses an extremely complex model, using 29 lags and 15 nodes in the hidden layer. This did not perform as well as I expected NNAR to do, but this is likely an overfit model. Arima also did not select a seasonal component, which is probably due to similar reasons as ETS. The seasonality of this model may be too complex for ARIMA to handle assuming a yearly period.

## Part 2c

First, we conduct the manual tuning for the ETS model. Initially, we consider AICc for the auto-selected model, a model with trend, two models with damped trend, a model with a weekly seasonal component and trend, and a model with a weekly seasonal component and no trend. We use the training fit set, so we can perform validation on the training validation set to select which model we would use.

``` r
beijing_train <- beijing_upd[beijing_upd$Date <= beijing_cutoff_date,]
beijing_test <- beijing_upd[beijing_upd$Date > beijing_cutoff_date,]

beijing_train_fit <- beijing_upd[beijing_upd$Date <= beijing_fit_date,]
beijing_train_val <- beijing_upd[beijing_upd$Date > beijing_fit_date & 
                                              beijing_upd$Date <= beijing_cutoff_date,]

# create the naive estimates 
beijing_naive <- beijing_train |>
  model(
    NAIVE(lPM)
  )

beijing_naive_preds <- refit(
  beijing_naive,
  beijing_upd,
  reestimate = FALSE
) |>
  augment() |>
  as_tibble() |>
  filter(Date > beijing_cutoff_date) |>
  transmute(
    model = .model,
    Date,
    actual = lPM,
    pred = .fitted
  )

beijing_bench <- beijing_naive_preds |>
  summarise(b = sqrt(mean((actual - pred)^2))) |> pull(b)

ets_beijing_fits <- beijing_train_fit |>
  model(
    auto_choice = ETS(lPM ~ error("A") + trend("N") +
                        season("N")),
    with_trend = ETS(lPM ~ error("A") + trend("A") +
                       season("N")),
    damped_0.9 = ETS(lPM ~ error("A") + trend("A",phi=0.9) +
                              season("N")),
    damped_0.8 = ETS(lPM ~ error("A") + trend("A",phi=0.8) +
                       season("N")),
    weekly_seasonal_trend = ETS(lPM ~ error("A") + trend("A") +
        season("A", period = 7)),
    weekly_seasonal_notrend = ETS(lPM ~ error("A") + trend("N") +
                            season("A", period = 7)))

ets_beijing_fits |>
  glance() |>
  dplyr::select(.model, AICc)
#> # A tibble: 6 × 2
#>   .model                   AICc
#>   <chr>                   <dbl>
#> 1 auto_choice             7142.
#> 2 with_trend              7146.
#> 3 damped_0.9              7146.
#> 4 damped_0.8              7146.
#> 5 weekly_seasonal_trend   7161.
#> 6 weekly_seasonal_notrend 7156.

ets_beijing_final_models <- beijing_train |> 
  model(auto_et_choice = ETS(lPM ~ error("A") + trend("N") +
                            season("N")))

ets_beijing_predictions <- forecast(
  ets_beijing_final_models,
  h = nrow(beijing_train_val)
)

ets_beijing_rmse <- sqrt(
  mean(
    (beijing_train_val$lPM - ets_beijing_predictions$.mean)^2,
    na.rm = TRUE
  ))
```

Similar to in part 1, the model that was auto selected performs best in terms of AICc. However, we saw in part b that this model performed poorly on the testing set when compared to baseline. Now, we consider performing ETS using the errors of a regression model with fouirer terms. We choose the value of k by considering different values of AICc:

``` r
beijing_fourier_lms <- beijing_train_fit |>
  model(
    K1  = TSLM(lPM ~ fourier("1 year", K = 1)),
    K2  = TSLM(lPM ~ fourier("1 year", K = 2)),
    K3  = TSLM(lPM ~ fourier("1 year", K = 3)),
    K4  = TSLM(lPM ~ fourier("1 year", K = 4)),
    K5  = TSLM(lPM ~ fourier("1 year", K = 5)),
    K6  = TSLM(lPM ~ fourier("1 year", K = 6)),
    K7  = TSLM(lPM ~ fourier("1 year", K = 7)),
    K8  = TSLM(lPM ~ fourier("1 year", K = 8)),
    K9  = TSLM(lPM ~ fourier("1 year", K = 9)),
    K10 = TSLM(lPM ~ fourier("1 year", K = 10))
  )

glance(beijing_fourier_lms) |>
  arrange(AICc)
#> # A tibble: 10 × 15
#>    .model r_squared adj_r_squared sigma2 statistic   p_value    df log_lik   AIC
#>    <chr>      <dbl>         <dbl>  <dbl>     <dbl>     <dbl> <int>   <dbl> <dbl>
#>  1 K9       0.0506        0.0347   0.653      3.18   8.38e-6    19  -1308. -445.
#>  2 K10      0.0507        0.0330   0.654      2.86   2.64e-5    21  -1308. -441.
#>  3 K4       0.0259        0.0187   0.664      3.60   3.89e-4     9  -1322. -437.
#>  4 K8       0.0401        0.0258   0.659      2.81   1.79e-4    17  -1314. -437.
#>  5 K3       0.0206        0.0151   0.666      3.80   9.45e-4     7  -1325. -435.
#>  6 K5       0.0270        0.0180   0.664      3.00   9.44e-4    11  -1322. -434.
#>  7 K6       0.0304        0.0197   0.663      2.82   8.02e-4    13  -1320. -434.
#>  8 K7       0.0313        0.0187   0.664      2.49   1.78e-3    15  -1319. -431.
#>  9 K1       0.00715       0.00533  0.673      3.92   2.01e-2     3  -1333. -428.
#> 10 K2       0.00978       0.00614  0.672      2.69   3.01e-2     5  -1331. -427.
#> # ℹ 6 more variables: AICc <dbl>, BIC <dbl>, CV <dbl>, deviance <dbl>,
#> #   df.residual <int>, rank <int>
```

The value of k=9 produces the smallest AICc. We will use the errors from this linear model to fit an ETS model (the automatically selected model was additive errors, no trend, no seasonality, which we hard code in). We will then compare the RMSE of this model using the training validation set with the regular auto-selected ETS model to see which performs better. Here, we will just predict all of the values in the validation set at once, instead of refitting the model on successive data points. Whichever performs better will be refit on all of the training data, and then the RMSE will be calculated using the testing data with one-at-a-time validation.

``` r
K9 <- beijing_train |>
  model(K9 = TSLM(lPM ~ fourier("1 year", K = 9)))

K9_resid <- augment(K9) |>
  dplyr::select(Date, .resid)

ets_fourier_beijing_fit <- K9_resid |>
  model(fourier_ets = ETS(.resid ~ error("A") + trend("N") +
                            season("N")))

ets_fourier_beijing_fc <- forecast(
  K9,
  new_data = beijing_train_val
)

ets_fourier_beijing_resid_fc <- forecast(
  ets_fourier_beijing_fit,
  h = nrow(beijing_train_val)
)

combined_fc <- ets_fourier_beijing_fc |>
  mutate(
    .mean = .mean + ets_fourier_beijing_resid_fc$.mean
  )

fourier_beijing_rmse <- sqrt(
  mean(
    (beijing_train_val$lPM - combined_fc$.mean)^2,
    na.rm = TRUE
  )
)

print(c(ets_beijing_rmse, fourier_beijing_rmse))
#> [1] 0.906627 0.830687
```

This model performs significantly better than the auto-selected exponential smoothing model. This means that the fourier terms helped improve our predictions by accounting for some of the variation due to seasonality. Now, we see how it performs compared to the baseline model when considering one-at-a-time validation with the entire testing set.

``` r
## now get the overall rmse ##

final_fourier_fit <- beijing_train |>
  model(
    ets_K9 = TSLM(lPM ~ fourier("1 year", K = 9))
  )

n_train <- nrow(beijing_train)
n_test <- nrow(beijing_test)

combined_preds <- numeric(n_test)

for (i in seq_len(n_test)) {
  train_i <- beijing_upd |>
    slice(seq_len(n_train + i - 1))
  fourier_fit <- train_i |>
    model(
      K9 = TSLM(lPM ~ fourier("1 year", K = 9))
    )
  K9_resid <- augment(fourier_fit) |>
    dplyr::select(Date, .resid)
  ets_resid_fit <- K9_resid |>
    model(
      residual_ets = ETS(.resid ~ error("A") + trend("N") +
                           season("N"))
    )
  fourier_fc <- forecast(fourier_fit, h = 1)
  ets_resid_fc <- forecast(ets_resid_fit, h = 1)
  combined_preds[i] <-
    as.numeric(fourier_fc$.mean[1]) +
    as.numeric(ets_resid_fc$.mean[1])
}

fourier_ets_results <- tibble(
  Date = beijing_test$Date,
  actual = beijing_test$lPM,
  pred = combined_preds
) |>
  mutate(error = actual - pred)

fourier_ets_test_rmse <- sqrt(
  mean(fourier_ets_results$error^2, na.rm = TRUE)
)

fourier_ets_test_mae <- mean(
  abs(fourier_ets_results$error),
  na.rm = TRUE
)

ets_beijing_baseline_models <- beijing_train |> 
  model(auto_ets_choice = ETS(lPM ~ error("A") + trend("N") +
                            season("N")))

ets_beijing_baseline_predictions <- refit(
  ets_beijing_baseline_models,
  beijing_upd,
  reestimate = FALSE
) |>
  augment() |>
  as_tibble() |>
  filter(Date > beijing_cutoff_date ) |>
  transmute(
    model = .model,
    Date,
    actual = lPM,
    pred = .fitted
  )

ets_beijing_results <- ets_beijing_baseline_predictions  |>
  group_by(model) |>
  summarise(
    RMSE = sqrt(mean((actual - pred)^2, na.rm = TRUE)),
    MAE = mean(abs(actual - pred), na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(`RMSE vs benchmark` = RMSE / beijing_bench) |>
  bind_rows(
    tibble(
      model = "Fourier + ETS",
      RMSE = fourier_ets_test_rmse,
      MAE = fourier_ets_test_mae,
      `RMSE vs benchmark` = fourier_ets_test_rmse / beijing_bench
    )
  ) |>
  arrange(RMSE)

ets_beijing_results
#> # A tibble: 2 × 4
#>   model            RMSE   MAE `RMSE vs benchmark`
#>   <chr>           <dbl> <dbl>               <dbl>
#> 1 auto_ets_choice 0.842 0.646               0.989
#> 2 Fourier + ETS   0.843 0.647               0.990
```

When refitting the model without making changes to its parameterization at each successive data point and making one-step-ahead predictions, the ETS with fouirer regression errors does not provide benefit over the auto-selected ETS. In this situation, including fourier terms to help explain seasonality does not help us to make one-step-ahead predictions. However, as we saw in the prior section, if we are making successive predictions without refitting the model using more data, this does provide a prediction improvement. Despite this value of RMSE being smaller, I would prefer this model in any situation where you need to predict more than 1 day out.

Now, we perform tuning for the ARIMA models. We fit models with yearly Fourier terms for a variety of k, Fourier terms for year and weekly period, Fourier terms for just the weekly period, and a SARIMA model with an intercept. These are fit on the training fit set and we perform validation using the training validation set and select the model with the smallest RMSE.

``` r
beijing_arima_fits <- beijing_train_fit |>
  model(
    four2 = ARIMA(lPM ~ fourier(period = "year", K = 2) + PDQ(0,0,0)),
    four3 = ARIMA(lPM ~ fourier(period = "year", K = 3) + PDQ(0,0,0)),
    four4 = ARIMA(lPM ~ fourier(period = "year", K = 4) + PDQ(0,0,0)),
    four5 = ARIMA(lPM ~ fourier(period = "year", K = 5) + PDQ(0,0,0)),
    four6 = ARIMA(lPM ~ fourier(period = "year", K = 6) + PDQ(0,0,0)),
    four7 = ARIMA(lPM ~ fourier(period = "year", K = 7) + PDQ(0,0,0)),
    four32 = ARIMA(lPM ~ fourier(period = "year", K = 3) +
                     fourier(period = "week", K = 2) + PDQ(0,0,0)),
    four42 = ARIMA(lPM ~ fourier(period = "year", K = 4) +
                     fourier(period = "week", K = 2) + PDQ(0,0,0)),
    seasonal_weekly = ARIMA(lPM ~ 1 + PDQ(period = 7)),
    four02 = ARIMA(lPM ~ fourier(period = "week", K = 2) + PDQ(0,0,0)),
    four03 = ARIMA(lPM ~ fourier(period = "week", K = 3) + PDQ(0,0,0))
  )

beijing_fit_arima_predictions <- refit(
  beijing_arima_fits,
  beijing_train,
  reestimate = FALSE
) |>
  augment() |>
  as_tibble() |>
  filter(Date >  beijing_fit_date)  |>
  transmute(
    model = .model,
    Date,
    actual = lPM,
    pred = .fitted
  )

beijing_fit_arima_results <- beijing_fit_arima_predictions |>
  group_by(model) |>
  summarise(
    RMSE = sqrt(mean((actual - pred)^2, na.rm = TRUE)),
    MAE = mean(abs(actual - pred), na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(RMSE)

print(beijing_fit_arima_results)
#> # A tibble: 11 × 3
#>    model            RMSE   MAE
#>    <chr>           <dbl> <dbl>
#>  1 four02          0.683 0.544
#>  2 four03          0.683 0.544
#>  3 seasonal_weekly 0.685 0.544
#>  4 four2           0.690 0.546
#>  5 four32          0.690 0.547
#>  6 four42          0.691 0.546
#>  7 four3           0.692 0.548
#>  8 four4           0.693 0.547
#>  9 four5           0.694 0.547
#> 10 four7           0.697 0.550
#> 11 four6           0.698 0.550
```

We choose the dynamic harmonic regression with k=2. This had the same RMSE as k=3, but we will choose the simpler model. The SARIMA model performed very similarly, so that could also be a reasonable choice. We will fit this model on the full testing set and perform testing to compare its performance to the baseline ARIMA model. I also added the SARIMA model out of curiosity to see how it performed.

``` r
beijing_arima_final <- beijing_train |>
  model(
    seasonal_weekly_ARIMA = ARIMA(lPM ~ 1 + PDQ(period = 7)),
    four02_ARIMA = ARIMA(lPM ~ fourier(period = "week", K = 2) + PDQ(0,0,0)),
    auto_choice_ARIMA = ARIMA(lPM ~ 1 + pdq(1,0,1) + PDQ(0,0,0))
  )

beijing_test_arima_predictions <- refit(
  beijing_arima_final,
  beijing_upd,
  reestimate = FALSE
) |>
  augment() |>
  as_tibble() |>
  filter(Date >  beijing_cutoff_date)  |>
  transmute(
    model = .model,
    Date,
    actual = lPM,
    pred = .fitted
  )

beijing_test_arima_results <- beijing_test_arima_predictions |>
  group_by(model) |>
  summarise(
    RMSE = sqrt(mean((actual - pred)^2, na.rm = TRUE)),
    MAE = mean(abs(actual - pred), na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(`RMSE vs benchmark` = RMSE / beijing_bench) |>
  arrange(RMSE)

print(beijing_test_arima_results)
#> # A tibble: 3 × 4
#>   model                  RMSE   MAE `RMSE vs benchmark`
#>   <chr>                 <dbl> <dbl>               <dbl>
#> 1 four02_ARIMA          0.717 0.556               0.842
#> 2 auto_choice_ARIMA     0.718 0.555               0.844
#> 3 seasonal_weekly_ARIMA 0.718 0.555               0.844
```

This model did show a small improvement over the auto-selected model. However, this gain is very modest. It is very possible an adjustment to the training/testing windows leads to different results.

Now, we tune the NNAR models. We will consider small values of p,P,k, since the small NNAR from the baseline fit performed the best. The NNAR fit without restriction from baseline is likely overfit, so we will not consider very large values of p and k.

``` r
nnar_pPks_beijing <- expand.grid(
  p = c(1,3,5),
  P = c(0,1),
  k = c(1,3,5),
  covs = c("yes", "no")
)

nnar_beijing_train_results <- data.frame(
  p = integer(),
  P = integer(),
  k = integer(),
  covs = character(),
  RMSE = numeric()
)

set.seed(12345)
for (i in seq_len(nrow(nnar_pPks_beijing))) {
  if(nnar_pPks_beijing$covs[i]=="no"){
    nnar_fit <- beijing_train_fit |>
    model(
      nnar = NNETAR(
        lPM ~ AR(
          p = nnar_pPks_beijing$p[i],
          P = nnar_pPks_beijing$P[i],
          period = 365
        ),
        n_nodes = nnar_pPks_beijing$k[i]
      )
    ) 
  } else{
    nnar_fit <- beijing_train_fit |>
    model(
      nnar = NNETAR(
        lPM ~ AR(
          p = nnar_pPks_beijing$p[i],
          P = nnar_pPks_beijing$P[i],
          period = 365
        ) + lag1_lwind + lag2_pres + lag1_dew + lag1_temp,
        n_nodes = nnar_pPks_beijing$k[i]
      )
    ) 
  }
  val_preds_nnar <- refit(
    nnar_fit,
    beijing_train,
    reestimate = FALSE
  ) |>
    augment() |>
    as_tibble() |>
    filter(.data[[index_var(beijing_train)]] %in%
         beijing_train_val[[index_var(beijing_train_val)]])
  rmse <- sqrt(mean(
    (val_preds_nnar$lPM - val_preds_nnar$.fitted)^2,
    na.rm = TRUE
  ))
  nnar_beijing_train_results <- rbind(
    nnar_beijing_train_results,
    data.frame(
      p = nnar_pPks_beijing$p[i],
      P = nnar_pPks_beijing$P[i],
      k = nnar_pPks_beijing$k[i],
      covs = nnar_pPks_beijing$covs[i],
      RMSE = rmse
    )
  )
}
nnar_beijing_train_results %>% arrange(RMSE)
#>    p P k covs      RMSE
#> 1  5 0 3  yes 0.6468956
#> 2  3 0 5  yes 0.6482752
#> 3  3 0 3  yes 0.6489026
#> 4  5 0 5  yes 0.6492491
#> 5  5 1 3  yes 0.6550918
#> 6  1 0 5  yes 0.6554334
#> 7  1 0 3  yes 0.6561053
#> 8  3 1 5  yes 0.6604626
#> 9  3 1 3  yes 0.6614694
#> 10 5 0 1  yes 0.6639430
#> 11 3 0 1  yes 0.6641705
#> 12 1 0 1  yes 0.6642294
#> 13 1 1 1  yes 0.6651493
#> 14 5 1 1  yes 0.6654970
#> 15 3 1 1  yes 0.6658652
#> 16 5 1 5  yes 0.6663328
#> 17 1 1 3  yes 0.6667913
#> 18 1 1 5  yes 0.6722334
#> 19 3 0 5   no 0.6758532
#> 20 3 0 3   no 0.6762738
#> 21 5 0 5   no 0.6779626
#> 22 5 0 1   no 0.6828253
#> 23 3 0 1   no 0.6837782
#> 24 5 1 1   no 0.6848342
#> 25 3 1 1   no 0.6849738
#> 26 3 1 3   no 0.6856017
#> 27 5 0 3   no 0.6856112
#> 28 1 1 5   no 0.6876385
#> 29 1 1 3   no 0.6884144
#> 30 5 1 3   no 0.6885918
#> 31 3 1 5   no 0.6899807
#> 32 1 0 3   no 0.6901117
#> 33 1 0 1   no 0.6909005
#> 34 1 1 1   no 0.6911616
#> 35 1 0 5   no 0.6933356
#> 36 5 1 5   no 0.6969045
```

Here, the best option is using lag 5 and 3 nodes in the hidden layer, in addition to including the covariate information. We will fit this on the full data to compare the seed spread and overall performance to the small NNAR baseline model, since that is the one which performed the best.

``` r
set.seed(2026)
nnar_models_beijing <- beijing_train |>
  model(nnar_manual = NNETAR(lPM ~ AR(p = 5 ,P = 0,
                                     period = 365) +
                               lag1_lwind + lag2_pres + lag1_dew + lag1_temp,
                      n_nodes = 3),
        nnar_auto = NNETAR(lPM ~ AR(p = 2 ,P = 0,
                                     period = 365),
                      n_nodes = 2)
  )


nnar_beijing_specs <- list(
  Manual = list(p = 5, P = 0, k = 3),
  Auto   = list(p = 2, P = 0, k = 2)
)

nnar_beijing_test_results_list <- vector("list", 8 * length(nnar_beijing_specs))
result_idx <- 1

for (model_name in names(nnar_beijing_specs)) {
  spec <- nnar_beijing_specs[[model_name]]
  for (seed in 1:8) {
    set.seed(seed)
    if (model_name=="Manual"){
      nnar_fit <- beijing_train |>
      model(
        nnar = NNETAR(
          lPM ~ AR(
            p = spec$p,
            P = spec$P,
            period = 365
         ) + lag1_lwind + lag2_pres + lag1_dew + lag1_temp,
          n_nodes = spec$k,
          repeats=1
      )
    ) 
    } else{
      nnar_fit <- beijing_train |>
      model(
        nnar = NNETAR(
          lPM ~ AR(
            p = spec$p,
            P = spec$P,
            period = 365
         ),
          n_nodes = spec$k,
          repeats=1
      )
    ) 
    } 
    nnar_preds <- refit(
      nnar_fit,
      beijing_upd,
      reestimate = FALSE
    ) |>
      augment() |>
      as_tibble() |>
      filter(
        .data[[index_var(beijing_upd)]] %in%
          beijing_test[[index_var(beijing_test)]]
      )
    rmse <- sqrt(mean(
      (nnar_preds$lPM - nnar_preds$.fitted)^2,
      na.rm = TRUE
    ))
    mae <- mean(abs(nnar_preds$lPM - nnar_preds$.fitted))
    nnar_beijing_test_results_list[[result_idx]] <- data.frame(
      seed = seed,
      model = model_name,
      RMSE = rmse
    )
    result_idx <- result_idx + 1
  }
}

nnar_beijing_test_results <- bind_rows(nnar_beijing_test_results_list)

nnar_beijing_test_results
#>    seed  model      RMSE
#> 1     1 Manual 0.7006518
#> 2     2 Manual 0.6999821
#> 3     3 Manual 0.7031421
#> 4     4 Manual 0.7057494
#> 5     5 Manual 0.7040401
#> 6     6 Manual 0.7060800
#> 7     7 Manual 0.7028096
#> 8     8 Manual 0.7051861
#> 9     1   Auto 0.7059844
#> 10    2   Auto 0.7064270
#> 11    3   Auto 0.7060079
#> 12    4   Auto 0.7062271
#> 13    5   Auto 0.7059429
#> 14    6   Auto 0.7066877
#> 15    7   Auto 0.7064811
#> 16    8   Auto 0.7064875

nnar_beijing_test_results |>
  group_by(model) |>
  summarize(
    mean_rmse = mean(RMSE, na.rm = TRUE),
    sd_rmse = sd(RMSE, na.rm = TRUE),
    .groups = "drop"
  )
#> # A tibble: 2 × 3
#>   model  mean_rmse  sd_rmse
#>   <chr>      <dbl>    <dbl>
#> 1 Auto       0.706 0.000280
#> 2 Manual     0.703 0.00227
```

The seed still had a minor effect on the RMSE. This was not much different from the equivalent question in part 1. Here is the full model comparison:

``` r
nnar_beijing_test_preds <- refit(
  nnar_models_beijing,
  beijing_upd,
  reestimate = FALSE
) |>
  augment() |>
  as_tibble() |>
  filter(
    .data[[index_var(beijing_upd)]] >
      max(beijing_train[[index_var(beijing_train)]])
  ) |>
  transmute(
    model = .model,
    date = .data[[index_var(beijing_upd)]],
    actual = lPM,
    pred = .fitted
  )

nnar_beijing_test_metrics <- nnar_beijing_test_preds |>
  group_by(model) |>
  summarise(
    RMSE = sqrt(mean((actual - pred)^2, na.rm = TRUE)),
    MAE = mean(abs(actual - pred), na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(`RMSE vs benchmark` = RMSE / beijing_bench) |>
  arrange(RMSE)

nnar_beijing_test_metrics
#> # A tibble: 2 × 4
#>   model        RMSE   MAE `RMSE vs benchmark`
#>   <chr>       <dbl> <dbl>               <dbl>
#> 1 nnar_manual 0.700 0.544               0.822
#> 2 nnar_auto   0.706 0.548               0.830
```

The manually tuned model performed slightly better than the automatically selected baseline NNAR model. Adding covariates provided some predictive benefit, and given the moderate size of the data, this gain is probably larger than the noise.

Now, we tune the VAR models. We first create some day of the week and fouirer series covariates. We will use k=6, which falls in between the choices of k=9 and k=3 from the previous model fits. It would likely be better to use a variety of k, but we will fix k to be one value to keep this part from having too many models to consider.

``` r
beijing_upd <- beijing_upd %>%
  arrange(Date) %>%
  mutate(time = as.numeric(Date - min(Date)))

dow_dummies_upd <- model.matrix(~ day_of_week, data = beijing_upd)[, -1]

K <- 6

fourier_terms <- do.call(cbind, lapply(seq_len(K), function(k) {
  cbind(
    sin(2 * pi * k * beijing_upd$time / 365.25),
    cos(2 * pi * k * beijing_upd$time / 365.25)
  )
}))

colnames(fourier_terms) <- as.vector(
  rbind(paste0("sin", 1:K), paste0("cos", 1:K))
)

X_all <- cbind(dow_dummies_upd, fourier_terms)

n_fit   <- nrow(beijing_train_fit)
n_train <- nrow(beijing_train)
n_all   <- nrow(beijing_upd)

train_fit_covs <- X_all[seq_len(n_fit), , drop = FALSE]
train_val_covs <- X_all[seq.int(n_fit + 1, n_train), , drop = FALSE]
train_covs <- X_all[seq_len(n_train), , drop = FALSE]
test_covs <- X_all[seq.int(n_train + 1, n_all), , drop = FALSE]

lpm_train_ts <- ts(beijing_train_fit$lPM, start=c(2010,4), frequency=365)
temp_train_ts <- ts(beijing_train_fit$Temp, start=c(2010,4), frequency=365)
dew_train_ts <- ts(beijing_train_fit$Dew, start=c(2010,4), frequency=365)
pres_train_ts <- ts(beijing_train_fit$Pres, start=c(2010,4), frequency=365)
lwind_train_ts <- ts(beijing_train_fit$lWind, start=c(2010,4), frequency=365)


VARselect(cbind(lpm_train_ts, temp_train_ts), lag.max=8, type='const')$selection
#> AIC(n)  HQ(n)  SC(n) FPE(n) 
#>      4      4      3      4
VARselect(cbind(lpm_train_ts, temp_train_ts, dew_train_ts), lag.max=8, type='const')$selection
#> AIC(n)  HQ(n)  SC(n) FPE(n) 
#>      6      4      3      6
VARselect(cbind(lpm_train_ts, temp_train_ts, dew_train_ts,
                pres_train_ts), lag.max=8, type='const')$selection
#> AIC(n)  HQ(n)  SC(n) FPE(n) 
#>      7      3      2      7
VARselect(cbind(lpm_train_ts, temp_train_ts, dew_train_ts,
                pres_train_ts, lwind_train_ts ), lag.max=8, type='const')$selection
#> AIC(n)  HQ(n)  SC(n) FPE(n) 
#>      5      3      2      5

VARselect(cbind(lpm_train_ts, temp_train_ts), lag.max=8, type='const',
          exogen=train_fit_covs)$selection
#> AIC(n)  HQ(n)  SC(n) FPE(n) 
#>      2      2      2      2
VARselect(cbind(lpm_train_ts, temp_train_ts, dew_train_ts), lag.max=8, type='const',
          exogen=train_fit_covs)$selection
#> AIC(n)  HQ(n)  SC(n) FPE(n) 
#>      2      2      2      2
VARselect(cbind(lpm_train_ts, temp_train_ts, dew_train_ts,
                pres_train_ts), lag.max=8, type='const', exogen=train_fit_covs)$selection
#> AIC(n)  HQ(n)  SC(n) FPE(n) 
#>      3      2      2      3
VARselect(cbind(lpm_train_ts, temp_train_ts, dew_train_ts,
                pres_train_ts, lwind_train_ts ), lag.max=8, type='const',
          exogen=train_fit_covs)$selection
#> AIC(n)  HQ(n)  SC(n) FPE(n) 
#>      3      2      2      3

# covariates are added in order temp,dew,pres,lwind
# name is # of covs, covariates included, a/b for aicc,bic
beijing_specs <- tribble(
  ~name, ~vars, ~covs, ~p,
  "2na", c("lPM", "Temp"), FALSE, 4,
  "2nb", c("lPM", "Temp"), FALSE, 3,
  "3na", c("lPM", "Temp", "Dew"), FALSE, 6,
  "3nb", c("lPM", "Temp", "Dew"), FALSE, 3,
  "4na", c("lPM", "Temp", "Dew", "Pres"), FALSE, 7,
  "4nb", c("lPM", "Temp", "Dew", "Pres"), FALSE, 2,
  "5na", c("lPM", "Temp", "Dew", "Pres", "lWind"), FALSE, 5,
  "5nb", c("lPM", "Temp", "Dew", "Pres", "lWind"), FALSE, 2,
  
  "2ya", c("lPM", "Temp"), TRUE, 2,
  "3ya", c("lPM", "Temp", "Dew"), TRUE, 3,
  "3yb", c("lPM", "Temp", "Dew"), TRUE, 2,
  "4ya", c("lPM", "Temp", "Dew", "Pres"), TRUE, 3,
  "4yb", c("lPM", "Temp", "Dew", "Pres"), TRUE, 2,
  "5ya", c("lPM", "Temp", "Dew", "Pres", "lWind"), TRUE, 3,
  "5yb", c("lPM", "Temp", "Dew", "Pres", "lWind"), TRUE, 2
)

run_beijing_var_models <- function(
    train_data, test_data, X_train, X_all, specs) {
  all_data <- dplyr::bind_rows(train_data, test_data)
  purrr::map_dfr(seq_len(nrow(specs)), function(i) {
    cur_specs <- specs[i, ]
    response_vars <- cur_specs$vars[[1]]
    use_covs <- cur_specs$covs[[1]]
    p <- cur_specs$p[[1]]
    Y_train <- as.matrix(
      train_data[, response_vars, drop = FALSE]
    )
    Y_all <- as.matrix(
      all_data[, response_vars, drop = FALSE]
    )
    fit <- vars::VAR(
      y = Y_train,
      p = p,
      type = "const",
      exogen = if (use_covs) X_train else NULL
    )
    B <- vars::Bcoef(fit)
    eq <- B["lPM", ]
    validation_indices <- seq.int(
      nrow(train_data) + 1,
      nrow(all_data)
    )
    predicted <- purrr::map_dbl(validation_indices, function(tt) {
      sum(purrr::map_dbl(names(eq), function(nm) {
        if (grepl("\\.l[0-9]+$", nm)) {
          v <- sub("\\.l[0-9]+$", "", nm)
          lag <- as.integer(sub(".*\\.l", "", nm))
          eq[nm] * Y_all[tt - lag, v]
        } else if (nm == "const") {
          eq[nm]
        } else if (
          use_covs &&
          nm %in% colnames(X_all)
        ) {
          eq[nm] * X_all[tt, nm]
        } else {
          0
        }
      }))
    })
    actual <- test_data$lPM
    tibble::tibble(
      model = cur_specs$name[[1]],
      lag = p,
      covs = use_covs,
      Date = test_data$Date,
      actual = actual,
      predicted = predicted
    )
  })
}

beijing_var_1step <- run_beijing_var_models(
  beijing_train_fit,
  beijing_train_val,
  train_fit_covs,
  train_covs,
  beijing_specs
)

beijing_var_1step_info <- beijing_var_1step |>
  group_by(model, lag) |>
  summarise(
    RMSE = sqrt(mean((actual - predicted)^2, na.rm = TRUE)),
    MAE = mean(abs(actual - predicted), na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(`RMSE vs benchmark` = RMSE / beijing_bench) |>
  arrange(RMSE)

beijing_var_1step_info
#> # A tibble: 15 × 5
#>    model   lag  RMSE   MAE `RMSE vs benchmark`
#>    <chr> <dbl> <dbl> <dbl>               <dbl>
#>  1 5nb       2 0.644 0.513               0.757
#>  2 5na       5 0.645 0.515               0.758
#>  3 5ya       3 0.652 0.520               0.766
#>  4 5yb       2 0.653 0.520               0.767
#>  5 4nb       2 0.662 0.526               0.778
#>  6 4na       7 0.664 0.528               0.780
#>  7 4ya       3 0.670 0.528               0.787
#>  8 4yb       2 0.670 0.528               0.787
#>  9 3nb       3 0.688 0.549               0.808
#> 10 3na       6 0.690 0.551               0.810
#> 11 2na       4 0.690 0.548               0.811
#> 12 2nb       3 0.691 0.548               0.811
#> 13 3ya       3 0.695 0.552               0.817
#> 14 3yb       2 0.697 0.552               0.818
#> 15 2ya       2 0.697 0.550               0.818
```

So, the best model for VAR is the model including all of the covariates, not including the seasonal covariates, and considering 2 lags. We will fit this on the full training data and then compare the testing RMSE to the baseline model.

``` r
beijing_specs_test <- tribble(
  ~name, ~vars, ~covs, ~p,
  "var_2lag_all_covs", c("lPM", "Temp", "Dew", "Pres", "lWind"), FALSE, 2,
  "var_baseline", c("lPM", "Temp", "Dew", "Pres", "lWind"), FALSE, 4
)

beijing_var_1step_test <- run_beijing_var_models(
  beijing_train,
  beijing_test,
  train_covs,
  X_all,
  beijing_specs_test
)

beijing_var_1step_test_info <- beijing_var_1step_test |>
  group_by(model, lag) |>
  summarise(
    RMSE = sqrt(mean((actual - predicted)^2, na.rm = TRUE)),
    MAE = mean(abs(actual - predicted), na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(`RMSE vs benchmark` = RMSE / beijing_bench) |>
  arrange(RMSE)

beijing_var_1step_test_info
#> # A tibble: 2 × 5
#>   model               lag  RMSE   MAE `RMSE vs benchmark`
#>   <chr>             <dbl> <dbl> <dbl>               <dbl>
#> 1 var_2lag_all_covs     2 0.697 0.544               0.818
#> 2 var_baseline          4 0.698 0.546               0.820
```

So, here we see a slight improvement in the test RMSE, but it is very minor. It is very possible a change in the test/training windows could lead to baseline performing better.

Finally, we have to fit the lagged regression models:

``` r
beijing_lr_formulas <- list(
  all_covs = lPM ~ lag1_temp + lag1_dew + lag2_pres +lag1_lwind,
  temp = lPM ~ lag1_temp, 
  dew = lPM ~ lag1_dew,
  wind = lPM ~ lag1_lwind,
  temp_wind = lPM ~ lag1_lwind + lag1_temp,
  temp_dew = lPM ~ lag1_temp + lag1_dew,
  sq_wind_temp = lPM ~ lag1_lwind + lag1_temp + lag1_lwind**2 + lag1_temp**2,
  dew_wind_int = lPM  ~ lag1_dew*lag1_lwind
)

beijing_lr_models <- imap(
  beijing_lr_formulas,
  function(formula, name) {
      beijing_train_fit |>
        model(
          fit = ARIMA(
            formula
          )
        )
  }
)

beijing_lr_train_rmse_results <- imap_dfr(beijing_lr_models, function(fit, model_name) {
  forecast(fit, new_data = beijing_train_val) |>
    accuracy(beijing_train_val) |>
    filter(.type == "Test") |>
    transmute(
      model = model_name,
      RMSE
    )
}) |>
  arrange(RMSE)

beijing_lr_train_rmse_results
#> # A tibble: 8 × 2
#>   model         RMSE
#>   <chr>        <dbl>
#> 1 dew_wind_int 0.780
#> 2 temp_wind    0.783
#> 3 sq_wind_temp 0.783
#> 4 dew          0.787
#> 5 temp         0.797
#> 6 wind         0.809
#> 7 all_covs     0.822
#> 8 temp_dew     0.891

beijing_lr_models$dew_wind_int
#> # A mable: 1 x 1
#>                                     fit
#>                                 <model>
#> 1 <LM w/ ARIMA(0,1,1)(0,0,1)[7] errors>
```

The model that performed the best was the model including the dew/wind interactions. Let’s see how this compares to the baseline model.

``` r
beijing_lr_formulas_final <- beijing_train %>% 
  model(
    manual_picked_lr = ARIMA(
      lPM ~ lag1_dew*lag1_lwind +
        pdq(0,1,1) + PDQ(0,0,1)
    ),
    auto_picked_lr = ARIMA(
      lPM ~ lag1_lwind + lag2_pres + lag1_dew + lag1_temp + 
        pdq(1,0,1)
    )
  )

beijing_lr_final_predictions <- refit(
  beijing_lr_formulas_final,
  beijing_upd,
  reestimate = FALSE
) |>
  augment() |>
  as_tibble() |>
  filter(Date > beijing_cutoff_date) |>
  transmute(
    model = .model,
    Date,
    actual = lPM,
    pred = .fitted
  )

beijing_lr_test_metrics <- beijing_lr_final_predictions |>
  group_by(model) |>
  summarise(
    RMSE = sqrt(mean((actual - pred)^2, na.rm = TRUE)),
    MAE = mean(abs(actual - pred), na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(`RMSE vs benchmark` = RMSE / beijing_bench) |>
  arrange(RMSE)

beijing_lr_test_metrics
#> # A tibble: 2 × 4
#>   model             RMSE   MAE `RMSE vs benchmark`
#>   <chr>            <dbl> <dbl>               <dbl>
#> 1 auto_picked_lr   0.711 0.552               0.836
#> 2 manual_picked_lr 0.775 0.617               0.911
```

The manually turned lagged regression performed worse.

## Part 2d

Here is a table of everything:

``` r
all_part2_results <- bind_rows(ets_beijing_results, beijing_test_arima_results,
                           nnar_beijing_test_metrics, beijing_var_1step_test_info %>% 
                             dplyr::select(model, RMSE, `RMSE vs benchmark`, MAE), 
                          beijing_lr_test_metrics )

all_part2_results %>% arrange(RMSE) %>% relocate(model, .before = everything())
#> # A tibble: 11 × 4
#>    model                  RMSE   MAE `RMSE vs benchmark`
#>    <chr>                 <dbl> <dbl>               <dbl>
#>  1 var_2lag_all_covs     0.697 0.544               0.818
#>  2 var_baseline          0.698 0.546               0.820
#>  3 nnar_manual           0.700 0.544               0.822
#>  4 nnar_auto             0.706 0.548               0.830
#>  5 auto_picked_lr        0.711 0.552               0.836
#>  6 four02_ARIMA          0.717 0.556               0.842
#>  7 auto_choice_ARIMA     0.718 0.555               0.844
#>  8 seasonal_weekly_ARIMA 0.718 0.555               0.844
#>  9 manual_picked_lr      0.775 0.617               0.911
#> 10 auto_ets_choice       0.842 0.646               0.989
#> 11 Fourier + ETS         0.843 0.647               0.990

part2_plot_data <- bind_rows(
  beijing_test %>%
     as_tibble() %>%
    transmute(Date, value = lPM, series = "Actual"),

  beijing_var_1step_test %>%
    as_tibble() %>%
    transmute(Date, value = predicted, series = "var_2lag_all_covs"),

  beijing_test_arima_predictions %>%
    as_tibble() %>%
    filter(model == "four02_ARIMA") %>%
    transmute(Date, value = pred, series = "four02_ARIMA")
)

ggplot(part2_plot_data, aes(x = Date, y = value, color = series)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Actual vs Predicted PM2.5 concentration",
    x = "Month",
    y = "Log PM2.5 concentration",
    color = "Series"
  ) +
  theme_minimal()
```

![](https://i.imgur.com/YkxgSxK.png)<!-- -->
The best performing models were the VAR models, dynamic harmonic regression, and NNAR. Let’s refit these on the new training and test period, and we will also evaluate the RMSE only for extreme observations.

``` r
beijing_train2 <- beijing_upd[beijing_upd$Date <= '2013-06-30',]
beijing_test2 <- beijing_upd[beijing_upd$Date > "2013-06-30",]

beijing_fits_newperiod <- beijing_train2 |>
  model(
    auto_ets_choice = ETS(lPM ~ error("A") + trend("N") +
                            season("N")),
    four02_ARIMA = ARIMA(lPM ~ fourier(period = "week", K = 2) + PDQ(0,0,0)),
    nnar_manual = NNETAR(lPM ~ AR(p = 5 ,P = 0,
                                     period = 365) +
                               lag1_lwind + lag2_pres + lag1_dew + lag1_temp,
                      n_nodes = 3),
    auto_picked_lr = ARIMA(
      lPM ~ lag1_lwind + lag2_pres + lag1_dew + lag1_temp + 
        pdq(1,0,1)
    )
  )

beijing_new_period_predictions <- refit(
  beijing_fits_newperiod,
  beijing_upd,
  reestimate = FALSE
) |>
  augment() |>
  as_tibble() |>
  filter(Date > as.Date("2013-06-30")) |>
  transmute(
    model = .model,
    date = Date,
    actual = lPM,
    pred = .fitted
  )

beijing_newperiod_test_metrics <- beijing_new_period_predictions |>
  group_by(model) |>
  summarise(
    RMSE = sqrt(mean((actual - pred)^2, na.rm = TRUE)),
    MAE = mean(abs(actual - pred), na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(RMSE)

n_train2 <- nrow(beijing_train2)
train_covs_newperiod <- X_all[seq_len(n_train2), , drop = FALSE]

beijing_var_newperiod <- run_beijing_var_models(
  beijing_train2,
  beijing_test2,
  train_covs_newperiod,
  X_all,
  beijing_specs_test
)

beijing_var_newperiod_info <- beijing_var_newperiod |>
  group_by(model) |>
  summarise(
    RMSE = sqrt(mean((actual - predicted)^2, na.rm = TRUE)),
    MAE = mean(abs(actual - predicted), na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(RMSE)

beijing_newperiod_test_metrics
#> # A tibble: 4 × 3
#>   model            RMSE   MAE
#>   <chr>           <dbl> <dbl>
#> 1 nnar_manual     0.679 0.530
#> 2 auto_picked_lr  0.687 0.538
#> 3 four02_ARIMA    0.692 0.537
#> 4 auto_ets_choice 0.818 0.635
beijing_var_newperiod_info
#> # A tibble: 2 × 3
#>   model              RMSE   MAE
#>   <chr>             <dbl> <dbl>
#> 1 var_2lag_all_covs 0.674 0.527
#> 2 var_baseline      0.677 0.531
```

The VAR models still performed the best. With the new training/test periods, NNAR now performed better than lagged regression and ARIMA, but in each case the RMSE is relatively close. Even with a large data set, the choice of training/testing window still plays an important role. ETS still performs the worst.

``` r
top10_threshold <- quantile(
  beijing_new_period_predictions$actual,
  probs = 0.90,
  na.rm = TRUE
)

beijing_newperiod_top10_metrics <- beijing_new_period_predictions |>
  filter(actual >= top10_threshold) |>
  group_by(model) |>
  summarise(
    n = n(),
    RMSE_top10 = sqrt(mean((actual - pred)^2, na.rm = TRUE)),
    MAE_top10 = mean(abs(actual - pred), na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(RMSE_top10)

top10_threshold_var <- quantile(
  beijing_var_newperiod$actual,
  probs = 0.90,
  na.rm = TRUE
)

beijing_var_top10_metrics <- beijing_var_newperiod |>
  filter(actual >= top10_threshold_var) |>
  group_by(model) |>
  summarise(
    n = n(),
    RMSE_top10 = sqrt(mean((actual - predicted)^2, na.rm = TRUE)),
    MAE_top10 = mean(abs(actual - predicted), na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(RMSE_top10)

beijing_top10_results <- bind_rows(
  beijing_newperiod_top10_metrics,
  beijing_var_top10_metrics
) |>
  arrange(RMSE_top10)

beijing_top10_results
#> # A tibble: 6 × 4
#>   model                 n RMSE_top10 MAE_top10
#>   <chr>             <int>      <dbl>     <dbl>
#> 1 var_2lag_all_covs    55      0.772     0.716
#> 2 nnar_manual          55      0.775     0.711
#> 3 var_baseline         55      0.782     0.725
#> 4 auto_picked_lr       55      0.799     0.756
#> 5 auto_ets_choice      55      0.822     0.695
#> 6 four02_ARIMA         55      0.869     0.825
```

If we are interesting in performance when predicting the extreme observations, ARIMA does significantly worse than VAR and NNAR. This is because VAR and NNAR are better suited to the complex seasonality and non-linear behavior that may characterize the high peaks, while simpler models like ARIMA and ETS tend to be more conservative in their predictions.

# Part 3

## Part 3a

``` r
housing_selected_models <- c(
  "ETS_auto_choice",
  "housing_autoarima",
  "nnar_manual",
  "all_lag1_simple",
  "VAR_permits_noseasonal_p8"
)

housing_results_table <- all_part1_results |>
  dplyr::filter(model %in% housing_selected_models) |>
  dplyr::select(model, RMSE) |>
  mutate(
    model = recode(
      model,
      "ETS_auto_choice" = "ETS",
      "housing_autoarima" = "ARIMA",
      "nnar_manual" = "NNAR",
      "all_lag1_simple" = "LaggedRegression",
      "VAR_permits_noseasonal_p8" = "VAR"
    ),
    Housing = RMSE
  ) %>%
   dplyr::select(-RMSE)

beijing_selected_models <- c(
  "auto_ets_choice",
  "four02_ARIMA",
  "nnar_manual",
  "var_2lag_all_covs",
  "manual_picked_lr"
)

beijing_results_table <- all_part2_results |>
  dplyr::filter(model %in% beijing_selected_models) |>
  dplyr::select(model, RMSE) |>
  mutate(
    model = recode(
      model,
      "auto_ets_choice" = "ETS",
      "four02_ARIMA" = "ARIMA",
      "nnar_manual" = "NNAR",
      "var_2lag_all_covs" = "VAR",
      "manual_picked_lr" = "LaggedRegression"
    ),
    Beijing = RMSE
  ) %>%
   dplyr::select(-RMSE)

combined_table <- dplyr::left_join(
  beijing_results_table ,
  housing_results_table ,
  by = "model"
)

combined_table |>
  gt() |>
   tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = Housing,
      rows = Housing == min(Housing, na.rm = TRUE)
    )
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = Beijing,
      rows = Beijing == min(Beijing, na.rm = TRUE)
    )
  )
```

<div id="bewvgrktlo" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#bewvgrktlo table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#bewvgrktlo thead, #bewvgrktlo tbody, #bewvgrktlo tfoot, #bewvgrktlo tr, #bewvgrktlo td, #bewvgrktlo th {
  border-style: none;
}
&#10;#bewvgrktlo p {
  margin: 0;
  padding: 0;
}
&#10;#bewvgrktlo .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#bewvgrktlo .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#bewvgrktlo .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#bewvgrktlo .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#bewvgrktlo .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#bewvgrktlo .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#bewvgrktlo .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#bewvgrktlo .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#bewvgrktlo .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#bewvgrktlo .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#bewvgrktlo .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#bewvgrktlo .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#bewvgrktlo .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#bewvgrktlo .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#bewvgrktlo .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#bewvgrktlo .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#bewvgrktlo .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#bewvgrktlo .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#bewvgrktlo .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#bewvgrktlo .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#bewvgrktlo .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#bewvgrktlo .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#bewvgrktlo .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#bewvgrktlo .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#bewvgrktlo .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#bewvgrktlo .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#bewvgrktlo .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#bewvgrktlo .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#bewvgrktlo .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#bewvgrktlo .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#bewvgrktlo .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#bewvgrktlo .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#bewvgrktlo .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#bewvgrktlo .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#bewvgrktlo .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#bewvgrktlo .gt_left {
  text-align: left;
}
&#10;#bewvgrktlo .gt_center {
  text-align: center;
}
&#10;#bewvgrktlo .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#bewvgrktlo .gt_font_normal {
  font-weight: normal;
}
&#10;#bewvgrktlo .gt_font_bold {
  font-weight: bold;
}
&#10;#bewvgrktlo .gt_font_italic {
  font-style: italic;
}
&#10;#bewvgrktlo .gt_super {
  font-size: 65%;
}
&#10;#bewvgrktlo .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#bewvgrktlo .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#bewvgrktlo .gt_indent_1 {
  text-indent: 5px;
}
&#10;#bewvgrktlo .gt_indent_2 {
  text-indent: 10px;
}
&#10;#bewvgrktlo .gt_indent_3 {
  text-indent: 15px;
}
&#10;#bewvgrktlo .gt_indent_4 {
  text-indent: 20px;
}
&#10;#bewvgrktlo .gt_indent_5 {
  text-indent: 25px;
}
&#10;#bewvgrktlo .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#bewvgrktlo div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="model">model</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Beijing">Beijing</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Housing">Housing</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="model" class="gt_row gt_left">ETS</td>
<td headers="Beijing" class="gt_row gt_right">0.8422123</td>
<td headers="Housing" class="gt_row gt_right" style="font-weight: bold;">0.07475261</td></tr>
    <tr><td headers="model" class="gt_row gt_left">ARIMA</td>
<td headers="Beijing" class="gt_row gt_right">0.7171727</td>
<td headers="Housing" class="gt_row gt_right">0.09672345</td></tr>
    <tr><td headers="model" class="gt_row gt_left">NNAR</td>
<td headers="Beijing" class="gt_row gt_right">0.6997427</td>
<td headers="Housing" class="gt_row gt_right">0.09552969</td></tr>
    <tr><td headers="model" class="gt_row gt_left">VAR</td>
<td headers="Beijing" class="gt_row gt_right" style="font-weight: bold;">0.6967676</td>
<td headers="Housing" class="gt_row gt_right">0.08795550</td></tr>
    <tr><td headers="model" class="gt_row gt_left">LaggedRegression</td>
<td headers="Beijing" class="gt_row gt_right">0.7752780</td>
<td headers="Housing" class="gt_row gt_right">0.08963593</td></tr>
  </tbody>
  &#10;</table>
</div>

## Part 3b

In general, the predictions about which model class would be best were reasonably accurate. For the housing data set, we had a smaller sample size, near linear behavior, and cyclical patterns in the series, which led me to believe the simple models such as ETS/ARIMA would do the best. The tuned ETS (which was the same as the baseline-selected ETS) performed very well, and while the tuned ARIMA performed poorly, the automatically selected ARIMA did well in terms of predicting on the test set. There were not any complex non-linear relationships or interactions between covariates that NNAR would provide benefit, so it made sense this model class performed poorly, as predicted. Since housing permits appeared a natural explanatory variable for housing starts, I was surprised lagged regression did not perform better than it did.

In terms of the Beijing data, we had a much larger time series, more complex cyclical behavior that seasonality poorly accounted for, non-linear relationships between the variables, interactions between the variables, and a considerable amount of noise. Given these factors, NNAR performed well as expected. VAR also performed well, indicating including the own series’ and related covariates’ past was beneficial. ETS performed poorly as predicted, since it could not account for the yearly seasonality in the data, even when performing ETS on the fourier errors. I was surprised ARIMA did well initially, once incorporating the fourier terms. However, it did extremely poorly in terms of predicting high periods of pollution. This indicates ARIMA is not a good choice for this data if that is the predictive goal, since it cannot model the more complex, irregular peaks well.

## Part 3c

For the housing data set, the biggest improvement came from manually tuning the VAR model to see which covariates should be included. Doing so improved the RMSE from 0.10 to 0.088. Improvements in NNAR (0.096 to 0.089) and lagged regression (0.096 to 0.09) also came from just trying different combinations of p/k or different combinations of lagged predictors. In general, attempting to account for seasonality by adding monthly regressors did not provide benefit in terms of predictive power. This seasonality was already well-accounted for in the baseline ETS model with additive seasonality (and log outcomes) and the seasonal ARIMA component. Tuning in terms of seasonality did not provide any additional benefit.

For the Beijing data set, the biggest benefit came from adding the interaction between dew and wind to the lagged regression (RMSE improved from 0.775 to 0.711). This helped the simple model account for a complex non-linear effect of the explanatory variables. Incorporating covariate information into NNAR and adjusting p/P/k provided a small benefit over the constrained baseline NNAR (RMSE improved from 0.706 to 0.6997), but this was a minor improvement. If we compare the the unconstrained baseline NNAR, there was a much more dramatic improvement, since that model was very overfit. None of the other tuning made much of a difference. Manually tuning the ARIMA by adding the Fourier terms led to a tiny improvement in RMSE from 0.702 to 0.699, indicating that explaining the seasonality in a deterministic way provided some minor benefit for the ARIMA model. Changes in VAR and ETS through attempting to add other variables or trying to use ETS errors fitted on residuals of fouirer regression were negligible.

## Part 3d

For a small, monthly data set, where the goal is one-step ahead prediction, I would recommend exponential smoothing or SARIMA. These can account for the monthly seasonality well and are good choices if there is not a lot of data. If there are other covariates that might help explain changes in the main series of interest, using VAR might also be a good idea given the relatively small data size. When comparing different model classes with this small data set, is it important to understand that the specific training/test windows can have a drastic impact on how the models perform.

For a larger, daily data set, I would first recommend NNAR. This can well explain non-linear patterns with other covariates and the series past observations. However, it is important to manually tune this model or incorporate some constraints to avoid over fitting. Other good choices include VAR, which performed very well for the Beijing data, and can model several outcomes jointly. If several strong predictors are included, this model class can work well. A third choice could be ARIMA, depending on the purpose of the prediction. If the goal is to model extreme values in the data, such as high levels of pollution, ARIMA can easily struggle. If we want a model that can make more moderate predictions and model seasonality well, ARIMA can be a good choice. Attempting to model seasonality in several ways, such as a seasonal component or Fourier terms as a regressor, can also help improve model performance.

<sup>Created on 2026-11-02 with [reprex v2.1.1](https://reprex.tidyverse.org)</sup>
