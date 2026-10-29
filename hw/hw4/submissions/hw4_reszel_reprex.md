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

![](https://i.imgur.com/FTNrb86.png)<!-- -->

``` r

housing$Mortgage %>% ts(start=c(2010,1), frequency=12) %>%
  as_tsibble() %>% gg_season(y=value) + 
  labs(title = "Seasonal Plot of Mortgage Rates",
    x = "Month",
    y = "Mortgage Rate")
```

![](https://i.imgur.com/Iz2Z1Ze.png)<!-- -->

``` r

housing$Permits %>% ts(start=c(2010,1), frequency=12) %>%
  as_tsibble() %>% gg_season(y=value) +
  labs(title = "Seasonal Plot of New Permits",
       x = "Month",
       y = "New Permits")
```

![](https://i.imgur.com/uC8I6S5.png)<!-- -->

``` r

housing$Starts %>% ts(start=c(2010,1), frequency=12) %>%
  as_tsibble() %>% gg_season(y=value) +
  labs(title = "Seasonal Plot of Housing Starts",
       x = "Month",
       y = "Housing Starts")
```

![](https://i.imgur.com/045u6zr.png)<!-- -->
There is a clear increasing trend in housing permits and starts, although it appears to level out instead of increasing linearly. Mortgage rate does not appear to have a constant trend, instead showing decreasing trend (with significant noise) from 2010 to 2020, then showing a period of significant decrease followed by a sharp increase.

Mortgage rates do not show any clear seasonality, but the seasonal plot shows mortgage rates are higher in more recent years. Housing starts and new permits appear to be higher in the summer months, but this seasonality is less pronounced for housing permits.

Changes in permits appears to be strongly correlated with changes in starts. It seems that a decrease in mortgage rate leads to a corresponding increase in starts/permits, while an increase in mortgage rate could correspond to a decrease in the acceleration of starts/permits.

The variance is housing permits/starts does not seem to be constant over time. We can look at the
scale-location plot from a simple regression on time to see if variance changes over time.

``` r
housing$date <- as.Date(housing$Month)
housing$time <- housing$date - min(housing$date)

plot(lm(Starts~time, data=housing), which=3)
```

![](https://i.imgur.com/mXzgHf8.png)<!-- -->

``` r
plot(lm(Permits~time, data=housing), which=3)
```

![](https://i.imgur.com/LdwZbB1.png)<!-- -->

``` r

housing$logStarts <- log(housing$Starts)
housing$logPermits <- log(housing$Permits)
```

While not drastic, both scale location plots do not exhibit completely straight line behavior. This evidence for non-constant variance, as well as a desire to get mortgages and housing starts/permits on the same scale causes us to take the log of these two variables.

Now, we can look at the cross-correlation plots to see which lags to include. We use the prewhiten trend to remove autocorrelation, and take the difference to remove the trend:

``` r
prewhiten(diff(housing$logStarts), diff(housing$logPermits))
```

![](https://i.imgur.com/Mqgym85.png)<!-- -->

``` r
prewhiten(diff(housing$logStarts), diff(housing$Mortgage))
```

![](https://i.imgur.com/cjDA61w.png)<!-- -->

``` r

acf(housing$logStarts)
```

![](https://i.imgur.com/QxkFlF3.png)<!-- -->

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

For this data set, since it expresses seasonality and is a relatively short series with only a few covariates, we might expect classic models such as ARIMA to perform the best. We might expect NNAR to do the worst due to the small data set.

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

![](https://i.imgur.com/xEyBHV2.png)<!-- -->

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
  summarise(b = sqrt(mean((actual - pred)^2))) |> pull(b)

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
  model(auto_choice = ETS(logStarts ~ error("M") + trend("N") +
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
#>   model         RMSE    MAE `RMSE vs benchmark`
#>   <chr>        <dbl>  <dbl>               <dbl>
#> 1 auto_choice 0.0748 0.0587               0.671
```

Now, we perform the same process for an ARIMA model. We fit the models on data through 2018, choose the candidate models with the same levels of differencing using Aicc, validate those choices on data from 2019-2020, and then refit the best model by RMSE on the full training set.

``` r
housing_train_fit <- housing_train[housing_train$date <= '2018-12-01',]
housing_train_val <- housing_train[housing_train$date > '2018-12-01',]

log_starts_train_fit_ts <- ts(housing_train_fit$logStarts, start=c(2010,4), frequency=12)
log_starts_all_train_ts <- ts(housing_train$logStarts, start=c(2010,4), frequency=12)

acf(log_starts_train_fit_ts, lag.max=48)
```

![](https://i.imgur.com/3Dm1kKm.png)<!-- -->

``` r
acf(diff(log_starts_train_fit_ts, 1), lag.max=48)
```

![](https://i.imgur.com/dslKSJh.png)<!-- -->

``` r
pacf(diff(log_starts_train_fit_ts, 1), lag.max=48)
```

![](https://i.imgur.com/FqEkVUx.png)<!-- -->

``` r

acf(diff(log_starts_train_fit_ts, 12), lag.max=48)
```

![](https://i.imgur.com/IgEWTxO.png)<!-- -->

``` r
pacf(diff(log_starts_train_fit_ts, 12), lag.max=48)
```

![](https://i.imgur.com/cv8nxJH.png)<!-- -->

``` r

acf(diff(diff(log_starts_train_fit_ts, 12)), lag.max=48)
```

![](https://i.imgur.com/VD9YXHr.png)<!-- -->

``` r
pacf(diff(diff(log_starts_train_fit_ts, 12)), lag.max=48)
```

![](https://i.imgur.com/V3xi89m.png)<!-- -->

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

![](https://i.imgur.com/l0gmviO.png)<!-- -->

``` r

arima_options |>
  dplyr::select(housing_arima_manual_D) |>
  gg_tsresiduals()
```

![](https://i.imgur.com/aj7WiXF.png)<!-- -->

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
    housing_autopicked = ARIMA(logStarts ~ pdq(1, 1, 1) + PDQ(0, 1, 1))
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
#>   model                RMSE    MAE `RMSE vs benchmark`
#>   <chr>               <dbl>  <dbl>               <dbl>
#> 1 housing_autopicked 0.0848 0.0705               0.761
#> 2 housing_autoarima  0.0967 0.0760               0.868
```

The tuned arima model does not appear to show any benefit over the baseline arima model.

Now, we fit the NNAR models for a variety of small p,k. We fit the models on the training fit set and then compare the RMSE for the training validation set.:

``` r
nnar_pPks <- expand.grid(
  p = c(1,2,3,5,7),
  P = 1,
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
        ) + lag1_logPermits + lag2_Mortgage,
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
#> 1  5 1 1 0.1028528
#> 2  3 1 1 0.1049826
#> 3  2 1 1 0.1050698
#> 4  1 1 1 0.1056514
#> 5  7 1 1 0.1069975
#> 6  5 1 2 0.1344414
#> 7  3 1 2 0.1356247
#> 8  2 1 2 0.1417994
#> 9  7 1 2 0.1425642
#> 10 1 1 2 0.1544605
#> 11 2 1 5 0.1619070
#> 12 5 1 7 0.1642056
#> 13 5 1 3 0.1661197
#> 14 7 1 3 0.1665025
#> 15 3 1 5 0.1716140
#> 16 2 1 3 0.1748084
#> 17 2 1 7 0.1748549
#> 18 3 1 3 0.1777901
#> 19 1 1 3 0.1784652
#> 20 3 1 7 0.1796996
#> 21 1 1 5 0.1871218
#> 22 5 1 5 0.1880536
#> 23 7 1 5 0.1938838
#> 24 7 1 7 0.1987446
#> 25 1 1 7 0.2036278
```

We can see the best choice is p=5, p=1,k=1. So, we consider 5 lags, 1 seasonal lag, and 1 node in the hidden layer. This is a relative simple nueral network, but it uses a moderate number of lags. Now, we consider changing the seeds to see how this choice compares with the baseline model.

``` r
nnar_manual <- housing_train |>
  model(nnar = NNETAR(logStarts ~ AR(p = 5 ,P = 1,
                                     period = 12) + lag1_logPermits + lag2_Mortgage,
                      n_nodes = 1
  )
  )

nnar_auto <-  housing_train |>
  model(nnar = NNETAR(logStarts ~ AR(p = 2 ,P = 1,
                                     period = 12) + lag1_logPermits + lag2_Mortgage,
                      n_nodes = 2
  )
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
          ) + lag1_logPermits + lag2_Mortgage,
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
      RMSE = rmse,
      mae = mae
    )
    result_idx <- result_idx + 1
  }
}

nnar_test_results <- bind_rows(nnar_test_results_list)

nnar_test_results
#>    seed  model      RMSE        mae
#> 1     1 Manual 0.1479209 0.12664552
#> 2     2 Manual 0.1477915 0.12652231
#> 3     3 Manual 0.1478830 0.12660747
#> 4     4 Manual 0.1478433 0.12657151
#> 5     5 Manual 0.1478507 0.12657666
#> 6     6 Manual 0.1480189 0.12673240
#> 7     7 Manual 0.1477990 0.12652828
#> 8     8 Manual 0.1477751 0.12650797
#> 9     1   Auto 0.1842271 0.14918576
#> 10    2   Auto 0.1609406 0.14131168
#> 11    3   Auto 0.1785024 0.15788301
#> 12    4   Auto 0.1747238 0.14834430
#> 13    5   Auto 0.1785223 0.15652454
#> 14    6   Auto 0.1110352 0.08560697
#> 15    7   Auto 0.1383344 0.12018711
#> 16    8   Auto 0.1455562 0.11349025

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
#> 1 Auto       0.159 0.0255   
#> 2 Manual     0.148 0.0000806

nnar_test_results |>
  group_by(model) |>
  summarize(
    mean_rmse = mean(RMSE, na.rm = TRUE),
    mean_mae = mean(mae, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(`RMSE vs benchmark` = mean_rmse / housing_bench)
#> # A tibble: 2 × 4
#>   model  mean_rmse mean_mae `RMSE vs benchmark`
#>   <chr>      <dbl>    <dbl>               <dbl>
#> 1 Auto       0.159    0.134                1.43
#> 2 Manual     0.148    0.127                1.33
```

There appears to be more variation depending on seed in the baseline model, while the manually tuned model shows more stability. This shows that depending on the specific NNAR fit, seed can have a drastic impact. This shows the importance of fitting multiple models and taking averages to have more stable estimates, which R generally does automatically. However, our manually tuned model seems to show a relatively large improvement in RMSE. There is a decent amount of variation in the automatic model due to the seeds, but the improvement seems to be sufficiently large to justify the choice.

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
  "Auto_selected", c("logStarts", "logPermits", "Mortgage"), FALSE, 4
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
#> 2 Auto_selected                 4 FALSE  0.1000 0.0800               0.898
```

Here, the manually tuned model showed a significant improvement over the auto selected model in terms of RMSE. The improvement is quite large, and noise is likely removed by having fewer variables in the model.

Perhaps removing the mortgage information improved out of sample testing accuracy, and it is possible this model was overfit initially.

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

![](https://i.imgur.com/qOBkpZg.png)<!-- -->

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

![](https://i.imgur.com/ZIKuMjm.png)<!-- -->
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

![](https://i.imgur.com/0m7zA2w.png)<!-- -->

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

![](https://i.imgur.com/zm4ss1u.png)<!-- -->

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

![](https://i.imgur.com/hpRKL64.png)<!-- -->

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

![](https://i.imgur.com/DOZ0gOB.png)<!-- -->

In terms of PM2.5, there appears to be higher concentrations in the winter months and low concentrations in the summer months, as well as high variation in the winter months. There may also be a slightly higher concentration on weekends, but the plot is somewhat unclear. This seasonality does not appear to be drastic, as the series is very noisy.

Here are some ACF plots of the data:

``` r
acf(lPM_full_ts, lag.max=365)
```

![](https://i.imgur.com/KhwVuwm.png)<!-- -->

``` r
acf(temp_full_ts, lag.max=365)
```

![](https://i.imgur.com/TCko4b6.png)<!-- -->

``` r
acf(dew_full_ts, lag.max=365)
```

![](https://i.imgur.com/3rY4FGl.png)<!-- -->

``` r
acf(pres_full_ts, lag.max=365)
```

![](https://i.imgur.com/QY7T3Wi.png)<!-- -->

``` r
acf(lWind_full_ts, lag.max=365)
```

![](https://i.imgur.com/RMtiRED.png)<!-- -->

PM25 concentration does not appear to be heavily autocorrelated, since it appears very noisy and already looks stationary. In all of the other series, there is obvious seasonality/oscilation.

Now, we can look at scatter plots of PM25 with the other variables:

``` r
ggplot(beijing, aes(x = lPM, y = Temp)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue")
#> `geom_smooth()` using formula = 'y ~ x'
```

![](https://i.imgur.com/pYFMqSe.png)<!-- -->

``` r

ggplot(beijing, aes(x = lPM, y = Dew)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue")
#> `geom_smooth()` using formula = 'y ~ x'
```

![](https://i.imgur.com/hCr32fr.png)<!-- -->

``` r

ggplot(beijing, aes(x = lPM, y = Wind)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue")
#> `geom_smooth()` using formula = 'y ~ x'
```

![](https://i.imgur.com/tORmOBx.png)<!-- -->

``` r

ggplot(beijing, aes(x = lPM, y = Pres)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue")
#> `geom_smooth()` using formula = 'y ~ x'
```

![](https://i.imgur.com/CZW9J63.png)<!-- -->

There appears to be very moderate linear relationships between PM2.5 concentration and dew point (positive) and between PM2.5 concentration and air pressure (negative). The relationship between wind and PM2.5 and wind does not appear to be linear. Regularly, this may prompt us to consider a transformation of wind speed, depending on which kind of model we choose to use.

Now we can look at ccf plots to consider which lags to use in the models. We consider differenced values if there is seasonality or trend:

``` r
ccf(lPM_full_ts, diff(diff(temp_full_ts,365)))
```

![](https://i.imgur.com/PRYslZs.png)<!-- -->

``` r
ccf(lPM_full_ts, diff(diff(dew_full_ts,365)))
```

![](https://i.imgur.com/hxE1WG0.png)<!-- -->

``` r
ccf(lPM_full_ts, diff(pres_full_ts,365))
```

![](https://i.imgur.com/RUgFvou.png)<!-- -->

``` r
ccf(lPM_full_ts, diff(diff(lWind_full_ts,365)))
```

![](https://i.imgur.com/BDQxySD.png)<!-- -->

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

![](https://i.imgur.com/aXg4DfD.png)<!-- -->

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
  model(auto_choice = ETS(lPM ~ error("A") + trend("N") +
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
    K9 = TSLM(lPM ~ fourier("1 year", K = 9))
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
    seasonal_weekly = ARIMA(lPM ~ 1 + PDQ(period = 7)),
    four02 = ARIMA(lPM ~ fourier(period = "week", K = 2) + PDQ(0,0,0)),
    auto_choice_ARIMA = ARIMA(lPM ~ 1 + pdq(1,0,1) + PDQ(0,0,0))
  )

beijing_test_arima_predictions <- refit(
  beijing_arima_final,
  beijing_upd,
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
#>   model              RMSE   MAE `RMSE vs benchmark`
#>   <chr>             <dbl> <dbl>               <dbl>
#> 1 four02            0.699 0.549               0.821
#> 2 auto_choice_ARIMA 0.702 0.549               0.824
#> 3 seasonal_weekly   0.702 0.549               0.824
```

This model did show a small improvement over the auto-selected model. However, this gain is very modest. It is very possible an adjustment to the training/testing windows leads to different results.

Now, we tune the NNAR models:

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
  "2lag_all_covs", c("lPM", "Temp", "Dew", "Pres", "lWind"), FALSE, 2,
  "baseline", c("lPM", "Temp", "Dew", "Pres", "lWind"), FALSE, 4
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
#>   model           lag  RMSE   MAE `RMSE vs benchmark`
#>   <chr>         <dbl> <dbl> <dbl>               <dbl>
#> 1 2lag_all_covs     2 0.697 0.544               0.818
#> 2 baseline          4 0.698 0.546               0.820
```

So, here we see a slight improvement in the test RMSE, but it is very minor. It is very possible a change in the test/training windows could lead to baseline performing better.

<sup>Created on 2026-10-28 with [reprex v2.1.1](https://reprex.tidyverse.org)</sup>
