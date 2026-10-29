# HW4_Dimmig


## HW 4

``` r
library(fpp3)
library(reprex)
library(vars)
library(tseries)
all_res <- list() 
source("https://raw.githubusercontent.com/jlivsey/UB-fall26-time-series/main/slides/11-path-forward/bakeoff-helpers.R")
select <- dplyr::select
onestep <- function(mdls, full, from) {
  refit(mdls, full, reestimate = FALSE) |> augment() |> as_tibble() |>
    filter(Month > from) |>
    transmute(model = .model, date = Month, actual = lstarts, pred = .fitted)
}
rmse <- function(d) sqrt(mean((d$actual - d$pred)^2))
val_tab <- function(mdls) onestep(mdls, h_train, val_fit_end) |> group_by(model) |>
  summarise(val_RMSE = sqrt(mean((actual - pred)^2))) |> arrange(val_RMSE)

var_pred <- function(vars, exog, ic, lag_max, fit_end, last_end = max(h$Month)) {
  d <- as_tibble(h |> filter(Month <= last_end))
  Y <- as.matrix(d[, vars]); X <- if (is.null(exog)) NULL else as.matrix(d[, exog])
  n_fit <- sum(d$Month <= fit_end)
  Ytr <- Y[1:n_fit, , drop = FALSE]
  Xtr <- if (is.null(X)) NULL else X[1:n_fit, , drop = FALSE]
  p  <- unname(VARselect(Ytr, lag.max = lag_max, type = "const", exogen = Xtr)$selection[ic])
  eq <- Bcoef(VAR(Ytr, p = p, type = "const", exogen = Xtr))["lstarts", ]
  idx <- (n_fit + 1):nrow(Y)
  pred <- sapply(idx, function(t) sum(sapply(names(eq), function(nm) {
    if (grepl("\\.l[0-9]+$", nm)) eq[nm] * Y[t - as.integer(sub(".*\\.l", "", nm)), sub("\\.l[0-9]+$", "", nm)]
    else if (nm == "const") eq[nm] else eq[nm] * X[t, nm]
  })))
  tibble(model = "VAR", date = d$Month[idx], actual = Y[idx, "lstarts"], pred = unname(pred))
}
rmse_tab <- function(d) d |> group_by(model) |>
  summarise(RMSE = sqrt(mean((actual - pred)^2)), MAE = mean(abs(actual - pred))) |>
  arrange(RMSE)
```

# Part 1

``` r
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

housing |> 
  select(Month, Starts, Permits, Mortgage) |>
  pivot_longer(-Month) |>
  ggplot(aes(Month, value)) + geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(x = NULL, y = NULL)
```

![](hw4_work_files/figure-commonmark/unnamed-chunk-2-1.png)

1.  There is a positive trend with both Permits and Starts; Mortgage
    appears to have a slight decreasing trend from 2010 to about 2021,
    then there is a sharp increase followed by a flattening out. Permits
    and Starts look to have a possible seasonality component with lower
    values generally in the winter time and higher in the summer.
    Permits and Starts appear to share similar relationships while
    Mortgage has an inverse relationship (as the other two variables are
    increasing, mortgage decreases). At around 2022 when Mortgage
    increases, there is a decrease in Permits and Starts.

    ``` r
    housing |> gg_season(Starts, period = "year") + labs(title = "Starts by month (one line per year)")
    ```

    ![](hw4_work_files/figure-commonmark/unnamed-chunk-3-1.png)

    ``` r
    housing |> gg_season(Permits, period = "year") + labs(title = "Permits by month (one line per year)")
    ```

    ![](hw4_work_files/figure-commonmark/unnamed-chunk-3-2.png)

Looking at the seasonal graphs, there is an increase in both starts and
permits throughout the years. There is a pretty consistent pattern/trend
throughout the year for both of the variables.

``` r
housing |>
  mutate(logStarts = log(Starts), logPermits = log(Permits)) |>
  pivot_longer(c(Starts, logStarts, Permits, logPermits)) |>
  ggplot(aes(Month, value)) + geom_line() +
  facet_wrap(~ name, scales = "free_y", ncol = 2) +
  labs(x = NULL, y = NULL, title = "Raw vs log")
```

![](hw4_work_files/figure-commonmark/unnamed-chunk-4-1.png)

Looking at the plots above, the log transformations greatly reduce the
variance in the data, but still keeps an increasing trend. Due to this,
I will use the log transformations of the variables with differencing in
later analyses.

``` r
h0 <- housing |> 
  mutate( lstarts = log(Starts),
          lPerm = log(Permits),
          Mort = Mortgage)
h0 |> CCF(lstarts, lPerm, lag_max = 24) |> autoplot() + labs(title = "CCF: Starts and Permits (levels)")
```

![](hw4_work_files/figure-commonmark/unnamed-chunk-5-1.png)

``` r
h0 |> CCF(lstarts, Mort, lag_max = 36) |> autoplot() + labs(title = "CCF: Starts and Mortgage rate (levels)")
```

![](hw4_work_files/figure-commonmark/unnamed-chunk-5-2.png)

1.  Looking at the raw CCF, it is heavily dominated by trend and
    seasonality. Therefore, I will difference the variables to see how
    that impacts the plots.

    ``` r
    h0 |>
      mutate(dy = difference(difference(lstarts, 12)), dPerm = difference(difference(lPerm, 12)),
             dMort = difference(difference(Mort, 12))) |>
      filter(!is.na(dy), !is.na(dPerm), !is.na(dMort)) |>
      CCF(dy, dPerm, lag_max = 12) |> autoplot() + labs(title = "CCF: Starts and Permits (differenced)")
    ```

    ![](hw4_work_files/figure-commonmark/unnamed-chunk-6-1.png)

    ``` r
    h0 |>
      mutate(dy = difference(difference(lstarts, 12)), dMort = difference(difference(Mort, 12))) |>
      filter(!is.na(dy), !is.na(dMort)) |>
      CCF(dy, dMort, lag_max = 36) |> autoplot() + labs(title = "CCF: Starts and Mortgage (differenced)")
    ```

    ![](hw4_work_files/figure-commonmark/unnamed-chunk-6-2.png)

    Looking at the differenced plots, the log starts and log permits
    have a strong spike at 0 followed by spurious random other peaks
    throughout. Likewise, the log starts and mortgage do not have any
    significant peaks until 2-3 years in time; if we were to use these
    lags, we would erase nearly 2 years of training data, thus hurting
    the model more than helping. Regarding lags, I will test lags 1-3
    for both log permits and mortgage as a general test.

2.  I think that the seasonal ARIMA could work best to model and
    forecast this data. There is an obvious trend in the starts along
    with roughly predictable seasonal increases and decreases. I think
    that the ARIMA could use the previous lag information and the
    covariates to provide a somewhat accurate forecast. The model that I
    think would work the worst would be a NNAR; NNAR models are complex
    with hidden filters and node levels that I do not think are
    necessarily appropriate for this data.

``` r
h  <- h0 |>
  mutate(Perm_l1 = lag(lPerm, 1), Perm_l2 = lag(lPerm, 2), Perm_l3 = lag(lPerm, 3),
         Mort_l1 = lag(Mort, 1), Mort_l2 = lag(Mort, 2), Mort_l3 = lag(Mort, 3)) |>
  filter(!is.na(Mort_l3), !is.na(Perm_l3))
mon <- as.integer(format(as.Date(h$Month), "%m"))
for (m in 2:12) h[[paste0("mon", m)]] <- as.numeric(mon == m)   # monthly dummies (VAR exog)
lag_cols <- c("Perm_l1", "Mort_l1")

val_fit_end <- yearmonth("2018 Dec")
train_end <- yearmonth("2021 Dec")
c(train = sum(h$Month <= train_end), test = sum(h$Month > train_end))
```

    train  test 
      141    57 

``` r
h_train <- h |> filter(Month <= train_end)
h_fit   <- h |> filter(Month <= val_fit_end)  
```

1.  

BASELINE

``` r
base_fable <- fit_fable_models(h, "lstarts", lag_cols, train_end, naive = "SNAIVE",
                               small_p = 2, small_k = 2, small_P = 1)
base_var <- var_onestep(h, c("lstarts", "lPerm", "Mort"), "lstarts", train_end, lag_max = 4)
base_res <- bind_rows(base_fable, base_var)
base_score <- score_table(base_res)
base_score                                                           
```

    # A tibble: 7 × 4
      model       RMSE   MAE `RMSE vs benchmark`
      <chr>      <dbl> <dbl>               <dbl>
    1 ETS        0.075 0.059               0.671
    2 ARIMA      0.085 0.071               0.761
    3 NNAR       0.087 0.072               0.779
    4 NNAR_small 0.087 0.073               0.785
    5 LagReg     0.096 0.078               0.862
    6 VAR(4)     0.1   0.08                0.898
    7 Benchmark  0.111 0.085               1    

``` r
plot_onestep(base_res, show = c("Benchmark", "ARIMA", "NNAR", "LagReg"),
             title = "Housing starts (baseline models)")             
```

![](hw4_work_files/figure-commonmark/unnamed-chunk-8-1.png)

``` r
attr(base_fable, "fits") 
```

    # A mable: 1 x 6
               ETS                     ARIMA              NNAR        NNAR_small
           <model>                   <model>           <model>           <model>
    1 <ETS(M,N,A)> <ARIMA(1,1,1)(0,1,1)[12]> <NNAR(2,1,2)[12]> <NNAR(2,1,2)[12]>
    # ℹ 2 more variables: LagReg <model>, Benchmark <model>

3.  

ETS

``` r
ets_specs <- list(
  auto = ETS(lstarts),
  ANA  = ETS(lstarts ~ error("A") + trend("N")  + season("A")),
  AAA  = ETS(lstarts ~ error("A") + trend("A")  + season("A")),
  AAdA = ETS(lstarts ~ error("A") + trend("Ad") + season("A")),
  MNA  = ETS(lstarts ~ error("M") + trend("N")  + season("A")),
  MNM  = ETS(lstarts ~ error("M") + trend("N")  + season("M")),
  MAdM = ETS(lstarts ~ error("M") + trend("Ad") + season("M")),
  ANN  = ETS(lstarts ~ error("A") + trend("N")  + season("N")))
ets_tab <- h_train |> model(!!!ets_specs) |> glance() |> select(.model, AICc) |> arrange(AICc)
ets_tab                                                              
```

    # A tibble: 8 × 2
      .model  AICc
      <chr>  <dbl>
    1 auto    39.3
    2 MNA     39.3
    3 ANA     39.4
    4 AAA     40.7
    5 AAdA    45.3
    6 MNM     47.4
    7 MAdM    52.4
    8 ANN     98.2

``` r
best_ets <- ets_tab$.model[1]
```

The optimal ETS model chosen by the lowest AICc is the MNA
(multiplicative error, no trend, and additive seasonal). This makes some
sense since there was a slight yearly trend, so the seasonal terms
should sum to around 0 over the course of a year period. Now I will look
at the ETS MNA model performance on the raw vs log scale data.

``` r
one_step_m <- function(mdls) refit(mdls, h_train, reestimate = FALSE) |> augment() |> as_tibble() |>
  filter(Month > val_fit_end) |> select(.model, Month, .fitted)
bind_rows(
  h_fit |> model(ETS_raw = ETS(Starts)) |> one_step_m(),
  h_fit |> model(ETS_log = ets_specs[[best_ets]]) |> one_step_m() |> mutate(.fitted = exp(.fitted))) |>
  left_join(as_tibble(h) |> select(Month, Starts), by = "Month") |>
  group_by(.model) |> summarise(val_RMSE_raw = sqrt(mean((Starts - .fitted)^2)))
```

    # A tibble: 2 × 2
      .model  val_RMSE_raw
      <chr>          <dbl>
    1 ETS_log         12.2
    2 ETS_raw         12.5

The ETS model performed slightly better when using the log scaled data
as opposed to the raw data. This can be explained by the reduction in
variance by the log scale, making different measurements appear less
drastic. Overall, trying different tuning parameters did not change or
improve the automatically selected model.

ARIMA

``` r
h_fit |> gg_tsdisplay(difference(difference(lstarts, 12), 1), plot_type = "partial", lag_max = 36)
```

![](hw4_work_files/figure-commonmark/unnamed-chunk-11-1.png)

``` r
arima_specs <- list(
  default    = ARIMA(lstarts),
  thorough   = ARIMA(lstarts, stepwise = FALSE, approximation = FALSE),
  A1 = ARIMA(lstarts ~ pdq(1,0,1) + PDQ(0,0,1)),
  A2 = ARIMA(lstarts ~ pdq(1,1,2) + PDQ(0,1,1)),
  A3 = ARIMA(lstarts ~ pdq(1,1,2) + PDQ(1,1,0)),
  A4 = ARIMA(lstarts ~ pdq(1,1,1) + PDQ(0,0,1)),
  A5 = ARIMA(lstarts ~ pdq(2,1,1) + PDQ(0,1,1)),
  A6 = ARIMA(lstarts ~ pdq(1,1,1) + PDQ(0,1,1)),
  A7 = ARIMA(lstarts ~ 0 + pdq(1,0,1) + PDQ(0,1,1)),         
  A8 = ARIMA(lstarts ~ 1 + pdq(1,0,1) + PDQ(0,1,1)),
  A9 = ARIMA(lstarts ~ 1 + pdq(1,1,1) + PDQ(0,0,1)))
```

Looking at the differenced residual plots above, there are clear spikes
at lag 1 and 12 of the ACF and 1 and 2 of the PACF.

``` r
h_train |> model(!!!arima_specs) |> glance() |> select(.model, AICc) |> arrange(AICc)
```

    # A tibble: 11 × 2
       .model    AICc
       <chr>    <dbl>
     1 A8       -226.
     2 default  -224.
     3 thorough -224.
     4 A6       -224.
     5 A5       -222.
     6 A2       -222.
     7 A7       -222.
     8 A9       -219.
     9 A4       -213.
    10 A1       -206.
    11 A3       -198.

``` r
arima_val <- val_tab(h_fit |> model(!!!arima_specs))
arima_val
```

    # A tibble: 11 × 2
       model    val_RMSE
       <chr>       <dbl>
     1 A8          0.113
     2 A9          0.113
     3 A4          0.113
     4 A1          0.115
     5 A7          0.115
     6 A6          0.124
     7 A5          0.128
     8 A2          0.135
     9 default     0.143
    10 thorough    0.143
    11 A3          0.158

``` r
best_arima <- arima_val$model[1]
```

Since there is different differencing between the models, I will use the
validation RMSE to assess each model’s performance. The optimal model
with the lowest AICc and RMSE was an ARIMA with drift
(1,0,1)(0,1,1)\[12\]. I am quite surprised that the top ARIMA models do
not have D=d=1; this suggests that I could have overdifferenced the data
and that just one could suffice along with AR and MA components.

NNAR

``` r
nn_grid <- expand_grid(p = c(1, 2, 4, 6), P = c(0, 1, 2), k = c(2, 4, 6))   
nn_grid$val_RMSE <- NA_real_
for (i in seq_len(nrow(nn_grid))) {
  g <- nn_grid[i, ]
  nn_grid$val_RMSE[i] <- tryCatch({
    set.seed(2026)
    d <- h_fit |> model(NNAR = NNETAR(as.formula(paste0("lstarts ~ AR(p = ", g$p, ", P = ", g$P, ")")),
                                      n_nodes = g$k)) |> onestep(h_train, val_fit_end)
    rmse(d)
  }, error = function(e) NA_real_)
}
nn_grid <- arrange(nn_grid, val_RMSE)
nn_grid
```

    # A tibble: 36 × 4
           p     P     k val_RMSE
       <dbl> <dbl> <dbl>    <dbl>
     1     1     0     4    0.142
     2     6     0     2    0.144
     3     1     0     6    0.145
     4     1     2     4    0.145
     5     6     1     2    0.150
     6     4     0     2    0.150
     7     1     0     2    0.152
     8     4     0     4    0.154
     9     2     2     2    0.155
    10     6     0     4    0.156
    # ℹ 26 more rows

``` r
nn_best <- nn_grid[1, ]
nn_form <- as.formula(paste0("lstarts ~ AR(p = ", nn_best$p, ", P = ", nn_best$P, ")"))
```

A grid search of different combinations of p, P, and k resulted in an
NNAR(1, 0, 4)\[12\] having the lowest RMSE. Once again, I am surprised
that there is no seasonal P identified since there appeared to be a
seasonal pattern with the summer high and winter low.

``` r
seed_test <- function(spec_fun, s) {
  set.seed(s)
  rmse(h_train |> model(NNAR = spec_fun()) |> onestep(h, train_end))
}
base_seeds <- sapply(1:8, function(s) seed_test(function() NNETAR(lstarts), s))
sel_seeds  <- sapply(1:8, function(s) seed_test(function() NNETAR(nn_form, n_nodes = nn_best$k), s))
tibble(network = c("baseline NNAR", "selected NNAR"),
       min = c(min(base_seeds), min(sel_seeds)), median = c(median(base_seeds), median(sel_seeds)),
       max = c(max(base_seeds), max(sel_seeds)), sd = c(sd(base_seeds), sd(sel_seeds)))
```

    # A tibble: 2 × 5
      network          min median    max       sd
      <chr>          <dbl>  <dbl>  <dbl>    <dbl>
    1 baseline NNAR 0.0870 0.0880 0.0888 0.000613
    2 selected NNAR 0.0969 0.0978 0.0984 0.000570

``` r
range(nn_grid$val_RMSE, na.rm = TRUE)
```

    [1] 0.1423896 0.2182787

For the seed test, I trained the base and selected NNAR model on all of
the training data and tested it on the testing data (January 2022 on) to
draw a comparison. When varying the seed for the NNAR models, there was
a relatively small spread in the estimated RMSE from 0.0969 to 0.0984.
When changing the parameters (seen in the grid search above), there was
a greater range in the estimated RMSE (0.14 to 0.21). However, the top
five performing NNAR models from the grid search have a small range from
0.1423 to 0.1452 with varying p and k values. Therefore, I think that
the choice of (p, P, k) has a greater impact over the seed and that the
spread of RMSE values has to be attributed to noise.

VAR

``` r
var_sets  <- list(Perm = c("lstarts", "lPerm"), Mort = c("lstarts", "Mortgage"),
                  Both = c("lstarts", "lPerm", "Mortgage"))
exog_sets <- list(none = NULL, monthly = paste0("mon", 2:12))
var_grid <- expand_grid(ic = c("AIC(n)", "HQ(n)", "SC(n)", "FPE(n)"),
                        vset = names(var_sets), eset = names(exog_sets), lag_max = c(4, 8))
var_grid$val_RMSE <- NA_real_
for (i in seq_len(nrow(var_grid))) {
  g <- var_grid[i, ]
  var_grid$val_RMSE[i] <- tryCatch(
    rmse(var_pred(var_sets[[g$vset]], exog_sets[[g$eset]], g$ic, g$lag_max, val_fit_end, train_end)),
    error = function(e) NA_real_)
}
var_grid <- arrange(var_grid, val_RMSE)
head(var_grid, 10)
```

    # A tibble: 10 × 5
       ic     vset  eset  lag_max val_RMSE
       <chr>  <chr> <chr>   <dbl>    <dbl>
     1 AIC(n) Perm  none        4   0.0941
     2 HQ(n)  Perm  none        4   0.0941
     3 HQ(n)  Perm  none        8   0.0941
     4 FPE(n) Perm  none        4   0.0941
     5 FPE(n) Perm  none        8   0.0943
     6 AIC(n) Perm  none        8   0.0977
     7 AIC(n) Both  none        4   0.0979
     8 FPE(n) Both  none        4   0.0979
     9 HQ(n)  Both  none        4   0.100 
    10 HQ(n)  Both  none        8   0.100 

``` r
vb <- var_grid[1, ]
```

After tuning, the top performing VAR models by RMSE had variable sets of
just log(Starts) and log(Permits) as well as no exogenous regressors.
Through this search, the optimal model chosen is a lag max 4 VAR with
the variable sets of log(Starts) and log(Permits).

Lag regression

``` r
lr_forms <- c(P_1 = "lstarts ~ Perm_l1", P_12 = "lstarts ~ Perm_l1 + Perm_l2",
              P_123 = "lstarts ~ Perm_l1 + Perm_l2 + Perm_l3",
              M_1 = "lstarts ~ Mort_l1", M_12 = "lstarts ~ Mort_l1 + Mort_l2",
              M_123 = "lstarts ~ Mort_l1 + Mort_l2 + Mort_l3",
              B_1 = "lstarts ~ Perm_l1 + Mort_l1",
              B_12 = "lstarts ~ Perm_l1 + Perm_l2 + Mort_l1 + Mort_l2",
              B_123 = "lstarts ~ Perm_l1 + Perm_l2 + Perm_l3 + Mort_l1 + Mort_l2 + Mort_l3")

err_forms <- c(                                   # fixed error structures (d, D shown)
  sAR1       = "pdq(0,0,0) + PDQ(1,1,0)",         # d=0, D=1
  sMA1       = "pdq(0,0,0) + PDQ(0,1,1)",         # d=0, D=1
  d1_sMA1    = "pdq(0,1,1) + PDQ(0,1,1)",         # d=1, D=1
  ar1_noseas = "pdq(1,0,0) + PDQ(0,0,0)",         # no differencing
  auto       = "")                                # automatic errors

lr_grid <- expand_grid(reg = names(lr_forms), err = names(err_forms))
lr_grid$name <- paste(lr_grid$reg, lr_grid$err, sep = "__")
lr_specs <- Map(function(r, e) {
  f <- if (nzchar(err_forms[[e]])) paste(lr_forms[[r]], "+", err_forms[[e]]) else lr_forms[[r]]
  ARIMA(as.formula(f))
}, lr_grid$reg, lr_grid$err)
names(lr_specs) <- lr_grid$name

lr_aicc <- h_train |> model(!!!lr_specs) |> glance() |> select(name = .model, AICc)
lr_val  <- val_tab(h_fit |> model(!!!lr_specs)) |> rename(name = model)
lr_tab  <- lr_grid |> left_join(lr_aicc, by = "name") |> left_join(lr_val, by = "name")


lr_tab |> filter(err != "auto") |> group_by(err) |> slice_min(AICc, n = 3)
```

    # A tibble: 12 × 5
    # Groups:   err [4]
       reg   err        name               AICc val_RMSE
       <chr> <chr>      <chr>             <dbl>    <dbl>
     1 P_12  ar1_noseas P_12__ar1_noseas  -238.    0.102
     2 P_123 ar1_noseas P_123__ar1_noseas -236.    0.101
     3 B_12  ar1_noseas B_12__ar1_noseas  -234.    0.101
     4 P_123 d1_sMA1    P_123__d1_sMA1    -246.    0.107
     5 P_12  d1_sMA1    P_12__d1_sMA1     -245.    0.104
     6 B_12  d1_sMA1    B_12__d1_sMA1     -243.    0.106
     7 P_123 sAR1       P_123__sAR1       -240.    0.119
     8 B_123 sAR1       B_123__sAR1       -237.    0.121
     9 P_12  sAR1       P_12__sAR1        -237.    0.117
    10 P_123 sMA1       P_123__sMA1       -257.   NA    
    11 P_12  sMA1       P_12__sMA1        -255.   NA    
    12 B_123 sMA1       B_123__sMA1       -252.   NA    

``` r
lr_tab <- arrange(lr_tab, val_RMSE)
head(lr_tab, 10)
```

    # A tibble: 10 × 5
       reg   err        name               AICc val_RMSE
       <chr> <chr>      <chr>             <dbl>    <dbl>
     1 B_12  ar1_noseas B_12__ar1_noseas  -234.    0.101
     2 P_123 ar1_noseas P_123__ar1_noseas -236.    0.101
     3 P_12  ar1_noseas P_12__ar1_noseas  -238.    0.102
     4 B_1   d1_sMA1    B_1__d1_sMA1      -241.    0.102
     5 P_1   d1_sMA1    P_1__d1_sMA1      -241.    0.102
     6 B_123 ar1_noseas B_123__ar1_noseas -231.    0.103
     7 P_12  d1_sMA1    P_12__d1_sMA1     -245.    0.104
     8 B_1   ar1_noseas B_1__ar1_noseas   -231.    0.104
     9 P_1   ar1_noseas P_1__ar1_noseas   -233.    0.105
    10 B_12  d1_sMA1    B_12__d1_sMA1     -243.    0.106

``` r
lr_tab |> group_by(err) |> summarise(best_val = min(val_RMSE, na.rm = TRUE))   
```

    # A tibble: 5 × 2
      err        best_val
      <chr>         <dbl>
    1 ar1_noseas    0.101
    2 auto          0.106
    3 d1_sMA1       0.102
    4 sAR1          0.114
    5 sMA1          0.165

``` r
lr_tune <- lr_specs[[lr_tab$name[1]]]    
```

Overall, the fixed error structure of AR(1) with no seasonal elements
performed the best, producing the top three lowest RMSE values. The
lowest value was achieved when using both regressors with lags 1 and 2.

D

``` r
run_split <- function(tr_end) {
  tr <- h |> filter(Month <= tr_end)
  set.seed(205734)
  m <- tr |> model(Benchmark   = SNAIVE(lstarts),
                   ETS_base    = ETS(lstarts),         ETS_tune    = ets_specs[[best_ets]],
                   ARIMA_base  = ARIMA(lstarts),       ARIMA_tune  = arima_specs[[best_arima]],
                   NNAR_base   = NNETAR(lstarts),      NNAR_tune   = NNETAR(nn_form, n_nodes = nn_best$k),
                   LagReg_base = ARIMA(lstarts ~ Perm_l1 + Mort_l1),
                   LagReg_tune = lr_tune)
  bind_rows(
    onestep(m, h, tr_end),
    var_pred(c("lstarts", "lPerm", "Mort"), NULL, "AIC(n)", 4, tr_end) |> mutate(model = "VAR_base"),
    var_pred(var_sets[[vb$vset]], exog_sets[[vb$eset]], vb$ic, vb$lag_max, tr_end) |> mutate(model = "VAR_tune"))
}
res_main <- run_split(train_end)
score_table(res_main)                                                
```

    # A tibble: 11 × 4
       model        RMSE   MAE `RMSE vs benchmark`
       <chr>       <dbl> <dbl>               <dbl>
     1 ETS_base    0.075 0.059               0.671
     2 ETS_tune    0.075 0.059               0.671
     3 VAR_tune    0.081 0.065               0.729
     4 ARIMA_base  0.085 0.071               0.761
     5 NNAR_base   0.088 0.073               0.791
     6 LagReg_tune 0.091 0.071               0.814
     7 LagReg_base 0.096 0.078               0.862
     8 NNAR_tune   0.097 0.077               0.873
     9 VAR_base    0.1   0.08                0.898
    10 ARIMA_tune  0.107 0.092               0.959
    11 Benchmark   0.111 0.085               1    

``` r
top3 <- score_table(res_main) |> filter(grepl("_tune", model)) |> slice(1:3) |> pull(model)
plot_onestep(res_main |> filter(model %in% c("Benchmark", top3)), show = c("Benchmark", top3),
             title = "Best tuned models, test period")             
```

![](hw4_work_files/figure-commonmark/unnamed-chunk-17-1.png)

Looking at the results above, the tuned versions of VAR and Lagged
Regression outperformed their baseline counterparts while ARIMA and NNAR
base outperformed their tuned versions (ETS tuned was the same model as
ETS baseline). This is interesting considering that I initially thought
that the ARIMA model would perform best, but ETS and VAR tuned both beat
ARIMA base while ARIMA tuned performed the second worst to Naive.

``` r
res_q1_2019 <- run_split(yearmonth("2019 Dec")); score_table(res_q1_2019)
```

    # A tibble: 11 × 4
       model        RMSE   MAE `RMSE vs benchmark`
       <chr>       <dbl> <dbl>               <dbl>
     1 VAR_tune    0.086 0.07                0.589
     2 ETS_base    0.094 0.071               0.641
     3 ETS_tune    0.094 0.071               0.641
     4 LagReg_tune 0.094 0.072               0.641
     5 LagReg_base 0.103 0.081               0.705
     6 VAR_base    0.105 0.084               0.715
     7 ARIMA_tune  0.106 0.085               0.728
     8 NNAR_base   0.112 0.087               0.762
     9 ARIMA_base  0.121 0.09                0.825
    10 NNAR_tune   0.128 0.103               0.878
    11 Benchmark   0.146 0.107               1    

``` r
res_q1_2023 <- run_split(yearmonth("2023 Dec")); score_table(res_q1_2023)
```

    # A tibble: 11 × 4
       model        RMSE   MAE `RMSE vs benchmark`
       <chr>       <dbl> <dbl>               <dbl>
     1 ETS_base    0.074 0.059               0.986
     2 ETS_tune    0.074 0.059               0.986
     3 Benchmark   0.075 0.057               1    
     4 LagReg_base 0.08  0.068               1.06 
     5 ARIMA_base  0.084 0.068               1.11 
     6 NNAR_base   0.086 0.071               1.14 
     7 LagReg_tune 0.089 0.075               1.19 
     8 VAR_tune    0.09  0.073               1.2  
     9 VAR_base    0.092 0.074               1.22 
    10 ARIMA_tune  0.093 0.08                1.24 
    11 NNAR_tune   0.103 0.081               1.37 

When the same models were re-fit on training data until December 2023
and tested on the remaining data, only ETS outperformed the Benchmark.
This surprised me at first, but then noticed that the Benchmark RMSE
dropped from 0.111 to 0.075 when shifting the training/testing split,
implying that the test window is too small, leading to the nosier model
rankings. Overall, I would say that ETS “won” due to its simplicity and
consistently low RMSE values. I would not say that I trust the ranking
blindly just with how close the top RMSE values are within each model
class as well as against one another. I think that any model can be
chosen to address this problem, but some will give easier
interpretation/implementation than others (like ETS that is simply just
error, trend, and seasonality compared to VAR with multiple variables
and higher lagged regressors).

``` r
q1 <- list(res = res_main, res2019 = res_q1_2019, res2023 = res_q1_2023,
           ets_tab = ets_tab, arima_val = arima_val, nn_grid = nn_grid, var_grid = var_grid,
           lr_tab = lr_tab, base_seeds = base_seeds, sel_seeds = sel_seeds)
```

# Part 2

``` r
onestep <- function(mdls, full, from) {
  refit(mdls, full, reestimate = FALSE) |> augment() |> as_tibble() |>
    filter(Date > from) |>
    transmute(model = .model, date = Date, actual = lPM, pred = .fitted)
}
rmse_tab <- function(d) d |> group_by(model) |>
  summarise(RMSE = sqrt(mean((actual - pred)^2)), MAE = mean(abs(actual - pred))) |>
  arrange(RMSE)

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

beijing |> 
  select(Date, lPM, Temp, Dew, Pres, lWind) |>
  pivot_longer(-Date) |>
  ggplot(aes(Date, value)) + geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(x = NULL, y = NULL)
```

![](hw4_work_files/figure-commonmark/unnamed-chunk-20-1.png)

Definitely a seasonal trend in Dew, Pres, and temp; Dew and Temp have
highs in summer and lows in the winter while Pressure has lows in the
summer and highs in the winter. The Dew predictor is nosier/larger
variance at lower values compare to higher. The Temp predictor has low
variance. The log PM and log Wind variables are appear as nosier, but
that could also be due to the smaller scale.

``` r
#yearly seasonality
beijing |> gg_season(lPM, period = "year") + labs(title = "lPM by day of year")
```

![](hw4_work_files/figure-commonmark/unnamed-chunk-21-1.png)

``` r
#weekly
beijing |> gg_season(lPM, period = "week") +
  labs(title = "lPM by day of week") +
  theme(legend.position = "none")
```

![](hw4_work_files/figure-commonmark/unnamed-chunk-21-2.png)

``` r
beijing |> as_tibble() |> mutate(dow = wday(Date, label = TRUE, week_start = 1)) |>
  group_by(dow) |> summarise(mean_lpm = mean(lPM), .groups = "drop")
```

    # A tibble: 7 × 2
      dow   mean_lpm
      <ord>    <dbl>
    1 Mon       4.23
    2 Tue       4.29
    3 Wed       4.24
    4 Thu       4.26
    5 Fri       4.35
    6 Sat       4.37
    7 Sun       4.33

``` r
beijing |> as_tibble() |> mutate(mon = month(Date, label = TRUE)) |>
  group_by(mon) |> summarise(mean_lpm = mean(lPM), .groups = "drop")
```

    # A tibble: 12 × 2
       mon   mean_lpm
       <ord>    <dbl>
     1 Jan       4.34
     2 Feb       4.50
     3 Mar       4.26
     4 Apr       4.19
     5 May       4.26
     6 Jun       4.39
     7 Jul       4.36
     8 Aug       4.24
     9 Sep       4.13
    10 Oct       4.41
    11 Nov       4.33
    12 Dec       4.16

``` r
beijing |> as_tibble() |>
  mutate(yr = year(Date)) |> 
  group_by(yr) |> summarise(mean_lPM = mean(lPM), sd_lPM = sd(lPM), .groups = "drop")
```

    # A tibble: 5 × 3
         yr mean_lPM sd_lPM
      <dbl>    <dbl>  <dbl>
    1  2010     4.36  0.785
    2  2011     4.28  0.836
    3  2012     4.22  0.840
    4  2013     4.34  0.781
    5  2014     4.27  0.836

``` r
beijing |> ACF(lPM, lag_max = 60) |> autoplot() +labs(title="ACF of lPM for 60 days")
```

![](hw4_work_files/figure-commonmark/unnamed-chunk-21-3.png)

``` r
beijing |> ACF(lPM, lag_max = 400) |> autoplot()+ scale_x_continuous(n.breaks =10)
```

![](hw4_work_files/figure-commonmark/unnamed-chunk-21-4.png)

Looking at the seasonal graphs, there does not appear to be a seasonal
component to the lPM measurements. The average lPM value fluctuates
slightly between days of the week, months, and years, but nothing that
is too noteworthy or deserving of an obvious seasonal component.
Additionally, there are no seasonal spikes present on neither of the ACF
plots above. Therefore, I will use a NAIVE benchmark rather than an
SNAIVE.

``` r
beijing |> 
  select(Date, lPM, Temp, Dew, Pres, lWind) |>
  pivot_longer(c(Temp, Dew, Pres, lWind)) |>
  ggplot(aes(value, lPM)) +
  geom_point(alpha = 0.25, size = 0.8) +
  geom_smooth(method = "loess", formula = y ~ x, colour = "red", se = FALSE) +
  facet_wrap(~ name, scales = "free_x") 
```

![](hw4_work_files/figure-commonmark/unnamed-chunk-22-1.png)

There appears to be a positive (somewhat cubic looking) trend between
Dew and lPM, a negative trend between lWind and lPM and somewhat with
Pres and lPM, and a pretty flat trend between Temp and lPM.

``` r
beijing |> CCF(lPM, Temp, lag_max = 14) |> autoplot() +labs(title = "CCF: lPM vs Temp")
```

![](hw4_work_files/figure-commonmark/unnamed-chunk-23-1.png)

``` r
beijing |> CCF(lPM, Dew, lag_max = 14) |> autoplot() +labs(title = "CCF: lPM vs Dew")
```

![](hw4_work_files/figure-commonmark/unnamed-chunk-23-2.png)

``` r
beijing |> CCF(lPM, Pres, lag_max = 14) |> autoplot() +labs(title = "CCF: lPM vs Pres")
```

![](hw4_work_files/figure-commonmark/unnamed-chunk-23-3.png)

``` r
beijing |> CCF(lPM, lWind, lag_max = 14) |> autoplot() +labs(title = "CCF: lPM vs lWind")
```

![](hw4_work_files/figure-commonmark/unnamed-chunk-23-4.png)

When looking at the CCF plots, I will only consider significant lags
that are positive/to the right of 0. Temp, lWind, and Pres all have a
significant lag at 1 while Dew has two significant lags at 1 and 2. With
this, I will add lagged 1 and 2 versions of the predictors.

``` r
bj <- beijing |>
  mutate(Temp_l1 = lag(Temp, 1), Temp_l2 = lag(Temp, 2),
         lWind_l1 = lag(lWind, 1), lWind_l2 = lag(lWind, 2),
         Pres_l1 = lag(Pres, 1), Pres_l2 = lag(Pres, 2),
         Dew_l1 = lag(Dew, 1), Dew_l2 = lag(Dew, 2),
         dow2 = as.numeric(wday(Date) == 2), dow3 = as.numeric(wday(Date) == 3),
         dow4 = as.numeric(wday(Date) == 4), dow5 = as.numeric(wday(Date) == 5),
         dow6 = as.numeric(wday(Date) == 6), dow7 = as.numeric(wday(Date) == 7),
         sin1 = sin(2 * pi * as.numeric(Date) / 365.25), cos1 = cos(2 * pi * as.numeric(Date) / 365.25),
         sin2 = sin(4 * pi * as.numeric(Date) / 365.25), cos2 = cos(4 * pi * as.numeric(Date) / 365.25)) |>
  filter(!is.na(Dew_l2))                       # drop first rows with NA lags

train_end <- as.Date("2013-12-31")
val_end   <- as.Date("2012-12-31")             # validation block = 2013
bj_train  <- bj |> filter(Date <= train_end)
bj_fit    <- bj |> filter(Date <= val_end)
c(train = nrow(bj_train), test = sum(bj$Date > train_end))
```

    train  test 
     1458   365 

3.  I think that a VAR or NNAR model will perform best due to the
    nonseasonal nature of the lPM values. I think that including the
    regressors . I think that an ETS model will perform the worst due to
    the unpredictability of the data and that there is no real trend to
    observe in the values.

    ``` r
    bj_fable <- fit_fable_models(bj, "lPM", c("Temp_l1", "lWind_l1", "Pres_l1", "Dew_l1", "Dew_l2"),
                                 train_end, naive = "NAIVE", small_p = 7, small_k = 3, small_P = 1)
    bj_var <- var_onestep(bj, c("lPM", "Temp", "Pres", "Dew", "lWind"), "lPM", train_end, lag_max = 8)
    bj_res <- bind_rows(bj_fable, bj_var)
    score_table(bj_res)                                                   # 2B table
    ```

        # A tibble: 7 × 4
          model       RMSE   MAE `RMSE vs benchmark`
          <chr>      <dbl> <dbl>               <dbl>
        1 LagReg     0.697 0.542               0.819
        2 VAR(5)     0.697 0.545               0.819
        3 NNAR_small 0.709 0.552               0.833
        4 ARIMA      0.718 0.555               0.844
        5 NNAR       0.752 0.584               0.884
        6 ETS        0.842 0.646               0.989
        7 Benchmark  0.851 0.65                1    

    ``` r
    plot_onestep(bj_res |> filter(date >= as.Date("2014-01-01"), date <= as.Date("2014-03-31")),
                 title = "Beijing, 2014 heating season")                  # 2B figure
    ```

    ![](hw4_work_files/figure-commonmark/unnamed-chunk-25-1.png)

    ``` r
    attr(bj_fable, "fits")  
    ```

        # A mable: 1 x 6
                   ETS                  ARIMA               NNAR       NNAR_small
               <model>                <model>            <model>          <model>
        1 <ETS(A,N,N)> <ARIMA(1,0,1) w/ mean> <NNAR(29,1,15)[7]> <NNAR(7,1,3)[7]>
        # ℹ 2 more variables: LagReg <model>, Benchmark <model>

    By default, a NNAR(29,1,15)\[7\] was chosen, which is quite a large
    number of lags to be considering in making a forecast. Also, an ETS
    model with only an additive error impact was chosen, which only
    slightly outperformed benchmark.

``` r
ets_specs <- list(
  auto = ETS(lPM),
  ANN  = ETS(lPM ~ error("A") + trend("N")  + season("N")),
  AAN  = ETS(lPM ~ error("A") + trend("A")  + season("N")),
  AAdN = ETS(lPM ~ error("A") + trend("Ad") + season("N")),
  ANA  = ETS(lPM ~ error("A") + trend("N")  + season("A", period = 7)),
  AAdA = ETS(lPM ~ error("A") + trend("Ad") + season("A", period = 7)))
ets_tab <- bj_train |> model(!!!ets_specs) |> glance() |> select(.model, AICc) |> arrange(AICc)
ets_tab                                                               # choose by AICc (training only)
```

    # A tibble: 6 × 2
      .model  AICc
      <chr>  <dbl>
    1 auto   9929.
    2 ANN    9929.
    3 AAN    9934.
    4 AAdN   9936.
    5 ANA    9937.
    6 AAdA   9953.

``` r
best_ets <- ets_tab$.model[1]


four_tab <- bj_train |> model(
  K0 = TSLM(lPM ~ 1),
  K1 = TSLM(lPM ~ fourier(period = "year", K = 1)), K2 = TSLM(lPM ~ fourier(period = "year", K = 2)),
  K3 = TSLM(lPM ~ fourier(period = "year", K = 3)), K4 = TSLM(lPM ~ fourier(period = "year", K = 4)),
  K6 = TSLM(lPM ~ fourier(period = "year", K = 6)), K8 = TSLM(lPM ~ fourier(period = "year", K = 8)),
  K10 = TSLM(lPM ~ fourier(period = "year", K = 10))) |>
  glance() |> select(.model, AICc) |> arrange(AICc)
four_tab
```

    # A tibble: 8 × 2
      .model  AICc
      <chr>  <dbl>
    1 K4     -606.
    2 K3     -606.
    3 K10    -603.
    4 K0     -602.
    5 K8     -601.
    6 K1     -600.
    7 K6     -599.
    8 K2     -599.

``` r
bestK <- readr::parse_number(four_tab$.model[1])
```

The best ETS model reported is the same as the automated one of only
having an additive error term. When looking at AICc upon adding in a
fourier yearly term, a k of 4 was determined as optimal.

``` r
fourier_ets <- function(fit_data, full, K, from) {
  f <- fit_data |> model(TSLM(as.formula(paste0("lPM ~ fourier(period = 'year', K = ", K, ")"))))
  four <- refit(f, full, reestimate = FALSE) |> augment() |> as_tibble() |> select(Date, four = .fitted)
  d <- full |> as_tibble() |> select(Date, lPM) |> left_join(four, by = "Date") |>
    mutate(r = lPM - four) |> as_tsibble(index = Date)
  m <- d |> filter(Date <= max(fit_data$Date)) |> model(ETS(r))
  refit(m, d, reestimate = FALSE) |> augment() |> as_tibble() |> filter(Date > from) |>
    left_join(select(as_tibble(d), Date, lPM, four), by = "Date") |>
    transmute(model = "ETS_fourier", date = Date, actual = lPM, pred = four + .fitted)
}
# validation comparison: best plain ETS vs Fourier+ETS
ets_val2 <- bind_rows(
  bj_fit |> model(ETS = ets_specs[[best_ets]]) |> onestep(bj_train, val_end),
  fourier_ets(bj_fit, bj_train, bestK, val_end)) |> rmse_tab()
ets_val2
```

    # A tibble: 2 × 3
      model        RMSE   MAE
      <chr>       <dbl> <dbl>
    1 ETS         0.775 0.609
    2 ETS_fourier 0.775 0.609

``` r
ets_pick2 <- ets_val2$model[1] 
```

The raw ETS model and the ETS model with fourier terms had approximately
the same RMSE. This tells me that adding in all of these fancy fourier
terms did not improve the model the ETS model when the data did not have
an explicit trend/seasonal component.

ARIMA

``` r
dhr_tab <- bj_train |> model(
  K1 = ARIMA(lPM ~ fourier(period = "year", K = 1) + PDQ(0, 0, 0)),
  K2 = ARIMA(lPM ~ fourier(period = "year", K = 2) + PDQ(0, 0, 0)),
  K3 = ARIMA(lPM ~ fourier(period = "year", K = 3) + PDQ(0, 0, 0)),
  K4 = ARIMA(lPM ~ fourier(period = "year", K = 4) + PDQ(0, 0, 0)),
  K6 = ARIMA(lPM ~ fourier(period = "year", K = 6) + PDQ(0, 0, 0)),
  K8 = ARIMA(lPM ~ fourier(period = "year", K = 8) + PDQ(0, 0, 0))) |>
  glance() |> select(.model, AICc) |> arrange(AICc)
dhr_tab                                                               # K by AICc
```

    # A tibble: 6 × 2
      .model  AICc
      <chr>  <dbl>
    1 K1     3079.
    2 K3     3081.
    3 K2     3082.
    4 K4     3084.
    5 K6     3091.
    6 K8     3097.

``` r
bestKa <- readr::parse_number(dhr_tab$.model[1])

arima_cands <- list(
  default     = ARIMA(lPM),
  no_seasonal = ARIMA(lPM ~ PDQ(0, 0, 0)),
  wk_seasAR   = ARIMA(lPM ~ PDQ(1, 0, 0)),
  wk_fourier3 = ARIMA(lPM ~ fourier(period = "week", K = 3) + PDQ(0, 0, 0)),
  DHR_year    = ARIMA(as.formula(paste0("lPM ~ fourier(period = 'year', K = ", bestKa, ") + PDQ(0,0,0)"))),
  DHR_year_wk = ARIMA(as.formula(paste0("lPM ~ fourier(period = 'year', K = ", bestKa,
                                        ") + fourier(period = 'week', K = 3) + PDQ(0,0,0)"))))
arima_val <- bj_fit |> model(!!!arima_cands) |> onestep(bj_train, val_end) |> rmse_tab()
arima_val                                                            
```

    # A tibble: 6 × 3
      model        RMSE   MAE
      <chr>       <dbl> <dbl>
    1 wk_fourier3 0.683 0.544
    2 wk_seasAR   0.685 0.544
    3 default     0.685 0.544
    4 no_seasonal 0.685 0.544
    5 DHR_year_wk 0.688 0.547
    6 DHR_year    0.690 0.547

``` r
best_arima <- arima_val$model[1]
```

When looking at all possible additional components to the ARIMA model,
the top performing RMSE model was an ARIMA with a k = 3 weekly fourier
component and no seasonality. This is a bit surprising since I was not
expecting a seasonal or even weekly pattern to the data.

NNAR

``` r
weather_terms <- "Temp_l1 + Dew_l1 + Pres_l1 + lWind_l1"
nn_form_of <- function(p, P, X)
  as.formula(paste0("lPM ~ AR(p = ", p, ", P = ", P, ")", if (X) paste0(" + ", weather_terms) else ""))

nn_grid <- expand_grid(p = c(1, 7, 14), P = c(0, 1), k = c(3, 5, 7), X = c(FALSE, TRUE))  # 36 settings
nn_grid$val_RMSE <- NA_real_
for (i in seq_len(nrow(nn_grid))) {
  g <- nn_grid[i, ]
  nn_grid$val_RMSE[i] <- tryCatch({
    set.seed(2026)
    d <- bj_fit |> model(NNAR = NNETAR(nn_form_of(g$p, g$P, g$X), n_nodes = g$k)) |>
      onestep(bj_train, val_end)
    sqrt(mean((d$actual - d$pred)^2))
  }, error = function(e) NA_real_)
}
nn_grid <- arrange(nn_grid, val_RMSE)
nn_grid
```

    # A tibble: 36 × 5
           p     P     k X     val_RMSE
       <dbl> <dbl> <dbl> <lgl>    <dbl>
     1     7     0     7 TRUE     0.626
     2     7     1     7 TRUE     0.626
     3     7     0     5 TRUE     0.628
     4     7     1     5 TRUE     0.628
     5     1     0     5 TRUE     0.639
     6     7     0     3 TRUE     0.640
     7     7     1     3 TRUE     0.640
     8    14     0     5 TRUE     0.641
     9    14     1     5 TRUE     0.641
    10    14     0     3 TRUE     0.646
    # ℹ 26 more rows

``` r
nn_best <- nn_grid[1, ]
nn_form <- nn_form_of(nn_best$p, nn_best$P, nn_best$X)


seed_rmse <- function(spec_fun, seed) {
  set.seed(seed)
  d <- bj_train |> model(NNAR = spec_fun()) |> onestep(bj, train_end)
  sqrt(mean((d$actual - d$pred)^2))
}
base_seeds <- sapply(1:8, function(s) seed_rmse(function() NNETAR(lPM), s))
sel_seeds  <- sapply(1:8, function(s) seed_rmse(function() NNETAR(nn_form, n_nodes = nn_best$k), s))
tibble(network = c("baseline NNAR", "selected NNAR"),
       min = c(min(base_seeds), min(sel_seeds)), median = c(median(base_seeds), median(sel_seeds)),
       max = c(max(base_seeds), max(sel_seeds)), sd = c(sd(base_seeds), sd(sel_seeds)))
```

    # A tibble: 2 × 5
      network         min median   max      sd
      <chr>         <dbl>  <dbl> <dbl>   <dbl>
    1 baseline NNAR 0.755  0.759 0.769 0.00454
    2 selected NNAR 0.692  0.697 0.698 0.00265

As presented in the top 4 NNAR models, the seasonal component of the
model (P=0 or P=1) does not change the RMSE when everything else about
the model is kept constant. This makes sense since when a model already
includes lag 7 in the p term, then a weekly seasonal component is
unwarranted. This suggests that no additional seasonal lag needs to be
added by the model. The optimal NNAR chosen was NNAR(7,0,7), which is
much lower in terms of regression lags and K values than the baseline
NNAR. Throughout different seeds, the RMSE of the selected model only
fluctuated by 0.007 while there is a much larger spread in RMSE over
different (p, P, k).

VAR

``` r
var_pred <- function(vars, exog, ic, lag_max, fit_end, last_date) {
  d <- as_tibble(bj |> filter(Date <= last_date))
  Y <- as.matrix(d[, vars]); X <- if (is.null(exog)) NULL else as.matrix(d[, exog])
  n_fit <- sum(d$Date <= fit_end)
  Ytr <- Y[1:n_fit, , drop = FALSE]
  Xtr <- if (is.null(X)) NULL else X[1:n_fit, , drop = FALSE]
  p  <- unname(VARselect(Ytr, lag.max = lag_max, type = "const", exogen = Xtr)$selection[ic])
  eq <- Bcoef(VAR(Ytr, p = p, type = "const", exogen = Xtr))["lPM", ]
  idx <- (n_fit + 1):nrow(Y)
  pred <- sapply(idx, function(t) sum(sapply(names(eq), function(nm) {
    if (grepl("\\.l[0-9]+$", nm)) eq[nm] * Y[t - as.integer(sub(".*\\.l", "", nm)), sub("\\.l[0-9]+$", "", nm)]
    else if (nm == "const") eq[nm] else eq[nm] * X[t, nm]
  })))
  tibble(model = "VAR", date = d$Date[idx], actual = Y[idx, "lPM"], pred = unname(pred))
}
var_sets  <- list(wind = c("lPM", "lWind"), wind_dew = c("lPM", "lWind", "Dew"),
                  wind_dew_temp = c("lPM", "lWind", "Dew", "Temp"),
                  all = c("lPM", "lWind", "Dew", "Temp", "Pres"))
exog_sets <- list(none = NULL, dow = paste0("dow", 2:7), fourier = c("sin1", "cos1", "sin2", "cos2"),
                  both = c(paste0("dow", 2:7), "sin1", "cos1", "sin2", "cos2"))
var_grid <- expand_grid(vset = names(var_sets), ic = c("AIC(n)", "HQ(n)", "SC(n)", "FPE(n)"),
                        lag_max = c(7, 14), eset = names(exog_sets))
var_grid$val_RMSE <- NA_real_
for (i in seq_len(nrow(var_grid))) {
  g <- var_grid[i, ]
  var_grid$val_RMSE[i] <- tryCatch({
    d <- var_pred(var_sets[[g$vset]], exog_sets[[g$eset]], g$ic, g$lag_max, val_end, train_end)
    sqrt(mean((d$actual - d$pred)^2))
  }, error = function(e) NA_real_)
}
var_grid <- arrange(var_grid, val_RMSE)
var_grid                                                            
```

    # A tibble: 128 × 5
       vset  ic     lag_max eset    val_RMSE
       <chr> <chr>    <dbl> <chr>      <dbl>
     1 all   AIC(n)       7 both       0.640
     2 all   AIC(n)      14 both       0.640
     3 all   FPE(n)       7 both       0.640
     4 all   FPE(n)      14 both       0.640
     5 all   AIC(n)       7 fourier    0.641
     6 all   AIC(n)      14 fourier    0.641
     7 all   FPE(n)       7 fourier    0.641
     8 all   FPE(n)      14 fourier    0.641
     9 all   HQ(n)        7 both       0.641
    10 all   HQ(n)       14 both       0.641
    # ℹ 118 more rows

``` r
var_best <- var_grid[1, ]
```

Looking at the RMSE from the validation set (2013), the optimal model
chosen was a VAR lag max of 7 that contained all variable regressors
(lPM, lWind, Dew, Temp, and Pres) and day of week and fourier terms for
exogenous sets. I find it interesting that the VAR model with only
fourier terms performed better than the model with only a day of the
week impact. This suggests to me that there is more of a yearly seasonal
pattern than I had originally thought. In my opinion, this model appears
on the surface to be quite complex and using a ton of outside factors to
forecast the lPM.

Lag Regression

``` r
lr_cands <- list(
  wind1    = ARIMA(lPM ~ lWind_l1 + pdq(1, 0, 0) + PDQ(0, 0, 0)),
  dew_wind = ARIMA(lPM ~ Dew_l1 + lWind_l1 + pdq(1, 0, 0) + PDQ(0, 0, 0)),
  all_l1   = ARIMA(lPM ~ Temp_l1 + Dew_l1 + Pres_l1 + lWind_l1 + pdq(1, 0, 0) + PDQ(0, 0, 0)),
  all_l12  = ARIMA(lPM ~ Temp_l1 + Dew_l1 + Pres_l1 + lWind_l1 + Temp_l2 + Dew_l2 + Pres_l2 + lWind_l2 +
                     pdq(1, 0, 0) + PDQ(0, 0, 0)),
  nonlin   = ARIMA(lPM ~ Temp_l1 + Dew_l1 + Pres_l1 + lWind_l1 + I(Temp_l1^2) + I(lWind_l1^2) +
                     I(Dew_l1 * lWind_l1) + pdq(1, 0, 0) + PDQ(0, 0, 0)),
  fourier  = ARIMA(lPM ~ Temp_l1 + Dew_l1 + Pres_l1 + lWind_l1 + fourier(period = "year", K = 3) +
                     pdq(1, 0, 0) + PDQ(0, 0, 0)),
  nonlin_fourier = ARIMA(lPM ~ Temp_l1 + Dew_l1 + Pres_l1 + lWind_l1 + I(Temp_l1^2) + I(lWind_l1^2) +
                           I(Dew_l1 * lWind_l1) + fourier(period = "year", K = 3) + pdq(1, 0, 0) + PDQ(0, 0, 0)),
  all_l12_auto  = ARIMA(lPM ~ Temp_l1 + Dew_l1 + Pres_l1 + lWind_l1 + Temp_l2 + Dew_l2 + Pres_l2 + lWind_l2))
lr_tab <- bj_train |> model(!!!lr_cands) |> glance() |> select(.model, AICc) |> arrange(AICc)
lr_tab        # same response + same AR(1) errors => AICc comparable
```

    # A tibble: 8 × 2
      .model          AICc
      <chr>          <dbl>
    1 all_l12_auto   2966.
    2 all_l12        2967.
    3 nonlin_fourier 2970.
    4 nonlin         2977.
    5 fourier        2979.
    6 all_l1         2987.
    7 wind1          3032.
    8 dew_wind       3034.

``` r
best_lr <- lr_tab$.model[1]
```

Since all of the models contain no differencing (the one auto model gave
a pdq(1, 0, 1)), I can compare the AICc directly across the models to
assess their performance. The lowest AICc was achieved by a lagged
regression model that contained lags 1 and 2 from each of the
predictors. This did not surprise me too much since the VAR model also
utilized multiple lagging weather terms rather than just the first.

Testing

``` r
run_split <- function(tr_end) {
  tr <- bj |> filter(Date <= tr_end)
  set.seed(2026)
  m <- tr |> model(Benchmark = NAIVE(lPM),
                   ETS    = ets_specs[[best_ets]],
                   ARIMA  = arima_cands[[best_arima]],
                   NNAR   = NNETAR(nn_form, n_nodes = nn_best$k),
                   LagReg = lr_cands[[best_lr]])
  bind_rows(
    onestep(m, bj, tr_end),
    fourier_ets(tr, bj, bestK, tr_end),
    var_pred(var_sets[[var_best$vset]], exog_sets[[var_best$eset]], var_best$ic, var_best$lag_max,
             tr_end, max(bj$Date)))
}

res1 <- run_split(train_end)
tuned_vs_base <- bind_rows(res1, bj_res |> filter(model != "Benchmark") |> mutate(model = paste0(model, "_base")))
score_table(tuned_vs_base)                  # 2D table: tuned classes + baselines + benchmark
```

    # A tibble: 13 × 4
       model            RMSE   MAE `RMSE vs benchmark`
       <chr>           <dbl> <dbl>               <dbl>
     1 VAR             0.696 0.55                0.817
     2 LagReg_base     0.697 0.542               0.819
     3 VAR(5)_base     0.697 0.545               0.819
     4 LagReg          0.698 0.543               0.819
     5 NNAR            0.701 0.548               0.823
     6 NNAR_small_base 0.709 0.552               0.833
     7 ARIMA           0.717 0.555               0.842
     8 ARIMA_base      0.718 0.555               0.844
     9 NNAR_base       0.752 0.584               0.884
    10 ETS             0.842 0.646               0.989
    11 ETS_base        0.842 0.646               0.989
    12 ETS_fourier     0.842 0.646               0.989
    13 Benchmark       0.851 0.65                1    

``` r
plot_onestep(res1 |> filter(model %in% c("Benchmark", "NNAR", "LagReg", "VAR"),
                            date >= as.Date("2014-01-01"), date <= as.Date("2014-03-31")),
             title = "Tuned models, early 2014")
```

![](hw4_work_files/figure-commonmark/unnamed-chunk-32-1.png)

``` r
res2 <- run_split(as.Date("2013-06-30"))
score_table(res2)
```

    # A tibble: 7 × 4
      model        RMSE   MAE `RMSE vs benchmark`
      <chr>       <dbl> <dbl>               <dbl>
    1 VAR         0.673 0.53                0.817
    2 NNAR        0.674 0.521               0.818
    3 LagReg      0.678 0.531               0.823
    4 ARIMA       0.691 0.537               0.839
    5 ETS         0.818 0.635               0.993
    6 ETS_fourier 0.818 0.635               0.993
    7 Benchmark   0.824 0.638               1    

Not too surprisingly, the tuned VAR model performed the best with the
lowest RMSE, closely followed by lagged regression. Tuning helped to
improve the RMSE from baseline in the VAR, NNAR, and ARIMA models. The
ETS model performed the worst, barely beating the benchmark. Once again,
this makes sense since there was no obvious trend present in the data,
so the ETS model struggles to predict the high variability.

``` r
hard_days <- function(d) {
  cut <- quantile(d$actual[d$model == "Benchmark"], 0.9)
  d |> mutate(day = ifelse(actual >= cut, "high (top 10%)", "other")) |>
    group_by(model, day) |>
    summarise(RMSE = sqrt(mean((actual - pred)^2)), MAE = mean(abs(actual - pred)), .groups = "drop") |>
    arrange(day, RMSE)
}
hard_days(res1)
```

    # A tibble: 14 × 4
       model       day             RMSE   MAE
       <chr>       <chr>          <dbl> <dbl>
     1 Benchmark   high (top 10%) 0.632 0.477
     2 ETS         high (top 10%) 0.795 0.663
     3 ETS_fourier high (top 10%) 0.796 0.664
     4 LagReg      high (top 10%) 0.799 0.746
     5 NNAR        high (top 10%) 0.808 0.727
     6 VAR         high (top 10%) 0.827 0.775
     7 ARIMA       high (top 10%) 0.909 0.865
     8 VAR         other          0.679 0.524
     9 LagReg      other          0.685 0.520
    10 NNAR        other          0.688 0.528
    11 ARIMA       other          0.691 0.520
    12 ETS_fourier other          0.847 0.644
    13 ETS         other          0.847 0.644
    14 Benchmark   other          0.873 0.670

When looking at the high pollution days (just the top 10%), all of the
tuned models performed worse (higher RMSE) than the Benchmark. Given
that high pollution days tend to be multi-day on length, the lPM values
will be consistently large with small variation when compared to the
previous day. This makes a benchmark model of just using the previous
value’s lPM work well because the values are near one another. I think
that it is interesting that lagged regression and VAR performed poorly
because I would have assumed that there would have been some sort of
lead up indication/pattern with the regressor values to signify an
upcoming large lPM.

``` r
q2 <- list(res = res1, tvb = tuned_vs_base, res2 = res2, ets_tab = ets_tab, ets_pick = ets_pick2,
           arima_val = arima_val, nn_grid = nn_grid, var_grid = var_grid,
           base_seeds = base_seeds, 
           sel_seeds = sel_seeds)
```

``` r
cls <- c("ETS", "ARIMA", "NNAR", "LagReg", "VAR")
rel1 <- score_table(q1$res) |>
  filter(model %in% c("Benchmark", paste0(cls, "_tune"))) |>
  transmute(class = sub("_tune$", "", model), Housing = `RMSE vs benchmark`)
rel2 <- score_table(q2$res) |>
  filter(model %in% c("Benchmark", "ARIMA", "NNAR", "LagReg", "VAR", q2$ets_pick)) |>
  transmute(class = ifelse(model == q2$ets_pick, "ETS", model), Beijing = `RMSE vs benchmark`)

fmt <- function(x, cls) {                      # bold the best (lowest) non-benchmark entry
  b <- min(x[cls != "Benchmark"], na.rm = TRUE)
  ifelse(cls != "Benchmark" & x == b, paste0("**", x, "**"), as.character(x))
}
full_join(rel1, rel2, by = "class") |>
  arrange(class == "Benchmark", class) |>
  mutate(Housing = fmt(Housing, class), Beijing = fmt(Beijing, class)) |>
  knitr::kable(align = "lrr", caption = "test RMSE / benchmark RMSE")
```

| class     |   Housing |   Beijing |
|:----------|----------:|----------:|
| ARIMA     |     0.959 |     0.842 |
| ETS       | **0.671** |     0.989 |
| LagReg    |     0.814 |     0.819 |
| NNAR      |     0.873 |     0.823 |
| VAR       |     0.729 | **0.817** |
| Benchmark |         1 |         1 |

test RMSE / benchmark RMSE

2.) Housing - I had originally predicted an ARIMA model to perform the
best and an NNAR to perform the worst. Looking at the test results,
these two models actually performed the worst out of the tuned models.
The housing data itself is of a smaller size, so it will favor simpler
models over the more complex ones. It seems to have an increasing trend
and cyclic structure to it. When outside regressors (permits and
mortgage) were added to the model (as in Lagged Regression and VAR),
they did not add much of any benefit to forecasting after taking into
account the seasonality.

Beijing - I had originally predicted NNAR to win and ETS to lose. The
test results showed the lowest RMSE with a VAR model and highest with
ETS. This data set is much larger in size, thus giving the more complex
models a chance to detect hidden patterns. There did not appear to be a
seasonal or true trend component to the lPM values when plotted, making
it difficult for the ETS model to predict. When lagged regressors were
available in tuning, the models utilized them. When looking at the RMSE
on high pollution days, the optimal model was the NAIVE benchmark,
indicating that high pollution days are usually a few days long and not
just a drastic increase.

3.)

``` r
s1  <- score_table(q1$res)
s2b <- score_table(q2$tvb)
gain <- function(s, tune, base) round(100 * (s$RMSE[s$model == tune] / s$RMSE[s$model == base] - 1), 1)
var_base2 <- grep("^VAR\\(", s2b$model, value = TRUE)
tibble(class = cls,
       Housing = c(gain(s1, "ETS_tune", "ETS_base"), gain(s1, "ARIMA_tune", "ARIMA_base"),
                   gain(s1, "NNAR_tune", "NNAR_base"), gain(s1, "LagReg_tune", "LagReg_base"),
                   gain(s1, "VAR_tune", "VAR_base")),
       Beijing = c(gain(s2b, q2$ets_pick, "ETS_base"), gain(s2b, "ARIMA", "ARIMA_base"),
                   gain(s2b, "NNAR", "NNAR_base"), gain(s2b, "LagReg", "LagReg_base"),
                   gain(s2b, "VAR", var_base2))) |>
  knitr::kable(caption = "Part 3.3a: % change in test RMSE from tuning (negative = better)")
```

| class  | Housing | Beijing |
|:-------|--------:|--------:|
| ETS    |     0.0 |     0.0 |
| ARIMA  |    25.9 |    -0.1 |
| NNAR   |    10.2 |    -6.8 |
| LagReg |    -5.2 |     0.1 |
| VAR    |   -19.0 |    -0.1 |

Part 3.3a: % change in test RMSE from tuning (negative = better)

``` r
tibble(data = c("Housing", "Beijing"),
       seed_sd = c(sd(q1$sel_seeds), sd(q2$sel_seeds)),
       seed_range = c(diff(range(q1$sel_seeds)), diff(range(q2$sel_seeds)))) |>
  mutate(across(where(is.numeric), ~ round(.x, 4)))
```

    # A tibble: 2 × 3
      data    seed_sd seed_range
      <chr>     <dbl>      <dbl>
    1 Housing  0.0006     0.0015
    2 Beijing  0.0026     0.0062

``` r
spread <- function(x, label) {
  x <- x[is.finite(x)]
  tibble(choice = label, best = min(x), worst = max(x), `worst vs best (%)` = 100 * (max(x) / min(x) - 1))
}
bind_rows(
  spread(q1$arima_val$val_RMSE, "Housing: ARIMA specification"),
  spread(q1$nn_grid$val_RMSE,   "Housing: NNAR (p, P, k)"),
  spread(q1$var_grid$val_RMSE,  "Housing: VAR (ic, vars, dummies, lag_max)"),
  spread(q1$lr_tab$val_RMSE,    "Housing: LagReg (regressors x errors)"),
  spread(q2$arima_val$RMSE,     "Beijing: ARIMA seasonal structure"),
  spread(q2$nn_grid$val_RMSE,   "Beijing: NNAR (p, P, k, weather)"),
  spread(q2$var_grid$val_RMSE,  "Beijing: VAR (ic, vars, lag_max, exog)")) |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  knitr::kable(caption = "Validation RMSE spread in tuning models")
```

| choice                                    |  best | worst | worst vs best (%) |
|:------------------------------------------|------:|------:|------------------:|
| Housing: ARIMA specification              | 0.113 | 0.158 |            40.197 |
| Housing: NNAR (p, P, k)                   | 0.142 | 0.218 |            53.297 |
| Housing: VAR (ic, vars, dummies, lag_max) | 0.094 | 0.141 |            50.213 |
| Housing: LagReg (regressors x errors)     | 0.101 | 0.171 |            69.261 |
| Beijing: ARIMA seasonal structure         | 0.683 | 0.690 |             0.919 |
| Beijing: NNAR (p, P, k, weather)          | 0.626 | 0.705 |            12.498 |
| Beijing: VAR (ic, vars, lag_max, exog)    | 0.640 | 0.672 |             4.941 |

Validation RMSE spread in tuning models

3.) Housing - The choices made in VAR mattered the most in decreasing
the RMSE. The optimal VAR model only took into account the target and
permit variables and did not handle any seasonality. I am surprised that
the tuned ARIMA model did much worse than the baseline considering that
I had originally believed that it had the best potential for optimal
performance.

Beijing - The choices made in the NNAR mattered the most in decreasing
the RMSE. The baseline NNAR selected included a massive number of lags
and nodes, which can over complicate and over fit the data. When the
NNAR parameters were reduced and weather predictors were added, the NNAR
model performed better. It is interesting how tuning did not really help
any of the other models on this data set.

4.) If someone were to give me a new monthly series of 150 observations,
I would try a seasonal ARIMA. I would start by looking at the raw ACF
and PACF to identify pdq PDQ values as well as testing other possible
options along with an auto model. If there are covariates available that
can add additional information beyond a seasonal pattern, then a lagged
regression with ARIMA errors could work better. I would also try an ETS
model since it can perform moderately well on smaller data sets
considering its simplicity.

If someone were to give me a new daily series of 2000 observations, I
would try an NNAR first since it can pick up on an underlying nonlinear
trend that may not be super obvious. I would also try an ARIMA with a
day of the week effect or fourier terms if there is a yearly cycle to
the data. Another model that I would throw in would be a VAR considering
how well it did with the Beijing data set. However, I would keep into
consideration a specified lag order max to help reduce the memory
storage.
