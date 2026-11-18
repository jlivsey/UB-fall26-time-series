``` r
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

# Part 1

## Part 1.1

Here, we will plot the variables:

``` r
ggplot(bike, aes(x = Date, y = Rentals)) +
  geom_line(color = "black", linewidth = 0.8) +
  geom_smooth(method = "lm", color = "red")
#> `geom_smooth()` using formula = 'y ~ x'
```

![](https://i.imgur.com/IxBFgiK.png)<!-- -->

``` r
  labs(x = "Date", y = "Rentals") +
  theme_minimal()
#> NULL

bike |> pivot_longer(-c(Date, Holiday, Weather, Workday)) |>
  ggplot(aes(Date, value)) + geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(x = NULL, y = "Value")
```

![](https://i.imgur.com/DKldahe.png)<!-- -->

``` r

bike |> pivot_longer(c(Rentals, Weather)) |>
  ggplot(aes(Date, value)) + geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(x = NULL, y = "Value")
```

![](https://i.imgur.com/97CH9Ts.png)<!-- -->

``` r

bike |> pivot_longer(c(Rentals, Holiday)) |>
  ggplot(aes(Date, value)) + geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(x = NULL, y = "Value")
```

![](https://i.imgur.com/Dbrv2iR.png)<!-- -->

``` r

bike |> pivot_longer(c(Rentals, Workday)) |>
  ggplot(aes(Date, value)) + geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(x = NULL, y = "Value")
```

![](https://i.imgur.com/VoM8VG3.png)<!-- -->

``` r

bike |>
  group_by(Holiday) |>
  as_tibble() |>
  summarize(avg_rentals = mean(Rentals))
#> # A tibble: 2 × 2
#>   Holiday avg_rentals
#>     <dbl>       <dbl>
#> 1       0       4527.
#> 2       1       3735

bike |>
  ggplot(aes(x=Temp, y=Rentals)) +
   geom_point(color = "black", size = 2) + 
  labs(title = "Rentals vs Temperature",
       x = "Temperature (C)",
       y = "Bike Rentals") +
  theme_minimal()
```

![](https://i.imgur.com/nSUUxet.png)<!-- -->

``` r

bike |>
  ggplot(aes(x=Temp, y=Hum)) +
   geom_point(color = "black", size = 2) + 
  labs(title = "Rentals vs Humidity",
       x = "% Humidity",
       y = "Bike Rentals") +
  theme_minimal()
```

![](https://i.imgur.com/Uffcv0k.png)<!-- -->

``` r

bike |>
  ggplot(aes(x=Temp, y=Wind)) +
   geom_point(color = "black", size = 2) + 
  labs(title = "Rentals vs Wind Speed",
       x = "Wind Speed",
       y = "Bike Rentals") +
  theme_minimal()
```

![](https://i.imgur.com/rNYM5WM.png)<!-- -->

``` r

ccf(bike$Rentals,bike$Wind)
```

![](https://i.imgur.com/Kb353Ih.png)<!-- -->

``` r
ccf(bike$Rentals,bike$Hum)
```

![](https://i.imgur.com/5ylYLys.png)<!-- -->

From these descriptive plots, there appears to be an increasing trend in bike rentals. This means we will have to account for this trend in our models, since they cannot directly handle the trend. There is a clear seasonal pattern, with higher numbers of rentals in the summer months. Temperature also follows a clear seasonal pattern, with warmer temperatures in the summer months. Drastic negative spikes in bike sales often correspond to the presence of weather category 3, which indicates rain or snow. Holidays also tend to correspond to fewer bike rentals.

There is also a weekly pattern to the data, with more bike rentals during week days and fewer on non-working days. Very high values of humidity and wind also seem to have lower numbers of bike rentals, however this relationship appears less linear in nature.

## Part 1.2

At the end of day t, we only know the values of Rentals, temperature, wind, humidity, and weather categoty up to day t. So, for predicting the value of rentals at t+3, we will not have information for these variables for days t+1 through t+3. However, will we know the values of holiday and work day for any future value, since these are just based off the calendar.

Given the goal of our forecasting is to ensure that enough bikes are available for high-demand days, it makes sense to give a higher penalty for under-forecasting, since this would mean not enough bikes are available. Using the quantile loss function could be a good candidate for this, since it gives different weights to over/under estimates depending on the value of tau chosen.

Since we want to predict 3 days out, we will choose the validation window to be 3 days out. We will choose a expanding window, since we have seasonality in the data in terms of both the yearly period and day of the week. We will include at least 6 months of initial data to fit the model, and then add one day at a time for each forecast. Including this information may help the models to learn patterns, as opposed to a fixed window which will be much more dependent on values of weather covariates, which we do not know future values of. Using at least 6 months of data will also allow the models to learn some of the seasonal behavior. Since we will have only one prediction at each window, the RMSE and MAE will end up being the same.

For benchmark values, we can take the weekly difference in number of sales from the same day of the previous week. We can also use the same value as the most recent available weekly difference in sales, which would be from the previous day. One of these will account for the workday effect, while the other will not. While knowing actual business metrics that could quantify the loss/benefit of the predictions would help, we will say we need at least a 15% or so improvement over both benchmarks for the models to be worth the implementation.

# Part 2

## Part 2.1

Here we create the features we will use. We also take the difference of rentals from the previous week’s value, in order to remove the trend. We should not use the normal values because these models cannot account for the trend.

``` r
bike_feat <- bike |>
  mutate(
    lag3 = lag(Rentals, 3),
    lag7 = lag(Rentals, 7),
    d7 = Rentals -lag7,
    lag14 = lag(Rentals, 14),
    naive_pred_3 = lag(Rentals,3),
    naive_pred_7 = lag(Rentals, 7),
    roll7 = zoo::rollmeanr(lag(Rentals, 3), 7, fill = NA, align = "right"),
    temp_roll7 = zoo::rollmeanr(lag(Temp, 3), 7, fill = NA, align = "right"),
    hum_roll7 = zoo::rollmeanr(lag(Hum, 3), 7, fill = NA, align = "right"),
    wind_lag3 = lag(Wind,3)
    ) |>
  filter(!is.na(lag14), !is.na(roll7))

ggplot(bike_feat, aes(x = Date, y = d7)) +
  geom_line(color = "black", linewidth = 0.8) +
  geom_smooth(method = "lm", color = "red")
#> `geom_smooth()` using formula = 'y ~ x'
```

![](https://i.imgur.com/boJY4hv.png)<!-- -->

``` r
  labs(x = "Date", y = "Differenced rentals") +
  theme_minimal()
#> NULL
```

## Part 2.2

For the features we are using to predict the value at time t+3, we use the most recent available value of bike rentals (lag 3), which is from time t, which is available. Likewise, we also use the lag 7 and lag 14 values of bike rentals to help capture the weekly seasonal patterns, these are from time t-4 and t-11. “roll7”, “temp_roll7”, and “hum_roll7” take the average of the last 7 available values of bike rentals, temperature, and humidity which are the average at time t-6 through t. We use the lag 3 value of wind, which comes from time t, since this was shown to be significant in the ccf plot. Lags of humidity were not shown to be significant in the ccf plot, but we compute this average so it can be included in the model.

## Part 2.3

``` r
train_end <- as.Date("2012-06-30")
tr <- bike_feat |> filter(Date <= train_end)
te <- bike_feat |> filter(Date >  train_end)

xs <- c("lag3", "lag7", "lag14", "roll7",
        "temp_roll7", "hum_roll7", "wind_lag3", "Holiday",
        "Workday")

c(train = nrow(tr), test = nrow(te), features = length(xs))
#>    train     test features 
#>      533      184        9
```

The naive benchmark predictions already exist as the values of lag3 and lag7.

# Part 3

## Part 3.1

First, here is the function to compute the quantile loss for the predictions. In our models, we will set tau to be 0.8, which will penalize under-predictions 4 times as much as over-predictions. We are going to include at least 6 months of data from the training set so some of the seasonal patterns can be learned before making any decisions using the validation data. Then, we show the function to fit the initial model:

``` r
all_rows <- bike_feat
all_train_rows <- tr

quantile_loss <- function(y, y_hat, tau) {
  error <- y - y_hat
  loss <- pmax(tau * error, (tau - 1) * error)
  return(mean(loss, na.rm = TRUE))
}

fit_eval_1 <- function(trn, tst) {
  rf1 <- ranger(d7 ~ ., data = all_train_rows[trn, c("d7", xs)], seed = 123)
  pred_d7 <- predict(rf1, all_train_rows[tst, xs])$predictions
  pred_rentals <- pred_d7 + all_train_rows$lag7[tst]
  
  quantile_loss(all_train_rows$Rentals[tst],pred_rentals,0.8)
}

valid_idx <- which(all_train_rows$Date > as.Date("2011-06-30"))

valid_origins <- seq(
  from = min(valid_idx),
  to = max(valid_idx) - 3,
  by = 1
)

ordered_1 <- lapply(valid_origins, function(i) {
  trn <- 1:i
  tst <- i + 3

  fit_eval_1(trn, tst)
})

overall_loss_1 <- mean(unlist(ordered_1))
```

## Part 3.2

Then, we can put some other constraint on the random forest and complete the same procedure as above. We will try a few values for maximum depth, to be equal to 2 to 5.

``` r
fit_eval_2 <- function(trn, tst,depth) {
  rf1 <- ranger(d7 ~ ., data = all_train_rows[trn, c("d7", xs)], seed = 123,
                max.depth=depth)
  pred_d7 <- predict(rf1, all_train_rows[tst, xs])$predictions
  pred_rentals <- pred_d7 + all_train_rows$lag7[tst]
  
  quantile_loss(all_train_rows$Rentals[tst],pred_rentals,0.8)
}

depths <- c(2,3,4,5)
ordered_2 <- lapply(depths, function(d) {
  lapply(valid_origins, function(i) {
    trn <- 1:i
    tst <- i + 3
    fit_eval_2(trn, tst, d)
  })
})

overall_loss_2 <- sapply(ordered_2, function(x) mean(unlist(x), na.rm = TRUE))

overall_loss_2
#> [1] 497.8652 491.5275 485.5485 481.0667
```

The smallest validation error occurs for the random forest with max depth of 5. We can compare this to the unconstrained version and to the naive benchmarks. We expect the 7-day naive forecast to perform better, so we will calculate the ratio of each score compared to that.

``` r
random_forest_options <- data.frame(model=c("Default setting", "Max-depth 5"), score = c(overall_loss_1, overall_loss_2[4]))

val_error_last_data <- quantile_loss(all_train_rows$Rentals[min(valid_idx):max(valid_idx-3)],
                                     all_train_rows$naive_pred_3[min(valid_idx):max(valid_idx-3)],
                                     0.8)
val_error_last_week <- quantile_loss(all_train_rows$Rentals[min(valid_idx):max(valid_idx-3)],
                                     all_train_rows$naive_pred_7[min(valid_idx):max(valid_idx-3)],
                                     0.8)

random_forest_options <- random_forest_options %>%
  add_row(model="Naive 3 days prior observation", score = val_error_last_data) %>%
  add_row(model = "Naive 7 days prior observation", score= val_error_last_week)

random_forest_options %>% 
  mutate(ratio_naive7 = score/val_error_last_week) %>%
  arrange(score)
#>                            model    score ratio_naive7
#> 1                Default setting 472.1900    0.9233531
#> 2                    Max-depth 5 481.0667    0.9407110
#> 3 Naive 7 days prior observation 511.3862    1.0000000
#> 4 Naive 3 days prior observation 530.4061    1.0371927
```

So, we can see a good improvement in the average quantile loss predicting 3 days out with a rolling window using the random forest when compared to either benchmark forecast. The random forest with default settings performs the best here.

## Part 3.3

Now, we can refit the model on the full training data and test on the testing data.

``` r
testing_idx <- which(all_rows$Date >train_end)
testing_origins <- seq(
  from = min(testing_idx),
  to = max(testing_idx) - 3,
  by = 1
)

fit_eval_final <- function(trn, tst) {
  rf1 <- ranger(d7 ~ ., data = all_rows[trn, c("d7", xs)], seed = 123)
  pred_d7 <- predict(rf1, all_rows[tst, xs])$predictions
  pred_rentals <- pred_d7 + all_rows$lag7[tst]
  
  test_loss <- quantile_loss(all_rows$Rentals[tst],pred_rentals,0.8)
  test_rmse <- sqrt(mean((all_rows$Rentals[tst] - pred_rentals)^2))
  test_mae <- mean(abs(all_rows$Rentals[tst] - pred_rentals))
  
  oob_pred_rentals <- rf1$predictions + all_rows$lag7[trn]
  oob_loss <- quantile_loss(all_rows$Rentals[trn],oob_pred_rentals,0.8)
  
  c(OOB_Quantile = oob_loss, Test_Quantile = test_loss,Test_RMSE = test_rmse,
    Test_MAE = test_mae)
}

ordered_final <- lapply(testing_origins, function(i) {
  trn <- 1:i
  tst <- i + 3

  fit_eval_final(trn, tst)
})

final_results <- do.call(rbind, ordered_final)

colMeans(final_results)
#>  OOB_Quantile Test_Quantile     Test_RMSE      Test_MAE 
#>      370.0263      501.7473     1018.5426     1018.5426
final_means <- colMeans(final_results)[!names(colMeans(final_results)) %in% "OOB_Quantile"]
```

The out of bag error is much lower than the testing error. This is likely because the out-of-bag error is in general over-optimistic. There is a potential for data leakage as future values may be used to inform the present prediction, which is not appropriate given the temporal nature of time series data.

## Part 3.4

Now, we will produce a permutation importance plot.

``` r
rf_final <- ranger(d7 ~ ., data = all_train_rows[,c("d7", xs)], seed = 123,
                importance = "permutation")
tibble(feature = names(rf_final$variable.importance),
                importance = rf_final$variable.importance) |>
  ggplot(aes(reorder(feature, importance), importance)) + geom_col(fill = "#2a78d6") +
  coord_flip() + labs(x = NULL, y = "Increase in MSE when shuffled")
```

![](https://i.imgur.com/MVH1OnP.png)<!-- -->

From this plot, the features that matter the most are the lagged values of bike rentals and the rolling average. The weather features have less of an impact, but still do provide some predictive value. Workday and holiday were very unimportant. However, we should recognize that taking the weekly difference as our outcome is going to remove any weekly “seasonality” that the workday variable accounted for. If we had chosen a different value to difference by, it is very possible this would’ve been found significant.

# Part 4

## Part 4.1

Here, we are going to fit the initial XGboost. We will use some of the initial settings from the course slides, and try several different options for number of rounds. We change the objective in the function to quantile error, since that is our loss function of interest instead of MSE.

``` r
fit_xgb_eval_1 <- function(trn, tst, num_rounds, depth) {
  dtr <- xgb.DMatrix(data = as.matrix(all_train_rows[trn, xs]),label = all_train_rows$d7[trn])

  dte <- xgb.DMatrix(data = as.matrix(all_train_rows[tst, xs]))
  
  xgb1 <- xgb.train(params = list(objective = "reg:quantileerror", eta = 0.05, max_depth = depth,
                subsample = 0.8, colsample_bytree = 0.8, seed=123, quantile_alpha=0.8),
                data = dtr, nrounds = num_rounds, verbose = 0)
  
  pred_train_d7 <- predict(xgb1, as.matrix(all_train_rows[trn, xs]))
  pred_test_d7 <- predict(xgb1, as.matrix(all_train_rows[tst, xs]))

  pred_train_rentals <- pred_train_d7 + all_train_rows$lag7[trn]
  pred_test_rentals <- pred_test_d7 + all_train_rows$lag7[tst]

  train_loss <- quantile_loss(all_train_rows$Rentals[trn], pred_train_rentals, 0.8)
  test_loss <- quantile_loss(all_train_rows$Rentals[tst], pred_test_rentals, 0.8)

  return(c(train_loss = train_loss, test_loss = test_loss))
}

num_rounds <- c(100,150,200,250,300,400, 500)
ordered_xgb_1 <- lapply(num_rounds, function(d) {
  lapply(valid_origins, function(i) {
    trn <- 1:i
    tst <- i + 3
    fit_xgb_eval_1(trn, tst, d, 4)
  })
})

overall_xgb_1 <- sapply(ordered_xgb_1, function(x) {
  errors <- do.call(rbind, x)
  c(train_loss = mean(errors[, "train_loss"], na.rm = TRUE),
    validation_loss = mean(errors[, "test_loss"], na.rm = TRUE)
    )
})

xgb_plot <- data.frame(
  nrounds = num_rounds,
  train_loss = overall_xgb_1["train_loss", ],
  validation_loss = overall_xgb_1["validation_loss", ]
)

ggplot(xgb_plot, aes(x = nrounds)) +
  geom_line(aes(y = train_loss, color = "Training Loss")) +
  geom_point(aes(y = train_loss, color = "Training Loss")) +
  geom_line(aes(y = validation_loss, color = "Validation Loss")) +
  geom_point(aes(y = validation_loss, color = "Validation Loss")) +
  labs(
    x = "Number of Boosting Rounds",
    y = "Quantile Loss",
    color = "Metric",
    title = "Training vs. Validation Loss"
  ) +
  theme_minimal()
```

![](https://i.imgur.com/mUXxZ2q.png)<!-- -->
For this model, using 100 rounds produced the lowest error in terms of the quantile loss function. Adding more rounds could be overfitting the model. We will compare this model with some other models in the next problem.

## Part 4.2

We will make some adjustments to this model to see how tuning the model can impact our results. For this problem, we will try a few different values of max_depth. If these are smaller, the tree will be simpler in general, and if the max depth is larger, we will have a larger, more complex tree.

``` r
max_depth = c(2, 3, 5)

xgb_results_2 <- lapply(max_depth, function(md) {
  lapply(num_rounds, function(nr) {
    errors <- lapply(valid_origins, function(i) {
      trn <- 1:i
      tst <- i + 3
      fit_xgb_eval_1(trn = trn,tst = tst, num_rounds = nr, depth = md
      )
    })
    errors <- do.call(rbind, errors)

    data.frame(
      max_depth = md,
      nrounds = nr,
      train_loss = mean(errors[, "train_loss"], na.rm = TRUE),
      validation_loss = mean(errors[, "test_loss"], na.rm = TRUE)
    )
  })
})

depth_results <- do.call(rbind, unlist(xgb_results_2, recursive = FALSE))


depth_long <- depth_results %>%
  pivot_longer(cols = c(train_loss, validation_loss), names_to = "type", values_to = "loss")

ggplot(depth_long, aes(x = nrounds, y = loss,
           color = factor(max_depth))) + geom_line() + geom_point(size = 2) +
  facet_wrap(~type, scales = "free_y") +
  labs(x = "Number of Boosting Rounds", y = "Quantile Loss", color = "Max Depth") +
  theme_minimal()
```

![](https://i.imgur.com/gfSfDJP.png)<!-- -->

This produces interesting results. Increasing maximum depth or increasing the number of boosting rounds inherently will improve training results, given a more complex model to fit to that data. However, we see a smaller maximum depth, leading to a less complex model, performs better in terms of the quantile loss function. Adding more rounds to the max depth 2 boosted regression model decreases the validation error, while for the max depth 3 and 5 models it actually increases the error, indicating those are potentially overfit. We see the plot level out around 300 rounds for the depth 2 model, and that shows nearly the lowest validation error across the board. This is the model we will proceed with, but first we will directly compare it to the model selected in part 1.

## Part 4.3

Let’s compare the depth 2 and depth 5 XGboost models in terms of quantile loss to see which one we should select. Then, we will refit it on the full training data and test on the full test data.

``` r
depth_results[depth_results$nrounds==300 & depth_results$max_depth==2,]
#>   max_depth nrounds train_loss validation_loss
#> 5         2     300   129.8041        294.8219
xgb_plot[xgb_plot$nrounds==100,]
#>   nrounds train_loss validation_loss
#> 1     100   118.7037        307.5395
```

The original model performs better in terms of the data used for fitting, but the tuned data performs better in terms of validation error. This suggests the initial model may be slightly too complex and overfit. We will use the tuned model as our final model.

``` r
fit_xgb_final <- function(trn, tst) {
  dtr <- xgb.DMatrix(data = as.matrix(all_rows[trn, xs]),label = all_rows$d7[trn])

  dte <- xgb.DMatrix(data = as.matrix(all_rows[tst, xs]))
  
  xgb1 <- xgb.train(params = list(objective = "reg:quantileerror", eta = 0.05, max_depth = 2,
                subsample = 0.8, colsample_bytree = 0.8, seed=123, quantile_alpha=0.8),
                data = dtr, nrounds = 300, verbose = 0)
  
  pred_test_d7 <- predict(xgb1, as.matrix(all_rows[tst, xs]))
  pred_test_rentals <- pred_test_d7 + all_rows$lag7[tst]

  test_loss <- quantile_loss(all_rows$Rentals[tst], pred_test_rentals, 0.8)
  test_rmse <- sqrt(mean((all_rows$Rentals[tst] - pred_test_rentals)^2))
  test_mae <- mean(abs(all_rows$Rentals[tst] - pred_test_rentals))

  c(Test_Quantile = test_loss,Test_RMSE = test_rmse,
    Test_MAE = test_mae)
}

final_xgb <- lapply(testing_origins, function(i) {
    trn <- 1:i
    tst <- i + 3
    fit_xgb_final(trn, tst)
  })

final_xgb_results <- do.call(rbind, final_xgb)

colMeans(final_xgb_results)
#> Test_Quantile     Test_RMSE      Test_MAE 
#>       294.421      1095.881      1095.881
xgb_means <- colMeans(final_xgb_results)
```

# Problem 5

## Part 5.1

Before we make the overall table, we will calculate the metrics for the naive benchmark. We will just use the 7 day naive benchmark, since that performed better in the previous section.

``` r
naive_eval <- function(trn, tst) {
  naive_pred <- all_rows$lag7[tst]
  test_loss <- quantile_loss(all_rows$Rentals[tst], naive_pred, 0.8)
  test_rmse <- sqrt(mean((all_rows$Rentals[tst] - naive_pred)^2))
  test_mae <- mean(abs(all_rows$Rentals[tst] - naive_pred))

  c(
    Test_Quantile = test_loss,
    Test_RMSE = test_rmse,
    Test_MAE = test_mae
  )
}

final_naive <- lapply(testing_origins, function(i) {
    trn <- 1:i
    tst <- i + 3
    naive_eval(trn, tst)
  })

final_naive_results <- do.call(rbind, final_naive)

naive_means <- colMeans(final_naive_results)

final_means <- as.data.frame(t(final_means))
final_means$model <- "Random Forest"
xgb_means <- as.data.frame(t(xgb_means))
xgb_means$model <- "XgBoost"
naive_means <- as.data.frame(t(naive_means))
naive_means$model <- "Naive 7 day"

naive_RMSE <- naive_means$Test_RMSE
naive_quant <- naive_means$Test_Quantile
naive_mae <- naive_means$Test_MAE

final_results <- bind_rows(final_means, xgb_means, naive_means)
final_results$quant_bl <- final_results$Test_Quantile / naive_quant
final_results$MAE_bl <- final_results$Test_MAE / naive_mae
final_results$RMSE_bl <- final_results$Test_RMSE / naive_RMSE

final_results
#>   Test_Quantile Test_RMSE Test_MAE         model  quant_bl    MAE_bl   RMSE_bl
#> 1      501.7473  1018.543 1018.543 Random Forest 0.9742394 0.8984790 0.8984790
#> 2      294.4210  1095.881 1095.881       XgBoost 0.5716754 0.9667008 0.9667008
#> 3      515.0144  1133.630 1133.630   Naive 7 day 1.0000000 1.0000000 1.0000000
```

When comparing the random forest and XgBoost to the naive 7 day predictions, we can see the XgBoost performs significantly better than both the random forest and naive forecasts in terms of the quantile loss function. Xbgoost barely performs better than the naive benchmark in terms of MAE or RMSE, while random forest does moderately better than both.

## Part 5.2

Here is a plot of the actual values and the predicted values from the models:

``` r
rf_predictions <- numeric(length(testing_origins))
xgb_predictions <- numeric(length(testing_origins))
actual_values <- numeric(length(testing_origins))
prediction_dates <- all_rows$Date[testing_origins + 3]

for (j in seq_along(testing_origins)) {
  i <- testing_origins[j]
  trn <- 1:i
  tst <- i + 3

  rf1 <- ranger(d7 ~ ., data = all_rows[trn, c("d7", xs)], seed = 123)
  pred_rf_d7 <- predict(rf1, all_rows[tst, xs])$predictions
  rf_predictions[j] <- pred_rf_d7 + all_rows$lag7[tst]
  
  dtr <- xgb.DMatrix(data = as.matrix(all_rows[trn, xs]), label = all_rows$d7[trn])

  xgb1 <- xgb.train(params = list(objective = "reg:quantileerror", eta = 0.05, max_depth = 5,
                subsample = 0.8, colsample_bytree = 0.8, seed=123, quantile_alpha=0.8),
                data = dtr, nrounds = 100, verbose = 0)

  pred_xgb_d7 <- predict(xgb1, as.matrix(all_rows[tst, xs]))
  xgb_predictions[j] <- pred_xgb_d7 + all_rows$lag7[tst]

  actual_values[j] <- all_rows$Rentals[tst]
}

plot_data <- data.frame(
  Date = prediction_dates,
  Actual = actual_values,
  Random_Forest = rf_predictions,
  XGBoost = xgb_predictions
)

plot_long <- plot_data %>%
  pivot_longer(cols = -Date, names_to = "Model", values_to = "Rentals")

ggplot(plot_long, aes(x = Date, y = Rentals, color = Model)) +
  geom_line(linewidth = 0.8) +
  labs(title = "Actual vs Predicted Bike Rentals",
       x = "Date",
       y = "Number of Rentals",
       color = "Model") +
  theme_minimal()
```

![](https://i.imgur.com/tjbYDSj.png)<!-- -->

## Part 5.3

From the above plot, it seems that the predictions from the XGboost model are generally larger than those of the random forest. This makes this model much more effective in terms of forecast demand on days of high demand. Looking at the majority of the red peaks on the plot, we can see the XGboost generally has larger forecasts. This leads to slightly less accurate predictions in general, when compared to random forest, and only slightly better predictions than the naive baseline. This makes sense, since we are asking for the 80% percentile of the distribution for our forecase, instead of the mean or median. This will in general push the predictions towards a larger value, which could be appropriate since the business goal requires that under-predictions have more financial cost.

We already attempted to account for this throughout the assignment, by using the weekly difference as the outcome and then adding back that value after making our forecasts. This may not remove all trend, but given our forecast target being 3 days out means we cannot use the difference from the previous day, it seemed the most appropriate. Using quantile loss as our loss function and within the Xgboost helped make the forecasts larger. If we had failed to do either or both of these, it is likely our models would have significantly under-predicted the number of rentals on high demand days, or even across the board.

## Part 5.4

Here, we want to refit the best model using the actual weather values, which would then require a weather forecast for 3 days into the future.

``` r
bike_feat_2 <- bike |>
  mutate(
    lag3 = lag(Rentals, 3),
    lag7 = lag(Rentals, 7),
    d7 = Rentals -lag7,
    lag14 = lag(Rentals, 14),
    naive_pred_3 = lag(Rentals,3),
    naive_pred_7 = lag(Rentals, 7),
    roll7 = zoo::rollmeanr(lag(Rentals, 3), 7, fill = NA, align = "right"),
    Temp = Temp,
    Hum =  Hum,
    Wind = Wind
    ) |>
  filter(!is.na(lag14), !is.na(roll7))


xs_2 <- c("lag3", "lag7", "lag14", "roll7",
        "Temp", "Hum", "Wind", "Holiday",
        "Workday")

fit_xgb_final_2 <- function(trn, tst) {
  dtr <- xgb.DMatrix(data = as.matrix(all_rows[trn, xs_2]),label = all_rows$d7[trn])

  dte <- xgb.DMatrix(data = as.matrix(all_rows[tst, xs_2]))
  
  xgb1 <- xgb.train(params = list(objective = "reg:quantileerror", eta = 0.05, max_depth = 2,
                subsample = 0.8, colsample_bytree = 0.8, seed=123, quantile_alpha=0.8),
                data = dtr, nrounds = 300, verbose = 0)
  
  pred_test_d7 <- predict(xgb1, as.matrix(all_rows[tst, xs_2]))
  pred_test_rentals <- pred_test_d7 + all_rows$lag7[tst]

  test_loss <- quantile_loss(all_rows$Rentals[tst], pred_test_rentals, 0.8)
  test_rmse <- sqrt(mean((all_rows$Rentals[tst] - pred_test_rentals)^2))
  test_mae <- mean(abs(all_rows$Rentals[tst] - pred_test_rentals))

  c(Test_Quantile = test_loss,Test_RMSE = test_rmse,
    Test_MAE = test_mae)
}

final_xgb_2 <- lapply(testing_origins, function(i) {
    trn <- 1:i
    tst <- i + 3
    fit_xgb_final_2(trn, tst)
  })

final_xgb_results_2 <- do.call(rbind, final_xgb_2)

colMeans(final_xgb_results_2)
#> Test_Quantile     Test_RMSE      Test_MAE 
#>      271.6556      897.5513      897.5513
xgb_means_2 <- colMeans(final_xgb_results_2)
```

There is a significant improvement in terms of all 3 metrics provided we use the actual weather values. There was a nearly 200 point improvement in terms of RMSE and MAE, and a more moderate improvement in terms of quantile loss. However, these are generally going to be overly optimistic predictions. There will always be a level of uncertainty in terms of weather. However, using weather forecasts may still provide a benefit given how large the improvement in test RMSE and MAE are. This would require the business to balance general predictive value with a desire to not under forecast.

## Part 5.5

Under the assumption that under-forecasts are significantly more harmful to business goals than over-forecasts, I would strongly suggest using the final XgBoost model. The ratio of quantile loss compared to the naive 7-day baseline being 0.571 shows an improvement that strongly exceeds our baseline goal of 0.85. While the values of RMSE/MAE show minimal improvement over baseline, indicating minor gains in overall predictive ability, further testing using 3-day weather forecasts instead of the included features might lead to improvements, since these both showed about a 200 point improvement when using actual weather data. This could be a potential future step if business goals led to concern regarding the small improvement in terms of RMSE and MAE.

<sup>Created on 2026-11-18 with [reprex v2.1.1](https://reprex.tidyverse.org)</sup>
