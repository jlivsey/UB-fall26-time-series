# ============================================================
# Helpers for the 10B bake-off
#
# Evaluation design ("train once, score one step ahead")
#   1. Fit every model on the TRAINING period only (parameters are
#      estimated once, using no test data).
#   2. Walk through the TEST period. At each time t every model gets
#      the data up to t-1 and predicts y_t. The parameters stay fixed.
#   3. Compare predictions with what actually happened.
# The same information is available to every model at every step,
# so the comparison is fair.
# ============================================================

library(fpp3)
library(vars)

# ---- fable models: ETS, ARIMA, NNAR, lag regression, naive -----
# df          tsibble containing the target and the (already lagged) predictors
# target      name of the response column (string)
# lag_cols    names of columns holding LAGGED predictors (e.g. "Income_l1")
# train_end   last index value of the training period
# naive       "NAIVE" or "SNAIVE" benchmark
# small_p, small_k, small_P  settings for the "smaller" NNAR variant
fit_fable_models <- function(df, target, lag_cols, train_end,
                             naive = "NAIVE", small_p = 2, small_k = 2,
                             small_P = 0, seed = 2026) {
  train <- df |> filter(.data[[index_var(df)]] <= train_end)
  full  <- df

  y  <- sym(target)
  rhs <- paste(lag_cols, collapse = " + ")
  lag_formula <- as.formula(paste(target, "~", rhs))
  nn_small_formula <- as.formula(
    paste0(target, " ~ AR(p = ", small_p, ", P = ", small_P, ")"))

  set.seed(seed)
  fits <- train |>
    model(
      ETS        = ETS(!!y),
      ARIMA      = ARIMA(!!y),
      NNAR       = NNETAR(!!y),
      NNAR_small = NNETAR(nn_small_formula, n_nodes = small_k),
      LagReg     = ARIMA(lag_formula),
      Benchmark  = if (naive == "SNAIVE") SNAIVE(!!y) else NAIVE(!!y)
    )

  # Re-apply the FIXED fitted models to the full series: the fitted
  # value at each test date is the one-step-ahead forecast.
  out <- refit(fits, full, reestimate = FALSE) |>
    augment() |>
    as_tibble() |>
    filter(.data[[index_var(df)]] > train_end) |>
    transmute(model = .model, date = .data[[index_var(df)]],
              actual = .data[[target]], pred = .fitted)
  attr(out, "fits") <- fits      # keep the fitted models so we can print them
  out
}

# ---- How much does NNAR depend on its random start? -----------
# Re-fit the default NNAR with different seeds and report the test RMSE.
nnar_seeds <- function(df, target, train_end, seeds = 1:8) {
  y <- sym(target)
  idx <- index_var(df)
  train <- df |> filter(.data[[idx]] <= train_end)
  sapply(seeds, function(s) {
    set.seed(s)
    fit <- train |> model(NNAR = NNETAR(!!y))
    a <- refit(fit, df, reestimate = FALSE) |> augment() |> as_tibble() |>
      filter(.data[[idx]] > train_end)
    sqrt(mean((a[[target]] - a$.fitted)^2, na.rm = TRUE))
  })
}

# ---- VAR: one-step forecasts by hand --------------------------
# vars::VAR estimates the coefficient matrix. A VAR(p) one-step
# forecast is just a linear combination of the last p observations
# (plus a constant and any exogenous terms), so we compute it directly.
var_onestep <- function(df, vars_in, target, train_end, lag_max = 4,
                        exog_cols = NULL) {
  idx   <- index_var(df)
  Y     <- as.matrix(as_tibble(df)[, vars_in])
  X     <- if (is.null(exog_cols)) NULL else as.matrix(as_tibble(df)[, exog_cols, drop = FALSE])
  n_tr  <- sum(df[[idx]] <= train_end)

  p <- VARselect(Y[1:n_tr, ], lag.max = lag_max, type = "const",
                 exogen = if (is.null(X)) NULL else X[1:n_tr, , drop = FALSE])$selection["AIC(n)"]
  fit <- VAR(Y[1:n_tr, ], p = p, type = "const",
             exogen = if (is.null(X)) NULL else X[1:n_tr, , drop = FALSE])
  B   <- Bcoef(fit)            # one row per equation; columns named y.l1, ..., const, exog

  eq  <- B[target, ]
  preds <- sapply((n_tr + 1):nrow(Y), function(t) {
    sum(sapply(names(eq), function(nm) {
      if (grepl("\\.l[0-9]+$", nm)) {                       # lagged y
        v <- sub("\\.l[0-9]+$", "", nm)
        l <- as.integer(sub(".*\\.l", "", nm))
        eq[nm] * Y[t - l, v]
      } else if (nm == "const") {
        eq[nm]
      } else {                                              # exogenous variable
        eq[nm] * X[t, nm]
      }
    }))
  })
  tibble(model = paste0("VAR(", p, ")"),
         date  = df[[idx]][(n_tr + 1):nrow(Y)],
         actual = Y[(n_tr + 1):nrow(Y), target],
         pred   = preds)
}

# ---- accuracy table -------------------------------------------
score_table <- function(res) {
  bench <- res |> filter(model == "Benchmark") |>
    summarise(b = sqrt(mean((actual - pred)^2))) |> pull(b)
  res |>
    group_by(model) |>
    summarise(RMSE = sqrt(mean((actual - pred)^2, na.rm = TRUE)),
              MAE  = mean(abs(actual - pred), na.rm = TRUE),
              .groups = "drop") |>
    mutate(`RMSE vs benchmark` = RMSE / bench) |>
    arrange(RMSE) |>
    mutate(across(where(is.numeric), ~ round(.x, 3)))
}

plot_onestep <- function(res, show = c("Benchmark", "ARIMA", "NNAR", "LagReg"), title = "") {
  res |>
    filter(model %in% show | grepl("^VAR", model)) |>
    ggplot(aes(date, pred, colour = model)) +
    geom_line(data = res |> filter(model == "Benchmark"), aes(y = actual),
              colour = "black", linewidth = 0.8) +
    geom_line(alpha = 0.8) +
    labs(x = NULL, y = NULL, title = title,
         subtitle = "Black = actual; coloured = one-step-ahead forecasts") +
    theme(legend.position = "bottom")
}
