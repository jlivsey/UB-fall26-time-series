# ============================================================
# PRE-COMPUTE: one LSTM on the Victoria electricity data
#
# Run this file by hand (it is NOT run when the slides render).
# It saves lstm-electricity.rds, which 11A loads. That way the
# slides render for everyone, even without Keras / Python.
#
# Setup used on the instructor's machine: keras3 + a Python 3.11
# conda environment (install_keras() needs Python 3.9-3.11).
# Edit the path below to point at your own Python.
#
# Same evaluation design as the other models:
#   - train on 2012-2013, then score ONE STEP AHEAD on 2014
#   - the weights are fit once on training data and never updated
# Inputs for predicting day t:
#   - the last 14 days of Demand and Temperature (a sequence)
#   - calendar information for day t itself: holiday + day of week
#     (the same known-in-advance inputs the VAR was given)
# ============================================================

Sys.setenv(RETICULATE_PYTHON = "/opt/miniconda3/envs/r-keras/bin/python")
library(fpp3)
library(keras3)

# ---- same daily data as the 11A slides ---------------------
el <- vic_elec |>
  index_by(Date) |>
  summarise(Demand = sum(Demand) / 1000,
            Temp = max(Temperature),
            Holiday = as.numeric(any(Holiday))) |>
  mutate(dow2 = as.numeric(wday(Date) == 2), dow3 = as.numeric(wday(Date) == 3),
         dow4 = as.numeric(wday(Date) == 4), dow5 = as.numeric(wday(Date) == 5),
         dow6 = as.numeric(wday(Date) == 6), dow7 = as.numeric(wday(Date) == 7)) |>
  as_tibble()
# the slides drop the first day (its lagged temperature is missing); do the same
el <- el[-1, ]

train_end <- as.Date("2013-12-31")
n_tr  <- sum(el$Date <= train_end)
win   <- 14                                     # days of history the LSTM sees

# ---- scale using TRAINING data only ------------------------
mu <- colMeans(el[1:n_tr, c("Demand", "Temp")])
sdv <- apply(el[1:n_tr, c("Demand", "Temp")], 2, sd)
Y <- scale(as.matrix(el[, c("Demand", "Temp")]), center = mu, scale = sdv)
cal <- as.matrix(el[, c("Holiday", paste0("dow", 2:7))])

# ---- turn the series into (window -> next day) examples ----
make_windows <- function(t_idx) {
  x_seq <- array(0, dim = c(length(t_idx), win, 2))
  for (i in seq_along(t_idx)) x_seq[i, , ] <- Y[(t_idx[i] - win):(t_idx[i] - 1), ]
  list(seq = x_seq, cal = cal[t_idx, , drop = FALSE], y = Y[t_idx, 1])
}
tr_idx <- (win + 1):n_tr
te_idx <- (n_tr + 1):nrow(el)
tr <- make_windows(tr_idx)
te <- make_windows(te_idx)

# ---- model: LSTM over the window + calendar for the target day ----
build_model <- function() {
  in_seq <- keras_input(shape = c(win, 2))
  in_cal <- keras_input(shape = c(7))
  h <- in_seq |> layer_lstm(units = 16)
  out <- layer_concatenate(list(h, in_cal)) |>
    layer_dense(units = 16, activation = "relu") |>
    layer_dense(units = 1)
  m <- keras_model(list(in_seq, in_cal), out)
  m |> compile(optimizer = optimizer_adam(learning_rate = 0.005), loss = "mse")
  m
}

seeds <- 1:5
runs <- lapply(seeds, function(s) {
  set_random_seed(s)
  m <- build_model()
  t0 <- Sys.time()
  m |> fit(
    x = list(tr$seq, tr$cal), y = tr$y,
    epochs = 200, batch_size = 32,
    validation_split = 0.15,          # the LAST 15% of training windows (chronological)
    callbacks = list(callback_early_stopping(patience = 20, restore_best_weights = TRUE)),
    verbose = 0)
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  p <- as.numeric(predict(m, list(te$seq, te$cal), verbose = 0))
  list(seed = s, secs = secs,
       pred = p * sdv["Demand"] + mu["Demand"])        # back to GWh
})

actual <- el$Demand[te_idx]
out <- list(
  date   = el$Date[te_idx],
  actual = actual,
  preds  = sapply(runs, `[[`, "pred"),                  # 365 x n_seeds
  seeds  = seeds,
  secs   = sapply(runs, `[[`, "secs"),
  info   = list(window = win, units = 16, train_n = length(tr_idx),
                keras = as.character(packageVersion("keras3")))
)
saveRDS(out, "lstm-electricity.rds")

rmse <- apply(out$preds, 2, function(p) sqrt(mean((actual - p)^2)))
cat("LSTM test RMSE by seed:", round(rmse, 2), "\n")
cat("Seconds per fit:", round(out$secs, 1), "\n")
