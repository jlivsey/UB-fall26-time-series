``` r
## ============================================================
## Homework 0 -- UB Time Series (Fall 2026)
##
## Task (see hw/hw0/hw0.md):
##   1. Load the most current Initial Claims (ICNSA) data --
##      NOT seasonally adjusted.
##   2. Use a regression-based technique to forecast ICNSA for the
##      week ending August 29, 2026.
##   3. The forecast estimate must be the LAST value printed by
##      this script.
##
## This script is self-contained: it uses only base R, pulls the
## data directly from FRED's public CSV endpoint (no API key or
## package install required), and prints the forecast as its final
## line of output -- so it can be dropped straight into reprex::
## reprex(input = "hw0_solution.R", venue = "gh") for submission.
## ============================================================
```

``` r

# FRED's public "fredgraph" CSV endpoint always serves the latest vintage
# of a series and needs no API key, which keeps this script reproducible
# for anyone who runs it (see also fredr::fredr(series_id = "ICNSA"),
# used in the course slides, as an alternative if an API key is set up).
fred_url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id=ICNSA"

icnsa_raw <- read.csv(fred_url, stringsAsFactors = FALSE)
names(icnsa_raw) <- c("date", "value")

icnsa <- data.frame(
  date  = as.Date(icnsa_raw$date),
  value = suppressWarnings(as.numeric(icnsa_raw$value))
)
icnsa <- icnsa[!is.na(icnsa$value), ]
icnsa <- icnsa[order(icnsa$date), ]

if (nrow(icnsa) == 0) {
  stop("Could not download ICNSA data from FRED -- check internet access.")
}

cat("Most recent ICNSA (Initial Claims, NSA) observations available:\n")
#> Most recent ICNSA (Initial Claims, NSA) observations available:
print(tail(icnsa, 5))
#>            date  value
#> 3108 2026-07-25 176535
#> 3109 2026-08-01 172472
#> 3110 2026-08-08 189203
#> 3111 2026-08-15 173017
#> 3112 2026-08-22 169786
```

``` r

target_date <- as.Date("2026-08-29")

# Use only data strictly BEFORE the target week, so this is a genuine
# out-of-sample forecast rather than a lookup of an already-published
# value (ICNSA is released with a short lag, so by the time this script
# is run the target week may or may not already be in the FRED series).
train <- icnsa[icnsa$date < target_date, ]

# Weekly initial claims have (a) a slow-moving trend/level and (b) a
# strong, stable annual seasonal pattern (e.g. claims spike every
# December/January). We capture this with harmonic (Fourier) regression
# on log(claims):
#
#   log(y_t) = b0 + b1*t + sum_{k=1}^{K} [a_k sin(2*pi*k*t/m)
#                                          + c_k cos(2*pi*k*t/m)] + e_t
#
# where t is a weekly time index and m = 365.25/7 is the (non-integer)
# number of weeks per year -- the classic regression-based way to model
# seasonality when the period doesn't divide evenly into the data.

train$t    <- as.numeric(train$date - min(train$date)) / 7   # weeks since start
train$logY <- log(train$value)

m <- 365.25 / 7   # weeks per year
K <- 3            # number of Fourier harmonic pairs

make_fourier <- function(t, K, m) {
  X <- matrix(NA_real_, nrow = length(t), ncol = 2 * K)
  for (k in seq_len(K)) {
    X[, 2 * k - 1] <- sin(2 * pi * k * t / m)
    X[, 2 * k]     <- cos(2 * pi * k * t / m)
  }
  colnames(X) <- paste0(rep(c("sin", "cos"), K), rep(seq_len(K), each = 2))
  as.data.frame(X)
}

fourier_train <- make_fourier(train$t, K, m)
model_df <- cbind(logY = train$logY, t = train$t, fourier_train)

fit <- lm(logY ~ ., data = model_df)

cat("\nRegression summary (harmonic regression on log ICNSA):\n")
#> 
#> Regression summary (harmonic regression on log ICNSA):
print(summary(fit))
#> 
#> Call:
#> lm(formula = logY ~ ., data = model_df)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -0.85826 -0.21086 -0.00732  0.17515  3.04023 
#> 
#> Coefficients:
#>               Estimate Std. Error  t value Pr(>|t|)    
#> (Intercept)  1.274e+01  1.230e-02 1035.806  < 2e-16 ***
#> t           -2.145e-05  6.845e-06   -3.133 0.001744 ** 
#> sin1         2.760e-02  8.703e-03    3.171 0.001531 ** 
#> cos1         1.444e-01  8.690e-03   16.621  < 2e-16 ***
#> sin2        -2.599e-02  8.697e-03   -2.988 0.002827 ** 
#> cos2         1.479e-01  8.696e-03   17.007  < 2e-16 ***
#> sin3        -3.307e-02  8.695e-03   -3.804 0.000145 ***
#> cos3         8.608e-03  8.697e-03    0.990 0.322412    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.343 on 3104 degrees of freedom
#> Multiple R-squared:  0.1635, Adjusted R-squared:  0.1616 
#> F-statistic: 86.69 on 7 and 3104 DF,  p-value: < 2.2e-16
```

``` r

t_target       <- as.numeric(target_date - min(train$date)) / 7
fourier_target <- make_fourier(t_target, K, m)
newdata        <- cbind(t = t_target, fourier_target)

log_pred <- predict(fit, newdata = newdata, interval = "prediction")
pred     <- exp(log_pred)   # back-transform from the log scale

forecast_ICNSA <- unname(pred[1, "fit"])
lower_95       <- unname(pred[1, "lwr"])
upper_95       <- unname(pred[1, "upr"])

cat(sprintf(
  "\nForecast of ICNSA (Initial Claims, NSA) for the week ending %s:\n",
  format(target_date, "%B %d, %Y")
))
#> 
#> Forecast of ICNSA (Initial Claims, NSA) for the week ending August 29, 2026:
cat(sprintf("Point forecast:                %s\n",
            format(round(forecast_ICNSA), big.mark = ",")))
#> Point forecast:                274,780
cat(sprintf("Approx. 95%% prediction interval: [%s, %s]\n",
            format(round(lower_95), big.mark = ","),
            format(round(upper_95), big.mark = ",")))
#> Approx. 95% prediction interval: [140,093, 538,955]

## Final line of output: the forecast estimate itself, as required.
forecast_ICNSA
#> [1] 274779.6
```

<sup>Created on 2026-09-01 with [reprex v2.1.1](https://reprex.tidyverse.org)</sup>