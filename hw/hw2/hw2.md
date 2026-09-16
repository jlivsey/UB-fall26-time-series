# Homework 2 - Due Wednesday 9/23

Please complete the following and upload **reproducible code** to the [Github HW2 submissions](https://github.com/jlivsey/UB-fall26-time-series/tree/main/hw/hw2/submissions) repository.

Your code must be **reproducible**, and the results must be **visible without downloading or running anything**. The easiest way to do this is to use the [`reprex`](https://reprex.tidyverse.org/) R package.

## Part 1: ICNSA

1. Load the most current **Initial Claims (ICNSA)** data. Use the **not seasonally adjusted** series.

1. Pick one or more **covariates** for a regression model of ICNSA (e.g. trend, seasonal dummies/harmonics, holiday effects, a COVID indicator, or an external series such as one you may have used in HW1). Justify your choice using visual evidence — plots of ICNSA and candidate covariates over time, scatterplots, and/or cross-correlation (CCF) plots.

1. Fit the regression **as if the errors were uncorrelated** (plain OLS/`lm`), and use `tsdisplay()`/`acf()`/`pacf()` on the residuals to visually identify the ARMA structure of the errors, following the AR/MA behavior table from slide 05a (ACF tails off vs. PACF cuts off, and vice versa). State the $(p, d, q)$ (and, if seasonal, $(P, D, Q)_m$) you read off the plots and explain why.

1. Fit the full **regARIMA** model using your visually-identified stochastic structure. You may use `sarima()` with an `xreg=` argument, `Arima()` with `xreg=`, or `fable`'s `ARIMA(y ~ covariates + pdq(p,d,q))`.

1. Now let the computer choose: use a **numerical search** to find the AICc-optimal stochastic structure for the *same* set of covariates (e.g. `auto.arima(..., xreg = ...)`, `fable::ARIMA(y ~ covariates)` with no `pdq()` specified, or a brute-force grid search over $(p,q)$ ranked by AICc as in slide 05a). Report the model the search selected.

1. Compare your hand-identified model to the numerically-optimal model:
   * Do they agree? If not, how do they differ?
   * Compare AICc for both.
   * Run diagnostics (`checkresiduals()`, `sarima()`'s residual panel, or `tsdisplay()` + Ljung-Box) on **both** models and report whether each passes.
   * State which model you would choose as final, and why (recall: AICc alone is not enough — a model must also pass residual diagnostics).

1. Using your **final chosen regARIMA model**, forecast ICNSA for the week ending **September 26, 2026**.

## Part 2: Retail Sales: Beer, Wine, and Liquor Stores

* **Series:** Retail Sales: Beer, Wine, and Liquor Stores (NAICS 4453) — **not seasonally adjusted**, monthly, in millions of dollars.
* **FRED series ID:** [`MRTSSM4453USS`](https://fred.stlouisfed.org/series/MRTSSM4453USS)
* **Where to get it:** download it directly as a CSV from `https://fred.stlouisfed.org/graph/fredgraph.csv?id=MRTSSM4453USS`, or use the `fredr` package (`fredr::fredr(series_id = "MRTSSM4453USS")`) if you have a FRED API key. Use the most current vintage available when you run your code.

1. Plot the series and describe what you see 

1. Repeat the same procedure as Part 1 for this series:
   * Choose and justify covariate(s) using visual diagnostics (e.g. trend, monthly seasonal dummies or harmonics, a December/holiday indicator).
   * Visually identify a candidate stochastic structure for the regression errors 
   * Fit that regARIMA model.
   * Use a numerical search (`auto.arima(..., xreg = ...)`, `fable::ARIMA()` without `pdq()`, or a brute-force AICc grid) to find the AICc-optimal stochastic structure for the same covariates.
   * Compare the two models (AICc + residual diagnostics) and choose a final model.

1. Using your **final chosen regARIMA model**, forecast Beer, Wine, and Liquor Store sales for **December 2026**.

## Notes

* Make sure your **two final forecasts (ICNSA and your second series) are among the last outputs of your code**, clearly labeled.
* Every modeling decision (covariates and stochastic structure) should be backed by a plot or a numerical comparison shown in your code.
