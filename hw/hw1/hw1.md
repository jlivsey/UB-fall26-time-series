# Homework 1

Please complete the following and upload **reproducible code** to the [Github HW1 submissions](https://github.com/jlivsey/UB-fall26-time-series/tree/main/hw/hw1/submissions) repository.

Your code must be **reproducible**, and the results must be **visible without downloading or running anything**. The easiest way to do this is to use the [`reprex`](https://reprex.tidyverse.org/) R package.

1. Load the most current **Initial Claims (ICNSA)** data. Use the **not seasonally adjusted** series.

1. Download another series that you think might be related to ICNSA to use as a covariate.

1. Use a **regression-based technique of your choice** to forecast the value of ICNSA for the week ending **September 5, 2026**. Consider the following when choosing your model:

   * Variable transformations
   * Lead and lag relationships
   * Trend and seasonality

1. Use a **regARIMA** model to forecast ICNSA for the same week.

1. Use empirical data analysis or numerical calculations to decide on the **stochastic structure** to use for your regARIMA model. You may use the same covariate as in Question 3 or choose a different one.

   * Think about how including the same variable versus a different variable is **affected** by having ARIMA errors versus uncorrelated errors.

1. Make sure your **two forecast estimates are the last outputs of your code**.
