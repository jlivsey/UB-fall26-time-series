# Homework 3

Please complete the following and upload **reproducible code** to the [Github HW3 submissions](https://github.com/jlivsey/UB-fall26-time-series/tree/main/hw/hw3/submissions) repository.

Your code must be **reproducible**, and the results must be **visible without downloading or running anything**. The easiest way to do this is to use the [`reprex`](https://reprex.tidyverse.org/) R package.

This assignment has two independent parts, following the workflows from slides **08-statespace** and **07-smoothing**. Both parts use the same series from Homework 2:

* **Series:** Retail Sales: Beer, Wine, and Liquor Stores (NAICS 4453), **not seasonally adjusted**, monthly, in millions of dollars.
* **FRED series ID:** [`MRTSSM4453USN`](https://fred.stlouisfed.org/series/MRTSSM4453USN)
* **Where to get it:** use the `fredr` package (`fredr::fredr(series_id = "MRTSSM4453USN")`), or download it directly as a CSV from `https://fred.stlouisfed.org/graph/fredgraph.csv?id=MRTSSM4453USN`. Use the most current vintage available when you run your code.

## Part 1: A structural model in state space form, with missing COVID data

1. Load the series described above.

1. Identify the COVID-19 window and **remove those observations** by setting them to `NA`. Keep a separate copy of the true, original values -- you will need them later for comparison. 

1. Specify a **structural state space model** for this series following the component framework from 08-statespace 

1. Fit your model to the series **with the COVID period missing**, using the $A_t = 0$-when-missing technique from 08-statespace / 08b-statespaceapplications, and use the **Kalman smoother** to impute the missing COVID-period values.

1. Plot your imputed values against the true values to see the imputation. 

1. Now go back and refit your structural state space model a second time, this time properly accounting for the COVID period (e.g. by adding an explicit intervention/level-shift component to the model, or by simply using the real, non-removed data). Compare the fitted trend component of this second model to the trend from Steps 3-4.

1. Using your final model, forecast this series for next month and year. 


## Part 2: Estimating a trend with smoothing methods

1. Using the same series, use **any smoothing technique from 07-smoothing** (e.g. a smoothing spline, simple exponential smoothing / Holt's level component, a moving average, or a kernel smoother) to estimate **only the trend** of the series. You are not fitting a full forecasting model for this part, and no forecast is required.

1. Produce at least two additional versions of your trend estimate using different smoothing parameters.

1. Plot all trend estimates together with the data, and comment on the trade-off you see (recall the "choppy vs. smooth ride" discussion from 07-smoothing).

1. State which of the trend estimates you would report as final, and **justify your choice**: why is it preferable to the smoother alternative, and why is it preferable to the rougher alternative?
