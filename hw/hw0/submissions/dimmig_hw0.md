``` r
library(fredr)
fredr_set_key("052142bc981666b4ebcb1c8df98d006b")
icnsa = fredr(series_id = "ICNSA")
fit1 <- lm(value~date, data=icnsa)
prediction <- predict(fit1, newdata = data.frame(date = as.Date("2026-09-03")), interval = "prediction")
print(prediction)
#>        fit       lwr      upr
#> 1 374079.5 -115821.5 863980.4
```

<sup>Created on 2026-09-02 with [reprex v2.1.1](https://reprex.tidyverse.org)</sup>
  