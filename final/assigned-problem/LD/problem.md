# Estimating oxygen uptake from a running watch

## The company

You work for a company that makes GPS running watches. The watch records heart rate and running speed every second. It cannot measure oxygen uptake ($\text{VO}_2$), which needs a lab mask and gas analyzer. Oxygen uptake is what the company's training-load and fitness ($\text{VO}_2\text{max}$) features are built on, so right now those features rely on rough formulas.

The company paid a sports lab for a large set of treadmill tests in which heart rate, speed **and** oxygen uptake were all measured.

## The question

How accurately can a runner's oxygen uptake be predicted, moment by moment, from what the watch measures (heart rate, speed and the runner's profile), **for a runner the model has never seen**? Does the recent history of heart rate and speed improve the prediction over the current values alone?

## The data

Graded exercise tests from the Exercise Physiology and Human Performance Lab at the University of Malaga, published on [PhysioNet](https://physionet.org/content/treadmill-exercise-cardioresp/1.0.1/). Each test starts with walking, then the speed rises (steadily on most tests, in steps on some) until the runner is exhausted, and ends with a recovery period at low speed.

The lab records one row per breath, every 1 to 3 seconds. The code below averages each test into 5-second intervals, so each test becomes an evenly spaced series with one row every 5 seconds (a median of 220 rows, about 18 minutes). It also drops 11 tests with no oxygen data, leaving 981 tests on 846 people.

| Variable | Meaning |
|---|---|
| `time` | start of the 5-second interval, in seconds since the start of the test |
| `Speed` | treadmill speed (km/h) |
| `HR` | heart rate (beats per minute) |
| `VO2`, `VCO2` | oxygen uptake and carbon dioxide output (mL/min) |
| `RR`, `VE` | breathing rate (breaths/min) and ventilation (L/min) |
| `ID`, `ID_test` | person and test identifiers (some people were tested more than once) |
| `Age`, `Weight`, `Height`, `Sex` | runner profile (weight in kg, height in cm, sex coded 0/1) |
| `Humidity`, `Temperature` | lab conditions |

## What to deliver

- **Target:** oxygen uptake per kilogram of body weight, `VO2 / Weight` (mL/kg/min).
- **Inputs:** only what a watch could know: `time`, `Speed`, `HR`, the runner profile, and their past values. `VCO2`, `RR` and `VE` need lab equipment, so use them for exploration only.
- **Test set:** all tests from people with `ID %% 5 == 0` (169 people). Choose model settings on a validation set of other people taken from the training data.
- **Metrics:** RMSE and MAE on the test set, and the error in each test's peak oxygen uptake, since that is what the watch reports as $\text{VO}_2\text{max}$.

## Load the data

```r
library(tidyverse)

base <- "https://physionet.org/files/treadmill-exercise-cardioresp/1.0.1/"
measure  <- read_csv(paste0(base, "test_measure.csv"), show_col_types = FALSE) |>
  filter(any(!is.na(VO2)), .by = ID_test)    # 11 tests have no oxygen data
subjects <- read_csv(paste0(base, "subject-info.csv"), show_col_types = FALSE)

# linear interpolation over missing values
fill_gaps <- function(t, x) {
  if (sum(!is.na(x)) < 2) return(x)
  approx(t, x, xout = t, rule = 2)$y
}

# average each test into 5-second intervals, then fill the few empty intervals
step <- 5
tests <- measure |>
  mutate(time = floor(time / step) * step) |>
  group_by(ID_test, ID, time) |>
  summarise(across(c(Speed, HR, VO2, VCO2, RR, VE), \(x) mean(x, na.rm = TRUE)),
            .groups = "drop_last") |>
  complete(time = seq(min(time), max(time), by = step)) |>
  mutate(across(c(Speed, HR, VO2, VCO2, RR, VE), \(x) fill_gaps(time, x))) |>
  ungroup() |>
  left_join(select(subjects, ID_test, Age, Weight, Height, Sex, Humidity, Temperature),
            by = "ID_test") |>
  mutate(VO2_kg = VO2 / Weight)

tests |> filter(ID_test == "2_1")   # one test
```
