# Forecasting NFL game margins

## The company

You work for a sports analytics company that sells game previews to newspapers and broadcasters. Every preview gives a predicted final margin for each NFL game. Clients compare these numbers with the betting point spread, and the company wants a model it can defend.

## The question

Using only information available before kickoff, how well can you forecast the final margin (home score minus away score) of each NFL game? How close do your forecasts come to the point spread, and what information drives them?

## The data

Every NFL game from 1999 to the current season, from the [nflverse](https://github.com/nflverse/nfldata) project. Each row is one game, including playoffs. The file is updated during the season, so games from the 2026 season appear as they are played. Games not yet played have missing scores.

| Variable | Meaning |
|---|---|
| `season`, `week`, `game_type`, `gameday` | when the game was played (`game_type` is `REG` for regular season, otherwise a playoff round) |
| `home_team`, `away_team`, `location` | who played, and `Neutral` for games at a neutral site |
| `home_score`, `away_score`, `result` | final score, and `result` = home score minus away score |
| `home_rest`, `away_rest` | days since each team's previous game |
| `div_game` | 1 if the teams are in the same division |
| `roof`, `surface`, `temp`, `wind` | stadium and weather (temperature in °F, wind in mph) |
| `home_qb_name`, `away_qb_name` | starting quarterbacks |
| `home_coach`, `away_coach` | head coaches |
| `spread_line` | the point spread: the betting market's forecast of `result` |

Optional: the play-by-play files from the same project describe every play, including expected points added (`epa`), a measure of how much each play helped the offense. Summaries from past games, such as a team's average `epa` per play, are useful measures of team strength.

## What to deliver

- **Target:** `result`, the home team's final margin.
- **Inputs:** anything known before kickoff. Scores and play-by-play from a game may only be used for later games. Do **not** use `spread_line` (or any other betting line) as an input; it is the benchmark.
- **Training and test periods:** train on 1999 through 2023 and test on the 2024 and 2025 seasons, including playoffs. Forecasts for a test game may use every game played before that game's date. Choose model settings on a validation season taken from the training period.
- **Metrics:** RMSE and MAE of the margin on the test seasons, and the share of games where your forecast picks the winner. Compare with two benchmarks: the average home margin in the training period, and `spread_line`.

## Things to consider

The data cover 28 seasons and need cleaning before they can be modeled. Among other things, look at:

- teams that changed their code over the years, so each team's history is linked across seasons,
- weather values that are missing or not realistic, and whether to impute or exclude them,
- changes within a season that affect team strength, such as a new starting quarterback.

## Bonus: the rest of the Bills' 2026 season

Using every game played before your presentation, forecast each Buffalo Bills game from December 6 to the end of the regular season. For each game, give:

- the forecast margin (from the Bills' side),
- the probability that the Bills win,
- and, across all the remaining games, the distribution of the Bills' final regular-season record.

Show the forecasts in your presentation. In your report, compare them with the Bills games played by December 15.

## Load the data

```r
library(tidyverse)

games <- read_csv("https://raw.githubusercontent.com/nflverse/nfldata/master/data/games.csv",
                  show_col_types = FALSE) |>
  select(game_id, season, week, game_type, gameday, home_team, away_team, location,
         home_score, away_score, result, home_rest, away_rest, div_game,
         roof, surface, temp, wind, home_qb_name, away_qb_name,
         home_coach, away_coach, spread_line)

games |> filter(season == 2026, home_team == "BUF" | away_team == "BUF")
```

Optional play-by-play, one file per season (about 10 seconds to download each):

```r
read_pbp <- function(season) {
  read_csv(paste0("https://github.com/nflverse/nflverse-data/releases/download/pbp/play_by_play_",
                  season, ".csv.gz"),
           col_select = c(game_id, season, week, posteam, defteam, play_type, epa, success),
           show_col_types = FALSE)
}

pbp <- map(2015:2026, read_pbp) |> list_rbind()
```
