# When to Attempt a Stolen Base

The R code behind [When to Attempt a Stolen Base](https://jakekleiman.github.io/stolen-bases/), an analysis of MLB's 2026 running game.

It uses every stolen base attempt from 2026, and every pitch where a runner could have gone, to answer five questions about the steal decision:

1. How many outs are there?
2. What's the score, the inning and the count?
3. Who's on the mound?
4. Has the pitcher already stepped off twice?
5. Should you trust last year's numbers?

## How it works

| Script | What it does |
| --- | --- |
| `R/01_collect.R` | Downloads the schedule and every game's play-by-play feed (MLB Stats API), plus Statcast catcher IDs, sprint speed, runner jumps, how well pitchers hold runners and catcher pop times (Baseball Savant) |
| `R/02_clean.R` | Replays every game pitch by pitch to get the bases, outs, count and score before every pitch, and what actually happened on every steal attempt |
| `R/03_run_expectancy.R` | Builds the run expectancy table and the break-even success rate for every steal situation, by outs and by count |
| `R/04_win_probability.R` | Builds a win probability model and redoes the break-even rates in terms of wins, by inning and score |
| `R/05_grade_attempts.R` | Estimates each attempt's chance of success before the pitch (a mixed-effects logistic model, predicted out of sample), then splits each attempt's value into decision and execution |
| `R/06_opportunities.R` | Grades every pitch a runner could have gone on: how often runners went, how pitchers changed that, and the step-off effect |
| `R/07_stability.R` | Compares back-to-back seasons (2023 to 2026) to see which results carry over from one year to the next |
| `R/helpers.R`, `R/model_inputs.R`, `R/config.R` | Shared functions, model inputs and the season setting |

## Running it

1. Set the season in `R/config.R`.
2. From the project folder, run `source("run_all.R")`. Missing packages install automatically.
3. The first run downloads about 2,400 game feeds and takes 1 to 2 hours. Downloads are cached in `data/raw/`, so later runs are much faster.

Results are written to `outputs/tables/` and `outputs/figures/`. Check `outputs/tables/00_data_checks.csv` first: it compares stolen base and caught stealing totals with official counts and reports how many attempts were tied to a specific pitch.

### Year-to-year check

`07_stability.R` needs each season run in its own copy of this folder, side by side:

```
stolen-bases/
  2023/   (config.R set to 2023, run_all.R already run)
  2024/
  2025/
  2026/   (run 07_stability.R from here)
```

Only seasons from 2023 on are used. That's when the disengagement limit, the pitch clock and today's bigger bases took effect, so earlier seasons aren't comparable.

## Data

Raw data isn't included in this repo. Everything is public and is downloaded by `01_collect.R` from the [MLB Stats API](https://statsapi.mlb.com/) and [Baseball Savant](https://baseballsavant.mlb.com/).

## Packages

`jsonlite`, `dplyr`, `purrr`, `readr`, `tidyr`, `ggplot2`, `scales`, `lme4`, `ggrepel`
