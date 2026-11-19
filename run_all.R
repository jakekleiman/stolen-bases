# Run the whole stolen base study, start to finish.
# Open this project folder in RStudio (or setwd() to it) and run:  source("run_all.R")
# The season is set in R/config.R.

needed <- c("jsonlite", "dplyr", "purrr", "readr", "tidyr", "ggplot2", "scales", "lme4", "ggrepel")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)

source("R/01_collect.R")          # downloads (slow the first time, cached after)
source("R/02_clean.R")            # replays every game pitch by pitch
source("R/03_run_expectancy.R")   # steps 1-2: run expectancy, break-even rates
source("R/04_win_probability.R")  # step 3: win probability, late-game break-evens
source("R/05_grade_attempts.R")   # step 4: success model, grades for teams/runners/catchers
source("R/06_opportunities.R")    # step 5: every chance to run, pitchers, sensitivity check
# source("R/07_stability.R")      # only after a second season has been run in a copy of this folder

message("\nAll done. Results are in outputs/tables and outputs/figures.")
