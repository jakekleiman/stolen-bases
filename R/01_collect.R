# =============================================================================
# 01_collect.R  -  Download everything the stolen base study needs (season set in R/config.R)
# -----------------------------------------------------------------------------
# What it pulls:
#   1. The regular season schedule (MLB Stats API)
#   2. The full play-by-play "live feed" for every final game (MLB Stats API)
#      -> every runner movement, including steals, caught stealings, pickoffs
#   3. Statcast pitch data, one day at a time (Baseball Savant)
#      -> only used to know WHO was catching each plate appearance
#   4. Sprint speed leaderboard (Baseball Savant), this season and last
#   5. Running-game leaderboards (Baseball Savant): runner jumps, how well
#      each pitcher holds runners, catcher pop times
#
# The script caches everything in data/raw/. If it crashes or you stop it,
# just run it again and it picks up where it left off.
# Expect roughly 1-2 hours for the first full run (about 2,430 game feeds).
# =============================================================================

suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(purrr)
  library(readr)
})

`%||%` <- function(a, b) if (is.null(a)) b else a
options(timeout = 300)   # MLB's servers can be slow; wait up to 5 minutes per request

source("R/config.R")
RAW_DIR   <- "data/raw"
FEED_DIR  <- file.path(RAW_DIR, "feeds")
SC_DIR    <- file.path(RAW_DIR, "statcast")
dir.create(FEED_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(SC_DIR,   recursive = TRUE, showWarnings = FALSE)

# Read a URL as text, retrying a few times with a growing pause
fetch_text <- function(url, tries = 4) {
  for (i in seq_len(tries)) {
    txt <- tryCatch(paste(readLines(url, warn = FALSE), collapse = "\n"),
                    error = function(e) NULL)
    if (!is.null(txt) && nchar(txt) > 0) return(txt)
    Sys.sleep(2 * i)
  }
  NULL
}

# -----------------------------------------------------------------------------
# 1. Schedule
# -----------------------------------------------------------------------------
message("Downloading the ", SEASON, " schedule...")
sched_url <- sprintf(
  "https://statsapi.mlb.com/api/v1/schedule?sportId=1&season=%d&gameType=R",
  SEASON)
sched <- fromJSON(fetch_text(sched_url), simplifyVector = FALSE)

games <- map_dfr(sched$dates, function(d) {
  map_dfr(d$games, function(g) tibble(
    game_pk        = as.character(g$gamePk),
    game_date      = d$date,
    official_date  = g$officialDate %||% d$date,
    status_code    = g$status$codedGameState %||% NA_character_,
    detailed_state = g$status$detailedState %||% NA_character_,
    away_team      = g$teams$away$team$name %||% NA_character_,
    home_team      = g$teams$home$team$name %||% NA_character_
  ))
})

# Keep finished games only. Suspended games can show up on two dates, so keep
# one row per game_pk (the date it was completed).
final_games <- games %>%
  filter(status_code == "F") %>%
  arrange(game_pk, desc(game_date)) %>%
  distinct(game_pk, .keep_all = TRUE)

write_csv(final_games, file.path(RAW_DIR, "schedule_final_games.csv"))
message(nrow(final_games), " final regular season games found.")

# -----------------------------------------------------------------------------
# 2. Live feeds (one gzipped JSON file per game)
# -----------------------------------------------------------------------------
message("Downloading game feeds (cached, safe to re-run)...")
todo <- final_games$game_pk[!file.exists(file.path(FEED_DIR, paste0(final_games$game_pk, ".json.gz")))]
message(length(todo), " feeds left to download.")

failed <- character(0)
for (k in seq_along(todo)) {
  pk  <- todo[k]
  url <- sprintf("https://statsapi.mlb.com/api/v1.1/game/%s/feed/live", pk)
  txt <- fetch_text(url)
  if (is.null(txt)) { failed <- c(failed, pk); next }
  con <- gzfile(file.path(FEED_DIR, paste0(pk, ".json.gz")), "w")
  writeLines(txt, con); close(con)
  if (k %% 50 == 0) message("  ", k, " / ", length(todo))
  Sys.sleep(0.2)
}
if (length(failed)) {
  warning(length(failed), " feeds failed. Re-run the script to retry them: ",
          paste(head(failed, 10), collapse = ", "))
}

# -----------------------------------------------------------------------------
# 3. Statcast pitch data, one day per request (only the columns we need)
# -----------------------------------------------------------------------------
message("Downloading Statcast data by day (cached, safe to re-run)...")
keep_cols <- c("game_pk", "game_date", "at_bat_number", "pitch_number",
               "inning", "inning_topbot", "batter", "pitcher", "fielder_2",
               "p_throws", "outs_when_up", "on_1b", "on_2b", "on_3b")

savant_day_url <- function(day) {
  paste0(
    "https://baseballsavant.mlb.com/statcast_search/csv?all=true",
    "&hfGT=R%7C&hfSea=", SEASON, "%7C&player_type=pitcher",
    "&game_date_gt=", day, "&game_date_lt=", day,
    "&min_pitches=0&min_results=0&group_by=name",
    "&sort_col=pitches&sort_order=desc&type=details")
}

days <- sort(unique(final_games$official_date))
for (day in days) {
  out_file <- file.path(SC_DIR, paste0("sc_", day, ".csv"))
  if (file.exists(out_file)) next
  df <- tryCatch(
    read_csv(savant_day_url(day), col_types = cols(.default = col_character()),
             progress = FALSE, show_col_types = FALSE),
    error = function(e) NULL)
  if (is.null(df)) { warning("Statcast failed for ", day, " (re-run to retry)"); next }
  if (nrow(df) >= 25000) warning(day, " hit Savant's 25,000 row cap; check this day.")
  df <- df %>% select(any_of(keep_cols))
  write_csv(df, out_file)
  message("  ", day, ": ", nrow(df), " pitches")
  Sys.sleep(1)
}

# -----------------------------------------------------------------------------
# 4. Sprint speed (this season, plus last season as a backup)
# -----------------------------------------------------------------------------
message("Downloading sprint speed leaderboards...")
for (yr in c(SEASON, SEASON - 1)) {
  url <- sprintf(
    "https://baseballsavant.mlb.com/leaderboard/sprint_speed?year=%d&position=&team=&min=0&csv=true",
    yr)
  ss <- tryCatch(read_csv(url, show_col_types = FALSE, progress = FALSE),
                 error = function(e) NULL)
  if (is.null(ss)) { warning("Sprint speed download failed for ", yr); next }
  write_csv(ss, file.path(RAW_DIR, sprintf("sprint_speed_%d.csv", yr)))
  message("  ", yr, ": ", nrow(ss), " players")
}

# -----------------------------------------------------------------------------
# 5. Running-game leaderboards (season averages, used as model inputs)
#    - Runners:  lead distance gained on steal attempts (how good their jumps are)
#    - Pitchers: lead distance runners gained against them (how well they hold runners)
#    - Catchers: pop time to 2nd base (how fast they get the ball there)
#    These are best effort: if Savant changes a URL or a column name, the
#    analysis drops that input and says so, instead of crashing.
# -----------------------------------------------------------------------------
message("Downloading running-game leaderboards...")
lb_urls <- list(
  runner_running = paste0(
    "https://baseballsavant.mlb.com/leaderboard/basestealing-run-value?game_type=Regular&n=0",
    "&pitch_hand=all&runner_moved=All&target_base=All&prior_pk=All",
    "&season_end=%1$d&season_start=%1$d&split=no&team=&type=Batter&with_team_only=1&csv=true"),
  pitcher_running = paste0(
    "https://baseballsavant.mlb.com/leaderboard/pitcher-running-game?game_type=Regular&n=0",
    "&pitch_hand=all&runner_moved=All&target_base=All&prior_pk=All",
    "&season_end=%1$d&season_start=%1$d&split=no&team=&with_team_only=1&csv=true"),
  catcher_poptime = "https://baseballsavant.mlb.com/leaderboard/poptime?year=%1$d&team=&min2b=0&min3b=0&csv=true")

for (nm in names(lb_urls)) {
  for (yr in c(SEASON, SEASON - 1)) {
    out <- file.path(RAW_DIR, sprintf("%s_%d.csv", nm, yr))
    if (file.exists(out)) next
    df <- tryCatch(read_csv(sprintf(lb_urls[[nm]], yr), show_col_types = FALSE, progress = FALSE),
                   error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) { warning(nm, " download failed for ", yr); next }
    write_csv(df, out)
    message("  ", nm, " ", yr, ": ", nrow(df), " rows; columns: ",
            paste(head(names(df), 12), collapse = ", "), if (ncol(df) > 12) ", ..." else "")
  }
}

message("Done. Raw data is in ", RAW_DIR, "/")
