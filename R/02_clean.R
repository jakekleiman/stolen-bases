# =============================================================================
# 02_clean.R  -  Turn the raw game feeds into analysis tables (pitch by pitch)
# -----------------------------------------------------------------------------
# Replays every game one event at a time: every pitch, pickoff throw and runner
# movement. That gives the exact bases, outs, count and score before every
# pitch, and what really happened on every steal attempt (including extra
# bases on bad throws and runs that scored on the play).
#
# Outputs (data/clean/):
#   pa.rds            one row per plate appearance
#   pitches.rds       one row per pitch: bases, outs, count and score BEFORE it,
#                     plus runs scored from there to the end of the inning
#   attempts.rds      one row per runner per steal attempt, with the state
#                     before it and the observed result of the play
#   opportunities.rds one row per pitch per runner who could have run
#                     (runner on 1st with 2nd open, or on 2nd with 3rd open),
#                     and whether he went on that pitch
#   batters.rds       each hitter's season wOBA and quality tier
#   players.rds       player id -> name
# =============================================================================

suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(purrr)
  library(readr)
  library(tidyr)
})

`%||%` <- function(a, b) if (is.null(a)) b else a
source("R/config.R")

RAW_DIR   <- "data/raw"
FEED_DIR  <- file.path(RAW_DIR, "feeds")
SC_DIR    <- file.path(RAW_DIR, "statcast")
CLEAN_DIR <- "data/clean"
dir.create(CLEAN_DIR, recursive = TRUE, showWarnings = FALSE)

# Steal event types in the MLB Stats API -> the base being stolen
STEAL_TARGET <- c(
  stolen_base_2b = "2B", stolen_base_3b = "3B", stolen_base_home = "HP",
  caught_stealing_2b = "2B", caught_stealing_3b = "3B", caught_stealing_home = "HP",
  pickoff_caught_stealing_2b = "2B", pickoff_caught_stealing_3b = "3B",
  pickoff_caught_stealing_home = "HP")
BASE_POS <- c("1B" = 1L, "2B" = 2L, "3B" = 3L)
SWING_CODES    <- c("S", "F", "T", "X", "D", "E", "W", "L", "M", "O", "Q", "R")
PITCHOUT_CODES <- c("P", "Q", "R")

chr <- function(x) if (is.null(x)) NA_character_ else as.character(x)

read_feed <- function(path) {
  con <- gzfile(path, "r"); on.exit(close(con))
  fromJSON(paste(readLines(con, warn = FALSE), collapse = "\n"), simplifyVector = FALSE)
}

# All runner movements in one play, in order
runner_moves <- function(runners) {
  if (!length(runners)) return(NULL)
  mv <- data.frame(
    play_index = vapply(runners, function(r) as.integer(r$details$playIndex %||% 9999L), integer(1)),
    runner     = vapply(runners, function(r) chr(r$details$runner$id), character(1)),
    start      = vapply(runners, function(r) chr(r$movement$start), character(1)),
    end        = vapply(runners, function(r) chr(r$movement$end), character(1)),
    is_out     = vapply(runners, function(r) isTRUE(r$movement$isOut), logical(1)),
    event_type = vapply(runners, function(r) chr(r$details$eventType), character(1)),
    stringsAsFactors = FALSE)
  mv$ord <- seq_len(nrow(mv))
  mv[order(mv$play_index, mv$ord), ]
}

# Collapse each runner's movements at one moment into a single move
collapse_moves <- function(m) {
  lapply(unique(m$runner), function(rid) {
    s  <- m[m$runner == rid, , drop = FALSE]
    st <- s[s$event_type %in% names(STEAL_TARGET), , drop = FALSE]
    list(runner = rid, start = s$start[1], end = s$end[nrow(s)], is_out = any(s$is_out),
         steal_type = if (nrow(st)) st$event_type[1] else NA_character_,
         steal_from = if (nrow(st)) st$start[1] else NA_character_,
         steal_out  = if (nrow(st)) st$is_out[1] else NA)
  })
}

# Apply moves: take everyone off their old base first, then place them
apply_moves <- function(bases, outs, coll) {
  runs <- 0
  for (x in coll) {
    j <- which(bases == x$runner)
    if (length(j)) bases[j] <- NA_character_
    else if (!is.na(x$start) && x$start %in% names(BASE_POS)) bases[BASE_POS[[x$start]]] <- NA_character_
  }
  for (x in coll) {
    if (x$is_out) outs <- outs + 1L
    else if (!is.na(x$end)) {
      if (x$end == "score") runs <- runs + 1
      else if (x$end %in% names(BASE_POS)) bases[BASE_POS[[x$end]]] <- x$runner
    }
  }
  list(bases = bases, outs = outs, runs = runs)
}

parse_game <- function(feed) {
  gd <- feed$gameData; ld <- feed$liveData
  if (!identical(gd$game$type, "R")) return(NULL)
  game_pk <- chr(feed$gamePk %||% gd$game$pk)
  team    <- c(away = gd$teams$away$abbreviation, home = gd$teams$home$abbreviation)
  plays   <- ld$plays$allPlays
  if (!length(plays)) return(NULL)

  has_post <- any(vapply(plays, function(p)
    any(c("postOnFirst", "postOnSecond", "postOnThird") %in% names(p$matchup)), logical(1)))

  bases <- rep(NA_character_, 3); outs <- 0L; cur <- ""
  sc <- c(away = 0, home = 0)
  pa_rows <- vector("list", length(plays)); att_rows <- list(); pitch_rows <- list()
  mismatch <- 0L

  for (i in seq_along(plays)) {
    p <- plays[[i]]
    if (!isTRUE(p$about$isComplete)) next
    inn  <- as.integer(p$about$inning)
    half <- p$about$halfInning
    key  <- paste(inn, half)
    if (key != cur) {
      bases <- rep(NA_character_, 3); outs <- 0L; cur <- key
      if (inn >= 10) bases[2] <- "ghost"           # extra-inning runner on 2nd
    }
    bat <- if (half == "bottom") "home" else "away"
    fld <- if (bat == "home") "away" else "home"
    ab  <- as.integer(p$about$atBatIndex)

    start_bases <- bases; start_outs <- outs; start_sc <- sc
    pitcher <- chr(p$matchup$pitcher$id)
    batter  <- chr(p$matchup$batter$id)
    hand    <- chr(p$matchup$pitchHand$code)

    pe <- p$playEvents %||% list()
    ev_idx <- vapply(seq_along(pe), function(k) as.integer(pe[[k]]$index %||% (k - 1L)), integer(1))
    mv <- runner_moves(p$runners)
    walk <- sort(unique(c(ev_idx, if (!is.null(mv)) mv$play_index)))

    balls <- 0L; strikes <- 0L; pk_before <- 0L
    window <- NA_integer_; w_balls <- 0L; w_strikes <- 0L; w_pk <- 0L; w_code <- NA_character_

    for (ix in walk) {
      e <- if (ix %in% ev_idx) pe[[which(ev_idx == ix)[1]]] else NULL

      if (!is.null(e) && isTRUE(e$isPitch)) {
        # Record the state BEFORE this pitch
        window <- ix; w_balls <- balls; w_strikes <- strikes; w_pk <- pk_before
        w_code <- chr(e$details$code)
        pitch_rows[[length(pitch_rows) + 1]] <- list(
          game_pk = game_pk, at_bat_index = ab, pitch_index = ix,
          inning = inn, half = half, bat_team = team[[bat]], fld_team = team[[fld]],
          outs = outs, b1_id = bases[1], b2_id = bases[2], b3_id = bases[3],
          bat_score = sc[[bat]], fld_score = sc[[fld]],
          balls = balls, strikes = strikes, pickoffs_before = pk_before,
          pitcher = pitcher, batter = batter, pitch_hand = hand, pitch_code = w_code)
        balls   <- min(as.integer(e$count$balls %||% balls), 3L)
        strikes <- min(as.integer(e$count$strikes %||% strikes), 2L)
      } else if (!is.null(e) && isTRUE(e$type %in% c("pickoff", "stepoff"))) {
        pk_before <- pk_before + 1L                # pickoff throw or step-off (disengagement)
      }

      if (is.null(mv)) next
      m <- mv[mv$play_index == ix, , drop = FALSE]
      if (!nrow(m)) next
      coll  <- collapse_moves(m)
      is_st <- vapply(coll, function(x) !is.na(x$steal_type), logical(1))

      if (any(is_st)) {
        # What the steal play itself did (leave out the batter's own result,
        # e.g. the strikeout on a strike-'em-out, throw-'em-out)
        not_batter <- coll[vapply(coll, function(x) !identical(x$runner, batter), logical(1))]
        ob <- apply_moves(bases, outs, not_batter)
        for (x in coll[is_st]) {
          att_rows[[length(att_rows) + 1]] <- list(
            game_pk = game_pk, at_bat_index = ab, play_index = as.integer(ix),
            window_pitch = window, inning = inn, half = half,
            bat_team = team[[bat]], fld_team = team[[fld]],
            outs = outs, b1 = !is.na(bases[1]), b2 = !is.na(bases[2]), b3 = !is.na(bases[3]),
            balls = if (is.na(window)) balls else w_balls,
            strikes = if (is.na(window)) strikes else w_strikes,
            pickoffs_before = if (is.na(window)) pk_before else w_pk,
            pitch_code = w_code,
            batter_swung = isTRUE(w_code %in% SWING_CODES),
            pitchout = isTRUE(w_code %in% PITCHOUT_CODES),
            runner = x$runner, from_base = x$steal_from,
            target = unname(STEAL_TARGET[x$steal_type]), event_type = x$steal_type,
            success = !isTRUE(x$steal_out), pickoff = grepl("pickoff", x$steal_type),
            n_runners_stealing = sum(is_st),
            bat_score = sc[[bat]], fld_score = sc[[fld]],
            pitcher = pitcher, batter = batter, pitch_hand = hand,
            post_b1 = !is.na(ob$bases[1]), post_b2 = !is.na(ob$bases[2]), post_b3 = !is.na(ob$bases[3]),
            post_outs = ob$outs, runs_on_play = ob$runs,
            runner_final_base = x$end)
        }
      }

      st <- apply_moves(bases, outs, coll)
      bases <- st$bases; outs <- st$outs; sc[[bat]] <- sc[[bat]] + st$runs
    }

    # Sync to the official end-of-play state
    if (has_post) {
      off <- c(chr(p$matchup$postOnFirst$id), chr(p$matchup$postOnSecond$id),
               chr(p$matchup$postOnThird$id))
      if ((p$count$outs %||% 0) < 3 && !identical(is.na(bases), is.na(off))) mismatch <- mismatch + 1L
      bases <- off
    }
    outs <- as.integer(p$count$outs %||% outs)
    sc <- c(away = as.numeric(p$result$awayScore %||% sc[["away"]]),
            home = as.numeric(p$result$homeScore %||% sc[["home"]]))

    pa_rows[[i]] <- list(
      game_pk = game_pk, at_bat_index = ab,
      inning = inn, half = half, bat_team = team[[bat]], fld_team = team[[fld]],
      outs_start = start_outs,
      b1_id = start_bases[1], b2_id = start_bases[2], b3_id = start_bases[3],
      bat_score_start = start_sc[[bat]], fld_score_start = start_sc[[fld]],
      bat_score_end = sc[[bat]], outs_end = outs,
      pitcher = pitcher, batter = batter, pitch_hand = hand,
      event_type = chr(p$result$eventType))
  }

  players <- map_dfr(gd$players, function(x)
    tibble(player_id = chr(x$id), name = chr(x$fullName),
           position = chr(x$primaryPosition$abbreviation)))

  list(pa = bind_rows(compact(pa_rows)), attempts = bind_rows(att_rows),
       pitches = bind_rows(pitch_rows), players = players,
       mismatch = mismatch, n_plays = length(plays))
}

# -----------------------------------------------------------------------------
# Parse every game
# -----------------------------------------------------------------------------
files <- list.files(FEED_DIR, pattern = "\\.json\\.gz$", full.names = TRUE)
message("Parsing ", length(files), " game feeds (pitch by pitch, takes a while)...")
res <- vector("list", length(files))
for (k in seq_along(files)) {
  res[[k]] <- tryCatch(parse_game(read_feed(files[k])),
                       error = function(e) { warning(basename(files[k]), ": ", conditionMessage(e)); NULL })
  if (k %% 200 == 0) message("  ", k, " / ", length(files))
}
res <- compact(res)

pa       <- bind_rows(map(res, "pa"))
attempts <- bind_rows(map(res, "attempts"))
pitches  <- bind_rows(map(res, "pitches"))
players  <- bind_rows(map(res, "players")) %>% distinct(player_id, .keep_all = TRUE)
mism     <- sum(map_int(res, "mismatch")); n_plays <- sum(map_int(res, "n_plays"))
rm(res); invisible(gc())

# -----------------------------------------------------------------------------
# Runs scored the rest of the inning, for every PA and every pitch
# -----------------------------------------------------------------------------
pa <- pa %>%
  arrange(game_pk, at_bat_index) %>%
  group_by(game_pk, inning, half) %>%
  mutate(half_end_score = last(bat_score_end),
         runs_roi       = half_end_score - bat_score_start,
         half_complete  = last(outs_end) == 3) %>%
  ungroup()

half_info <- pa %>% distinct(game_pk, inning, half, half_end_score, half_complete)
pitches <- pitches %>%
  left_join(half_info, by = c("game_pk", "inning", "half")) %>%
  mutate(runs_roi = half_end_score - bat_score) %>%
  select(-half_end_score)

# -----------------------------------------------------------------------------
# Hitter quality: season wOBA from the same play-by-play
# -----------------------------------------------------------------------------
woba_w <- c(walk = 0.69, hit_by_pitch = 0.72, single = 0.89, double = 1.27,
            triple = 1.62, home_run = 2.10)
batters <- pa %>%
  filter(!event_type %in% c("intent_walk", "sac_bunt", "catcher_interf")) %>%
  group_by(batter) %>%
  summarise(pa = n(), woba = sum(coalesce(woba_w[event_type], 0)) / n(), .groups = "drop")
cuts <- quantile(batters$woba[batters$pa >= 100], c(1/3, 2/3), na.rm = TRUE)
batters <- batters %>%
  mutate(batter_tier = case_when(pa < 100 ~ "Middle",
                                 woba < cuts[1] ~ "Weak",
                                 woba < cuts[2] ~ "Middle",
                                 TRUE ~ "Strong"))
tier_of <- function(ids) coalesce(batters$batter_tier[match(ids, batters$batter)], "Middle")
pa$batter_tier       <- tier_of(pa$batter)
pitches$batter_tier  <- tier_of(pitches$batter)
attempts$batter_tier <- tier_of(attempts$batter)

# -----------------------------------------------------------------------------
# Who was catching? (Statcast fielder_2 on the first pitch of each PA)
# -----------------------------------------------------------------------------
sc_files <- list.files(SC_DIR, pattern = "^sc_.*\\.csv$", full.names = TRUE)
scd <- map_dfr(sc_files, ~ read_csv(.x, col_types = cols(.default = col_character()),
                                    show_col_types = FALSE, progress = FALSE))
catchers <- scd %>%
  mutate(at_bat_index = as.integer(at_bat_number) - 1L,
         pitch_number = as.integer(pitch_number)) %>%
  arrange(game_pk, at_bat_index, pitch_number) %>%
  group_by(game_pk, at_bat_index) %>%
  summarise(catcher = first(fielder_2), sc_batter = first(batter), .groups = "drop")
rm(scd); invisible(gc())

pa <- pa %>% left_join(catchers, by = c("game_pk", "at_bat_index"))
match_rate <- mean(pa$sc_batter == pa$batter, na.rm = TRUE)
pa <- pa %>% select(-sc_batter)
catcher_only <- catchers %>% select(game_pk, at_bat_index, catcher)
attempts <- attempts %>% left_join(catcher_only, by = c("game_pk", "at_bat_index"))
pitches  <- pitches  %>% left_join(catcher_only, by = c("game_pk", "at_bat_index"))

# -----------------------------------------------------------------------------
# Steal opportunities, pitch by pitch
# -----------------------------------------------------------------------------
opp2 <- pitches %>% filter(!is.na(b1_id), is.na(b2_id), b1_id != "ghost") %>%
  mutate(runner = b1_id, target = "2B")
opp3 <- pitches %>% filter(!is.na(b2_id), is.na(b3_id), b2_id != "ghost") %>%
  mutate(runner = b2_id, target = "3B")
tried <- attempts %>%
  filter(!is.na(window_pitch)) %>%
  distinct(game_pk, at_bat_index, pitch_index = window_pitch, runner, target) %>%
  mutate(attempted = TRUE)

opportunities <- bind_rows(opp2, opp3) %>%
  left_join(tried, by = c("game_pk", "at_bat_index", "pitch_index", "runner", "target")) %>%
  mutate(attempted = coalesce(attempted, FALSE),
         b1 = !is.na(b1_id), b2 = !is.na(b2_id), b3 = !is.na(b3_id))

# -----------------------------------------------------------------------------
# Save + sanity checks
# -----------------------------------------------------------------------------
saveRDS(pa, file.path(CLEAN_DIR, "pa.rds"))
saveRDS(pitches, file.path(CLEAN_DIR, "pitches.rds"))
saveRDS(attempts, file.path(CLEAN_DIR, "attempts.rds"))
saveRDS(opportunities, file.path(CLEAN_DIR, "opportunities.rds"))
saveRDS(batters, file.path(CLEAN_DIR, "batters.rds"))
saveRDS(players, file.path(CLEAN_DIR, "players.rds"))
write_csv(attempts, file.path(CLEAN_DIR, "attempts.csv"))
write_csv(players, file.path(CLEAN_DIR, "players.csv"))

linked <- attempts %>% filter(!is.na(window_pitch)) %>% nrow()
checks <- tibble(
  check = c("Games parsed", "Plate appearances", "Pitches",
            "Steal attempts (runner level)", "Stolen bases",
            "Caught stealing (incl. pickoff CS)", "Pickoff caught stealing",
            "Attempts in double/triple steals", "Steals of home attempted",
            "Attempts tied to a specific pitch (%)",
            "Attempts where the batter swung (%)", "Attempts into a pitchout (%)",
            "Steal opportunities (runner-pitch)", "Opportunities where the runner went (%)",
            "Base-state mismatches vs official (% of plays)",
            "Statcast batter match rate (catcher join)",
            "Attempts with no catcher found"),
  value = c(length(unique(pa$game_pk)), nrow(pa), nrow(pitches),
            nrow(attempts), sum(attempts$success), sum(!attempts$success),
            sum(attempts$pickoff), sum(attempts$n_runners_stealing > 1),
            sum(attempts$target == "HP"),
            round(100 * linked / nrow(attempts), 1),
            round(100 * mean(attempts$batter_swung), 1),
            round(100 * mean(attempts$pitchout), 1),
            nrow(opportunities), round(100 * mean(opportunities$attempted), 2),
            round(100 * mism / n_plays, 2), round(match_rate, 4),
            sum(is.na(attempts$catcher))))
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
write_csv(checks, "outputs/tables/00_data_checks.csv")
print(checks, n = Inf)
message("For 2026, SB and CS totals should match the earlier run (3,338 and 965). ",
        "'Attempts tied to a specific pitch' should be above ~90%.")
