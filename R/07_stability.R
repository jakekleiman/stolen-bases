# =============================================================================
# 07_stability.R  -  Is any of this a real, repeatable skill?
# -----------------------------------------------------------------------------
# Compares results across seasons. Each season is a copy of the project folder
# (same scripts, R/config.R set to that season), already run through 01-06.
#
# For every pair of back-to-back seasons (2023->24, 2024->25, 2025->26) we line up
# the same teams / runners / catchers / pitchers and ask: if you were good at this
# one year, were you good at it the next? We pool all the pairs together, which
# gives roughly three times as many comparisons as a single pair of seasons.
#
#   correlation near 1 = a real, repeatable skill
#   correlation near 0 = mostly luck
#
# Run this from the LATEST season's folder:  source("R/07_stability.R")
# Seasons whose folders don't exist yet are skipped with a message.
# =============================================================================

source("R/helpers.R")

# Folder for each season, relative to this (latest) season's folder.
# Only use seasons from 2023 on: the pitch clock, the two-disengagement limit and
# bigger bases all started in 2023, so earlier seasons aren't comparable.
SEASON_DIRS <- c("2023" = "../2023", "2024" = "../2024", "2025" = "../2025", "2026" = ".")

has_tables <- function(d) file.exists(file.path(d, "outputs/tables/04_team_grades.csv"))
available  <- SEASON_DIRS[vapply(SEASON_DIRS, has_tables, logical(1))]
missing    <- setdiff(names(SEASON_DIRS), names(available))
if (length(missing)) message("Skipping seasons with no results yet: ", paste(missing, collapse = ", "))
yrs <- sort(as.integer(names(available)))
pairs <- lapply(seq_along(yrs)[-1], function(i) c(yrs[i - 1], yrs[i]))
pairs <- Filter(function(p) p[2] - p[1] == 1, pairs)          # back-to-back seasons only
if (!length(pairs)) stop("Need at least two back-to-back seasons with results.")
pair_labels <- vapply(pairs, function(p) paste0(p[1], "-", p[2] %% 100), character(1))
message("Comparing: ", paste(pair_labels, collapse = ", "))

read_season <- function(yr, file) {
  read_csv(file.path(available[[as.character(yr)]], "outputs/tables", file),
           show_col_types = FALSE)
}

per_att <- function(d) {
  if (all(c("decision_runs", "attempts") %in% names(d)))
    d <- d %>% mutate(decision_per_100  = 100 * decision_runs / attempts,
                      execution_per_100 = 100 * execution_runs / attempts)
  d
}

# Stack every back-to-back pair of one table, matched on `key`
stack_pairs <- function(file, key, min_n_col = NULL, min_n = 0) {
  bind_rows(lapply(seq_along(pairs), function(i) {
    p <- pairs[[i]]
    a <- per_att(read_season(p[1], file)) %>% select(-any_of(c("name", "team_name")))
    b <- per_att(read_season(p[2], file)) %>% select(-any_of(c("name", "team_name")))
    if (key != "team") { a <- select(a, -any_of("team")); b <- select(b, -any_of("team")) }
    j <- inner_join(a, b, by = key, suffix = c("_prev", "_next"))
    if (!is.null(min_n_col))
      j <- j %>% filter(.data[[paste0(min_n_col, "_prev")]] >= min_n,
                        .data[[paste0(min_n_col, "_next")]] >= min_n)
    # Center every number on that season's league average before pooling.
    # Otherwise a league-wide shift between seasons (e.g. everyone running more
    # in 2024) can fake a correlation, or hide one, when the pairs are stacked.
    num_cols <- names(j)[grepl("_(prev|next)$", names(j)) & vapply(j, is.numeric, logical(1))]
    j %>% mutate(across(all_of(num_cols), ~ .x - mean(.x, na.rm = TRUE)),
                 pair = pair_labels[i])
  }))
}

corr_row <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) <= 3) return(c(n = sum(ok), r = NA, lo = NA, hi = NA))
  ct <- cor.test(x[ok], y[ok])
  c(n = sum(ok), r = unname(ct$estimate), lo = ct$conf.int[1], hi = ct$conf.int[2])
}

stability <- function(stacked, metrics, level) {
  metrics <- metrics[paste0(metrics, "_prev") %in% names(stacked) &
                     paste0(metrics, "_next") %in% names(stacked)]
  bind_rows(lapply(metrics, function(m) {
    pooled <- corr_row(stacked[[paste0(m, "_prev")]], stacked[[paste0(m, "_next")]])
    by_pair <- vapply(pair_labels, function(pl) {
      s <- stacked[stacked$pair == pl, ]
      corr_row(s[[paste0(m, "_prev")]], s[[paste0(m, "_next")]])[["r"]]
    }, numeric(1))
    tibble(level = level, metric = m, n = pooled[["n"]],
           correlation = pooled[["r"]], ci_low = pooled[["lo"]], ci_high = pooled[["hi"]],
           !!!setNames(as.list(round(by_pair, 2)), paste0("r_", pair_labels)))
  }))
}

# ---- Each level -------------------------------------------------------------------------
team_g  <- stack_pairs("04_team_grades.csv", "team")
team_a  <- stack_pairs("05_team_aggressiveness.csv", "team")
runners <- stack_pairs("04_runner_grades.csv", "runner", "attempts", 10)
catch   <- stack_pairs("04_catcher_grades.csv", "catcher", "attempts_against", 20)
pitch   <- stack_pairs("05_pitcher_deterrence.csv", "pitcher", "opportunities", 300)

stab <- bind_rows(
  stability(team_g, c("success_rate", "pct_good_decisions", "decision_per_100",
                      "execution_per_100", "runs_added"), "Team"),
  stability(team_a, c("attempts_per_100", "selectivity", "rate_in_best_spots"), "Team"),
  stability(runners, c("success_rate", "decision_per_100", "execution_per_100"),
            "Runner (10+ attempts both years)"),
  stability(catch, c("cs_rate", "runs_saved_vs_expected"), "Catcher (20+ both years)"),
  stability(pitch, c("deterrence_pct", "prevented_per_100"),
            "Pitcher (300+ opportunity pitches both years)")) %>%
  mutate(across(c(correlation, ci_low, ci_high), ~ round(.x, 2)),
         reading = case_when(is.na(correlation) ~ "not enough data",
                             ci_low > 0.3 ~ "looks like a real skill",
                             ci_low > 0 ~ "some carryover",
                             TRUE ~ "can't tell it apart from luck"))
write_csv(stab, "outputs/tables/06_year_to_year_stability.csv")
print(stab, n = 40, width = Inf)

# ---- Chart: team approach, one season vs. the next (all pairs pooled) --------------------
strength <- function(r) ifelse(r >= 0.4, "carries over", ifelse(r >= 0.2, "carries over a little", "barely carries over"))
m_often <- "How often they ran"
m_picky <- "How well they picked their spots"
stab_df <- bind_rows(
  team_a %>% transmute(team, pair, measure = m_often,
                       prev = attempts_per_100_prev, nxt = attempts_per_100_next),
  team_a %>% transmute(team, pair, measure = m_picky,
                       prev = 100 * selectivity_prev, nxt = 100 * selectivity_next)) %>%
  group_by(measure) %>% mutate(r = cor(prev, nxt, use = "complete.obs")) %>% ungroup()
r_often <- stab_df$r[stab_df$measure == m_often][1]
r_picky <- stab_df$r[stab_df$measure == m_picky][1]
lab <- function(m, r) sprintf("%s\n%s (correlation %.2f)", m, strength(r), r)
stab_df <- stab_df %>%
  mutate(measure = factor(ifelse(measure == m_often, lab(m_often, r_often), lab(m_picky, r_picky)),
                          levels = c(lab(m_often, r_often), lab(m_picky, r_picky))))

p_stab <- ggplot(stab_df, aes(prev, nxt)) +
  geom_hline(yintercept = 0, color = "grey80") +
  geom_vline(xintercept = 0, color = "grey80") +
  geom_smooth(method = "lm", se = FALSE, color = RED, linewidth = 1, formula = y ~ x) +
  geom_point(color = NAVY, size = 2.2, alpha = 0.65) +
  facet_wrap(~ measure, scales = "free") +
  labs(title = "Teams keep how often they run, but not how well they pick spots",
       subtitle = paste("Each dot is a team, comparing one season with the next.",
                        "If the dots follow the red line upward, the habit carries over"),
       x = "This season  (right of the grey line = above league average)",
       y = "Next season  (above the grey line = above average)",
       caption = paste0("Data: MLB Stats API, Baseball Savant | ", min(yrs), "-", max(yrs), " regular seasons")) +
  theme_sb() + theme(strip.text = element_text(face = "bold", size = 12),
                     axis.text = element_blank(), panel.grid = element_blank(),
                     panel.spacing.x = unit(2, "lines"))
save_fig(p_stab, "06_year_to_year_teams.png", w = 10, h = 5.8)
