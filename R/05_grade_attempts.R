# =============================================================================
# 05_grade_attempts.R  -  Step 4
# -----------------------------------------------------------------------------
# 1. Estimate each attempt's chance of success BEFORE it happened.
#    "Decision model" inputs:
#      - runner: sprint speed, and how big a jump he usually gets (Savant)
#      - pitcher: how much of a jump runners usually get on him (Savant), his hand
#      - catcher: pop time (Savant) and his own track record
#      - situation: base, outs, count, pickoff throws already made this PA
#    No runner-specific term, so a runner's own luck can't leak into it, and
#    every attempt is predicted by a model that never saw it (5-fold CV).
# 2. Grade every attempt:
#      Runs added     = what the play actually did, including extra bases on
#                       bad throws (observed outcome, count- and hitter-aware)
#      Decision value = what the attempt was worth before it happened
#      Execution      = Runs added - Decision value
# 3. Roll it up by team (all play types), runner and catcher, with 90% ranges.
# =============================================================================

source("R/helpers.R")
suppressPackageStartupMessages(library(lme4))
set.seed(SEASON)

# Grading uses classic bases/outs run expectancy. The count- and hitter-aware
# versions (step 1) are shown as charts, but they were too noisy to grade
# individual attempts: a steal's value is a small difference between two big
# numbers, and small errors in each made break-even rates jump around.
re_obj   <- readRDS("data/model/re24.rds")
wp       <- readRDS("data/model/wp_model.rds")
attempts <- readRDS("data/clean/attempts.rds")
players  <- readRDS("data/clean/players.rds")

source("R/model_inputs.R")

message("Reading season-level inputs...")
lead_pat <- c("lead.*gain", "gain.*lead", "sec.*minus.*prim", "secondary.*lead", "lead")
inputs <- list(
  sprint       = read_lb("sprint_speed", "^sprint_speed$"),
  runner_lead  = read_lb("runner_running", lead_pat),
  pitcher_lead = read_lb("pitcher_running", lead_pat),
  catcher_pop  = read_lb("catcher_poptime", c("pop_2b_sba$", "pop_2b", "pop")))
saveRDS(inputs, "data/model/running_inputs.rds")

# ---- Model data ----------------------------------------------------------------
att_all <- attempts %>%
  mutate(state = state_str(b1, b2, b3), bat_lead = bat_score - fld_score,
         play_type = case_when(target == "HP" ~ "Steal of home",
                               n_runners_stealing > 1 ~ "Double/triple steal",
                               state == "1_3" & target == "2B" ~ "First-and-third steal of 2nd",
                               TRUE ~ "Standard steal"))

att <- att_all %>% filter(play_type == "Standard steal") %>% add_inputs(inputs)
ref <- att %>% select(sprint_speed, runner_lead, pitcher_lead, catcher_pop)
saveRDS(ref, "data/model/input_reference.rds")
att <- scale_inputs(att, ref)

# Only use an input if Savant gave us enough of it
extra <- c(if (mean(!is.na(att$runner_lead)) > 0.5) "runlead_z",
           if (mean(!is.na(att$pitcher_lead)) > 0.5) "pitlead_z",
           if (mean(!is.na(att$catcher_pop)) > 0.5) "pop_z")
if (length(extra) < 3) message("Note: some Savant inputs were unavailable and are left out: ",
                               paste(setdiff(c("runlead_z", "pitlead_z", "pop_z"), extra), collapse = ", "))
fixed <- c("target", "outs_f", "pitch_hand", "balls_f", "strikes_f", "pk_f", "sprint_z", extra)
f_decision <- as.formula(paste("success ~", paste(fixed, collapse = " + "), "+ (1 | catcher)"))
f_full     <- as.formula(paste("success ~", paste(fixed, collapse = " + "), "+ (1 | runner) + (1 | catcher)"))
f_base     <- as.formula(paste("success ~", paste(setdiff(fixed, "pop_z"), collapse = " + ")))  # ignores the catcher
saveRDS(list(fixed = fixed, f_decision = f_decision), "data/model/model_spec.rds")
ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

# ---- 1. Decision model, predicted out of sample -----------------------------------
message("Cross-validating the decision model (", nrow(att), " attempts, 5 folds)...")
att$fold <- sample(rep(1:5, length.out = nrow(att)))
att$p_hat <- NA_real_; att$p_base <- NA_real_
for (k in 1:5) {
  tr <- att$fold != k; te <- att$fold == k
  m  <- glmer(f_decision, data = att[tr, ], family = binomial, control = ctrl)
  mb <- glm(f_base, data = att[tr, ], family = binomial)
  att$p_hat[te]  <- predict(m, newdata = att[te, ], type = "response", allow.new.levels = TRUE)
  att$p_base[te] <- predict(mb, newdata = att[te, ], type = "response")
}
fit_dec <- glmer(f_decision, data = att, family = binomial, control = ctrl)
saveRDS(fit_dec, "data/model/success_model.rds")
print(summary(fit_dec))

# How much does each input matter? Shown as the change in success chance (percentage
# points) from a 1 SD change, starting from the league average
fit_full <- glmer(f_full, data = att, family = binomial, control = ctrl)
fe <- fixef(fit_full); vc <- as.data.frame(VarCorr(fit_full))
lab <- c(sprint_z = "Runner sprint speed (1 SD faster)",
         runlead_z = "Runner's usual jump (1 SD bigger)",
         pitlead_z = "Pitcher allows bigger jumps (1 SD)",
         pop_z = "Catcher pop time (1 SD slower)",
         target3B = "Stealing 3rd instead of 2nd",
         outs_f2 = "Two outs instead of none",
         pitch_handR = "Right-handed pitcher instead of lefty",
         `pk_f2+` = "Pitcher already used 2+ pickoffs/step-offs this PA",
         strikes_f2 = "Two strikes instead of none")
effects <- tibble(term = names(lab), effect = unname(lab)) %>%
  mutate(log_odds = unname(fe[term])) %>%
  filter(!is.na(log_odds)) %>%
  bind_rows(tibble(term = c("catcher_sd", "runner_sd"),
                   effect = c("Catcher's own track record (1 SD worse arm)",
                              "Runner beyond his speed and jump (1 SD better)"),
                   log_odds = c(vc$sdcor[vc$grp == "catcher"], vc$sdcor[vc$grp == "runner"]))) %>%
  mutate(pct_points = round(100 * (plogis(qlogis(mean(att$success)) + log_odds) - mean(att$success)), 1)) %>%
  arrange(desc(abs(pct_points)))
write_csv(effects %>% mutate(log_odds = round(log_odds, 3)), "outputs/tables/04_model_effects.csv")
print(effects)

# ---- Out-of-sample check ------------------------------------------------------------
base_rate <- mean(att$success)
logloss <- function(p, y) -mean(y * log(p) + (1 - y) * log(1 - p))
model_check <- tibble(
  metric = c("Log loss: always guess league rate", "Log loss: decision model (out of sample)",
             "Brier: always guess league rate", "Brier: decision model (out of sample)",
             "Improvement over league rate (log loss, %)",
             "Spread of predictions: 10th percentile", "Spread of predictions: 90th percentile"),
  value = c(logloss(base_rate, att$success), logloss(att$p_hat, att$success),
            mean((base_rate - att$success)^2), mean((att$p_hat - att$success)^2),
            100 * (1 - logloss(att$p_hat, att$success) / logloss(base_rate, att$success)),
            quantile(att$p_hat, 0.1), quantile(att$p_hat, 0.9))) %>%
  mutate(value = round(value, 4))
write_csv(model_check, "outputs/tables/04_model_out_of_sample.csv")
print(model_check)

calib <- att %>%
  mutate(bin = cut(p_hat, breaks = c(0, .55, .6, .65, .7, .75, .8, .85, .9, 1), include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarise(attempts = n(), predicted = mean(p_hat), actual = mean(success), .groups = "drop")
write_csv(calib %>% mutate(across(where(is.numeric), ~ round(.x, 3))),
          "outputs/tables/04_model_calibration.csv")
print(calib)

p_cal <- ggplot(calib, aes(predicted, actual)) +
  geom_abline(color = GREY, linetype = "dashed") +
  geom_point(aes(size = attempts), color = NAVY) +
  scale_x_continuous(labels = scales::percent, limits = c(0.4, 1)) +
  scale_y_continuous(labels = scales::percent, limits = c(0.2, 1)) +
  scale_size_area(max_size = 8) +
  labs(title = "Does the model's confidence match reality?",
       subtitle = "Out-of-sample predictions, grouped. On the dashed line = perfectly calibrated",
       x = "Predicted chance of success", y = "Actual success rate", size = "Attempts",
       caption = CAPTION) +
  theme_sb()
save_fig(p_cal, "04_model_calibration.png", w = 7, h = 6)

# ---- 2. Grade every standard attempt ---------------------------------------------------
att <- att %>%
  mutate(target = as.character(target)) %>%
  add_steal_values(re_obj, wp) %>%
  mutate(
    runs_added       = observed_run_value(., re_obj),           # what really happened
    runs_added_ideal = ifelse(success, run_gain, -run_loss),    # safe/out only
    runs_expected    = p_hat * run_gain - (1 - p_hat) * run_loss,
    runs_execution   = runs_added - runs_expected,
    runs_exp_base    = p_base * run_gain - (1 - p_base) * run_loss,   # ignores the catcher
    wpa              = ifelse(success, wp_gain, -wp_loss),
    wpa_expected     = p_hat * wp_gain - (1 - p_hat) * wp_loss,
    good_decision    = p_hat > be_runs,
    edge             = p_hat - be_runs,
    late_close       = inning >= 7 & abs(bat_lead) <= 1)
saveRDS(att, "data/model/graded_attempts.rds")

# ---- Every other play type: graded on what actually happened -----------------------
# One row per play (a double steal is one play), so runs aren't double counted
other_plays <- att_all %>%
  filter(play_type != "Standard steal") %>%
  group_by(game_pk, at_bat_index, play_index) %>%
  slice(1) %>% ungroup() %>%
  mutate(runs_added = observed_run_value(., re_obj))
other_summary <- other_plays %>%
  group_by(play_type) %>%
  summarise(plays = n(), avg_runs_per_play = mean(runs_added), total_runs = sum(runs_added),
            .groups = "drop")
write_csv(other_summary %>% mutate(across(where(is.double), ~ round(.x, 3))),
          "outputs/tables/04_other_attempts.csv")
print(other_summary)

name_of <- function(ids) players$name[match(ids, players$player_id)]
boot_ci <- function(x, B = 2000) {
  if (length(x) < 2) return(c(NA_real_, NA_real_))
  s <- replicate(B, sum(sample(x, replace = TRUE)))
  unname(quantile(s, c(0.05, 0.95)))
}

# ---- 3. Team grades --------------------------------------------------------------------
other_by_team <- other_plays %>%
  group_by(team = bat_team) %>%
  summarise(other_plays = n(), other_runs = sum(runs_added), .groups = "drop")

team <- att %>%
  group_by(team = bat_team) %>%
  summarise(ci = list(boot_ci(runs_added)),          # before runs_added is summed below
            attempts = n(), sb = sum(success), cs = sum(!success),
            success_rate = mean(success),
            avg_breakeven = mean(be_runs, na.rm = TRUE),
            avg_pred_success = mean(p_hat),
            pct_good_decisions = mean(good_decision, na.rm = TRUE),
            pct_batter_swung = mean(batter_swung),
            runs_added = sum(runs_added),
            decision_runs = sum(runs_expected),
            execution_runs = sum(runs_execution),
            ci_exec = list(boot_ci(runs_execution)),
            wins_added = sum(wpa),
            late_close_attempts = sum(late_close),
            late_close_wins_added = sum(wpa[late_close]),
            .groups = "drop") %>%
  mutate(runs_added_lo = map_dbl(ci, 1), runs_added_hi = map_dbl(ci, 2),
         execution_lo = map_dbl(ci_exec, 1), execution_hi = map_dbl(ci_exec, 2),
         interval_includes_zero = runs_added_lo <= 0 & runs_added_hi >= 0) %>%
  select(-ci, -ci_exec) %>%
  left_join(other_by_team, by = "team") %>%
  mutate(other_plays = coalesce(other_plays, 0L), other_runs = coalesce(other_runs, 0),
         runs_added_all_plays = runs_added + other_runs) %>%
  arrange(desc(runs_added))
write_csv(team %>% mutate(across(where(is.double), ~ round(.x, 3))),
          "outputs/tables/04_team_grades.csv")
print(team, n = 30)

p_team <- team %>%
  mutate(team = reorder(team, runs_added)) %>%
  ggplot(aes(y = team)) +
  geom_vline(xintercept = 0, color = "grey40") +
  geom_segment(aes(x = runs_added_lo, xend = runs_added_hi, yend = team),
               color = "grey75", linewidth = 2.2, lineend = "round") +
  geom_point(aes(x = decision_runs, color = "Decision value (what the attempts were worth before the pitch)"),
             shape = 21, fill = "white", size = 2.6, stroke = 1.1) +
  geom_point(aes(x = runs_added, color = "Runs added (what actually happened)"), size = 3) +
  scale_color_manual(values = c("Runs added (what actually happened)" = NAVY,
                                "Decision value (what the attempts were worth before the pitch)" = RED),
                     breaks = c("Runs added (what actually happened)",
                                "Decision value (what the attempts were worth before the pitch)")) +
  guides(color = guide_legend(ncol = 1)) +
  labs(title = paste("Runs added by stealing,", SEASON),
       subtitle = "Standard steals of 2nd and 3rd. Grey bar = 90% range for runs added; a bar that crosses 0 could be luck",
       x = "Runs", y = NULL, color = NULL, caption = CAPTION) +
  theme_sb() + theme(panel.grid.major.y = element_blank())
save_fig(p_team, "04_team_runs_added.png", w = 9, h = 9)

# Success rate by how many pickoffs/step-offs the pitcher had already used
# (read by 06_opportunities.R for the disengagement chart)
succ_pk <- att %>%
  group_by(pk_f) %>%
  summarise(attempts = n(), success_rate = mean(success), .groups = "drop")
write_csv(succ_pk %>% mutate(success_rate = round(success_rate, 3)),
          "outputs/tables/04_success_by_disengagements.csv")

# Were teams pickier with nobody out? Compare the model's PRE-PITCH success chance
# (out-of-sample) for steals of 2nd from 1st, by outs. Higher predicted success with
# nobody out = teams only sent their best chances then.
pick_by_outs <- att %>%
  filter(state == "1__", target == "2B") %>%
  group_by(outs) %>%
  summarise(attempts         = n(),
            actual_success   = round(mean(success), 3),
            predicted_success = round(mean(p_hat), 3),
            avg_sprint_speed = round(mean(sprint_speed, na.rm = TRUE), 2),
            avg_pitcher_lead_allowed = round(mean(pitcher_lead, na.rm = TRUE), 2),
            .groups = "drop")
write_csv(pick_by_outs, "outputs/tables/04_pickiness_by_outs.csv")
print(pick_by_outs)

# ---- Runner grades (10+ attempts) ---------------------------------------------------
runners <- att %>%
  group_by(runner) %>%
  summarise(team = paste(unique(bat_team), collapse = "/"),
            attempts = n(), sb = sum(success), cs = sum(!success),
            success_rate = mean(success), sprint_speed = first(sprint_speed),
            runner_lead = first(runner_lead),
            avg_breakeven = mean(be_runs, na.rm = TRUE), avg_pred_success = mean(p_hat),
            runs_added = sum(runs_added), decision_runs = sum(runs_expected),
            execution_runs = sum(runs_execution), wins_added = sum(wpa),
            pct_batter_swung = mean(batter_swung),
            .groups = "drop") %>%
  filter(attempts >= 10) %>%
  mutate(name = name_of(runner)) %>% relocate(name) %>%
  arrange(desc(runs_added))
write_csv(runners %>% mutate(across(where(is.double), ~ round(.x, 3))),
          "outputs/tables/04_runner_grades.csv")

top_bot <- bind_rows(head(runners, 10) %>% mutate(group = "Top 10"),
                     tail(runners, 10) %>% mutate(group = "Bottom 10")) %>%
  distinct(runner, .keep_all = TRUE) %>%
  mutate(group = factor(group, levels = c("Top 10", "Bottom 10")),
         label = paste0(name, " (", sb, "-for-", attempts, ")"),
         label = reorder(label, runs_added))
part_levels <- c("Total runs added", "Decision\n(picking the spot)", "Execution\n(beating the odds)")
p_run <- top_bot %>%
  transmute(label, group,
            `Total runs added` = runs_added,
            `Decision\n(picking the spot)` = decision_runs,
            `Execution\n(beating the odds)` = execution_runs) %>%
  pivot_longer(-c(label, group), names_to = "part", values_to = "runs") %>%
  mutate(part = factor(part, levels = part_levels),
         sign = ifelse(runs >= 0, "plus", "minus")) %>%
  ggplot(aes(runs, label, fill = sign)) +
  geom_vline(xintercept = 0, color = "grey40") +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%+.1f", runs), hjust = ifelse(runs >= 0, -0.15, 1.15)),
            size = 3, color = "grey25") +
  scale_fill_manual(values = c(plus = NAVY, minus = RED), guide = "none") +
  scale_x_continuous(expand = expansion(mult = 0.3)) +
  facet_grid(group ~ part, scales = "free_y", space = "free_y") +
  labs(title = "Best and worst base stealers by runs added",
       subtitle = "Total = decision + execution. Minimum 10 attempts",
       x = "Runs", y = NULL, caption = CAPTION) +
  theme_sb() +
  theme(panel.grid.major.y = element_blank(), panel.spacing.x = unit(1.2, "lines"),
        strip.text = element_text(face = "bold"), strip.text.y = element_text(angle = 0))
save_fig(p_run, "04_runner_runs_added.png", w = 11.5, h = 7.5)

# ---- Catchers: runs saved vs. a model that ignores who was catching ----------------
catchers <- att %>%
  filter(catcher != "unknown") %>%
  group_by(catcher) %>%
  summarise(attempts_against = n(), caught = sum(!success), cs_rate = mean(!success),
            pop_time = first(catcher_pop),
            runs_saved = -sum(runs_added),
            runs_saved_vs_expected = -sum(runs_added - runs_exp_base),
            ci = list(boot_ci(runs_added - runs_exp_base)), .groups = "drop") %>%
  mutate(rsve_lo = -map_dbl(ci, 2), rsve_hi = -map_dbl(ci, 1)) %>% select(-ci) %>%
  filter(attempts_against >= 20) %>%
  mutate(name = name_of(catcher)) %>% relocate(name) %>%
  arrange(desc(runs_saved_vs_expected))
write_csv(catchers %>% mutate(across(where(is.double), ~ round(.x, 3))),
          "outputs/tables/04_catcher_grades.csv")

# ---- Headline numbers ------------------------------------------------------------
lg_ci <- boot_ci(att$runs_added)
headline <- tibble(
  metric = c("Standard attempts graded", "League success rate",
             "Average break-even rate on those attempts",
             "Share of attempts that were good decisions (pred. success > break-even)",
             "Runs added, standard steals (league)", "  90% range: low", "  90% range: high",
             "Runs added, all other steal plays (league)",
             "Runs added if graded as simply safe/out (league)",
             "Wins added, standard steals (league)",
             "Late & close attempts (7th inning on, within 1 run)",
             "Attempts where the batter swung (possible hit-and-run), %",
             "Teams whose 90% range for runs added excludes zero",
             "Team runs added: best", "Team runs added: worst"),
  value = c(nrow(att), mean(att$success), mean(att$be_runs, na.rm = TRUE),
            mean(att$good_decision, na.rm = TRUE),
            sum(att$runs_added), lg_ci[1], lg_ci[2],
            sum(other_plays$runs_added), sum(att$runs_added_ideal),
            sum(att$wpa), sum(att$late_close), 100 * mean(att$batter_swung),
            sum(!team$interval_includes_zero),
            max(team$runs_added), min(team$runs_added))) %>%
  mutate(value = round(value, 3))
write_csv(headline, "outputs/tables/04_headline.csv")
print(headline)
