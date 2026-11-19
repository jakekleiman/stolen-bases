# =============================================================================
# 06_opportunities.R  -  Step 5: who runs, who doesn't, and does the pitcher matter?
# -----------------------------------------------------------------------------
# Every PITCH thrown with a runner on 1st (2nd open) or on 2nd (3rd open) is an
# opportunity. For each one we estimate what an attempt would have been worth:
#
#   edge = predicted success chance - break-even rate (count- and hitter-aware)
#
# Then:
#   a) Do runners go more in better spots? (two charts, one free of the
#      "fast guys run more" circularity)
#   b) Team aggressiveness vs. selectivity
#   c) Runners who rarely go in great spots
#   d) "You steal off the pitcher": does the pitcher change whether runners GO?
#   e) Sensitivity check: the model only learns from real attempts (when
#      runners got a good jump), so it's too optimistic about spots where
#      nobody ran. We re-run the rankings assuming those spots were worse and
#      see which conclusions survive.
# =============================================================================

source("R/helpers.R")
source("R/model_inputs.R")
suppressPackageStartupMessages(library(lme4))

re_obj  <- readRDS("data/model/re24.rds")   # classic run expectancy (see step 4 note)
wp      <- readRDS("data/model/wp_model.rds")
fit     <- readRDS("data/model/success_model.rds")
inputs  <- readRDS("data/model/running_inputs.rds")
ref     <- readRDS("data/model/input_reference.rds")
players <- readRDS("data/clean/players.rds")
opps    <- readRDS("data/clean/opportunities.rds")
name_of <- function(ids) players$name[match(ids, players$player_id)]

opps <- opps %>%
  mutate(state = state_str(b1, b2, b3), bat_lead = bat_score - fld_score) %>%
  filter(!(state == "1_3" & target == "2B")) %>%     # first-and-third: its own play
  add_inputs(inputs) %>%
  scale_inputs(ref)

opps$p_hat <- predict(fit, newdata = opps, type = "response", allow.new.levels = TRUE)

opps <- opps %>%
  mutate(target = as.character(target)) %>%
  add_steal_values(re_obj, wp) %>%
  filter(!is.na(be_runs)) %>%
  mutate(edge     = p_hat - be_runs,
         ev_runs  = p_hat * run_gain - (1 - p_hat) * run_loss,
         edge_pct = percent_rank(edge),
         tier     = case_when(edge_pct >= 0.90 ~ "Best 10% of spots",
                              edge_pct <= 0.50 ~ "Worst half of spots",
                              TRUE ~ "In between"))
saveRDS(opps, "data/model/graded_opportunities.rds")
message(nrow(opps), " runner-pitch opportunities graded.")

# ---- a) Do runners go more in better spots? -----------------------------------------
by_decile <- opps %>%
  mutate(decile = ntile(edge, 10)) %>%
  group_by(decile) %>%
  summarise(opportunities = n(), attempt_rate = mean(attempted),
            avg_pred_success = mean(p_hat), avg_breakeven = mean(be_runs),
            avg_edge = mean(edge), .groups = "drop")
write_csv(by_decile %>% mutate(across(where(is.double), ~ round(.x, 4))),
          "outputs/tables/05_attempt_rate_by_edge_decile.csv")

p_dec <- ggplot(by_decile, aes(factor(decile), attempt_rate)) +
  geom_col(fill = NAVY, width = 0.7) +
  geom_text(aes(label = scales::percent(attempt_rate, accuracy = 0.1)), vjust = -0.4, size = 3.5) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Do runners go more often in better spots?",
       subtitle = paste("Every", SEASON, "pitch with a runner who could steal, in 10 groups from worst spot (1) to best (10)"),
       x = "Opportunity quality (decile)", y = "How often the runner went on that pitch", caption = CAPTION) +
  theme_sb()
save_fig(p_dec, "05_attempt_rate_by_decile.png")

# Free of circularity: rate the spot by the game situation only (its break-even
# rate, which ignores who is running), within speed groups
by_situation <- opps %>%
  mutate(speed_group = factor(c("Slower third", "Middle third", "Fastest third")[ntile(sprint_z, 3)],
                              levels = c("Slower third", "Middle third", "Fastest third")),
         be_group = cut(be_runs, breaks = c(-Inf, 0.65, 0.70, 0.73, 0.76, 0.80, Inf),
                        labels = c("Under 65%", "65-70%", "70-73%", "73-76%", "76-80%", "Over 80%"))) %>%
  filter(!is.na(be_group)) %>%
  group_by(speed_group, be_group) %>%
  summarise(opportunities = n(), attempt_rate = mean(attempted), .groups = "drop")
write_csv(by_situation %>% mutate(attempt_rate = round(attempt_rate, 4)),
          "outputs/tables/05_attempt_rate_by_situation_and_speed.csv")
print(by_situation, n = 30)

p_sit <- ggplot(by_situation, aes(be_group, attempt_rate, color = speed_group, group = speed_group)) +
  geom_line(linewidth = 1.1) + geom_point(size = 3) +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
  scale_color_manual(values = c("Slower third" = "#b9cde3", "Middle third" = "#6f9cc4", "Fastest third" = NAVY)) +
  labs(title = "Runners avoid bad spots, but speed decides who goes",
       subtitle = "Share of pitches a runner took off on, by how good the game situation was for stealing (runner speed groups)",
       x = "Break-even rate of the situation (lower = better spot to run)",
       y = "How often the runner went on that pitch", color = NULL, caption = CAPTION) +
  theme_sb()
save_fig(p_sit, "05_attempt_rate_by_situation.png", w = 9.5, h = 5.5)

# ---- b) Team aggressiveness vs. selectivity ------------------------------------------
team_summary <- function(o) {
  o %>%
    group_by(team = bat_team) %>%
    summarise(opportunities = n(), attempts = sum(attempted),
              attempts_per_100 = 100 * mean(attempted),
              rate_in_best_spots  = mean(attempted[tier == "Best 10% of spots"]),
              rate_in_worst_spots = mean(attempted[tier == "Worst half of spots"]),
              avg_edge_when_running = mean(edge[attempted]),
              avg_edge_all_spots    = mean(edge),
              best_spots = sum(tier == "Best 10% of spots"),
              best_spots_passed = sum(tier == "Best 10% of spots" & !attempted),
              worst_spot_attempts = sum(tier == "Worst half of spots" & attempted),
              .groups = "drop") %>%
    mutate(selectivity = avg_edge_when_running - avg_edge_all_spots)
}
team <- team_summary(opps) %>% arrange(desc(attempts_per_100))
write_csv(team %>% mutate(across(where(is.double), ~ round(.x, 4))),
          "outputs/tables/05_team_aggressiveness.csv")
print(team, n = 30)

suppressPackageStartupMessages(library(ggrepel))
mx <- median(team$attempts_per_100); my <- median(team$selectivity)
corner <- function(x, y, lab, hj, vj, col)
  annotate("text", x = x, y = y, label = lab, hjust = hj, vjust = vj,
           fontface = "bold", color = col, size = 3.8)
p_quad <- ggplot(team, aes(attempts_per_100, 100 * selectivity)) +
  geom_vline(xintercept = mx, color = GREY, linetype = "dashed") +
  geom_hline(yintercept = 100 * my, color = GREY, linetype = "dashed") +
  geom_point(color = NAVY, size = 3) +
  geom_text_repel(aes(label = team), size = 3.3, seed = 1, box.padding = 0.35, min.segment.length = 0.2) +
  corner(Inf,  Inf, "Runs a lot, picks good spots", 1.05, 1.5, NAVY) +
  corner(-Inf, Inf, "Runs rarely, picks good spots", -0.05, 1.5, GREY) +
  corner(Inf, -Inf, "Runs a lot, less picky", 1.05, -0.8, RED) +
  corner(-Inf, -Inf, "Runs rarely, less picky", -0.05, -0.8, GREY) +
  scale_y_continuous(expand = expansion(mult = 0.12)) +
  labs(title = "Who runs, and who runs at the right time?",
       subtitle = "Across: attempts per 100 pitches with a runner who could go.\nUp: how much better its chosen spots were than its average spot.",
       x = "Attempts per 100 opportunity pitches",
       y = "Selectivity: how much better its chosen spots were\n(success-rate points above its average spot)",
       caption = CAPTION) +
  theme_sb()
save_fig(p_quad, "05_team_aggressiveness_quadrant.png", w = 9.5, h = 7)

# ---- c) Runners who rarely go in great spots ----------------------------------------------
runner_summary <- function(o) {
  o %>%
    group_by(runner) %>%
    summarise(team = paste(unique(bat_team), collapse = "/"),
              sprint_speed = first(sprint_speed), runner_lead = first(runner_lead),
              opportunities = n(), attempts = sum(attempted),
              best_spots = sum(tier == "Best 10% of spots"),
              went_in_best_spots = mean(attempted[tier == "Best 10% of spots"]),
              .groups = "drop")
}
runners <- runner_summary(opps) %>%
  filter(best_spots >= 50) %>%
  mutate(name = name_of(runner)) %>% relocate(name) %>%
  arrange(went_in_best_spots)
write_csv(runners %>% mutate(across(where(is.double), ~ round(.x, 3))),
          "outputs/tables/05_runner_passivity.csv")
print(head(runners, 15))

# ---- d) "You steal off the pitcher": does the pitcher change whether runners GO? ------
# Attempt model: did the runner go on this pitch? Controls for the runner's
# speed and usual jump, the catcher, the count, outs and the game situation.
go_terms <- c("sprint_z", "runlead_z", "pitlead_z", "pop_z", "be_runs", "outs_f",
              "balls_f", "strikes_f", "pk_f", "pitch_hand", "target")
go_terms <- go_terms[vapply(go_terms, function(v) length(unique(opps[[v]])) > 1, logical(1))]
fit_go <- glm(as.formula(paste("attempted ~", paste(go_terms, collapse = " + "))),
              data = opps, family = binomial)
base_go <- mean(opps$attempted)
go_lab <- c(pitlead_z = "Pitcher allows bigger jumps (1 SD)",
            pop_z = "Catcher pop time (1 SD slower)",
            sprint_z = "Runner sprint speed (1 SD faster)",
            runlead_z = "Runner's usual jump (1 SD bigger)",
            pitch_handR = "Right-handed pitcher instead of lefty",
            `pk_f2+` = "Pitcher already used 2+ pickoffs/step-offs this PA",
            strikes_f2 = "Two strikes instead of none",
            balls_f3 = "Three balls instead of none")
go_effects <- tibble(term = names(go_lab), effect = unname(go_lab)) %>%
  mutate(log_odds = unname(coef(fit_go)[term])) %>%
  filter(!is.na(log_odds)) %>%
  mutate(change_in_attempt_rate_pct_points =
           round(100 * (plogis(qlogis(base_go) + log_odds) - base_go), 2),
         relative_change_pct = round(100 * (exp(log_odds) - 1), 1)) %>%
  arrange(desc(abs(relative_change_pct)))
write_csv(go_effects %>% mutate(log_odds = round(log_odds, 3)),
          "outputs/tables/05_what_makes_runners_go.csv")
print(go_effects)

# ---- Disengagements: does a pitcher who has already stepped off twice get run on? ----
pk_lab <- c("0" = "None", "1" = "One", "2+" = "Two\n(a third that fails is a balk)")
go_pk <- opps %>%
  group_by(pk_f) %>%
  summarise(n = n(), value = mean(attempted), .groups = "drop") %>%
  mutate(panel = "How often the runner took off")
succ_file <- "outputs/tables/04_success_by_disengagements.csv"
if (file.exists(succ_file)) {
  succ_pk <- read_csv(succ_file, show_col_types = FALSE) %>%
    transmute(pk_f = as.character(pk_f), n = attempts, value = success_rate,
              panel = "How often he was safe")
  pk_df <- bind_rows(go_pk %>% mutate(pk_f = as.character(pk_f)), succ_pk)
} else {
  pk_df <- go_pk %>% mutate(pk_f = as.character(pk_f))
}
pk_df <- pk_df %>%
  mutate(pk_lab = factor(pk_lab[pk_f], levels = pk_lab),
         panel = factor(panel, levels = c("How often the runner took off", "How often he was safe")),
         label = ifelse(panel == "How often he was safe",
                        scales::percent(value, accuracy = 1),
                        scales::percent(value, accuracy = 0.1)))
write_csv(pk_df %>% select(panel, pk_f, n, value) %>% mutate(value = round(value, 4)),
          "outputs/tables/05_disengagements.csv")
print(pk_df)

p_pk <- ggplot(pk_df, aes(pk_lab, value, fill = pk_f == "2+")) +
  geom_col(width = 0.65) +
  geom_text(aes(label = label), vjust = -0.4, size = 4) +
  scale_fill_manual(values = c(`FALSE` = "#8fb3d6", `TRUE` = NAVY), guide = "none") +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.15))) +
  facet_wrap(~ panel, scales = "free_y") +
  labs(title = "Once a pitcher has stepped off twice, runners go",
       subtitle = "By how many pickoff throws or step-offs the pitcher had already used in the plate appearance",
       x = "Pickoffs or step-offs already used", y = NULL, caption = CAPTION) +
  theme_sb() + theme(panel.grid.major.x = element_blank())
save_fig(p_pk, "05_disengagements.png", w = 10, h = 5.5)

# Pitchers: how often runners went against them vs. what the runners, catchers
# and situations would predict
fit_go_np <- glm(as.formula(paste("attempted ~", paste(setdiff(go_terms, c("pitlead_z", "pitch_hand")), collapse = " + "))),
                 data = opps, family = binomial)
opps$go_expected <- predict(fit_go_np, type = "response")
pitchers <- opps %>%
  group_by(pitcher) %>%
  summarise(opportunities = n(), attempts_against = sum(attempted),
            expected_attempts = sum(go_expected),
            attempts_vs_expected = attempts_against - expected_attempts,
            lead_allowed = first(pitcher_lead), .groups = "drop") %>%
  filter(opportunities >= 300) %>%
  mutate(name = name_of(pitcher), deterrence_pct = round(100 * (1 - attempts_against / expected_attempts), 1),
         prevented_per_100 = 100 * (expected_attempts - attempts_against) / opportunities) %>%
  relocate(name) %>%
  arrange(desc(deterrence_pct))
write_csv(pitchers %>% mutate(across(where(is.double), ~ round(.x, 2))),
          "outputs/tables/05_pitcher_deterrence.csv")

if ("pitlead_z" %in% go_terms) {
  # Per-100-chances scale avoids the lopsided percentages of the old version
  pit_plot <- pitchers %>% filter(!is.na(lead_allowed))
  named <- c("Shohei Ohtani", "Luke Weaver", "Paul Skenes", "Eury Pérez", "José Alvarado")
  pit_lab <- bind_rows(pit_plot %>% filter(name %in% named),
                       pit_plot %>% slice_max(prevented_per_100, n = 3),
                       pit_plot %>% slice_min(prevented_per_100, n = 3)) %>%
    distinct(pitcher, .keep_all = TRUE)
  r_pit <- cor(pit_plot$lead_allowed, pit_plot$prevented_per_100)
  yr <- range(pit_plot$prevented_per_100)
  p_pit <- ggplot(pit_plot, aes(lead_allowed, prevented_per_100)) +
    geom_hline(yintercept = 0, color = GREY) +
    geom_point(color = NAVY, alpha = 0.45, size = 2) +
    geom_smooth(method = "lm", se = FALSE, color = RED, linewidth = 1.1, formula = y ~ x) +
    geom_point(data = pit_lab, color = NAVY, size = 2.6) +
    ggrepel::geom_text_repel(data = pit_lab, aes(label = name), size = 3.3, seed = 2,
                             box.padding = 0.4, min.segment.length = 0) +
    annotate("text", x = -Inf, y = yr[2], hjust = -0.05, vjust = 1, size = 3.6, color = "grey30",
             label = "↑ Runners stayed put more than expected") +
    annotate("text", x = -Inf, y = yr[1], hjust = -0.05, vjust = 0, size = 3.6, color = "grey30",
             label = "↓ Runners went more than expected") +
    labs(title = "Pitchers who hold runners see far fewer attempts",
         subtitle = sprintf("Each dot is a pitcher with 300+ chances for a runner to go. Correlation: %.2f", r_pit),
         x = "Average jump runners got against him (feet of lead gained)\nFurther right = worse at holding runners",
         y = "Attempts prevented per 100 chances\n(expected minus actual)", caption = CAPTION) +
    theme_sb() + theme(plot.title.position = "plot")
  save_fig(p_pit, "05_pitcher_deterrence.png", w = 9, h = 6)

  # Simpler article version: for the pitchers runners avoided most and least
  # (per 100 chances), how many attempts they faced vs. how many were expected
  pit_ends <- bind_rows(
    pit_plot %>% slice_max(prevented_per_100, n = 8) %>% mutate(group = "Runners stayed put"),
    pit_plot %>% slice_min(prevented_per_100, n = 8) %>% mutate(group = "Runners ran wild")) %>%
    mutate(group = factor(group, levels = c("Runners stayed put", "Runners ran wild")),
           name = reorder(name, -prevented_per_100))
  p_pit2 <- ggplot(pit_ends, aes(y = name)) +
    geom_segment(aes(x = expected_attempts, xend = attempts_against, yend = name),
                 color = "grey75", linewidth = 1.6) +
    geom_point(aes(x = expected_attempts, color = "Expected attempts"), size = 3.6) +
    geom_point(aes(x = attempts_against, color = "Actual attempts"), size = 3.6) +
    scale_color_manual(values = c("Expected attempts" = "grey55", "Actual attempts" = NAVY),
                       breaks = c("Expected attempts", "Actual attempts")) +
    facet_grid(group ~ ., scales = "free_y", space = "free_y") +
    labs(title = "Some pitchers shut the running game down. Others invite it",
         subtitle = paste("Steal attempts each pitcher faced vs. how many an average pitcher would have faced",
                          "with the same runners, catchers and situations"),
         x = "Steal attempts against", y = NULL, color = NULL, caption = CAPTION) +
    theme_sb() +
    theme(panel.grid.major.y = element_blank(), strip.text.y = element_text(angle = 0, face = "bold"))
  save_fig(p_pit2, "05_pitcher_actual_vs_expected.png", w = 9.5, h = 7)
}

# ---- e) Sensitivity check -------------------------------------------------------------
# Suppose spots where nobody ran were really worse than the model thinks:
# lower the predicted success of every non-attempt by a set amount (in
# log-odds) and see how much the team rankings and runner lists change.
shift_tiers <- function(o, delta) {
  o %>%
    mutate(p_adj = ifelse(attempted, p_hat, plogis(qlogis(p_hat) - delta)),
           edge  = p_adj - be_runs,
           edge_pct = percent_rank(edge),
           tier = case_when(edge_pct >= 0.90 ~ "Best 10% of spots",
                            edge_pct <= 0.50 ~ "Worst half of spots",
                            TRUE ~ "In between"))
}
base_team <- team %>% select(team, best0 = rate_in_best_spots, sel0 = selectivity)
base_pass <- head(runners$runner, 15)
sens <- bind_rows(lapply(c(0.15, 0.30, 0.50), function(d) {
  o  <- shift_tiers(opps, d)
  tm <- team_summary(o) %>% inner_join(base_team, by = "team")
  rn <- runner_summary(o) %>% filter(best_spots >= 50) %>% arrange(went_in_best_spots)
  tibble(shift_log_odds = d,
         approx_pct_points_lower = round(100 * (mean(opps$p_hat[!opps$attempted]) -
                                                mean(plogis(qlogis(opps$p_hat[!opps$attempted]) - d))), 1),
         team_rank_corr_best_spot_rate = cor(tm$rate_in_best_spots, tm$best0, method = "spearman"),
         team_rank_corr_selectivity    = cor(tm$selectivity, tm$sel0, method = "spearman"),
         passive_runner_list_overlap   = mean(base_pass %in% head(rn$runner, 15)))
}))
write_csv(sens %>% mutate(across(where(is.double), ~ round(.x, 3))),
          "outputs/tables/05_sensitivity_check.csv")
print(sens)
message("Sensitivity: correlations near 1 and overlap near 1 = conclusions hold even if ",
        "the model is too optimistic about spots where nobody ran.")
