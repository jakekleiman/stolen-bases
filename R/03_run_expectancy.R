# =============================================================================
# 03_run_expectancy.R  -  Steps 1 and 2
# -----------------------------------------------------------------------------
# Step 1: Run expectancy. Average runs scored from each bases/outs situation to
#         the end of the inning. Also: versions that know the ball-strike count
#         and how good the hitter at the plate is.
# Step 2: Break-even success rates for every steal situation, how often teams
#         actually ran and succeeded in each (with 90% ranges), and how the
#         break-even moves with the count and the hitter.
# =============================================================================

source("R/helpers.R")

pa       <- readRDS("data/clean/pa.rds")
pitches  <- readRDS("data/clean/pitches.rds")
attempts <- readRDS("data/clean/attempts.rds")

# Complete half innings only; skip the bottom of the 9th and extras (walk-offs
# cut those short, and extras start with a free runner).
use_half <- function(df) df %>% filter(half_complete, inning <= 8 | (inning == 9 & half == "top"))

# ---- Step 1a: classic run expectancy (bases + outs) --------------------------------
re_base <- pa %>%
  use_half() %>%
  mutate(state = state_str(!is.na(b1_id), !is.na(b2_id), !is.na(b3_id)), outs = outs_start)

re24 <- re_base %>%
  group_by(state, outs) %>%
  summarise(re = mean(runs_roi), n = n(), .groups = "drop") %>%
  mutate(state = factor(state, levels = ALL_STATES)) %>%
  arrange(state, outs) %>% mutate(state = as.character(state))

# Full distribution of runs from each state, smoothed in the rare 3+ run tail
re_dist_raw <- re_base %>%
  mutate(runs = pmin(runs_roi, 10)) %>%
  count(state, outs, runs) %>%
  group_by(state, outs) %>% mutate(prob = n / sum(n)) %>% ungroup()
re_dist <- smooth_run_dist(re_dist_raw)

# ---- Step 1b: run expectancy that knows the count and the hitter --------------------
pitches_use <- pitches %>%
  use_half() %>%
  mutate(state = state_str(!is.na(b1_id), !is.na(b2_id), !is.na(b3_id)))
re_obj <- build_re_full(pitches_use, re24)

saveRDS(re24, "data/model/re24.rds")
saveRDS(re_dist, "data/model/re_dist.rds")
saveRDS(re_obj, "data/model/re_full.rds")

re24_wide <- re24 %>% select(state, outs, re) %>% mutate(re = round(re, 3)) %>%
  pivot_wider(names_from = outs, values_from = re, names_prefix = "outs_")
write_csv(re24_wide, "outputs/tables/01_re24.csv")
write_csv(re_obj$re_tier %>% mutate(across(where(is.double), ~ round(.x, 3))),
          "outputs/tables/01_re_by_hitter_tier.csv")
print(re24_wide)

state_labels <- c("___" = "Empty", "1__" = "1st", "_2_" = "2nd", "__3" = "3rd",
                  "12_" = "1st & 2nd", "1_3" = "1st & 3rd", "_23" = "2nd & 3rd",
                  "123" = "Loaded")
p1 <- re24 %>%
  mutate(state_lab = factor(state_labels[state], levels = rev(state_labels)),
         outs_lab  = paste(outs, ifelse(outs == 1, "out", "outs"))) %>%
  ggplot(aes(outs_lab, state_lab, fill = re)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.2f", re)), size = 4.5) +
  scale_fill_gradient(low = "#eef3f8", high = "#6f9cc4", guide = "none") +
  labs(title = paste(SEASON, "run expectancy"),
       subtitle = "Average runs scored from this point to the end of the inning",
       x = NULL, y = "Runners on", caption = CAPTION) +
  theme_sb() + theme(panel.grid = element_blank())
save_fig(p1, "01_re24.png", w = 7, h = 6)

# ---- Step 2a: break-even by situation (average count, average hitter) ---------------
be <- expand_grid(state = ALL_STATES, outs = 0:2, target = c("2B", "3B", "HP")) %>%
  filter(can_steal(state, target))
be <- bind_cols(be, steal_states(be$state, be$outs, be$target)) %>%
  mutate(re_pre   = re_lookup(re24, state, outs),
         re_succ  = runs_succ + re_lookup(re24, state_succ, outs_succ),
         re_fail  = re_lookup(re24, state_fail, outs_fail),
         run_gain = re_succ - re_pre,
         run_loss = re_pre - re_fail,
         be_runs  = run_loss / (run_gain + run_loss))

actual <- attempts %>%
  filter(n_runners_stealing == 1) %>%
  mutate(state = state_str(b1, b2, b3)) %>%
  group_by(state, outs, target) %>%
  summarise(attempts = n(), successes = sum(success), success_rate = mean(success), .groups = "drop")

wilson <- function(x, n, z = 1.645) {
  p <- x / n; d <- 1 + z^2 / n
  m <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))
  cbind(lo = (p + z^2 / (2 * n) - m) / d, hi = (p + z^2 / (2 * n) + m) / d)
}

be_table <- be %>%
  left_join(actual, by = c("state", "outs", "target")) %>%
  mutate(attempts = coalesce(attempts, 0L), successes = coalesce(successes, 0L),
         situation = paste0(state_labels[state], ", ", outs, " out",
                            ifelse(outs == 1, "", "s"), " -> steal ", target))
ci <- wilson(be_table$successes, pmax(be_table$attempts, 1))
be_table <- be_table %>%
  mutate(success_lo = ifelse(attempts > 0, ci[, "lo"], NA),
         success_hi = ifelse(attempts > 0, ci[, "hi"], NA),
         runs_total    = attempts * (success_rate * run_gain - (1 - success_rate) * run_loss),
         runs_total_lo = attempts * (success_lo * run_gain - (1 - success_lo) * run_loss),
         runs_total_hi = attempts * (success_hi * run_gain - (1 - success_hi) * run_loss),
         clearly_above_breakeven = success_lo > be_runs,
         clearly_below_breakeven = success_hi < be_runs) %>%
  select(situation, state, outs, target, re_pre, run_gain, run_loss, be_runs,
         attempts, success_rate, success_lo, success_hi,
         runs_total, runs_total_lo, runs_total_hi,
         clearly_above_breakeven, clearly_below_breakeven) %>%
  arrange(target, desc(attempts))
write_csv(be_table %>% mutate(across(where(is.double), ~ round(.x, 3))),
          "outputs/tables/02_breakeven_runs.csv")
print(be_table %>% select(situation, attempts, be_runs, success_rate, success_lo, success_hi, runs_total), n = 40)

common <- be_table %>% filter(target != "HP", attempts >= 25, !(state == "1_3" & target == "2B"))
base_names <- c("1__" = "Runner on 1st", "_2_" = "Runner on 2nd", "12_" = "1st & 2nd")
p2_data <- common %>%
  mutate(nice = paste0(coalesce(base_names[state], state_labels[state]), ", ",
                       outs, " out", ifelse(outs == 1, "", "s"), ": steal ",
                       ifelse(target == "2B", "2nd", "3rd")),
         situation = paste0(nice, "  (", scales::comma(attempts), ")"),
         situation = reorder(situation, attempts),
         small = attempts < 50)
p2 <- ggplot(p2_data, aes(y = situation)) +
  geom_segment(aes(x = success_lo, xend = success_hi, yend = situation, alpha = small),
               color = NAVY, linewidth = 0.8) +
  geom_point(aes(x = be_runs, color = "Break-even rate", alpha = small), shape = 18, size = 5) +
  geom_point(aes(x = success_rate, color = "Actual success rate (line = 90% range)", alpha = small), size = 3.4) +
  scale_alpha_manual(values = c(`FALSE` = 1, `TRUE` = 0.35), guide = "none") +
  scale_x_continuous(labels = scales::percent, limits = c(0.4, 1)) +
  scale_color_manual(values = c("Break-even rate" = RED, "Actual success rate (line = 90% range)" = NAVY)) +
  labs(title = "How often do you need to be safe?",
       subtitle = "Break-even rate vs. what runners actually did\nAttempts in parentheses. Faded rows have fewer than 50, so read them loosely",
       x = "Success rate", y = NULL, color = NULL, caption = CAPTION) +
  theme_sb() + theme(panel.grid.major.y = element_blank(), plot.title.position = "plot")
save_fig(p2, "02_breakeven_vs_actual.png", w = 9.5, h = 6)

# Article version: only the most common steal (runner on 1st, stealing 2nd), by outs
p2b_data <- be_table %>%
  filter(state == "1__", target == "2B") %>%
  mutate(outs_lab = factor(paste(outs, ifelse(outs == 1, "out", "outs")),
                           levels = c("2 outs", "1 out", "0 outs")),
         gap = success_rate - be_runs,
         verdict = ifelse(gap >= 0, "Runners beat the bar", "Runners fell short"),
         label = paste0(scales::comma(attempts), " attempts\n",
                        sprintf("%+.0f runs", runs_total)))
p2b <- ggplot(p2b_data, aes(y = outs_lab)) +
  geom_segment(aes(x = success_lo, xend = success_hi, yend = outs_lab),
               color = NAVY, linewidth = 1.2, alpha = 0.5) +
  geom_point(aes(x = be_runs, color = "Break-even rate"), shape = 18, size = 7) +
  geom_point(aes(x = success_rate, color = "Actual success rate (line = 90% range)"), size = 5) +
  geom_text(aes(x = 0.81, label = label), hjust = 0, size = 3.8, color = "grey25", lineheight = 0.95) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.64, 0.86), breaks = seq(0.64, 0.80, 0.04)) +
  scale_color_manual(values = c("Break-even rate" = RED, "Actual success rate (line = 90% range)" = NAVY)) +
  labs(title = "Nobody out is the best time to steal second",
       subtitle = "Runner on 1st stealing 2nd: the success rate needed to break even vs. what runners did",
       x = "Success rate", y = NULL, color = NULL, caption = CAPTION) +
  theme_sb() + theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size = 13, face = "bold"))
save_fig(p2b, "02_breakeven_steal2nd_by_outs.png", w = 9.5, h = 4.8)

# ---- Step 2b: break-even by count (runner on 1st only, stealing 2nd) -----------------
counts <- expand_grid(balls = 0:3, strikes = 0:2)
be_count <- expand_grid(state = "1__", outs = 0:2, target = "2B", counts, batter_tier = "Middle")
be_count <- bind_cols(be_count, steal_states(be_count$state, be_count$outs, be_count$target)) %>%
  mutate(re_pre   = re_full_lookup(re_obj, state, outs, balls, strikes, batter_tier),
         re_succ  = re_full_lookup(re_obj, state_succ, outs_succ, balls, strikes, batter_tier),
         re_fail  = re_full_lookup(re_obj, state_fail, outs_fail, balls, strikes, batter_tier),
         be_runs  = (re_pre - re_fail) / ((re_succ - re_pre) + (re_pre - re_fail)),
         count    = paste0(balls, "-", strikes))
write_csv(be_count %>% select(outs, count, be_runs) %>% mutate(be_runs = round(be_runs, 3)),
          "outputs/tables/02_breakeven_by_count.csv")

p_count <- be_count %>%
  mutate(outs_lab = paste(outs, ifelse(outs == 1, "out", "outs")),
         count = factor(count, levels = paste0(rep(0:3, each = 3), "-", rep(0:2, 4)))) %>%
  ggplot(aes(count, outs_lab, fill = be_runs)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.0f", 100 * be_runs)), size = 3.6) +
  scale_fill_gradient2(low = NAVY, mid = "white", high = RED, midpoint = 0.72,
                       labels = scales::percent) +
  labs(title = "The count changes the math too",
       subtitle = "Break-even % for stealing 2nd (runner on 1st only, average hitter). Blue = easier to justify, red = harder",
       x = "Count (balls-strikes)", y = NULL, fill = "Break-even",
       caption = paste0("Estimates come from smoothed count-level run values, so treat single cells as approximate.\n", CAPTION)) +
  theme_sb() +
  theme(panel.grid = element_blank(),
        legend.key.width = unit(1.6, "cm"), legend.key.height = unit(0.35, "cm"))
save_fig(p_count, "02_breakeven_by_count.png", w = 10, h = 4)

# ---- Step 2c: break-even by who's batting ---------------------------------------------
be_tier <- expand_grid(state = "1__", outs = 0:2, target = "2B", balls = 0L, strikes = 0L,
                       batter_tier = c("Weak", "Middle", "Strong"))
be_tier <- bind_cols(be_tier, steal_states(be_tier$state, be_tier$outs, be_tier$target)) %>%
  mutate(re_pre  = re_full_lookup(re_obj, state, outs, balls, strikes, batter_tier),
         re_succ = re_full_lookup(re_obj, state_succ, outs_succ, balls, strikes, batter_tier),
         re_fail = re_full_lookup(re_obj, state_fail, outs_fail, balls, strikes, batter_tier),
         be_runs = (re_pre - re_fail) / ((re_succ - re_pre) + (re_pre - re_fail)))
write_csv(be_tier %>% select(outs, batter_tier, re_pre, be_runs) %>%
            mutate(across(where(is.double), ~ round(.x, 3))),
          "outputs/tables/02_breakeven_by_hitter.csv")
print(be_tier %>% select(outs, batter_tier, be_runs))

message("Steps 1-2 done.")
