# =============================================================================
# 04_win_probability.R  -  Step 3
# -----------------------------------------------------------------------------
# Late in close games one run can matter much more (or less) than average, so
# break-even rates based on runs can mislead. This builds a win probability
# model from the season's run distributions and recomputes break-even rates in
# terms of wins for every inning, score, outs and base state.
# =============================================================================

source("R/helpers.R")

re24    <- readRDS("data/model/re24.rds")
re_dist <- readRDS("data/model/re_dist.rds")

wp <- build_wp_model(re_dist)
saveRDS(wp, "data/model/wp_model.rds")

# ---- Sanity checks ------------------------------------------------------------
sanity <- tibble(
  situation = c("Start of game (home team)",
                "Bottom 9, tied, nobody on, 0 outs (home team)",
                "Top 9, home up 1, nobody on, 0 outs (home team)",
                "Bottom 9, home down 1, bases loaded, 2 outs (home team)",
                "Tied game going to extras (home team)"),
  home_wp = c(wp$wp_home(1, "top", 0, "___", 0),
              wp$wp_home(9, "bottom", 0, "___", 0),
              wp$wp_home(9, "top", 0, "___", 1),
              wp$wp_home(9, "bottom", 2, "123", -1),
              wp$extras_home_wp)) %>%
  mutate(home_wp = round(home_wp, 3))
print(sanity)
write_csv(sanity, "outputs/tables/03_wp_sanity_checks.csv")

# ---- Break-even (wins) for stealing 2nd with a runner on 1st only ------------
grid <- expand_grid(inning = 1:9, half = c("top", "bottom"), outs = 0:2,
                    bat_lead = -4:4, state = "1__", target = "2B") %>%
  filter(!(inning == 9 & half == "bottom" & bat_lead > 0)) %>%
  add_steal_values(re24, wp)

write_csv(grid %>% select(inning, half, outs, bat_lead, state, target,
                          be_runs, be_wp, wp_pre, wp_gain, wp_loss) %>%
            mutate(across(where(is.numeric), ~ round(.x, 4))),
          "outputs/tables/03_breakeven_wp_grid.csv")

plot_grid <- function(h) {
  grid %>%
    filter(half == h) %>%
    mutate(outs_lab = paste(outs, ifelse(outs == 1, "out", "outs"))) %>%
    # A break-even of 100%+ means being on 2nd instead of 1st adds essentially
    # nothing there, so no success rate makes the risk worth it
    # NA here means being on 2nd instead of 1st gains (almost) nothing
    mutate(label = case_when(is.na(be_wp) | be_wp >= 0.995 ~ "—",
                             TRUE ~ sprintf("%.0f", 100 * be_wp)),
           fill_val = ifelse(is.na(be_wp), 1, pmin(be_wp, 1))) %>%
    ggplot(aes(factor(inning), factor(bat_lead), fill = fill_val)) +
    geom_tile(color = "white") +
    geom_text(aes(label = label), size = 3) +
    scale_fill_gradient2(low = NAVY, mid = "white", high = RED,
                         midpoint = 0.75, limits = c(0.5, 1),
                         breaks = c(0.6, 0.7, 0.8, 0.9, 1),
                         labels = scales::percent, na.value = "grey90") +
    guides(fill = guide_colorbar(barwidth = unit(14, "lines"))) +
    facet_wrap(~ outs_lab) +
    labs(title = paste0("Break-even rate for stealing 2nd (",
                        ifelse(h == "top", "visiting", "home"), " team batting)"),
         subtitle = "Runner on 1st only, based on win probability. — = not worth it at any success rate",
         x = "Inning", y = "Batting team's lead", fill = "Break-even",
         caption = CAPTION) +
    theme_sb() + theme(panel.grid = element_blank())
}
save_fig(plot_grid("top"),    "03_breakeven_wp_top.png",    w = 11, h = 5.5)
save_fig(plot_grid("bottom"), "03_breakeven_wp_bottom.png", w = 11, h = 5.5)

# Simpler version for the article: a line per score, visiting team batting
p_lines <- grid %>%
  filter(half == "top", bat_lead %in% c(-2, -1, 0, 1)) %>%
  mutate(be_plot = ifelse(be_wp >= 0.995, NA, be_wp),
         outs_lab = paste(outs, ifelse(outs == 1, "out", "outs")),
         score = factor(case_when(bat_lead == 0 ~ "Tied", bat_lead > 0 ~ paste("Up", bat_lead),
                                  TRUE ~ paste("Down", abs(bat_lead))),
                        levels = c("Down 2", "Down 1", "Tied", "Up 1"))) %>%
  ggplot(aes(inning, be_plot, color = score)) +
  geom_line(linewidth = 1.1) + geom_point(size = 1.6) +
  scale_x_continuous(breaks = 1:9) +
  scale_y_continuous(labels = scales::percent, limits = c(0.55, 1), breaks = seq(0.6, 1, 0.1)) +
  scale_color_manual(values = c("Down 2" = RED, "Down 1" = "#e0a39c", "Tied" = NAVY, "Up 1" = "#8fb3d6")) +
  facet_wrap(~ outs_lab) +
  labs(title = "Late in close games, the break-even rate swings both ways",
       subtitle = "Success rate needed to steal 2nd (runner on 1st only, visiting team batting), based on win probability",
       x = "Inning", y = "Break-even success rate", color = "Batting team is:", caption = CAPTION) +
  theme_sb() + theme(strip.text = element_text(face = "bold", size = 12))
save_fig(p_lines, "03_breakeven_wp_lines.png", w = 10, h = 5)

# A few headline situations for the article
headline <- grid %>%
  filter((inning == 1 & bat_lead == 0) |
         (inning %in% c(8, 9) & bat_lead %in% c(-1, 0, 1)) |
         (inning == 7 & bat_lead %in% c(-3, 3))) %>%
  select(inning, half, outs, bat_lead, be_runs, be_wp) %>%
  mutate(across(c(be_runs, be_wp), ~ round(.x, 3))) %>%
  arrange(outs, inning, half, bat_lead)
write_csv(headline, "outputs/tables/03_breakeven_headline_situations.csv")
print(headline, n = 60)
