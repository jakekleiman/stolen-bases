# =============================================================================
# helpers.R  -  Shared functions used by the analysis scripts
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(purrr)
})

`%||%` <- function(a, b) if (is.null(a)) b else a
source("R/config.R")

dir.create("outputs/tables",  recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("data/model",      recursive = TRUE, showWarnings = FALSE)

# ---- Base-out states ---------------------------------------------------------
# A state is a 3-character string: "1__" = runner on first, "_23" = second and
# third, "___" = empty, etc.
ALL_STATES <- c("___", "1__", "_2_", "__3", "12_", "1_3", "_23", "123")

state_str <- function(b1, b2, b3) {
  paste0(ifelse(b1, "1", "_"), ifelse(b2, "2", "_"), ifelse(b3, "3", "_"))
}

# Can a lone runner try to steal `target` from this state?
can_steal <- function(state, target) {
  on <- function(k) substr(state, k, k) != "_"
  ifelse(target == "2B", on(1) & !on(2),
  ifelse(target == "3B", on(2) & !on(3),
  ifelse(target == "HP", on(3), FALSE)))
}

# Idealized result of a steal attempt: on success the runner moves up one base
# (or scores), everyone else holds. On failure the runner is out.
steal_states <- function(state, outs, target) {
  n <- length(state)
  s_succ <- state; s_fail <- state; runs <- integer(n)
  from <- unname(c("2B" = 1L, "3B" = 2L, "HP" = 3L)[target])
  for (k in 1:3) {
    idx <- which(from == k)
    if (!length(idx)) next
    substr(s_fail[idx], k, k) <- "_"
    substr(s_succ[idx], k, k) <- "_"
    if (k < 3) substr(s_succ[idx], k + 1, k + 1) <- as.character(k + 1)
    else runs[idx] <- 1L
  }
  tibble(state_succ = s_succ, outs_succ = as.integer(outs), runs_succ = runs,
         state_fail = s_fail, outs_fail = as.integer(outs) + 1L)
}

# Run expectancy lookup (3 outs = 0 runs left in the inning)
re_lookup <- function(re24, state, outs) {
  key <- paste(re24$state, re24$outs)
  out <- re24$re[match(paste(state, outs), key)]
  out[outs >= 3] <- 0
  out
}

# ---- Run expectancy that also knows the count and who's batting ----------------
# re_count: average runs to the end of the inning by bases, outs AND count
#           (a steal on 3-2 with two outs is not the same as one on 0-2).
# re_tier:  how much more or less a strong / weak hitter at the plate changes
#           that number, as a ratio to the average hitter.
# Both are "shrunk" toward the plain bases-and-outs table so that rare
# combinations with few plate appearances don't produce noisy values.
build_re_full <- function(pitches_use, re24, k_count = 1000, k_tier = 1500) {
  base <- pitches_use %>% mutate(re_base = re_lookup(re24, state, outs))
  re_count <- base %>%
    group_by(state, outs, balls, strikes) %>%
    summarise(n = n(), mean_runs = mean(runs_roi), re_base = first(re_base), .groups = "drop") %>%
    mutate(re = (n * mean_runs + k_count * re_base) / (n + k_count))
  re_tier <- base %>%
    group_by(state, outs, batter_tier) %>%
    summarise(n = n(), mean_runs = mean(runs_roi), re_base = first(re_base), .groups = "drop") %>%
    mutate(re_shrunk = (n * mean_runs + k_tier * re_base) / (n + k_tier),
           ratio = ifelse(re_base > 0, re_shrunk / re_base, 1))
  list(re24 = re24, re_count = re_count, re_tier = re_tier)
}

re_full_lookup <- function(re_obj, state, outs, balls, strikes, tier) {
  kc <- paste(re_obj$re_count$state, re_obj$re_count$outs, re_obj$re_count$balls, re_obj$re_count$strikes)
  kt <- paste(re_obj$re_tier$state, re_obj$re_tier$outs, re_obj$re_tier$batter_tier)
  rc <- re_obj$re_count$re[match(paste(state, outs, balls, strikes), kc)]
  rc <- ifelse(is.na(rc), re_lookup(re_obj$re24, state, outs), rc)
  rt <- re_obj$re_tier$ratio[match(paste(state, outs, tier), kt)]
  out <- rc * coalesce(rt, 1)
  out[outs >= 3] <- 0
  out
}

# ---- Win probability model ---------------------------------------------------
# Built entirely from the season's run distributions: for every base-out state we
# know how often an inning from that point produced 0, 1, 2, ... runs. Chaining
# those half-innings together (with the extra-inning runner on second) gives the
# home team's chance of winning from any inning / half / outs / bases / score.
# It assumes two league-average teams.
build_wp_model <- function(re_dist, max_r = 10, max_d = 20) {
  D  <- -max_d:max_d
  nd <- length(D)
  r  <- 0:max_r

  dist_of <- function(st, o) {
    v <- numeric(max_r + 1)
    x <- re_dist[re_dist$state == st & re_dist$outs == o, ]
    v[x$runs + 1] <- x$prob
    if (sum(v) == 0) stop("No run distribution for state ", st, " ", o, " outs")
    v / sum(v)
  }
  P0 <- dist_of("___", 0)   # a normal inning
  Pg <- dist_of("_2_", 0)   # an extra inning (runner starts on second)

  # vec is a home win probability indexed by score diff D (home minus away)
  lookup <- function(vec, d) {
    out <- numeric(length(d))
    out[d >  max_d] <- 1
    out[d < -max_d] <- 0
    ok <- d >= -max_d & d <= max_d
    out[ok] <- vec[d[ok] + max_d + 1]
    out
  }

  # Tied after a full extra inning repeats forever: solve it directly
  S1 <- sum(sapply(r, function(a) Pg[a + 1] * sum(Pg[r > a])))
  S2 <- sum(Pg^2)
  X  <- S1 / (1 - S2)   # home WP when a game goes to extras tied
  outcome <- function(x) ifelse(x > 0, 1, ifelse(x == 0, X, 0))

  # F[i, ] = home WP at the start of the top of inning i
  # G[i, ] = home WP at the start of the bottom of inning i
  # Row 10 stands for every extra inning
  Fm <- matrix(NA_real_, 10, nd); Gm <- matrix(NA_real_, 10, nd)
  Gm[10, ] <- sapply(D, function(d) if (d > 0) 1 else sum(Pg * outcome(d + r)))
  Fm[10, ] <- sapply(D, function(d) sum(Pg * lookup(Gm[10, ], d - r)))
  Gm[9, ]  <- sapply(D, function(d) if (d > 0) 1 else sum(P0 * outcome(d + r)))
  Fm[9, ]  <- sapply(D, function(d) sum(P0 * lookup(Gm[9, ], d - r)))
  for (i in 8:1) {
    Gm[i, ] <- sapply(D, function(d) sum(P0 * lookup(Fm[i + 1, ], d + r)))
    Fm[i, ] <- sapply(D, function(d) sum(P0 * lookup(Gm[i, ], d - r)))
  }

  # Home WP the moment a half inning ends with score diff d
  end_top <- Gm
  end_bot <- rbind(Fm[2:9, ], outcome(D), outcome(D))

  # Home WP in the middle of an inning, for every state
  keys <- as.vector(outer(ALL_STATES, 0:2, paste))
  arr  <- array(NA_real_, dim = c(10, 2, length(keys), nd))
  for (k in seq_along(keys)) {
    st <- substr(keys[k], 1, 3); o <- as.integer(substr(keys[k], 5, 5))
    Ps <- dist_of(st, o)
    for (i in 1:10) {
      arr[i, 1, k, ] <- sapply(D, function(d) sum(Ps * lookup(end_top[i, ], d - r)))
      arr[i, 2, k, ] <- sapply(D, function(d) sum(Ps * lookup(end_bot[i, ], d + r)))
    }
  }

  # Vectorized: home team win probability
  wp_home <- function(inning, half, outs, state, d) {
    n  <- length(d)
    i  <- pmin(as.integer(inning), 10L)
    h  <- ifelse(half == "top", 1L, 2L)
    di <- pmax(pmin(d, max_d), -max_d) + max_d + 1
    out <- rep(NA_real_, n)
    is_end <- outs >= 3
    mid <- which(!is_end)
    k <- match(paste(state, outs), keys)
    out[mid] <- arr[cbind(i[mid], h[mid], k[mid], di[mid])]
    e1 <- which(is_end & h == 1L); out[e1] <- end_top[cbind(i[e1], di[e1])]
    e2 <- which(is_end & h == 2L); out[e2] <- end_bot[cbind(i[e2], di[e2])]
    out[d >  max_d] <- 1
    out[d < -max_d] <- 0
    out
  }

  # Batting team win probability, given the batting team's lead
  wp_bat <- function(inning, half, outs, state, bat_lead) {
    d_home <- ifelse(half == "bottom", bat_lead, -bat_lead)
    w <- wp_home(inning, half, outs, state, d_home)
    ifelse(half == "bottom", w, 1 - w)
  }

  list(wp_home = wp_home, wp_bat = wp_bat, extras_home_wp = X,
       P0 = P0, Pg = Pg)
}

# ---- Smoother run distributions for the win probability model ----------------
# The chance of a 3+ run inning from a rare state rests on few innings, and that
# noise once produced impossible break-even rates (over 100%). Here each state
# keeps its own chance of scoring 0, 1, 2 or 3+ runs, but HOW the 3+ innings are
# spread over 3, 4, 5... runs is shared by all states with the same outs.
smooth_run_dist <- function(re_dist, tail_from = 3) {
  shape <- re_dist %>%
    filter(runs >= tail_from) %>%
    group_by(outs, runs) %>% summarise(n = sum(n), .groups = "drop") %>%
    group_by(outs) %>% mutate(shape = n / sum(n)) %>% ungroup() %>% select(outs, runs, shape)
  head_part <- re_dist %>% filter(runs < tail_from) %>% select(state, outs, runs, prob)
  tail_mass <- re_dist %>% group_by(state, outs) %>%
    summarise(p_tail = sum(prob[runs >= tail_from]), .groups = "drop")
  tail_part <- tail_mass %>% inner_join(shape, by = "outs", relationship = "many-to-many") %>%
    mutate(prob = p_tail * shape) %>% select(state, outs, runs, prob)
  bind_rows(head_part, tail_part) %>% arrange(state, outs, runs)
}

# ---- Value of a steal attempt in runs and win probability --------------------
# `re` is the list from build_re_full(). When the data has the count
# (balls, strikes) and the hitter's tier (batter_tier), run values use them;
# otherwise they fall back to plain bases-and-outs run expectancy.
# Adds: re_pre, re_succ, re_fail, run_gain, run_loss, be_runs,
#       wp_pre, wp_succ, wp_fail, wp_gain, wp_loss, be_wp
add_steal_values <- function(df, re, wp) {
  tr <- steal_states(df$state, df$outs, df$target)
  df <- bind_cols(df, tr)
  full <- is.list(re) && !is.data.frame(re) &&
          all(c("balls", "strikes", "batter_tier") %in% names(df))
  rel <- function(st, o) {
    if (full) re_full_lookup(re, st, o, df$balls, df$strikes, df$batter_tier)
    else re_lookup(if (is.data.frame(re)) re else re$re24, st, o)
  }
  df %>%
    mutate(
      re_pre   = rel(state, outs),
      re_succ  = runs_succ + rel(state_succ, outs_succ),
      re_fail  = rel(state_fail, outs_fail),
      run_gain = re_succ - re_pre,
      run_loss = re_pre - re_fail,
      be_runs  = ifelse(run_gain + run_loss > 0, run_loss / (run_gain + run_loss), NA_real_),
      wp_pre   = wp$wp_bat(inning, half, outs,      state,      bat_lead),
      wp_succ  = wp$wp_bat(inning, half, outs_succ, state_succ, bat_lead + runs_succ),
      wp_fail  = wp$wp_bat(inning, half, outs_fail, state_fail, bat_lead),
      wp_gain  = wp_succ - wp_pre,
      wp_loss  = wp_pre - wp_fail,
      be_wp    = ifelse(wp_gain > 0.0005 & wp_gain + wp_loss > 0.002,
                        wp_loss / (wp_gain + wp_loss), NA_real_)
    )
}

# What the play actually did, in runs: runs that scored on it plus the change
# in run expectancy from the state before to the state after (any extra bases
# on a bad throw, a run scoring on a first-and-third play, etc.)
observed_run_value <- function(df, re) {
  post_state <- state_str(df$post_b1, df$post_b2, df$post_b3)
  full <- is.list(re) && !is.data.frame(re)
  rel <- function(st, o) {
    if (full) re_full_lookup(re, st, o, df$balls, df$strikes, df$batter_tier)
    else re_lookup(re, st, o)
  }
  pre  <- rel(df$state, df$outs)
  post <- rel(post_state, df$post_outs)
  df$runs_on_play + post - pre
}

# ---- Plot styling (matches the site's navy) -----------------------------------
NAVY <- "#1f4e79"; RED <- "#b2443a"; GREY <- "#8c8c8c"

theme_sb <- function() {
  theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16,
                                    margin = margin(b = 4)),
          plot.subtitle = element_text(color = "grey35", hjust = 0.5, lineheight = 1.1,
                                       margin = margin(b = 10)),
          plot.caption = element_text(color = "grey50", size = 9, hjust = 0.5,
                                      margin = margin(t = 10)),
          # Title, subtitle and caption are centered on the whole image, not just
          # the plotting area, so long axis labels can't push them off-center
          plot.title.position = "plot",
          plot.caption.position = "plot",
          axis.title = element_text(color = "grey25"),
          strip.text = element_text(face = "bold", size = 11),
          panel.grid.minor = element_blank(),
          legend.position = "top",
          legend.justification = "center",
          plot.margin = margin(15, 20, 10, 15))
}

# Wrap any text that is too long for the image width so nothing gets cut off.
# Rough characters-per-inch at each text size (base_size 13).
wrap_text <- function(x, width_chars) {
  if (is.null(x) || !is.character(x) || width_chars < 10) return(x)
  # Balanced wrapping: if a line is too long, split it into lines of roughly
  # equal length so we never leave one stray word on the last line
  balance <- function(line) {
    n <- nchar(line)
    if (n <= width_chars) return(line)
    n_lines <- ceiling(n / width_chars)
    target <- ceiling(n / n_lines) + 4
    paste(strwrap(line, target), collapse = "\n")
  }
  paste(vapply(strsplit(x, "\n", fixed = TRUE)[[1]], balance, character(1)),
        collapse = "\n")
}

save_fig <- function(plot, name, w = 9, h = 5.5) {
  usable <- w - 0.6                                   # minus plot margins
  lab <- tryCatch(plot$labels, error = function(e) list())
  new <- list(title    = wrap_text(lab$title,    floor(usable * 8.5)),
              subtitle = wrap_text(lab$subtitle, floor(usable * 12)),
              caption  = wrap_text(lab$caption,  floor(usable * 16)),
              x        = wrap_text(lab$x,        floor(usable * 11)),
              y        = wrap_text(lab$y,        floor(h * 8)))
  new <- new[!vapply(new, is.null, logical(1))]
  if (length(new)) plot <- plot + do.call(labs, new)
  ggsave(file.path("outputs/figures", name), plot, width = w, height = h,
         dpi = 200, bg = "white")
}

CAPTION <- paste0("Data: MLB Stats API, Baseball Savant | ", SEASON, " regular season")
