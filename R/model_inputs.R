# =============================================================================
# model_inputs.R  -  Season-level inputs (Savant) and how they enter the models
# Shared by steps 4 and 5 so both use exactly the same inputs.
# =============================================================================

# ---- Season-level inputs from Baseball Savant ----------------------------------------
# Finds the id and value columns by name, so small changes in Savant's exports
# don't break anything. Uses this season, falling back to last season.
read_lb <- function(name, value_patterns, id_patterns = c("^player_id$", "^entity_id$", "_id$", "^id$")) {
  out <- NULL
  for (yr in c(SEASON, SEASON - 1)) {
    f <- sprintf("data/raw/%s_%d.csv", name, yr)
    if (!file.exists(f)) next
    d <- read_csv(f, show_col_types = FALSE, progress = FALSE)
    nm <- tolower(names(d))
    idc <- NA; for (pt in id_patterns) { h <- which(grepl(pt, nm)); if (length(h)) { idc <- h[1]; break } }
    vc  <- NA; for (pt in value_patterns) { h <- which(grepl(pt, nm) & vapply(d, is.numeric, logical(1))); if (length(h)) { vc <- h[1]; break } }
    if (is.na(idc) || is.na(vc)) {
      warning(f, ": couldn't find id/value columns. Columns: ", paste(names(d), collapse = ", "))
      next
    }
    message("  ", basename(f), ": using '", names(d)[vc], "' (id = '", names(d)[idc], "')")
    x <- tibble(id = as.character(d[[idc]]), value = as.numeric(d[[vc]])) %>%
      filter(!is.na(id), is.finite(value)) %>% distinct(id, .keep_all = TRUE)
    out <- if (is.null(out)) x else bind_rows(out, anti_join(x, out, by = "id"))
  }
  out %||% tibble(id = character(), value = numeric())
}

# z-score helper with league mean/sd saved for step 5
zs <- function(x, ref) (x - mean(ref, na.rm = TRUE)) / sd(ref, na.rm = TRUE)

add_inputs <- function(df, inputs) {
  look <- function(tab, ids) tab$value[match(ids, tab$id)]
  df %>%
    mutate(sprint_speed  = look(inputs$sprint, runner),
           runner_lead   = look(inputs$runner_lead, runner),
           pitcher_lead  = look(inputs$pitcher_lead, pitcher),
           catcher_pop   = look(inputs$catcher_pop, catcher))
}

scale_inputs <- function(df, ref) {
  # Missing = league average (0 after scaling), except sprint speed: a runner
  # with no sprint speed on record is usually not a regular runner, so a bit slow
  df %>%
    mutate(sprint_z  = coalesce(zs(sprint_speed, ref$sprint_speed), -1),
           runlead_z = coalesce(zs(runner_lead, ref$runner_lead), 0),
           pitlead_z = coalesce(zs(pitcher_lead, ref$pitcher_lead), 0),
           pop_z     = coalesce(zs(catcher_pop, ref$catcher_pop), 0),
           catcher   = coalesce(catcher, "unknown"),
           pitcher   = coalesce(pitcher, "unknown"),
           pitch_hand = coalesce(pitch_hand, "R"),
           outs_f    = factor(outs, levels = 0:2),
           balls_f   = factor(pmin(balls, 3), levels = 0:3),
           strikes_f = factor(pmin(strikes, 2), levels = 0:2),
           pk_f      = factor(pmin(pickoffs_before, 2), levels = 0:2, labels = c("0", "1", "2+")),
           target    = factor(target, levels = c("2B", "3B")))
}

