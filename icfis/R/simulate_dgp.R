# Shared data-generating process for the ICFIS talk. Sourced by icfis/app.R,
# icfis/analysis.qmd, and the case-study documents. This is the single
# implementation of the model; nothing else should redefine these functions.
#
# Requires dplyr, tidyr, and purrr to already be loaded (or namespaced) by
# the caller.

# Keeps qlogis() away from its undefined boundary at p = 0 or p = 1, in case
# a rate parameter is ever pushed to (or past) the edge of a slider/range.
safe_logit <- function(p, eps = 1e-4) {
  qlogis(pmin(pmax(p, eps), 1 - eps))
}

# Response model for one examiner-item pair.
#
# `inconclusive_rate` is the baseline inconclusive rate for different-source
# items. By default it is also used for same-source items (one shared
# baseline). Supply `inconclusive_rate_same` to give same-source items their
# own baseline; the Ames II cartridge-case data prefer separate baselines
# (roughly 50% different-source vs. 17% same-source -- see
# icfis/fit_ames2.qmd). Leaving it NULL reproduces the original model exactly.
simulate_response <- function(ground_truth,
                              decision_challenge,
                              examiner_inconclusive_tendency,
                              false_positive_rate,
                              false_negative_rate,
                              inconclusive_rate,
                              inconclusive_rate_same = NULL) {
  if (ground_truth == 1) {
    error_probability <- plogis(safe_logit(false_negative_rate) + decision_challenge)
    baseline_inconclusive_rate <- if (is.null(inconclusive_rate_same)) inconclusive_rate else inconclusive_rate_same
  } else {
    error_probability <- plogis(safe_logit(false_positive_rate) + decision_challenge)
    baseline_inconclusive_rate <- inconclusive_rate
  }

  inconclusive_probability <- plogis(
    safe_logit(baseline_inconclusive_rate) +
      0.5 * decision_challenge +
      examiner_inconclusive_tendency
  )

  total_probability <- error_probability + inconclusive_probability

  if (total_probability >= 0.95) {
    scaling_factor <- 0.95 / total_probability
    error_probability <- error_probability * scaling_factor
    inconclusive_probability <- inconclusive_probability * scaling_factor
  }

  draw <- runif(1)

  if (ground_truth == 1) {
    if (draw < error_probability) {
      "elimination"
    } else if (draw < error_probability + inconclusive_probability) {
      "inconclusive"
    } else {
      "identification"
    }
  } else {
    if (draw < error_probability) {
      "identification"
    } else if (draw < error_probability + inconclusive_probability) {
      "inconclusive"
    } else {
      "elimination"
    }
  }
}

# Fully-crossed design: every examiner evaluates every item. See
# simulate_response() for `inconclusive_rate` / `inconclusive_rate_same`.
generate_sim_data <- function(seed,
                              n_examiners,
                              n_comparisons,
                              match_rate,
                              false_positive_rate,
                              false_negative_rate,
                              inconclusive_rate,
                              examiner_skill_sd,
                              examiner_inconclusive_sd,
                              question_sd,
                              inconclusive_rate_same = NULL) {
  set.seed(seed)

  comparison_set <- tibble(
    question_id = seq_len(n_comparisons),
    ground_truth = rbinom(n_comparisons, 1, match_rate),
    question_difficulty = rnorm(n_comparisons, mean = 0, sd = question_sd)
  )

  examiner_panel <- tibble(
    examiner_id = paste0("E", seq_len(n_examiners)),
    examiner_skill = rnorm(n_examiners, mean = 0, sd = examiner_skill_sd),
    examiner_inconclusive_tendency = rnorm(n_examiners, mean = 0, sd = examiner_inconclusive_sd)
  )

  sim_test <- tidyr::crossing(examiner_panel, comparison_set) %>%
    mutate(
      ground_truth_label = if_else(ground_truth == 1, "same-source", "different-source"),
      decision_challenge = question_difficulty - examiner_skill
    ) %>%
    arrange(examiner_id, question_id)

  sim_test %>%
    mutate(
      response = purrr::pmap_chr(
        list(
          ground_truth = ground_truth,
          decision_challenge = decision_challenge,
          examiner_inconclusive_tendency = examiner_inconclusive_tendency
        ),
        simulate_response,
        false_positive_rate = false_positive_rate,
        false_negative_rate = false_negative_rate,
        inconclusive_rate = inconclusive_rate,
        inconclusive_rate_same = inconclusive_rate_same
      )
    )
}

# Incomplete design: each examiner is randomly assigned `items_per_examiner`
# items from a pool of `n_comparisons`, independently across examiners (a
# random incomplete design, not a balanced incomplete block design). This
# mirrors the real study's design (~30 items per examiner from a pool of
# ~300) more closely than the fully-crossed design above.
generate_sim_data_incomplete <- function(seed,
                                         n_examiners,
                                         n_comparisons,
                                         items_per_examiner,
                                         match_rate,
                                         false_positive_rate,
                                         false_negative_rate,
                                         inconclusive_rate,
                                         examiner_skill_sd,
                                         examiner_inconclusive_sd,
                                         question_sd,
                                         inconclusive_rate_same = NULL) {
  set.seed(seed)

  comparison_set <- tibble(
    question_id = seq_len(n_comparisons),
    ground_truth = rbinom(n_comparisons, 1, match_rate),
    question_difficulty = rnorm(n_comparisons, mean = 0, sd = question_sd)
  )

  examiner_panel <- tibble(
    examiner_id = paste0("E", seq_len(n_examiners)),
    examiner_skill = rnorm(n_examiners, mean = 0, sd = examiner_skill_sd),
    examiner_inconclusive_tendency = rnorm(n_examiners, mean = 0, sd = examiner_inconclusive_sd)
  )

  # Each examiner independently gets a random subset of the item pool,
  # without replacement.
  assignment <- purrr::map_dfr(examiner_panel$examiner_id, function(id) {
    tibble(
      examiner_id = id,
      question_id = sample(comparison_set$question_id, size = items_per_examiner, replace = FALSE)
    )
  })

  sim_test <- assignment %>%
    left_join(examiner_panel, by = "examiner_id") %>%
    left_join(comparison_set, by = "question_id") %>%
    mutate(
      ground_truth_label = if_else(ground_truth == 1, "same-source", "different-source"),
      decision_challenge = question_difficulty - examiner_skill
    ) %>%
    arrange(examiner_id, question_id)

  sim_test %>%
    mutate(
      response = purrr::pmap_chr(
        list(
          ground_truth = ground_truth,
          decision_challenge = decision_challenge,
          examiner_inconclusive_tendency = examiner_inconclusive_tendency
        ),
        simulate_response,
        false_positive_rate = false_positive_rate,
        false_negative_rate = false_negative_rate,
        inconclusive_rate = inconclusive_rate,
        inconclusive_rate_same = inconclusive_rate_same
      )
    )
}

# Ground truth x decision crosstab (counts and within-ground-truth
# proportions) for one simulated study.
study_crosstab <- function(sim_data, replicate_id = NA) {
  sim_data %>%
    count(ground_truth_label, response, name = "n") %>%
    group_by(ground_truth_label) %>%
    mutate(proportion = n / sum(n)) %>%
    ungroup() %>%
    mutate(replicate_id = replicate_id)
}

# Canonical defaults: maximum-likelihood fit of this model, with separate
# same-/different-source inconclusive baselines, to the raw Ames II
# cartridge-case responses (icfis/fit_ames2.qmd). Item and examiner-skill SD are
# NOT identified by that study (every item was seen by one examiner), so they were
# fixed at values inside their profile-likelihood ranges (0.8, 0.5) and the
# remaining parameters re-estimated conditional on them. Realized crosstab and
# examiner-level spread from these defaults match the real study (see
# fit_ames2.qmd). `match_rate` stays at 0.5 (the real study is one-third
# same-source; rates within ground truth are unaffected).
#
# The previous defaults -- one inconclusive baseline shared by both ground truths
# (FPR 1%, FNR 2%, inconclusive 36%, examiner skill SD 0.7, examiner
# inconclusive SD 0.35) -- came from icfis/analysis.qmd's calibration to a
# benchmark reconstructed from Cuellar et al. (2024). To reproduce them, set
# inconclusive_rate_same = NULL.
dgp_defaults <- list(
  n_examiners = 50,
  n_comparisons = 100,
  match_rate = 0.5,
  false_positive_rate = 0.006,
  false_negative_rate = 0.011,
  inconclusive_rate = 0.50,          # different-source
  inconclusive_rate_same = 0.17,     # same-source
  examiner_skill_sd = 0.5,
  examiner_inconclusive_sd = 1.3,
  question_sd = 0.8
)

# Same rates and heterogeneity, but for the study's actual (incomplete)
# design: a pool of ~300 items, ~30 assigned per examiner.
incomplete_defaults <- modifyList(
  dgp_defaults,
  list(n_comparisons = 300, items_per_examiner = 30)
)
