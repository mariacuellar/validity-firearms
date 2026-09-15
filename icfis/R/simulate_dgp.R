# Shared data-generating process for the ICFIS talk. Sourced by icfis/app.R,
# icfis/analysis.qmd, and the case-study documents -- this is the single
# implementation of the model; nothing else should redefine these functions.
#
# Requires dplyr, tidyr, and purrr to already be loaded (or namespaced) by
# the caller.

# Keeps qlogis() away from its undefined boundary at p = 0 or p = 1, in case
# a rate parameter is ever pushed to (or past) the edge of a slider/range.
safe_logit <- function(p, eps = 1e-4) {
  qlogis(pmin(pmax(p, eps), 1 - eps))
}

# Response model for one examiner-item pair. See icfis/draft.qmd's
# "Response probabilities" and "The response, formally" slides for the
# corresponding equations.
simulate_response <- function(ground_truth,
                              decision_challenge,
                              examiner_inconclusive_tendency,
                              false_positive_rate,
                              false_negative_rate,
                              inconclusive_rate) {
  if (ground_truth == 1) {
    error_probability <- plogis(safe_logit(false_negative_rate) + decision_challenge)
  } else {
    error_probability <- plogis(safe_logit(false_positive_rate) + decision_challenge)
  }

  inconclusive_probability <- plogis(
    safe_logit(inconclusive_rate) +
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

# Fully-crossed design: every examiner evaluates every item.
generate_sim_data <- function(seed,
                              n_examiners,
                              n_comparisons,
                              match_rate,
                              false_positive_rate,
                              false_negative_rate,
                              inconclusive_rate,
                              examiner_skill_sd,
                              examiner_inconclusive_sd,
                              question_sd) {
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
        inconclusive_rate = inconclusive_rate
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
                                         question_sd) {
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
        inconclusive_rate = inconclusive_rate
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

# Canonical defaults, retuned in icfis/analysis.qmd's "Retuning the
# baseline rates" section against a benchmark reconstructed from
# Cuellar et al. (2024)'s reported bounding calculations for
# Monson, Smith, and Bajic (2023a). If these change, re-run
# icfis/analysis.qmd's calibration and update both here and this comment.
dgp_defaults <- list(
  n_examiners = 50,
  n_comparisons = 100,
  match_rate = 0.5,
  false_positive_rate = 0.01,
  false_negative_rate = 0.02,
  inconclusive_rate = 0.36,
  examiner_skill_sd = 0.7,
  examiner_inconclusive_sd = 0.35,
  question_sd = 0.8
)

# Same rates and heterogeneity, but for the study's actual (incomplete)
# design: a pool of ~300 items, ~30 assigned per examiner.
incomplete_defaults <- modifyList(
  dgp_defaults,
  list(n_comparisons = 300, items_per_examiner = 30)
)
