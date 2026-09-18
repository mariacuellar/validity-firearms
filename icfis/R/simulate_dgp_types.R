# Extension to R/simulate_dgp.R for case study 2 (non-representative
# sampling of firearm types, Flaw B). Source R/simulate_dgp.R first --
# this file reuses simulate_response() from it and assumes dplyr, tidyr,
# and purrr are already loaded.

# A fixed population of firearm "types" -- makes/models (e.g. Beretta
# M9A3 vs. Glock 19), not individual firearms -- encountered in real
# casework. Each type has its own baseline classification difficulty:
# Cuellar et al. (2024, p.10) cite evidence from Guyll et al. (2023) and
# Bajic et al. (2020, "AMES II") that different firearm makes/models (their
# example: Jimenez vs. Beretta) differ in how easy they are to match. (This
# is distinct from the *within*-model, individual-firearm variation --
# e.g. the 0%-62% "hard" range across 27 otherwise-identical Beretta
# pistols -- which is a separate, smaller-scale effect; see the
# "planned firearm-clustering extension" backup slide in draft.qmd.)
# `population_weight` is a hardcoded, illustrative stand-in for how common
# each type actually is in casework -- not derived from any real inventory,
# and deliberately independent of `type_difficulty` (commonness and
# classifiability aren't the same thing).
make_type_population <- function(seed = 2020, type_difficulty_sd = 1.2) {
  set.seed(seed)
  tibble(
    type_id = 1:8,
    population_weight = c(0.28, 0.22, 0.16, 0.12, 0.09, 0.06, 0.04, 0.03),
    type_difficulty = rnorm(8, mean = 0, sd = type_difficulty_sd)
  )
}

# A "world population" of individual firearms nested within each type --
# every physical gun of each make/model that a study could possibly draw
# from. Each firearm has its own difficulty offset, added on top of its
# type's difficulty (a within-model effect, additive with the
# between-model effect from make_type_population()). Cuellar et al. (2024)
# cite a 0%-62% "hard" range across 27 otherwise-identical Beretta
# pistols in Ames II as evidence individual firearms of the same model can
# differ substantially -- `firearm_difficulty_sd` is set large enough to
# take that seriously, but is not fit to that (or any) specific number;
# treat it as illustrative, like the other uncalibrated heterogeneity
# parameters in this project.
make_firearm_population <- function(type_population, firearms_per_type = 50, firearm_difficulty_sd = 0.9, seed = 2021) {
  set.seed(seed)
  type_population %>%
    select(type_id) %>%
    slice(rep(seq_len(n()), each = firearms_per_type)) %>%
    mutate(
      firearm_id = row_number(),
      firearm_difficulty = rnorm(n(), mean = 0, sd = firearm_difficulty_sd)
    )
}

# Generates one study's data by (1) drawing items from `type_population`
# according to `sampling_weights` as before, then (2) for each item,
# drawing an individual firearm *uniformly* from that type's slice of
# `firearm_population` (with replacement -- the world population is
# treated as large enough that this approximates the full diversity of
# firearms of that type). Used for the large reference simulation that
# defines the true population-average rate; not meant to represent any one
# study's design, which only ever uses a handful of specific firearms
# (see generate_sim_data_by_type_and_firearm() below).
generate_population_reference_by_firearm <- function(seed,
                                                      type_population,
                                                      firearm_population,
                                                      sampling_weights,
                                                      n_items_total,
                                                      n_examiners,
                                                      match_rate,
                                                      false_positive_rate,
                                                      false_negative_rate,
                                                      inconclusive_rate,
                                                      examiner_skill_sd,
                                                      examiner_inconclusive_sd,
                                                      question_sd) {
  set.seed(seed)

  item_types <- tibble(
    question_id = seq_len(n_items_total),
    type_id = sample(type_population$type_id, size = n_items_total, replace = TRUE, prob = sampling_weights)
  )

  comparison_set <- item_types %>%
    group_by(type_id) %>%
    mutate(
      firearm_id = sample(
        firearm_population$firearm_id[firearm_population$type_id == type_id[1]],
        size = n(),
        replace = TRUE
      )
    ) %>%
    ungroup() %>%
    left_join(type_population, by = "type_id") %>%
    left_join(firearm_population %>% select(firearm_id, firearm_difficulty), by = "firearm_id") %>%
    mutate(
      ground_truth = rbinom(n(), 1, match_rate),
      question_difficulty = type_difficulty + firearm_difficulty + rnorm(n(), mean = 0, sd = question_sd)
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

# Generates one *study's* data: for each included type (sampling_weights >
# 0), draws `n_firearms_per_type` specific individual firearms (without
# replacement -- a study can't reuse the same physical gun as if it were
# two different ones) from that type's slice of `firearm_population`, then
# generates `items_per_firearm` comparisons per drawn firearm. This is
# where within-model heterogeneity actually bites: a study's few firearms
# of a given type are a small, possibly unrepresentative sample of all the
# firearms of that type that exist, on top of (not instead of) whichever
# types the study chose to include at all.
generate_sim_data_by_type_and_firearm <- function(seed,
                                                   type_population,
                                                   firearm_population,
                                                   sampling_weights,
                                                   n_firearms_per_type,
                                                   items_per_firearm,
                                                   n_examiners,
                                                   match_rate,
                                                   false_positive_rate,
                                                   false_negative_rate,
                                                   inconclusive_rate,
                                                   examiner_skill_sd,
                                                   examiner_inconclusive_sd,
                                                   question_sd) {
  set.seed(seed)

  included_type_ids <- type_population$type_id[sampling_weights > 0]

  drawn_firearms <- purrr::map_dfr(included_type_ids, function(k) {
    candidates <- firearm_population %>% filter(type_id == k)
    candidates %>% slice_sample(n = n_firearms_per_type)
  })

  comparison_set <- drawn_firearms %>%
    slice(rep(seq_len(n()), each = items_per_firearm)) %>%
    mutate(question_id = row_number()) %>%
    left_join(type_population, by = "type_id") %>%
    mutate(
      ground_truth = rbinom(n(), 1, match_rate),
      question_difficulty = type_difficulty + firearm_difficulty + rnorm(n(), mean = 0, sd = question_sd)
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

# Generates one study's data by drawing items from `type_population`
# according to `sampling_weights` (same length/order as
# `type_population`'s rows; a weight of zero means that type never
# appears in the sample). Every examiner evaluates every sampled item
# (fully crossed) -- case study 2 is about *which firearms* end up in the
# study, not about per-examiner assignment, which is case study 1's
# territory.
#
# Superseded by generate_sim_data_by_type_and_firearm() for the main case
# study now that it models within-type firearm heterogeneity too; kept
# here since it's still a valid (simpler, type-only) building block.
generate_sim_data_by_type <- function(seed,
                                      type_population,
                                      sampling_weights,
                                      n_items_total,
                                      n_examiners,
                                      match_rate,
                                      false_positive_rate,
                                      false_negative_rate,
                                      inconclusive_rate,
                                      examiner_skill_sd,
                                      examiner_inconclusive_sd,
                                      question_sd) {
  set.seed(seed)

  comparison_set <- tibble(
    question_id = seq_len(n_items_total),
    type_id = sample(type_population$type_id, size = n_items_total, replace = TRUE, prob = sampling_weights)
  ) %>%
    left_join(type_population, by = "type_id") %>%
    mutate(
      ground_truth = rbinom(n(), 1, match_rate),
      question_difficulty = type_difficulty + rnorm(n(), mean = 0, sd = question_sd)
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

# Pooled false positive / false negative rate for one study's data
# (ignoring type -- this is what a study would actually report), using the
# "reported" definition from case_study_inconclusive.qmd (identifications
# / all different-source cases, and the same-source analog).
compute_pooled_rates <- function(sim_data) {
  diff_data <- sim_data %>% filter(ground_truth_label == "different-source")
  same_data <- sim_data %>% filter(ground_truth_label == "same-source")

  tibble(
    metric = c("FPR", "FNR"),
    rate = c(
      sum(diff_data$response == "identification") / nrow(diff_data),
      sum(same_data$response == "elimination") / nrow(same_data)
    )
  )
}

# Per-type version of the same rates, needed for reweighting.
compute_type_level_rates <- function(sim_data) {
  sim_data %>%
    group_by(type_id, ground_truth_label) %>%
    summarise(
      n = n(),
      identifications = sum(response == "identification"),
      eliminations = sum(response == "elimination"),
      .groups = "drop"
    ) %>%
    mutate(
      rate = if_else(
        ground_truth_label == "different-source",
        identifications / n,
        eliminations / n
      ),
      metric = if_else(ground_truth_label == "different-source", "FPR", "FNR")
    ) %>%
    select(type_id, metric, rate, n)
}

# Reweights a sample's per-type rates by population_weight, renormalized
# over just the types actually present in the sample -- a common ad hoc
# fix attempted when a study wasn't designed with full population
# coverage. When the sample has zero coverage of most of the population's
# weight, this renormalization silently treats the missing types as if
# they don't exist, which is the point of the case study's third beat.
reweight_by_population <- function(type_rates, type_population) {
  observed_types <- type_rates %>% distinct(type_id) %>% pull(type_id)

  weights <- type_population %>%
    filter(type_id %in% observed_types) %>%
    mutate(renormalized_weight = population_weight / sum(population_weight))

  type_rates %>%
    left_join(weights %>% select(type_id, renormalized_weight), by = "type_id") %>%
    group_by(metric) %>%
    summarise(reweighted_rate = sum(rate * renormalized_weight), .groups = "drop")
}
