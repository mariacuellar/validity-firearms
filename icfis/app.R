library(shiny)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(DT)

# safe_logit(), simulate_response(), generate_sim_data(),
# generate_sim_data_incomplete(), study_crosstab(), and the canonical
# dgp_defaults / incomplete_defaults all live in R/simulate_dgp.R, and the
# firearm-type / individual-firearm generators used by the "Non-representative
# sample" tab live in R/simulate_dgp_types.R -- the single shared implementations
# used here, in icfis/analysis.qmd, and in the case-study documents. Edit those
# files, not a copy of them.
source("R/simulate_dgp.R", local = TRUE)
source("R/simulate_dgp_types.R", local = TRUE)

build_summary <- function(sim_data) {
  tibble(
    quantity = c(
      "Rows in simulated dataset",
      "Examiners",
      "Questions",
      "Same-source comparisons",
      "Different-source comparisons",
      "Identification responses",
      "Elimination responses",
      "Inconclusive responses",
      "Mean examiner skill",
      "Mean question difficulty",
      "Mean decision challenge"
    ),
    value = c(
      nrow(sim_data),
      n_distinct(sim_data$examiner_id),
      n_distinct(sim_data$question_id),
      mean(sim_data$ground_truth == 1),
      mean(sim_data$ground_truth == 0),
      mean(sim_data$response == "identification"),
      mean(sim_data$response == "elimination"),
      mean(sim_data$response == "inconclusive"),
      mean(sim_data$examiner_skill),
      mean(sim_data$question_difficulty),
      mean(sim_data$decision_challenge)
    )
  ) %>%
    mutate(
      value = case_when(
        quantity %in% c("Rows in simulated dataset", "Examiners", "Questions") ~ as.character(round(value, 0)),
        grepl("Mean", quantity) ~ sprintf("%.2f", value),
        TRUE ~ scales::percent(value, accuracy = 0.1)
      )
    )
}

overview_panel <- withMathJax(
  tags$div(
    style = "max-width: 800px;",
    h4("What this app does"),
    p(
      "It simulates black-box firearms validation studies from the model behind our talk, and lets ",
      "you change the assumptions we made. Set the assumptions in the sidebar, then use the tabs:"
    ),
    tags$ul(
      tags$li(strong("Your simulation"), ": summary output for one simulated study at the settings you chose."),
      tags$li(
        strong("Inconclusives"), ": how the reported error rate compares with the bounds you get by ",
        "recoding every inconclusive response as an error (a remediable flaw: the raw responses are enough)."
      ),
      tags$li(
        strong("Non-representative sample"), ": what happens when a study uses only easy-to-classify ",
        "firearm makes/models, and whether reweighting can rescue it (a flaw that reanalysis cannot fix)."
      )
    ),
    h4("The model, simplified"),
    p(
      "Each simulated study has a panel of examiners and a set of comparison items, each with a known ",
      "ground truth (same-source or different-source)."
    ),
    tags$ul(
      tags$li(
        "Each item \\(i\\) has a latent ", strong("difficulty"),
        " \\(d_i \\sim N(0, \\sigma_Q^2)\\)."
      ),
      tags$li(
        "Each examiner \\(j\\) has a latent ", strong("skill"),
        " \\(s_j \\sim N(0, \\sigma_S^2)\\) and a latent ",
        strong("tendency to call items inconclusive"),
        " \\(\\tau_j \\sim N(0, \\sigma_T^2)\\)."
      ),
      tags$li(
        "Pairing item \\(i\\) with examiner \\(j\\) creates a ",
        strong("decision challenge"), ":"
      )
    ),
    p("$$c_{ij} = d_i - s_j$$"),
    p(
      "A harder item or a weaker examiner makes the challenge larger, which ",
      "pushes up both the chance of an error and the chance of an ",
      "inconclusive call, on the logit (log-odds) scale:"
    ),
    p("$$P(\\text{error}) = \\text{expit}\\big[\\text{logit}(\\text{baseline rate}) + c_{ij}\\big]$$"),
    p("$$P(\\text{inconclusive}) = \\text{expit}\\big[\\text{logit}(\\text{baseline inconclusive rate}) + 0.5\\,c_{ij} + \\tau_j\\big]$$"),
    p(
      "The baseline error rate is the false positive rate for different-source items ",
      "and the false negative rate for same-source items. The baseline inconclusive rate ",
      "can differ by ground truth: the defaults use separate baselines for same-source and ",
      "different-source items, because in the real study (Ames II cartridge cases) about half of ",
      "different-source responses but under a quarter of same-source responses were inconclusive. ",
      "Tick the box under the sliders to use a single shared baseline instead."
    ),
    p(
      "The baseline rates and the heterogeneity \\(\\sigma_Q\\), \\(\\sigma_S\\), ",
      "\\(\\sigma_T\\) are the sliders on the left. They feed every tab. Note that the baseline error rates ",
      "are model inputs, not the rates a study would report: heterogeneity pushes the realized rates above them."
    )
  )
)

reading_the_plots <- p(
  "The histograms below show the ", strong("simulated sample"),
  " of examiner skill and item difficulty. The overlaid curve is the ",
  strong("assumed population distribution"), " implied by the sliders — ",
  "how far the bars stray from the curve is just sampling variability from ",
  "a finite panel of examiners and items, not a sign that the model is wrong."
)

inconclusive_panel <- tags$div(
  style = "max-width: 900px;",
  h4("Reported error rates vs. bounds on inconclusives"),
  p(
    "Studies typically report an error rate that counts inconclusives in the denominator but never as errors. ",
    "That number equals one end of a range: the ", strong("worst case"), " recodes every inconclusive as an error. ",
    "Both come from the same raw responses, so this flaw can be repaired after the fact."
  ),
  p(
    "The ", strong("X"), " marks a possible true rate: reported rate + h × (share of responses that are inconclusive), ",
    "where ", em("h"), " is the share of inconclusives that would have been errors if the examiner had been forced to decide. ",
    em("h"), " cannot be estimated from the responses of a typical study. The defaults come from Ames II, where examiners also ",
    "recorded which way an inconclusive leaned (Inc-A toward identification, Inc-C toward elimination): 12.3% of different-source ",
    "and 6.8% of same-source inconclusives leaned toward the error. That is a floor; undetermined (Inc-B) inconclusives could ",
    "also come out wrong."
  ),
  p(
    "The x-axis scales your two inconclusive baselines by 0.3x, 0.6x, and 1x. Each setting is ", strong("one simulated study"),
    ", all using the same random draws (same examiners, items, and ground truths), so differences between settings come only from ",
    "the inconclusive baseline. The design is incomplete, as in the non-representative-sample tab: each examiner sees a random ",
    "30 of the items in the pool (the sidebar's number of comparisons). Change the random seed to see how much a single study varies."
  ),
  fluidRow(
    column(6, sliderInput("h_fp", "h for false positives", min = 0, max = 1, value = 0.123, step = 0.005)),
    column(6, sliderInput("h_fn", "h for false negatives", min = 0, max = 1, value = 0.068, step = 0.005))
  ),
  actionButton("run_inconclusive", "Run inconclusive analysis"),
  br(), br(),
  plotOutput("inconclusive_plot", height = "360px"),
  tableOutput("inconclusive_table")
)

nonrep_panel <- tags$div(
  style = "max-width: 900px;",
  h4("Non-representative sampling of firearms"),
  p(
    "Eight simulated firearm ", strong("types"), " (makes/models) differ in how hard they are to classify, and each type has a pool of ",
    strong("individual firearms"), " that differ too. A convenience sample uses only the two easiest types. ",
    "The comparison below holds the total number of firearms fixed, so it isolates ", em("which types"), " are included:"
  ),
  tags$ul(
    tags$li(strong("Convenience sample (naive)"), ": two easiest types, half the firearms each; the reported error rate."),
    tags$li(strong("Full-coverage study (reweighted)"), ": all eight types, recorded and reweighted to the true casework mix."),
    tags$li(strong("Convenience sample (\"reweighted\")"), ": the same reweighting applied to the two-type sample.")
  ),
  p(
    "The dashed line is the population-average error rate under a casework-representative mix of firearms; each dot is ",
    strong("one simulated study"), ". Reweighting can recover the true rate when every type is observed, but not when most of the ",
    "population was never sampled. The analysis uses the sidebar's error rates, inconclusive baselines, and examiner/item ",
    "heterogeneity, and an incomplete design where each examiner sees a random 30 of the study's comparisons. A single study is ",
    "noisy: change the random seed to see how much."
  ),
  fluidRow(
    column(4, sliderInput("total_firearms", "Total firearms per study", min = 16, max = 96, value = 80, step = 16)),
    column(4, sliderInput("type_difficulty_sd", "Between-model difficulty SD", min = 0, max = 2.5, value = 1.2, step = 0.1)),
    column(4, sliderInput("firearm_difficulty_sd", "Within-model (firearm) difficulty SD", min = 0, max = 2, value = 0.9, step = 0.1))
  ),
  actionButton("run_nonrep", "Run non-representative analysis"),
  br(), br(),
  plotOutput("nonrep_plot", height = "380px"),
  tableOutput("nonrep_table"),
  p(
    style = "color: #666;",
    "The true rate comes from one large simulated population (approximate, so it can wobble by a fraction of a ",
    "percentage point between seeds). The bias you see is driven mostly by the between-model difficulty SD; the ",
    "between-model and firearm SDs here are illustrative, not fit to data."
  )
)

# Reported and worst-case error rates for one simulated study. Same definitions as
# compute_reported_and_worst_case() in case_study_inconclusive.qmd: reported =
# errors / all comparisons of that ground truth (inconclusives in the denominator);
# worst case additionally counts every inconclusive as an error.
reported_and_worst_case <- function(sim_data) {
  diff_data <- sim_data %>% filter(ground_truth_label == "different-source")
  same_data <- sim_data %>% filter(ground_truth_label == "same-source")
  tibble(
    rate = c("False positive rate", "False negative rate"),
    reported = c(
      mean(diff_data$response == "identification"),
      mean(same_data$response == "elimination")
    ),
    worst_case = c(
      mean(diff_data$response != "elimination"),
      mean(same_data$response != "identification")
    )
  )
}

ui <- fluidPage(
  titlePanel("Firearms Validity Data Generator"),
  p(
    "This app simulates data from the model behind our ICFIS talk on ",
    em("Remediable and Non-Remediable Flaws in Firearms Validity Studies"),
    ". If you disagree with a default we used in the talk, change it here ",
    "and see how the results change. See the Overview tab for a ",
    "summary of the model."
  ),
  sidebarLayout(
    sidebarPanel(
      numericInput("seed", "Random seed", value = 123, min = 1, step = 1),
      numericInput("n_examiners", "Number of examiners", value = dgp_defaults$n_examiners, min = 1, step = 1),
      numericInput("n_comparisons", "Number of comparisons (Your simulation tab; item pool for Inconclusives tab)", value = dgp_defaults$n_comparisons, min = 1, step = 1),
      sliderInput("match_rate", "Proportion same-source", min = 0, max = 1, value = dgp_defaults$match_rate, step = 0.01),
      sliderInput("false_positive_rate", "Baseline false positive rate", min = 0.001, max = 0.5, value = dgp_defaults$false_positive_rate, step = 0.001),
      sliderInput("false_negative_rate", "Baseline false negative rate", min = 0.001, max = 0.5, value = dgp_defaults$false_negative_rate, step = 0.001),
      sliderInput("inconclusive_rate", "Baseline inconclusive rate (different-source)", min = 0.01, max = 0.9, value = dgp_defaults$inconclusive_rate, step = 0.01),
      sliderInput("inconclusive_rate_same", "Baseline inconclusive rate (same-source)", min = 0.01, max = 0.9, value = dgp_defaults$inconclusive_rate_same, step = 0.01),
      checkboxInput("shared_inconclusive", "Use the different-source rate for both (one shared baseline)", value = FALSE),
      sliderInput("examiner_skill_sd", "Examiner skill heterogeneity (SD)", min = 0, max = 2, value = dgp_defaults$examiner_skill_sd, step = 0.05),
      sliderInput("examiner_inconclusive_sd", "Examiner inconclusive-tendency heterogeneity (SD)", min = 0, max = 2, value = dgp_defaults$examiner_inconclusive_sd, step = 0.05),
      sliderInput("question_sd", "Question difficulty heterogeneity (SD)", min = 0, max = 2, value = dgp_defaults$question_sd, step = 0.05),
      actionButton("generate", "Generate data"),
      hr(),
      downloadButton("download_rda", "Download .rda"),
      downloadButton("download_csv", "Download .csv")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Overview",
          br(),
          overview_panel
        ),
        tabPanel(
          "Your simulation",
          br(),
          h4("Summary"),
          tableOutput("summary_table"),
          h4("Ground truth × decision"),
          tableOutput("crosstab_table"),
          h4("Latent distributions"),
          reading_the_plots,
          plotOutput("skill_plot", height = "300px"),
          plotOutput("difficulty_plot", height = "300px"),
          h4("First rows of the data"),
          DTOutput("preview_table")
        ),
        tabPanel("Inconclusives", br(), inconclusive_panel),
        tabPanel("Non-representative sample", br(), nonrep_panel)
      )
    )
  )
)

server <- function(input, output, session) {
  sim_result <- eventReactive(input$generate, {
    list(
      data = generate_sim_data(
        seed = input$seed,
        n_examiners = input$n_examiners,
        n_comparisons = input$n_comparisons,
        match_rate = input$match_rate,
        false_positive_rate = input$false_positive_rate,
        false_negative_rate = input$false_negative_rate,
        inconclusive_rate = input$inconclusive_rate,
        inconclusive_rate_same = if (isTRUE(input$shared_inconclusive)) NULL else input$inconclusive_rate_same,
        examiner_skill_sd = input$examiner_skill_sd,
        examiner_inconclusive_sd = input$examiner_inconclusive_sd,
        question_sd = input$question_sd
      ),
      params = list(
        examiner_skill_sd = input$examiner_skill_sd,
        examiner_inconclusive_sd = input$examiner_inconclusive_sd,
        question_sd = input$question_sd
      )
    )
  }, ignoreNULL = FALSE)

  # The simulated sample (as before, `sim_data()` was the tibble directly).
  sim_data <- reactive(sim_result()$data)
  # The population parameters used to generate that sample, so the "assumed
  # population distribution" overlay always matches what's on screen even if
  # the sliders have moved since the last time "Generate data" was pressed.
  sim_params <- reactive(sim_result()$params)

  output$preview_table <- renderDT({
    preview_data <- sim_data() %>%
      filter(examiner_id %in% paste0("E", seq_len(min(3, input$n_examiners)))) %>%
      group_by(examiner_id) %>%
      slice_head(n = 6) %>%
      ungroup() %>%
      select(
        examiner_id,
        question_id,
        ground_truth_label,
        question_difficulty,
        examiner_skill,
        decision_challenge,
        response
      ) %>%
      mutate(
        question_difficulty = round(question_difficulty, 2),
        examiner_skill = round(examiner_skill, 2),
        decision_challenge = round(decision_challenge, 2)
      )

    datatable(preview_data, options = list(pageLength = 18, scrollX = TRUE), rownames = FALSE)
  })

  output$summary_table <- renderTable({
    build_summary(sim_data())
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$skill_plot <- renderPlot({
    skill_sd <- sim_params()$examiner_skill_sd
    plot_data <- sim_data() %>% distinct(examiner_id, examiner_skill)

    p <- ggplot(plot_data, aes(x = examiner_skill)) +
      geom_histogram(aes(y = after_stat(density)), bins = 15, fill = "grey70", color = "white")

    if (skill_sd > 0) {
      p <- p + stat_function(
        fun = dnorm,
        args = list(mean = 0, sd = skill_sd),
        color = "#D55E00",
        linewidth = 1
      )
    } else {
      p <- p + geom_vline(xintercept = 0, linetype = "dashed", color = "#D55E00", linewidth = 1)
    }

    p +
      labs(
        title = "Distribution of examiner skill",
        x = "Relative examiner skill (latent scale)",
        y = "Density"
      ) +
      theme_minimal()
  })

  output$difficulty_plot <- renderPlot({
    question_sd <- sim_params()$question_sd
    plot_data <- sim_data() %>% distinct(question_id, question_difficulty)

    p <- ggplot(plot_data, aes(x = question_difficulty)) +
      geom_histogram(aes(y = after_stat(density)), bins = 15, fill = "grey70", color = "white")

    if (question_sd > 0) {
      p <- p + stat_function(
        fun = dnorm,
        args = list(mean = 0, sd = question_sd),
        color = "#D55E00",
        linewidth = 1
      )
    } else {
      p <- p + geom_vline(xintercept = 0, linetype = "dashed", color = "#D55E00", linewidth = 1)
    }

    p +
      labs(
        title = "Distribution of question difficulty",
        x = "Relative question difficulty (latent scale)",
        y = "Density"
      ) +
      theme_minimal()
  })

  output$crosstab_table <- renderTable({
    sim_data() %>%
      mutate(
        `Ground truth` = ground_truth_label,
        Decision = factor(
          case_when(
            response == "identification" ~ "Identification",
            response == "inconclusive" ~ "Inconclusive",
            response == "elimination" ~ "Elimination"
          ),
          levels = c("Identification", "Inconclusive", "Elimination")
        )
      ) %>%
      count(`Ground truth`, Decision) %>%
      tidyr::pivot_wider(names_from = Decision, values_from = n, values_fill = 0)
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  # ---- Inconclusives tab ---------------------------------------------------
  # One simulated study per inconclusive-baseline setting, all with the same seed (so the same
  # examiners, items, and ground truths), under an incomplete design.
  inconclusive_result <- eventReactive(input$run_inconclusive, {
    scales_used <- c(0.3, 0.6, 1)
    shared <- isTRUE(input$shared_inconclusive)
    items_per_examiner <- min(30, input$n_comparisons)

    per_setting <- withProgress(message = "Simulating studies", value = 0, {
      map(seq_along(scales_used), function(k) {
        ir_d <- scales_used[k] * input$inconclusive_rate
        ir_s <- scales_used[k] * (if (shared) input$inconclusive_rate else input$inconclusive_rate_same)
        study <- generate_sim_data_incomplete(
          seed = input$seed,
          n_examiners = input$n_examiners,
          n_comparisons = input$n_comparisons,
          items_per_examiner = items_per_examiner,
          match_rate = input$match_rate,
          false_positive_rate = input$false_positive_rate,
          false_negative_rate = input$false_negative_rate,
          inconclusive_rate = ir_d,
          inconclusive_rate_same = if (shared) NULL else ir_s,
          examiner_skill_sd = input$examiner_skill_sd,
          examiner_inconclusive_sd = input$examiner_inconclusive_sd,
          question_sd = input$question_sd
        )
        incProgress(1 / length(scales_used))
        reported_and_worst_case(study) %>%
          mutate(setting = sprintf("%.0f%% / %.0f%%", 100 * ir_d, 100 * ir_s), scale = scales_used[k])
      })
    })

    bind_rows(per_setting) %>%
      mutate(
        inc_share = worst_case - reported,
        h = if_else(rate == "False positive rate", input$h_fp, input$h_fn),
        truth = reported + h * inc_share,
        rate = factor(rate, levels = c("False positive rate", "False negative rate")),
        setting = factor(setting, levels = unique(setting[order(scale)]))
      )
  })

  output$inconclusive_plot <- renderPlot({
    d <- inconclusive_result()
    x_label <- "True rate if a share h of inconclusives were errors"

    points_long <- d %>%
      select(setting, rate, Reported = reported, !!x_label := truth) %>%
      pivot_longer(c(Reported, all_of(x_label)), names_to = "measure", values_to = "value")

    ggplot(points_long, aes(x = setting)) +
      geom_errorbar(
        data = d, aes(ymin = reported, ymax = worst_case),
        width = 0.18, linewidth = 0.9, color = "#0072B2"
      ) +
      geom_point(aes(y = value, shape = measure, color = measure), size = 3.5, stroke = 1.4) +
      facet_wrap(~ rate) +
      scale_shape_manual(values = setNames(c(16, 4), c("Reported", x_label))) +
      scale_color_manual(values = setNames(c("#0072B2", "#D55E00"), c("Reported", x_label))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(
        title = "Reported rates understate the true rate; bounds show what could be hidden",
        subtitle = "Blue brackets: reported to worst case (every inconclusive an error)",
        x = "Baseline inconclusive rate (different-source / same-source)",
        y = "Error rate", shape = NULL, color = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })

  output$inconclusive_table <- renderTable({
    inconclusive_result() %>%
      arrange(rate, scale) %>%
      transmute(
        Rate = as.character(rate),
        `Inconclusive baseline (diff. / same)` = as.character(setting),
        Reported = scales::percent(reported, accuracy = 0.1),
        `Inconclusive share` = scales::percent(inc_share, accuracy = 0.1),
        `True rate (X)` = scales::percent(truth, accuracy = 0.1),
        `Worst case` = scales::percent(worst_case, accuracy = 0.1)
      )
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  # ---- Non-representative sample tab --------------------------------------------
  # One simulated study per sampling approach (convenience and full coverage), same seed.
  nonrep_result <- eventReactive(input$run_nonrep, {
    total_fw <- input$total_firearms
    shared <- isTRUE(input$shared_inconclusive)
    ir_same <- if (shared) NULL else input$inconclusive_rate_same

    type_pop <- make_type_population(type_difficulty_sd = input$type_difficulty_sd)
    firearm_pop <- make_firearm_population(type_pop, firearm_difficulty_sd = input$firearm_difficulty_sd)
    easiest <- type_pop %>% arrange(type_difficulty) %>% slice(1:2) %>% pull(type_id)
    conv_weights <- if_else(type_pop$type_id %in% easiest, 1, 0)
    full_weights <- rep(1, nrow(type_pop))
    n_fw_conv <- total_fw / 2
    n_fw_full <- total_fw / nrow(type_pop)
    items_per_examiner <- 30

    withProgress(message = "Simulating studies", value = 0, {
      reference <- generate_population_reference_by_firearm(
        seed = input$seed,
        type_population = type_pop,
        firearm_population = firearm_pop,
        sampling_weights = type_pop$population_weight,
        n_items_total = 600,
        n_examiners = 30,
        match_rate = input$match_rate,
        false_positive_rate = input$false_positive_rate,
        false_negative_rate = input$false_negative_rate,
        inconclusive_rate = input$inconclusive_rate,
        examiner_skill_sd = input$examiner_skill_sd,
        examiner_inconclusive_sd = input$examiner_inconclusive_sd,
        question_sd = input$question_sd,
        inconclusive_rate_same = ir_same
      )
      truth_rates <- compute_pooled_rates(reference)
      incProgress(0.5)

      sim_one <- function(weights, n_fw, items_per_fw) {
        generate_sim_data_by_type_and_firearm_incomplete(
          seed = input$seed,
          type_population = type_pop,
          firearm_population = firearm_pop,
          sampling_weights = weights,
          n_firearms_per_type = n_fw,
          items_per_firearm = items_per_fw,
          items_per_examiner = items_per_examiner,
          n_examiners = input$n_examiners,
          match_rate = input$match_rate,
          false_positive_rate = input$false_positive_rate,
          false_negative_rate = input$false_negative_rate,
          inconclusive_rate = input$inconclusive_rate,
          examiner_skill_sd = input$examiner_skill_sd,
          examiner_inconclusive_sd = input$examiner_inconclusive_sd,
          question_sd = input$question_sd,
          inconclusive_rate_same = ir_same
        )
      }

      conv <- sim_one(conv_weights, n_fw_conv, 10)
      full <- sim_one(full_weights, n_fw_full, 4)
      incProgress(0.5)

      estimates <- bind_rows(
        compute_pooled_rates(conv) %>% mutate(scenario = "Convenience sample (naive)"),
        reweight_by_population(compute_type_level_rates(full), type_pop) %>%
          rename(rate = reweighted_rate) %>% mutate(scenario = "Full-coverage study (reweighted)"),
        reweight_by_population(compute_type_level_rates(conv), type_pop) %>%
          rename(rate = reweighted_rate) %>% mutate(scenario = "Convenience sample (\"reweighted\")")
      )
    })

    list(
      results = estimates %>%
        mutate(
          scenario = factor(scenario, levels = c("Convenience sample (naive)", "Full-coverage study (reweighted)", "Convenience sample (\"reweighted\")")),
          metric = factor(metric, levels = c("FPR", "FNR"))
        ),
      truth = truth_rates %>% mutate(metric = factor(metric, levels = c("FPR", "FNR")))
    )
  })

  output$nonrep_plot <- renderPlot({
    r <- nonrep_result()
    ggplot(r$results, aes(x = scenario, y = rate)) +
      geom_hline(data = r$truth, aes(yintercept = rate), color = "#D55E00", linetype = "dashed", linewidth = 1) +
      geom_point(size = 4) +
      geom_text(aes(label = scales::percent(rate, accuracy = 0.1)), vjust = -1.1, size = 3.8) +
      facet_wrap(~ metric, scales = "free_y") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), expand = expansion(mult = c(0.05, 0.2))) +
      scale_x_discrete(labels = scales::label_wrap(14)) +
      labs(
        title = "Naive vs. reweighted estimates vs. the true rate",
        subtitle = "Dashed line: true population-average rate. Each dot: one simulated study",
        x = NULL, y = "Rate"
      ) +
      theme_minimal(base_size = 13)
  })

  output$nonrep_table <- renderTable({
    r <- nonrep_result()
    r$results %>%
      left_join(rename(r$truth, true_rate = rate), by = "metric") %>%
      arrange(metric, scenario) %>%
      transmute(
        Metric = as.character(metric),
        Scenario = as.character(scenario),
        `Estimate (this study)` = scales::percent(rate, accuracy = 0.01),
        `True rate` = scales::percent(true_rate, accuracy = 0.01),
        `True / estimate` = sprintf("%.1fx", true_rate / rate)
      )
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$download_rda <- downloadHandler(
    filename = function() {
      "sim_data.rda"
    },
    content = function(file) {
      sim_data_out <- sim_data()
      save(sim_data_out, file = file)
    }
  )

  output$download_csv <- downloadHandler(
    filename = function() {
      "sim_data.csv"
    },
    content = function(file) {
      write.csv(sim_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
