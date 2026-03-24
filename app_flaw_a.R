library(shiny)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(DT)
library(scales)

simulate_response <- function(ground_truth,
                              decision_challenge,
                              examiner_inconclusive_tendency,
                              false_positive_rate,
                              false_negative_rate,
                              inconclusive_rate) {
  if (ground_truth == 1) {
    error_probability <- plogis(qlogis(false_negative_rate) + decision_challenge)
  } else {
    error_probability <- plogis(qlogis(false_positive_rate) + decision_challenge)
  }

  inconclusive_probability <- plogis(
    qlogis(inconclusive_rate) +
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

simulate_study <- function(n_examiners,
                           n_comparisons,
                           match_rate,
                           false_positive_rate,
                           false_negative_rate,
                           inconclusive_rate,
                           examiner_sd,
                           question_sd) {
  comparison_set <- tibble(
    question_id = seq_len(n_comparisons),
    ground_truth = rbinom(n_comparisons, 1, match_rate),
    question_difficulty = rnorm(n_comparisons, mean = 0, sd = question_sd)
  )

  examiner_panel <- tibble(
    examiner_id = paste0("E", seq_len(n_examiners)),
    examiner_skill = rnorm(n_examiners, mean = 0, sd = examiner_sd),
    examiner_inconclusive_tendency = rnorm(n_examiners, mean = 0, sd = examiner_sd / 2)
  )

  sim_test <- tidyr::crossing(examiner_panel, comparison_set) %>%
    mutate(
      decision_challenge = question_difficulty - examiner_skill
    )

  sim_test %>%
    mutate(
      error_probability = if_else(
        ground_truth == 1,
        plogis(qlogis(false_negative_rate) + decision_challenge),
        plogis(qlogis(false_positive_rate) + decision_challenge)
      ),
      inconclusive_probability = plogis(
        qlogis(inconclusive_rate) +
          0.5 * decision_challenge +
          examiner_inconclusive_tendency
      ),
      total_probability = error_probability + inconclusive_probability,
      scaling_factor = if_else(total_probability >= 0.95, 0.95 / total_probability, 1),
      error_probability = error_probability * scaling_factor,
      inconclusive_probability = inconclusive_probability * scaling_factor,
      draw = runif(n()),
      response = case_when(
        ground_truth == 1 & draw < error_probability ~ "elimination",
        ground_truth == 1 & draw < error_probability + inconclusive_probability ~ "inconclusive",
        ground_truth == 1 ~ "identification",
        ground_truth == 0 & draw < error_probability ~ "identification",
        ground_truth == 0 & draw < error_probability + inconclusive_probability ~ "inconclusive",
        TRUE ~ "elimination"
      )
    ) %>%
    select(-error_probability, -inconclusive_probability, -total_probability, -scaling_factor, -draw)
}

summarize_study <- function(study_data) {
  nonmatch_total <- sum(study_data$ground_truth == 0)
  match_total <- sum(study_data$ground_truth == 1)

  false_positives <- sum(study_data$response == "identification" & study_data$ground_truth == 0)
  false_negatives <- sum(study_data$response == "elimination" & study_data$ground_truth == 1)

  tibble(
    estimated_fpr = false_positives / nonmatch_total,
    estimated_fnr = false_negatives / match_total,
    inconclusive_rate = mean(study_data$response == "inconclusive"),
    zero_false_positives = false_positives == 0,
    nonmatch_total = nonmatch_total,
    match_total = match_total
  )
}

ui <- fluidPage(
  titlePanel("Flaw A Explorer: Inadequate Sample Size"),
  sidebarLayout(
    sidebarPanel(
      p("Use the sliders to change the size of the study and see how much the estimated false positive rate moves around across repeated simulations."),
      actionButton("run_simulation", "Run simulation"),
      br(),
      br(),
      numericInput("seed", "Random seed", value = 123, min = 1, step = 1),
      sliderInput("n_examiners", "Number of examiners", min = 5, max = 300, value = 20, step = 5),
      sliderInput("n_comparisons", "Comparisons per examiner", min = 10, max = 300, value = 30, step = 10),
      sliderInput("n_replications", "Number of replications", min = 25, max = 300, value = 100, step = 25),
      hr(),
      sliderInput("match_rate", "Proportion same-source", min = 0.1, max = 0.9, value = 0.5, step = 0.05),
      sliderInput("false_positive_rate", "True false positive rate", min = 0.001, max = 0.10, value = 0.02, step = 0.001),
      sliderInput("false_negative_rate", "True false negative rate", min = 0.001, max = 0.20, value = 0.05, step = 0.001),
      sliderInput("inconclusive_rate", "Baseline inconclusive rate", min = 0.01, max = 0.50, value = 0.10, step = 0.01)
    ),
    mainPanel(
      h4("What this app is showing"),
      p("Each point or bar comes from one repeated study under the sample size you selected. In this version of the app, examiner heterogeneity and item-difficulty heterogeneity are set to zero so that the only thing changing is sample size. Small studies can easily produce estimated false positive rates that look extremely low, even when the true false positive rate is fixed."),
      fluidRow(
        column(4, strong("True FPR"), textOutput("true_fpr_text")),
        column(4, strong("Median estimated FPR"), textOutput("median_fpr_text")),
        column(4, strong("Range of estimated FPR"), textOutput("range_fpr_text"))
      ),
      fluidRow(
        column(6, strong("Studies with zero observed false positives"), textOutput("zero_fp_text")),
        column(6, strong("Total responses per study"), textOutput("study_size_text"))
      ),
      tabsetPanel(
        tabPanel("FPR distribution", br(), plotOutput("fpr_plot", height = "360px")),
        tabPanel("Summary table", br(), DTOutput("summary_table")),
        tabPanel("Replicated results", br(), DTOutput("results_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  simulation_results <- eventReactive(input$run_simulation, {
    set.seed(input$seed)

    purrr::map_dfr(
      seq_len(input$n_replications),
      function(replicate_id) {
        simulate_study(
          n_examiners = input$n_examiners,
          n_comparisons = input$n_comparisons,
          match_rate = input$match_rate,
          false_positive_rate = input$false_positive_rate,
          false_negative_rate = input$false_negative_rate,
          inconclusive_rate = input$inconclusive_rate,
          examiner_sd = 0,
          question_sd = 0
        ) %>%
          summarize_study() %>%
          mutate(replicate_id = replicate_id)
      }
    )
  }, ignoreNULL = FALSE)

  summary_stats <- reactive({
    results <- simulation_results()

    tibble(
      quantity = c(
        "True false positive rate",
        "Median estimated false positive rate",
        "Minimum estimated false positive rate",
        "Maximum estimated false positive rate",
        "Studies with zero observed false positives",
        "Median estimated false negative rate",
        "Median inconclusive rate"
      ),
      value = c(
        percent(input$false_positive_rate, accuracy = 0.1),
        percent(median(results$estimated_fpr), accuracy = 0.1),
        percent(min(results$estimated_fpr), accuracy = 0.1),
        percent(max(results$estimated_fpr), accuracy = 0.1),
        percent(mean(results$zero_false_positives), accuracy = 0.1),
        percent(median(results$estimated_fnr), accuracy = 0.1),
        percent(median(results$inconclusive_rate), accuracy = 0.1)
      )
    )
  })

  output$true_fpr_text <- renderText({
    percent(input$false_positive_rate, accuracy = 0.1)
  })

  output$median_fpr_text <- renderText({
    percent(median(simulation_results()$estimated_fpr), accuracy = 0.1)
  })

  output$range_fpr_text <- renderText({
    paste0(
      percent(min(simulation_results()$estimated_fpr), accuracy = 0.1),
      " to ",
      percent(max(simulation_results()$estimated_fpr), accuracy = 0.1)
    )
  })

  output$zero_fp_text <- renderText({
    percent(mean(simulation_results()$zero_false_positives), accuracy = 0.1)
  })

  output$study_size_text <- renderText({
    comma(input$n_examiners * input$n_comparisons)
  })

  output$fpr_plot <- renderPlot({
    ggplot(simulation_results(), aes(x = estimated_fpr)) +
      geom_histogram(bins = 25, fill = "#5B8FF9", color = "white") +
      geom_vline(xintercept = input$false_positive_rate, linetype = "dashed", linewidth = 1) +
      scale_x_continuous(
        limits = c(0, 0.10),
        labels = percent_format(accuracy = 1)
      ) +
      scale_y_continuous(
        limits = c(0, input$n_replications),
        expand = expansion(mult = c(0, 0.02))
      ) +
      labs(
        title = "Estimated false positive rates across repeated studies",
        subtitle = "Dashed line marks the true false positive rate",
        x = "Estimated false positive rate",
        y = "Number of simulated studies"
      ) +
      theme_minimal()
  })

  output$summary_table <- renderDT({
    datatable(summary_stats(), options = list(dom = "t"), rownames = FALSE)
  })

  output$results_table <- renderDT({
    display_results <- simulation_results() %>%
      mutate(
        estimated_fpr = percent(estimated_fpr, accuracy = 0.1),
        estimated_fnr = percent(estimated_fnr, accuracy = 0.1),
        inconclusive_rate = percent(inconclusive_rate, accuracy = 0.1)
      )

    datatable(
      display_results,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
}

shinyApp(ui = ui, server = server)
