library(shiny)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(DT)

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

generate_sim_data <- function(seed,
                              n_examiners,
                              n_comparisons,
                              match_rate,
                              false_positive_rate,
                              false_negative_rate,
                              inconclusive_rate,
                              examiner_sd,
                              question_sd) {
  set.seed(seed)

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

ui <- fluidPage(
  titlePanel("Firearms Validity Data Generator"),
  sidebarLayout(
    sidebarPanel(
      numericInput("seed", "Random seed", value = 123, min = 1, step = 1),
      numericInput("n_examiners", "Number of examiners", value = 50, min = 1, step = 1),
      numericInput("n_comparisons", "Number of comparisons", value = 100, min = 1, step = 1),
      sliderInput("match_rate", "Proportion same-source", min = 0, max = 1, value = 0.5, step = 0.01),
      sliderInput("false_positive_rate", "Baseline false positive rate", min = 0, max = 0.5, value = 0.02, step = 0.01),
      sliderInput("false_negative_rate", "Baseline false negative rate", min = 0, max = 0.5, value = 0.05, step = 0.01),
      sliderInput("inconclusive_rate", "Baseline inconclusive rate", min = 0, max = 0.9, value = 0.10, step = 0.01),
      sliderInput("examiner_sd", "Examiner heterogeneity (SD)", min = 0, max = 2, value = 0.7, step = 0.05),
      sliderInput("question_sd", "Question difficulty heterogeneity (SD)", min = 0, max = 2, value = 0.8, step = 0.05),
      actionButton("generate", "Generate data"),
      hr(),
      downloadButton("download_rda", "Download .rda"),
      downloadButton("download_csv", "Download .csv")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Preview",
          br(),
          DTOutput("preview_table")
        ),
        tabPanel(
          "Summary",
          br(),
          tableOutput("summary_table")
        ),
        tabPanel(
          "Plots",
          br(),
          plotOutput("skill_plot", height = "300px"),
          plotOutput("difficulty_plot", height = "300px"),
          plotOutput("response_plot", height = "300px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  sim_data <- eventReactive(input$generate, {
    generate_sim_data(
      seed = input$seed,
      n_examiners = input$n_examiners,
      n_comparisons = input$n_comparisons,
      match_rate = input$match_rate,
      false_positive_rate = input$false_positive_rate,
      false_negative_rate = input$false_negative_rate,
      inconclusive_rate = input$inconclusive_rate,
      examiner_sd = input$examiner_sd,
      question_sd = input$question_sd
    )
  }, ignoreNULL = FALSE)

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
    ggplot(sim_data() %>% distinct(examiner_id, examiner_skill), aes(x = examiner_skill)) +
      geom_histogram(bins = 15) +
      labs(
        title = "Distribution of examiner skill",
        x = "Relative examiner skill (latent scale)",
        y = "Count"
      ) +
      theme_minimal()
  })

  output$difficulty_plot <- renderPlot({
    ggplot(sim_data() %>% distinct(question_id, question_difficulty), aes(x = question_difficulty)) +
      geom_histogram(bins = 15) +
      labs(
        title = "Distribution of question difficulty",
        x = "Relative question difficulty (latent scale)",
        y = "Count"
      ) +
      theme_minimal()
  })

  output$response_plot <- renderPlot({
    sim_data() %>%
      count(response) %>%
      ggplot(aes(x = response, y = n, fill = response)) +
      geom_col(show.legend = FALSE) +
      labs(
        title = "Distribution of simulated responses",
        x = "Response",
        y = "Count"
      ) +
      theme_minimal()
  })

  output$download_rda <- downloadHandler(
    filename = function() {
      "sim_data.rda"
    },
    content = function(file) {
      sim_data <- sim_data()
      save(sim_data, file = file)
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
