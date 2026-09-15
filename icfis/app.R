library(shiny)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(DT)

# safe_logit(), simulate_response(), generate_sim_data(),
# generate_sim_data_incomplete(), study_crosstab(), and the canonical
# dgp_defaults / incomplete_defaults all live in R/simulate_dgp.R -- the
# single shared implementation used here, in icfis/analysis.qmd, and in the
# case-study documents. Edit that file, not a copy of it.
source("R/simulate_dgp.R", local = TRUE)

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
    h4("What this app simulates"),
    p(
      "Each simulated study has a fixed panel of examiners and a fixed set of ",
      "comparison items. Every examiner evaluates every item, and each item ",
      "has a known ground truth (same-source or different-source)."
    ),
    h4("The model, simplified"),
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
      "The baseline rates and the heterogeneity \\(\\sigma_Q\\), \\(\\sigma_S\\), ",
      "\\(\\sigma_T\\) are exactly the sliders on the left. Change them and press ",
      "\"Generate data\" to see how the simulated responses shift."
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

ui <- fluidPage(
  titlePanel("Firearms Validity Data Generator"),
  p(
    "This app simulates data from the model behind our ICFIS talk on ",
    em("Remediable and Non-Remediable Flaws in Firearms Validity Studies"),
    ". If you disagree with a default we used in the talk, change it here ",
    "and see how the simulated data change. See the Overview tab for a ",
    "summary of the model."
  ),
  sidebarLayout(
    sidebarPanel(
      numericInput("seed", "Random seed", value = 123, min = 1, step = 1),
      numericInput("n_examiners", "Number of examiners", value = dgp_defaults$n_examiners, min = 1, step = 1),
      numericInput("n_comparisons", "Number of comparisons", value = dgp_defaults$n_comparisons, min = 1, step = 1),
      sliderInput("match_rate", "Proportion same-source", min = 0, max = 1, value = dgp_defaults$match_rate, step = 0.01),
      sliderInput("false_positive_rate", "Baseline false positive rate", min = 0.01, max = 0.5, value = dgp_defaults$false_positive_rate, step = 0.01),
      sliderInput("false_negative_rate", "Baseline false negative rate", min = 0.01, max = 0.5, value = dgp_defaults$false_negative_rate, step = 0.01),
      sliderInput("inconclusive_rate", "Baseline inconclusive rate", min = 0.01, max = 0.9, value = dgp_defaults$inconclusive_rate, step = 0.01),
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
          "Preview",
          br(),
          DTOutput("preview_table")
        ),
        tabPanel(
          "Summary",
          br(),
          tableOutput("summary_table"),
          h4("Ground truth × decision"),
          tableOutput("crosstab_table")
        ),
        tabPanel(
          "Plots",
          br(),
          reading_the_plots,
          plotOutput("skill_plot", height = "300px"),
          plotOutput("difficulty_plot", height = "300px")
        )
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
