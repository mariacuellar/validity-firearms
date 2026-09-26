# Pre-computes the small summary of the Ames II cartridge-case data that the
# slides need, so the deck does not depend on the raw study spreadsheet.
# Run from the repo root:  Rscript icfis/R/prepare_ames2_slide_data.R
# Output: icfis/data/ames2_firearm_counts.csv
#   one row per (firearm type, ground truth, individual firearm, response) with
#   the count of responses; response "NA" = unsuitable / no answer.
#   firearm_order reproduces the left-to-right ordering used in the bar chart.
library(tidyverse)

ames2 <- readxl::read_xlsx(here::here("real-data/ames2.xlsx"), sheet = 3) |>
  mutate(response_3 = case_when(
    str_detect(`Examiner's Analysis`, "Inc") ~ "Inconclusive",
    str_detect(`Examiner's Analysis`, "ID") ~ "Identification",
    str_detect(`Examiner's Analysis`, "Elim") ~ "Elimination"
  ))

ames2_long <- ames2 |>
  pivot_longer(c(Gun_1, Gun_2), names_to = "index", values_to = "individual_firearm") |>
  mutate(individual_firearm = fct_reorder(
    individual_firearm, response_3 == "Inconclusive",
    .fun = sum, .desc = TRUE, .na_rm = FALSE
  ))

ames2_long |>
  mutate(response = replace_na(response_3, "NA")) |>
  count(Ber_NonBer, M_NM, individual_firearm, response, name = "n") |>
  mutate(firearm_order = as.integer(individual_firearm),
         individual_firearm = as.character(individual_firearm)) |>
  rename(ber = Ber_NonBer, m_nm = M_NM, firearm = individual_firearm) |>
  write_csv(here::here("icfis/data/ames2_firearm_counts.csv"))
