#############################################################
# Test rate change calculation                              #
#############################################################

# Importing tidyverse packages, in case it has not been initialized yet
library(tidyr)
library(dplyr)
library(testthat)

example_dataset <- "../example_data/crc_lung_mortality.csv" %>%
  read.csv(check.names=FALSE) %>%
  pivot_longer(
    one_of("Colorectal (C18)", "Lung (C33-34)"),
    names_to = "Diagnosis", values_to = "N_cases"
  ) %>%
  mutate(
    Age = as.character(Age)
  )

source("../scripts/expected_case_numbers.R")

expectation_table <- example_dataset %>%
  filter(Age != "Total") %>%
  calculate_poisson_expectation(
    grouping_vars = c("Diagnosis", "Sex")
  )

expected_cases <- expectation_table %>%
  mutate(
    estimate = round(Expected_numbers, 1)
  )  %>%
  filter(Period == 2020) %>%
  arrange(Diagnosis, Sex) %>%
  .$estimate

test_that(
  "Expected cases in 2020",
  expect_equal(expected_cases, c(100, 200, 300, 400))
)
