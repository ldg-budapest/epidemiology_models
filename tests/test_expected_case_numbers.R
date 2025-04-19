#############################################################
# Test rate change calculation                              #
#############################################################

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
  expect_equal(expected_cases, c(1389.3, 1724.8, 3398.1, 4692.2))
)
