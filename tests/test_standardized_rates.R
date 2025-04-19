#############################################################
# Test rate standardization                                 #
#############################################################

source("../scripts/standardized_rates.R")

standardized_table <- example_dataset %>%
  calculate_standardized_rate(
    extra_grouping_cols="Diagnosis",
    standard_population = "esp213"
  )

rates_in_2021 <- standardized_table %>%
  filter(Period == "2021", Sex != "Total") %>%
  select(Age, Sex, Diagnosis, value=Std_rate) %>%
  mutate(
    value = round(value, 2)
  )  %>%
  arrange(Age, Sex, Diagnosis) %>%
  .$value

test_that(
  "ESP2013 rates in 2021",
  expect_equal(rates_in_2021, c(25.70, 55.72, 48.55, 113.71))
)
