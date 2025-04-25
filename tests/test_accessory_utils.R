#############################################################
# Test that helper functions work as expected               #
#############################################################

source("../scripts/accessory_utils.R", chdir=TRUE)
source("../scripts/standardized_rates.R", chdir=TRUE)

standardized_table <- example_dataset %>%#
  filter(Age != "Total") %>%
  execute_model_on_age_bins(
    seq(0, 80, 10), calculate_standardized_rate,
    extra_grouping_vars="Diagnosis", standard_population = "esp213"
  )

rates_60_69_in_2021 <- standardized_table %>%
  filter(Period == "2021", Sex != "Total", Age == "60-69") %>%
  select(Age, Sex, Diagnosis, value=Std_rate) %>%
  mutate(
    value = round(value, 2)
  )  %>%
  arrange(Age, Sex, Diagnosis) %>%
  .$value

test_that(
  "ESP2013 rates in 2021",
  expect_equal(rates_60_69_in_2021, c(13.95, 51.29, 30.49, 105.64))
)