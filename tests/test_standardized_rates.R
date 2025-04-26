#############################################################
# Test rate standardization                                 #
#############################################################

source("../scripts/standardized_rates.R", chdir=TRUE)

# Testing helpers

example_esp_tab <- get_esp_pop(2013, breaks = seq(40, 70, 10))
test_that(
  "ESP2013 weights",
  expect_equal(example_esp_tab$Std_size, c(47000, 14000, 13500, 11500, 14000))
)
test_that(
  "Column names by default",
  expect_equal(colnames(example_esp_tab), c("Std_size", "Age"))
)

example_esp_tab <- get_esp_pop(
  2013, breaks = seq(0, 90, 10), age_column = "A1", pop_column = "B2"
)
test_that(
  "ESP2013 weights wider",
  expect_equal(example_esp_tab$B2, c(10500, 11000, 12000, 13500, 14000, 13500, 11500, 9000, 4000, 1000))
)
test_that(
  "Column names customized",
  expect_equal(colnames(example_esp_tab), c("A1", "B2"))
)

example_esp_tab <- get_esp_pop(1976, breaks = seq(0, 90, 10))
test_that(
  "ESP1976 weights",
  expect_equal(example_esp_tab$Std_size, c(15000, 14000, 14000, 14000, 14000, 13000, 9000, 5000, 1600, 400))
)


# Tests to ensure consistency with previous results

standardized_table <- example_dataset %>%
  calculate_standardized_rate(
    extra_grouping_vars="Diagnosis",
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
