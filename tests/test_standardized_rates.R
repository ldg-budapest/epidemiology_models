#############################################################
# Test rate standardization                                 #
#############################################################

source("../scripts/standardized_rates.R", chdir=TRUE)


# Testing ESP weight generator

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
  expect_setequal(colnames(example_esp_tab), c("A1", "B2"))
)

example_esp_tab <- get_esp_pop(1976, breaks = seq(0, 90, 10))
test_that(
  "ESP1976 weights",
  expect_equal(example_esp_tab$Std_size, c(15000, 14000, 14000, 14000, 14000, 13000, 9000, 5000, 1600, 400))
)


# Testing weight extraction from population

dummy_pop <- data.frame(
  Age = c("20-29", "30-39", "40-49", "20-29", "30-39", "40-49", "20-29", "30-39", "40-49"),
  Sex = c("Female", "Female", "Female", "Male", "Male", "Male", "Total", "Total", "Total"),
  Population = c(20, 50, 30, 25, 70, 10, 45, 120, 40),
  Period = 2011
)

dummy_pop <- bind_rows(dummy_pop, mutate(dummy_pop, Period=2020, Population=Population+10))

test_that(
  "Weights are calculated correctly",
  expect_equal(
    round(.create_weight_table_from_population(dummy_pop, 2011)$Std_size, 1),
    c(20000, 50000, 30000, 23809.5, 66666.7, 9523.8, 21951.2, 58536.6, 19512.2)
  )
)
test_that(
  "Weights for another period",
  expect_equal(
    round(.create_weight_table_from_population(dummy_pop, 2020)$Std_size, 1),
    c(23076.9, 46153.8, 30769.2, 25925.9, 59259.3, 14814.8, 23404.3, 55319.1, 21276.6)
  )
)


# Testing CI margin calculation

test_that(
  "Confirm that CI margin is calculated",
  expect_equal(
    round(.calculate_esp_confidence_margin(c(0.1, 0.2, 0.5), c(10, 20, 50), c(100, 80, 120)), 1),
    c(19.6, 31, 40)
  )
)

# Test assessing the impact of missing values

standardized_table <- example_dataset %>%
  calculate_standardized_rate(
    extra_grouping_vars="Diagnosis",
    standard_population = "esp213"
  )

#TODO: another level of handling missing values is when expansion does not work; improve later
standardized_missing_table <- example_dataset %>%
  mutate(
    N_cases = ifelse(N_cases == 0, NA, N_cases)
  ) %>%
  calculate_standardized_rate(
    extra_grouping_vars="Diagnosis",
    standard_population = "esp2013"
  )

test_that(
  "Missing values are imputed correctly",
  expect_identical(standardized_missing_table, standardized_table)
)


# Tests to ensure consistency with previous results

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
