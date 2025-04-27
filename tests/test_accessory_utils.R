#############################################################
# Test that helper functions work as expected               #
#############################################################

source("../scripts/accessory_utils.R", chdir=TRUE)


# Testing safe execution wrapper

dummy_tab <- data.frame(
  a = seq(1, 4),
  b = seq(5, 8),
  c = c("a", "a", "b", "b")
)
dummy_stat_fun <- function(x, grouping_vars, vcol="a") {
  sum(x[[vcol]])
}
test_that(
  "Executing function with default, no errors",
  expect_equal(
    execute_model_safely(dummy_tab, dummy_stat_fun, "c"),
    10
  )
)
test_that(
  "Executing function passing an argument",
  expect_equal(
    execute_model_safely(dummy_tab, dummy_stat_fun, "c", vcol="b"),
    26
  )
)
test_that(
  "Executing function with error",
  expect_equal(
    execute_model_safely(dummy_tab, dummy_stat_fun, "c", vcom="a"),
    data.frame(c=c("a", "b"))
  )
)
test_that(
  "Custom error function",
  expect_equal(
    execute_model_safely(
      dummy_tab, dummy_stat_fun, "c", vcom="a",
      err_fun=function(x) "Hello world"
    ),
    "Hello world"
  )
)
test_that(
  "Executing function in debug mode, no error",
  expect_equal(
    execute_model_safely(dummy_tab, dummy_stat_fun, "c", debug_mode=TRUE),
    10
  )
)
test_that(
  "Executing function in debug mode, no error",
  expect_error(
    execute_model_safely(dummy_tab, dummy_stat_fun, "c", vcom="a", debug_mode=TRUE)
  )
)


# Testing layer-wise execution

dummy_tab <- data.frame(
  a = seq(1, 4),
  b = seq(5, 8),
  c = c("a", "a", "b", "b")
)
dummy_stat_fun <- function(x, grouping_vars, vcol="a") {
  out_df <- x %>%
    select(one_of(grouping_vars)) %>%
    distinct()
  
  out_df$v <- sum(x[[vcol]])
  
  out_df
}
dummy_results <- calculate_model_across_layers(
  dummy_tab, dummy_stat_fun, "c", vcol="b"
)
test_that(
  "Columns in result",
  expect_equal(colnames(dummy_results), c("c", "v"))
)
test_that(
  "Groups present",
  expect_equal(dummy_results$c, c("a", "b"))
)
test_that(
  "Expected values",
  expect_equal(dummy_results$v, c("11", "15"))
)


# Tests to ensure consistency with previous results

standardized_table <- example_dataset %>%#
  filter(Age != "Total") %>%
  execute_model_on_age_bins(
    seq(0, 80, 10), calculate_standardized_rate,
    extra_grouping_vars="Diagnosis", standard_population = "esp213"
  )

rates_in_2021 <- standardized_table %>%
  filter(Period == "2021", Sex != "Total", ) %>%
  select(Age, Sex, Diagnosis, value=Std_rate) %>%
  mutate(
    value = round(value, 2)
  )  %>%
  arrange(Age, Sex, Diagnosis)

test_that(
  "Age groups created",
  expect_equal(
    unique(rates_in_2021$Age),
    c('0-9', '10-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80-90', 'Total'))
)
test_that(
  "ESP2013 rates in 2021, Total Age",
  expect_equal(filter(rates_in_2021, Age == "Total")$value, c(25.7, 55.72, 48.55, 113.71))
)
test_that(
  "ESP2013 rates in 2021, Age 40-49",
  expect_equal(filter(rates_in_2021, Age == "40-49")$value, c(4.23, 1.97, 3.1, 7.99))
)
test_that(
  "ESP2013 rates in 2021, Age 50-59",
  expect_equal(filter(rates_in_2021, Age == "50-59")$value, c(7.65, 30.89, 15.15, 55.36))
)
test_that(
  "ESP2013 rates in 2021, Age 60-69",
  expect_equal(filter(rates_in_2021, Age == "60-69")$value, c(13.95, 51.29, 30.49, 105.64))
)
test_that(
  "ESP2013 rates in 2021, Age 70-79",
  expect_equal(filter(rates_in_2021, Age == "70-79")$value, c(13.55, 30.09, 29.34, 66.38))
)
