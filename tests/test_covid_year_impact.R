#############################################################
# Test COVID impact model                                   #
#############################################################

source("../scripts/covid_year_impact.R", chdir=TRUE)

yearly_impact_table <- example_dataset %>%
  filter(Period < 2022) %>%
  filter(Age %in% seq(60, 69)) %>%
  calculate_impact_of_year(
    grouping_vars = c("Diagnosis", "Sex"),
    impacted_years = c(2020, 2021)
  )

impact_percent <- yearly_impact_table %>%
  filter(Modelparam == "Period_2020") %>%
  arrange(Diagnosis, Sex) %>%
  mutate(
    estimate = round(estimate, 3)
  )  %>%
  .$estimate

test_that(
  "Impact of pandemic in 2020",
  expect_equal(impact_percent, c(0.065, -0.075, -0.019, -0.096))
)
