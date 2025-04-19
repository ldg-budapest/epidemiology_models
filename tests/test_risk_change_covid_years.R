#############################################################
# Test COVID impact model                                   #
#############################################################

source("../scripts/risk_change_covid_years.R")

yearly_impact_table <- example_dataset %>%
  filter(Period < 2022) %>%
  filter(Age %in% seq(60, 69)) %>%
  group_split(Diagnosis, Sex) %>%
  map_dfr(
    calculate_impact_of_year,
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
  expect_equal(impact_percent, c(0.068, -0.076, -0.018, -0.095))
)
