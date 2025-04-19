#############################################################
# Test rate change calculation                              #
#############################################################

source("../scripts/annual_rate_change.R")

age_10_dataset <- example_dataset %>%
  filter(Age != "Total") %>%
  mutate(
    Age = as.numeric(Age),
    Age_c = cut(Age, seq(-1, 100, 10)),
    Age_d = gsub(",.*", "", Age_c),
    Age_d = gsub("\\(", "", Age_d),
    Age_c = gsub(".*,", "", Age_c),
    Age_c = gsub("\\]", "", Age_c),
    Age = paste(as.numeric(Age_d)+1, "-", Age_c, sep=""),
    Age = ifelse(Age == "90-99", "90-x", Age)
  ) %>%
  group_by(Age, Sex, Period, Diagnosis) %>%
  summarise(
    Population = sum(Population, na.rm=TRUE),
    N_cases = sum(N_cases, na.rm=TRUE)
  ) %>%
  ungroup()

change_estimations <- age_10_dataset %>%
  filter(Period %in% seq(2011, 2019)) %>%
  filter(Age != "Total", Sex != "Total") %>%
  calculate_poisson_rate(
    c("Diagnosis")
  )

estimated_total_changes <- change_estimations  %>%
  mutate(
    estimate = round(estimate, 3)
  )  %>%
  arrange(Diagnosis) %>%
  .$estimate

test_that(
  "ESP2013 rates in 2021",
  expect_equal(estimated_total_changes, c(-0.015, -0.010))
)
