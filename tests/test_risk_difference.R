#############################################################
# Test COVID impact model                                   #
#############################################################

source("../scripts/risk_difference.R", chdir=TRUE)

rr_table <- example_dataset %>%
  filter(Period < 2022) %>%
  filter(Age %in% seq(60, 69)) %>%
  calculate_risk_difference(
    grouping_vars = c("Diagnosis", "Sex")
  )
