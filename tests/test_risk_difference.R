#############################################################
# Test COVID impact model                                   #
#############################################################

source("../scripts/risk_difference.R", chdir=TRUE)

rr_table <- data.frame(
  Sex = c("M", "M", "F", "F"),
  N_patients = c(10, 10, 25, 5),
  Population_size = c(200, 100, 100, 100),
  Exposure = c("a", "b", "a", "b")
) %>%
    calculate_risk_difference(
      grouping_vars = c("Sex"), contrast=list(Exposure=c("b", "a")),
      case_var="N_patients", population_var="Population_size"
    )

test_that(
  "Risk difference in simplified example",
  expect_equal(rr_table$Riskdiff, c(-4, 0.5))
)
test_that(
  "Right column names in output",
  expect_equal(
    colnames(rr_table),
    c(
      'Sex', 'Population_size_b', 'Population_size_a',
      'N_patients_b', 'N_patients_a','Riskdiff', 'lower', 'upper'
    )
  )
)