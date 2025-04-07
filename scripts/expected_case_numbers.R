#############################################################
# Calculate expected case numbers based on previous years   #
# using a Poisson-regression model                          #
#############################################################

# Importing tidyverse packages, in case it has not been initialized yet
library(tidyr)
library(dplyr)

#' @param in_tab Input dataframe, containing .
#' @param grouping_vars  The columns that identify strata, in addtion to Age and Sex.
#' @param base_min Starting year (time) of the period to use for regression.
#' @param base_max End year (time) of the period to use for regression.
#' @param collapse_ages If ages shold be used only as a co-variate or provide estimates separately.
#' @return A dataframe with grouping variables, the observed and the expected case numbers
#' @examples
#' calculate_poisson_expectation("Diagnosis", 2011, 2019))
calculate_poisson_expectation <- function(
    in_tab, grouping_vars, base_min=2011, base_max=2019,
    collapse_ages=TRUE, ...
) {
  model_input <- in_tab %>%
    mutate(
      N_cases  = round(N_cases, 0),
      Period   = factor(Period),
      Age      = as.numeric(factor(Age)),
      logpop   = log(Population)
    )
  
  model <- model_input %>%
    filter(Period %in% seq(base_min, base_max)) %>%
    mutate(
      Period = as.numeric(Period)
    )
  
  if (collapse_ages) {
    model <- model %>%
      glm(
        # N_cases ~ Period + offset(Age) + offset(logpop),
        N_cases ~ Period + Age + offset(logpop),
        family = poisson(link = "log"), data = ., ...
      )
  } else {
    model <- model %>%
      glm(
        N_cases ~ Period + offset(logpop),
        family = poisson(link = "log"), data = ., ...
      )
  }
  
  predicted_vector <- model_input %>%
    mutate(
      Period = as.numeric(Period)
    ) %>%
    select(Period, Age, logpop) %>%
    predict(model, newdata=.)
  
  in_tab$Predicted_numbers <- exp(predicted_vector)
  
  in_tab %>%
    group_by(across(all_of(c(grouping_vars, "Period")))) %>%
    summarise(
      Population        = sum(Population, na.rm=TRUE),
      N_cases           = sum(N_cases, na.rm=TRUE),
      Predicted_numbers = sum(Predicted_numbers, na.rm=TRUE)
    ) %>%
    ungroup()
  
}
