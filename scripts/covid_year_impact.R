#############################################################
# Calculate risk difference based on expected and observed  #
# case numbers; primarily intented to assess COVID impact.  #
#############################################################

# Importing tidyverse packages, in case it has not been initialized yet
library(tidyr)
library(dplyr)
library(purrr)
library(tibble)
library(epitools)

source("accessory_utils.R")

#' Calculate the estimated impact and the Confidence Interval from a Poisson-model
#'
#' @param calculated_model A poisson model fit using `glm`.
#' @param stub_df A dataframe with unique grouping variables in the input. Output columns will be appended.
#' @return A dataframe with grouping variables and the calcualted estimated value, CI_lo, CI_hi and p-value.
#' @examples
#' add_impact_CI_from_model(calculated_model, stub_df)
  .add_impact_CI_from_model <- function(calculated_model, stub_df) {
  
  # Use the robust method in the sandwich package to estimate significance
  result_estimate_p <- calculated_model %>%
    lmtest::coeftest(vcov = sandwich::sandwich) %>%
    .[-1,] %>% 
    data.frame() %>%
    tibble::rownames_to_column("Modelparam") %>%
    select(Modelparam, estimate=Estimate, pvalue=`Pr...z..`)
    

  # Use the robust method in the sandwich package to estimate CIs
  ci_df <- calculated_model %>%
    lmtest::coefci(vcov = sandwich::sandwich) %>%
    data.frame() %>%
    tibble::rownames_to_column("Modelparam") %>%
    slice(-1)
  
  colnames(ci_df) <- c("Modelparam", "CI_lo", "CI_hi")
  
  stub_df %>%
    crossing(result_estimate_p) %>%
    left_join(ci_df, by="Modelparam")
  
}


#' @param in_tab Input dataframe, containing case numbers, population size and time informaiton.
#' @param grouping_vars  The columns that identify strata, in addtion to Age and Sex.
#' @param impacted_years Years, where the impact (difference) is estimated.
#' @return A dataframe with grouping variables and an estimate of the impact for the given period(s).
#' @examples
#' .calculate_impact_of_year_for_layer()"Diagnosis", 2011, 2019)
.calculate_impact_of_year_for_layer <- function(
    in_tab, grouping_vars, impacted_years, ...
) {
  
  model_input <- in_tab %>%
    mutate(
      N_cases  = round(N_cases, 0),
      Period   = factor(as.character(Period)),
      Age      = factor(Age),
      logpop   = log(Population)
    )
  
  #TODO: a cleaner way to one-hot encode the COVID-years
  for (PR_year in impacted_years) {
    model_input[[paste("Period", PR_year, sep="_")]] <- ifelse(
      model_input$Period == PR_year, 1, 0
    )
  }
  
  if (n_distinct(in_tab$Age) == 1) {
    custom_formula <- "N_cases ~ Period + offset(logpop)"
  } else {
    custom_formula <- "N_cases ~ Period + Age + offset(logpop)"
  }
  
  custom_formula <- custom_formula %>%
    c(paste("Period", impacted_years, sep="_")) %>%
    paste(collapse = " + ") %>%
    as.formula()
  
  model <- model_input %>%
    mutate(
      Period = as.numeric(Period)
    ) %>%
    glm(
      custom_formula, family = poisson(link = "log"),
      data = ., ...
    )
  
  # Extract information from model
  .add_impact_CI_from_model(
      model, distinct(in_tab[, grouping_vars])
    )

}

#' @param in_tab Input dataframe, containing case numbers, population size and time informaiton.
#' @param grouping_vars  The columns that identify strata, in addtion to Age and Sex.
#' @param impacted_years Years, where the impact (difference) is estimated.
#' @return A dataframe with grouping variables and an estimate of the impact for the given period(s).
#' @examples
#' calculate_impact_of_year("Diagnosis", 2011, 2019)
calculate_impact_of_year <- function(
    in_tab, grouping_vars, impacted_years, ...
) {
  calculate_model_across_layers(
    in_tab, .calculate_impact_of_year_for_layer, grouping_vars=grouping_vars,
    impacted_years=impacted_years, ...
  )
}
