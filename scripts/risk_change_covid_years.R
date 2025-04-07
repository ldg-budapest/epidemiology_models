#############################################################
# Calculate risk difference based on expected and observed  #
# case numbers; primarily intented to assess COVID impact.  #
#############################################################

# Importing tidyverse packages, in case it has not been initialized yet
library(tidyr)
library(dplyr)
library(purrr)
library(epitools)


#' A helper function to calculate RR layer-wise; it is intended to be used within a pmap wrapper.
#'
#' @param N_cases Number of observed cases.
#' @param Population Population on which the observations are made.
#' @param Expected_numbers Number of expected cases.
#' @param Population_base Base population used to calculate expectetions. Needs to be declared, even if same as Population.
#' @return A dataframe with one single row, containing input data and risk difference.
#' @examples
#' .errorprone_rr_for_stratum(10, 100, 25, 200)
.errorprone_rr_for_stratum <- function(N_cases, Population, Expected_numbers, Population_base, ...){
  
  tryCatch(
    expr = {
      out_df <- data.frame(...)
      in_data <- matrix(
        c(
          Population-N_cases, Population_base-Expected_numbers,
          N_cases, Expected_numbers
        ),
        nrow=2
      )
      
      rr_res <- in_data %>%
        epitools::riskratio() %>%
        .$measure %>%
        .[2,]
      
      out_df %>%
        mutate(
          RR    = 1 - rr_res["estimate"],
          lower = 1 - rr_res["upper"],
          upper = 1 - rr_res["lower"]
        )
    },
    error = function(e){
      data.frame(...)
    }
  )
}


#' Calculate the change in disease (or dectection) risk (chance), 
#' based on a dataframe containing expected numbers, observed numbers and
#' the population(s) these numbers should be projected to.
#'
#' @param in_tab The input dataframe containg case numbers and base population size.
#' @return A dataframe with risk difference and 95%CI.
#' @examples
#' calculate_risk_difference(expectations_table)
calculate_risk_difference <- function(in_tab){
  
  if(! "Population_base" %in% colnames(in_tab)) {
    in_tab$Population_base <- in_tab$Population
  }

  in_tab %>%
    mutate(
      N_cases           = round(N_cases, 0),
      Population        = round(Population, 0),
      Expected_numbers = round(Expected_numbers, 0),
      Population_base   = round(Population_base, 0)
    ) %>%
    purrr::pmap_dfr(.errorprone_rr_for_stratum)
}


#' Calculate the estimated impact and the Confidence Interval from a Poisson-model
#'
#' @param calculated_model A poisson model fit using `glm`.
#' @param stub_df A dataframe with unique grouping variables in the input. Output columns will be appended.
#' @return A dataframe with grouping variables and the calcualted estimated value, CI_lo, CI_hi and p-value.
#' @examples
#' add_impact_CI_from_model(calculated_model, stub_df)
  .add_impact_CI_from_model <- function(
    calculated_model, stub_df, mid_var="estimate"
) {
  
  #TODO: extract estimate for year dummies, not the timeline

  # Use the robust method in the sandwich package to estimate significance
  result_estimate_p <- calculated_model %>%
    lmtest::coeftest(vcov = sandwich::sandwich) %>%
    .[2,] %>% 
    data.frame() %>%
    t()

  # Use the robust method in the sandwich package to estimate CIs
  ci_df <- calculated_model %>%
    lmtest::coefci(vcov = sandwich::sandwich) %>%
    data.frame() %>%
    slice(2)
  
  # Add calculated values to a stub of the input
  stub_df[["impact"]] <- result_estimate_p[1]
  stub_df$CI_lo <- ci_df[[1]]
  stub_df$CI_hi <- ci_df[[2]]
  stub_df$p_value <- result_estimate_p[4]
    
  stub_df
}


#' @param in_tab Input dataframe, containing case numbers, population size and time informaiton.
#' @param grouping_vars  The columns that identify strata, in addtion to Age and Sex.
#' @param base_min Starting year (time) of the period to use for regression.
#' @param base_max End year (time) of the period to use for regression.
#' @param impacted_years Years, where the impact (difference) is estimated.
#' @return A dataframe with grouping variables and an estimate of the impact for the given period(s).
#' @examples
#' calculate_impact_of_year("Diagnosis", 2011, 2019)
calculate_impact_of_year <- function(
    in_tab, grouping_vars, base_min=2011, base_max=2019,
    impacted_years = c(2020, 2021), ...
) {
  model_input <- in_tab %>%
    mutate(
      N_cases  = round(N_cases, 0),
      Period   = factor(Period),
      Age      = as.numeric(factor(Age)),
      logpop   = log(Population)
    )
  
  for (PR_year in impacted_years) {
    model_input[[paste("Period", PR_year, sep="_")]] <- ifelse(
      model_input$Period == PR_year, 1, 0
    )
  }
  
  custom_formula <- "N_cases ~ Period + Age + offset(logpop)" %>%
    c(paste("Period", impacted_years, sep="_")) %>%
    paste(collapse = " + ") %>%
    as.formula()
  
  model <- model_input %>%
    filter(Period %in% seq(base_min, base_max)) %>%
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
