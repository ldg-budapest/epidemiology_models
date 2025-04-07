#############################################################
# Calculate average annual change in incidence or mortality #
# by fitting a Poisson regression model                     #
#############################################################

# Importing tidyverse packages, in case it has not been initialized yet
library(tidyr)
library(dplyr)
library(purrr)

# Importing specific packages
library(lmtest)
library(sandwich)


#' Calculate the estimated value and the Confidence Interval from a Poisson-model
#'
#' @param calculated_model A poisson model fit using `glm`.
#' @param stub_df A dataframe with unique grouping variables in the input. Output columns will be appended.
#' @param mid_var Column name for the estimate.
#' @return A dataframe with grouping variables and the calcualted estimated value, CI_lo, CI_hi and p-value.
#' @examples
#' add_mid_CI_from_model(calculated_model, stub_df)
  .add_mid_CI_from_model <- function(
    calculated_model, stub_df, mid_var="estimate"
) {
  
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
  stub_df[[mid_var]] <- result_estimate_p[1]
  stub_df$CI_lo <- ci_df[[1]]
  stub_df$CI_hi <- ci_df[[2]]
  stub_df$p_value <- result_estimate_p[4]
    
  stub_df
}


#' Fit a Poisson-model
#'
#' @param in_tab A poisson model fit using `glm`.
#' @param grouping_vars  The columns that identify strata. At least Sex, Age and Period will be required.
#' @param rate_var Column name for crude numbers.
#' @param mid_var Column name for the estimate.
#' @param ... Options passed on `glm` (the model fitting process).
#' @return A dataframe with grouping variables and the calculated estimate, CI_lo, CI_hi and p-value.
#' @examples
#' add_mid_CI_from_model(calculated_model, stub_df)
.create_poisson_df <- function(
    in_tab, grouping_vars, mid_var="estimate",
    ...
) {
  
  # Tidy input to match expected names and data format
  model_data_tab <- in_tab %>%
    mutate(
      N_cases    = round(N_cases, 0),
      Period     = as.numeric(factor(Period)),
      Age        = factor(Age),
      Sex        = factor(Sex),
      logpop     = log(Population)
    )
  
  # Fit a Poisson model, taking Sex into account only if input contains multiple
  if(n_distinct(model_data_tab$Sex) > 1) {
    # Same for age
    if(n_distinct(model_data_tab$Age) > 1) {
      model <- glm(
        N_cases ~ offset(logpop) + Period + Age + Sex,
        family = poisson(link = "log"), data = model_data_tab, ...
      )
    } else {
      model <- glm(
        N_cases ~ offset(logpop) + Period + Sex,
        family = poisson(link = "log"), data = model_data_tab, ...
      )
    }
  } else {
    if(n_distinct(model_data_tab$Age) > 1) {
      model <- glm(
        N_cases ~ offset(logpop) + Period + Age,
        family = poisson(link = "log"), data = model_data_tab, ...
      )
    } else {
      model <- glm(
        N_cases ~ offset(logpop) + Period,
        family = poisson(link = "log"), data = model_data_tab, ...
      )
    }
  }
  
  # Extract information from model
  .add_mid_CI_from_model(
      model, distinct(in_tab[, grouping_vars]), mid_var=mid_var
    )

}


#' Wrapper that handles error for one single data layer
.calc_errorprone_stats <- function(
    in_tab, stat_fun, grouping_vars=c("Diagnosis"), ...
){
  tryCatch(
    expr = {
      stat_fun(in_tab, grouping_vars=grouping_vars, ...)
    },
    error = function(e){
      in_tab %>%
        select(one_of(grouping_vars)) %>%
        distinct()
    }
  )
}


#' Wrapper that applies calculation for each data layer
.calc_layered_stat <- function(in_tab, stat_fun, grouping_vars, ...) {
  in_tab %>%
    group_by(across(all_of(grouping_vars))) %>%
    group_split() %>%
    purrr::map_dfr(
      .calc_errorprone_stats, stat_fun=stat_fun,
      grouping_vars=grouping_vars, ...
    )
  
}


#' High-level convenience function to fit a Poisson model on each data layer, skipping errors
calculate_poisson_rate <- function(in_tab, grouping_vars, ...) {
  .calc_layered_stat(in_tab, .create_poisson_df, grouping_vars, ...)
}