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

source("shared_wrappers.R")

#' A helper function to calculate RR layer-wise; it is intended to be used within a pmap wrapper.
#'
#' @param N_cases Number of observed cases.
#' @return A dataframe with one single row, containing input data and risk difference.
#' @examples
#' .risk_difference_for_layer(10, 100, 25, 200)
.risk_difference_for_layer <- function(in_data, contrast_var, ...){
  
  out_df <- in_data %>%
    pivot_wider(names_from=contrast_var, values_from=c("Population", "N_cases"))
  
  rr_res <- in_data %>%
    select(Population, N_cases) %>%
    t() %>% t() %>%
    epitools::riskratio() %>%
    .$measure %>%
    .[2,]
  
  out_df %>%
    mutate(
      Riskdiff = 1 - rr_res[["estimate"]],
      lower    = 1 - rr_res[["upper"]],
      upper    = 1 - rr_res[["lower"]]
    )
}


#' Calculate the change in disease (or detection) risk (chance), 
#' based on a dataframe containing expected numbers, observed numbers and
#' the population(s) these numbers should be projected to.
#'
#' @param in_tab The input dataframe containing case numbers and base population size.
#' @return A dataframe with risk difference and 95%CI.
#' @examples
#' calculate_risk_difference(expectations_table)
calculate_risk_difference <- function(
  in_tab, grouping_vars, contrast, case_var="N_cases", population_var="Population",
  subtract_from_bg=TRUE, ...
) {
  
  contrast_var <- names(contrast)[1]
  contrast <- contrast[[1]]
  
  formatted_tab <- in_tab[in_tab[[contrast_var]] %in% contrast, ]
  formatted_tab[[contrast_var]] <- factor(formatted_tab[[contrast_var]], levels=contrast)
  formatted_tab$N_cases <- formatted_tab[[case_var]]
  formatted_tab$Population <- formatted_tab[[population_var]]
  
  formatted_tab <- formatted_tab %>%
    group_by(across(all_of(c(grouping_vars, contrast_var)))) %>%
    summarise(
      Population  = sum(Population, na.rm=TRUE),
      N_cases     = sum(N_cases, na.rm=TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      N_cases     = round(N_cases, 0),
      Population  = round(Population, 0)
    )
  
  if(subtract_from_bg) {
    formatted_tab <- formatted_tab %>%
      arrange(!!sym(contrast_var)) %>%
      mutate(
        Population = Population - N_cases
      )
  }
  
  out_tab <- calculate_model_across_layers(
    formatted_tab, .risk_difference_for_layer, grouping_vars=grouping_vars,
    contrast_var=contrast_var, 
    err_fun = function(x) {
      pivot_wider(x, names_from=contrast_var, values_from=c("Population", "N_cases"))
    },
    ...
  ) %>%
    rename_with(~gsub("Population", population_var, .x)) %>%
    rename_with(~gsub("N_cases", case_var, .x))
  
  out_tab
  
}
