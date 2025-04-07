#########################################################
# Calculate Directly Standardized Rates (DSR) based on  #
# the 1976 and 2013 European Standard Populations (ESP) #
#########################################################

# Importing tidyverse packages, in case it has not been initialized yet
library(tidyr)
library(dplyr)
library(purrr)


#' Create a dataframe with ESP weight summarized for age categories in a given bin
#'
#' @param esp_year 2013 (falls back to this default silently!) or 1976.
#' @param breaks Age categories/bins for which we need the weights.
#' @param age_labels Labels for each bin. Generated automatically by default.
#' @param include_age_extremes Logical; if the firts age category (e.g 0-20) needs to be included.
#' @param age_column Column name for age categories.
#' @param pop_column Column name for weights.
#' @return A dataframe with two columns: Age and Std_size.
#' @examples
#' get_esp_pop(1976)
#' get_esp_pop(2013, seq(30, 80, 5))
get_esp_pop <- function(
    esp_year, breaks = seq(20, 80, 10),
    age_labels = NULL, include_age_extremes = TRUE,
    age_column = "Age", pop_column = "Std_size"
) {
  
  # European Standard Population weights for 5-year age bins
  # Standard sizes from https://www.causesofdeath.org/docs/standard.pdf
  if (esp_year == 1976) {
    esp_weights <- c(
      0.016, 0.064, 0.07, 0.07, 0.07, 0.07, 0.07, 0.07, 0.07, 0.07, 0.07,
      0.07, 0.06, 0.05, 0.04, 0.03, 0.02, 0.01, 0.006, 0.0032, 0.0008
    )
  } else {
    esp_weights <- c(
      0.01, 0.04, 0.055, 0.055, 0.055, 0.06, 0.06, 0.065, 0.07, 0.07,
      0.07, 0.07, 0.065, 0.06, 0.055, 0.05, 0.04, 0.025, 0.015, 0.008, 0.002
    )
  }
  
  # Expand weights for single-year age bins
  esp_weights <- c(
    esp_weights[1],
    rep(esp_weights[2] / 4, each=4),
    rep(esp_weights[c(-1, -2, -21)] / 5, each=5),
    esp_weights[21]
  ) * 100000
  
  # Generate labels if not supplied
  if (is.null(age_labels)) {
    age_break_start <- breaks[-length(breaks)]
    age_break_end <- breaks[-1]-1
    if(all(age_break_start == age_break_end)) {
      age_labels <- age_break_start
    } else {
      age_labels <- paste(age_break_start, age_break_end, sep="-")
    }
  }
  
  # Create a temporary table with the finest granularity possible
  out_pop_tab <- data.frame(SINGLETON_AGE_NUMBERING=seq(0, 95))
  out_pop_tab[[pop_column]] <- esp_weights
  out_pop_tab[[age_column]] <- cut(out_pop_tab$SINGLETON_AGE_NUMBERING, breaks=breaks-1, labels=age_labels)
  
  # Deal with labels on the extremes
  if (include_age_extremes) {
    out_pop_tab[[age_column]] <- case_when(
      is.na(out_pop_tab[[age_column]]) & out_pop_tab$SINGLETON_AGE_NUMBERING < breaks[1] ~ paste("0", breaks[1]-1, sep="-"),
      is.na(out_pop_tab[[age_column]]) & is.na(as.numeric(breaks[length(breaks)])) ~ paste(breaks[length(breaks)], "x", sep="-"),
      is.na(out_pop_tab[[age_column]]) ~ as.character(breaks[length(breaks)]),
      TRUE ~ as.character(out_pop_tab[[age_column]])
    )
  } else {
    out_pop_tab <- out_pop_tab[!is.na(out_pop_tab[[age_column]]),]
  }
  
  # Collapse age groups to the level of granularity that is needed
  out_pop_tab %>%
    group_by(!!sym(age_column)) %>%
    group_split() %>%
    purrr::map_dfr(
      function(.in_df) {
        .out <- data.frame(TMP_VAR_DEL=1)
        .out[[pop_column]] <- sum(.in_df[[pop_column]])
        .out[[age_column]] <- unique(.in_df[[age_column]])
        .out[,-1]
      }
    ) %>%
    arrange(!!sym(age_column))
  
}


#' Calculate confidence interval (more specifically, just the margin at this step) for a given DSR
#'
#' @param input_tab A dataframe containing at least a population size and a period column.
#' @param period_of_interest The period/year we want to use as standard of population distribution.
#' @return A dataframe with age group and weight.
#' @examples
#' .create_weight_table_from_population(x, 2013)
.create_weight_table_from_population <- function(input_tab, period_of_interest) {
  
  age_weight_tab <- input_tab %>%
    filter(Period == period_of_interest) %>%
    distinct(Age, Sex, Std_size=Population) %>%
    group_by(Sex) %>%
    mutate(
      Std_sum = sum(Std_size),
      Std_size = Std_size / Std_sum * 100000,
      Std_sum = sum(Std_size)
    ) %>%
    ungroup()

}


#' Calculate confidence interval (more specifically, just the margin at this step) for a given DSR
#'
#' @param Std_rate The calculated DSR
#' @param Raw_rate Crude number of persons affected.
#' @param Pop_size Number at risk; the total population.
#' @return A vector describing confidence margins.
#' @examples
#' .calculate_esp_confidence_margin(x, y, z)
.calculate_esp_confidence_margin <- function(Std_rate, Raw_rate, Pop_size) {
  
  # Simple approach used previously
  # 1.96*(Std_rate/sqrt(Raw_rate))
  
  # Slightly less narrow CI margins with
  # https://pophealthmetrics.biomedcentral.com/articles/10.1186/s12963-018-0177-1
  # (qchisq(0.975, 2 * Raw_rate + 2) / 2 -  Raw_rate) * sqrt(Raw_rate / Pop_size)
  
  # Current implementation
  qnorm(0.975, mean=0, sd=1) * sqrt(Std_rate * (100000 - Std_rate) / Pop_size)
  
}


#' A higher-level wrapper to be used in a dplyr chain, adding DSRs and confidence intervals to a dataframe
#'
#' @param input_tab A dataframe with grouping columns, like age group and year, crude numbers and population size.
#' @param standard_population Indicates if an ESP population should be used; also accepts a dataframe with weights.
#' @param grouping_cols The columns that identify strata. At least Sex, Age and Period (Year) will be required.
#' @param extra_grouping_cols A shortcut to just add a layer (like Diagnosis) over the usual grouping layers.
#' @return A dataframe with the standardized rates.
#' @examples
#' add_standardized_rate(calculate_standardized_rate, "esp2013")
calculate_standardized_rate <- function(
    input_tab, standard_population = NA,
    grouping_cols = c("Age", "Sex", "Period"),
    extra_grouping_cols = c(), dg_cols= c()
  ) {
  
  if (!is.null(extra_grouping_cols)) grouping_cols <- c(extra_grouping_cols, grouping_cols)
  
  # TODO: add some more checks
  #stopifnot(all(grouping_cols %in% colnames(input_tab)), "Expected columns missing")
  
  # The initial step is to calculate simple raw rates, without any weighting
  rate_tab <- input_tab %>%
    mutate(
      N_cases     = ifelse(is.na(N_cases), 0, N_cases)
    ) %>%
    group_by(across(all_of(grouping_cols))) %>%
    summarise(
      N_cases     = sum(N_cases, na.rm=TRUE),
      Population  = sum(Population, na.rm=TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      Std_rate    = N_cases / Population * 100000,
      Std_rate    = ifelse(
        Population == 0, NA_real_, Std_rate
      )
    )
  
  # A major bifurcation point is if direct standardization should be used;
  # and if yes, what weight (which population standard)
  if(!all(is.na(standard_population))) {
    
    # The whole point of the standardization process is to correct size of each age group;
    # The "Total" group makes no sense in this context and has to be avoided
    rate_tab <- rate_tab %>%
      dplyr::filter(Age != "Total")
    
    # Standardization weights need to be added
    if(is.data.frame(standard_population)) {
      
      # Easiest is if a weight table is provided directly
      age_weight_tab <- select(standard_population, everything())
      
      # Just to be sure, count the total of the age groups; might not add up to 100,000
      if("Sex" %in% colnames(age_weight_tab)) {
        age_weight_tab <- age_weight_tab %>%
          group_by(Sex) %>%
          mutate(
            Std_sum = sum(Std_size, na.rm=TRUE)
          ) %>%
          ungroup()
      } else {
        age_weight_tab$Std_sum <- sum(age_weight_tab$Std_size)
      }
    } else {
      
      # It is still easy to retrieve ESP weights
      if(grepl("esp", standard_population)) {
        
        # Age group labels are retrieved from input data; expected to have a specific format
        age_weight_breaks <- rate_tab %>%
          distinct(Age) %>%
          mutate(
            Age = as.numeric(gsub("(-|\\s|\\.).*", "", Age))
          ) %>%
          arrange(Age) %>%
          .$Age
        
        # Calling the function that builds the weights table
        age_weight_tab <- get_esp_pop(
          gsub("esp", "", standard_population),
          age_weight_breaks
        ) %>%
          # Should add up to 100000, just because the getter function works this way
          mutate(
            Std_sum = 100000
          )
        
      } else {
        if(standard_population %in% rate_tab$Period) {
          age_weight_tab <- .create_weight_table_from_population(rate_tab, standard_population)
        } else {
          #TODO maybe replace with hard stop; likely not the right call if it gets here
          age_weight_tab <- data.frame(
            Age = unique(rate_tab$Age),
            Std_size = 1
          )
        }
      }
    }
    
    # Calculate the weighted rates (first, for age groups)
    rate_tab <- rate_tab %>%
      right_join(age_weight_tab) %>%
      mutate(
        Spc_rate    = Std_rate * Std_size / Std_sum
      ) %>%
      # Summarize for total population
      group_by(across(all_of(setdiff(grouping_cols, "Age")))) %>%
      summarise(
        Std_rate    = sum(Spc_rate, na.rm=TRUE),
        N_cases     = sum(N_cases, na.rm=TRUE),
        Population  = sum(Population, na.rm=TRUE),
        Std_size    = sum(Std_size, na.rm=TRUE),
        Std_sum    = unique(Std_sum, na.rm=TRUE)
      ) %>%
      ungroup() %>%
      mutate( 
        Age         = "Total"
      )
  }
  
  rate_tab %>%
    mutate( 
      CI_margin   = .calculate_esp_confidence_margin(
        Std_rate, N_cases, Population
      ),
      CI_lo       = Std_rate - CI_margin,
      CI_hi       = Std_rate + CI_margin
    ) %>%
    select(one_of(grouping_cols), Std_rate, CI_lo, CI_hi, any_of(dg_cols))
  
}