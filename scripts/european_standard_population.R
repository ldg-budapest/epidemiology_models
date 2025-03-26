#########################################################
# Calculate Directly Standardized Rates (DSR) based on  #
# the 1976 and 2013 European Standard Populations (ESP) #
#########################################################

# Importing tidyverse packages, in case it has not been initialized yet
library(tidyr)
library(dplyr)
library(stringr)
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
    age_labels <- paste(breaks[-length(breaks)], breaks[-1]-1, sep="-")
  }
  
  # Create a temporary table with the finest granularity possible
  out_pop_tab <- data.frame(SINGLETON_AGE_NUMBERING=seq(0, 95))
  out_pop_tab[[pop_column]] <- esp_weights
  out_pop_tab[[age_column]] <- cut(out_pop_tab$SINGLETON_AGE_NUMBERING, breaks=breaks-1, labels=age_labels)
  
  # Deal with labels on the extremes
  if (include_age_extremes) {
    out_pop_tab[[age_column]] <- case_when(
      is.na(out_pop_tab[[age_column]]) & out_pop_tab$SINGLETON_AGE_NUMBERING < breaks[1] ~ paste("00", breaks[1]-1, sep="-"),
      is.na(out_pop_tab[[age_column]]) ~ paste(breaks[length(breaks)], "X", sep="-"),
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
#' @param Std_rate The calculated DSR
#' @param Raw_rate Crude number of persons affected.
#' @param Pop_size Number at risk; the total population.
#' @return A vector describing confidence margins.
#' @examples
#' calculate_esp_confidence_margin(x, y, z)
calculate_esp_confidence_margin <- function(Std_rate, Raw_rate, Pop_size) {
  
  # Simple approach used previously
  # 1.96*(Std_rate/sqrt(Raw_rate))
  
  # Slightly less narrow CI margins with
  # https://pophealthmetrics.biomedcentral.com/articles/10.1186/s12963-018-0177-1
  # (qchisq(0.975, 2 * Raw_rate + 2) / 2 -  Raw_rate) * sqrt(Raw_rate / Pop_size)
  
  # Current implementation
  qnorm(0.975, mean=0, sd=1) * sqrt(Std_rate * (100000 - Std_rate) / Pop_size)

}


#' A higher-lavel wrapper to be used in a dplyr chain, adding DSRs and confidence intervals to a dataframe
#'
#' @param input_tab A dataframe with grouping columns, like age group and year, crude number, DSR weights, and population size
#' @param rate_column The column name for the crude numbers in the input (typically "Raw_incidence")
#' @param rate_suffix The suffix to be used for DSR columns (e.g. if crude number is "Raw_incidence", suffix would be "incidence" by default).
#' @param grouping_cols The columns that identify strata. At least Sex, Age and Year will be required.
#' @param extra_grouping_cols A shortcut to just add a layer (like Diagnosis) over the usual grouping layers..
#' @param keep_draft_cols If the columns storing intermediate steps should be kept for debugging/checking the calculation.
#' @return A dataframe with the grouping columns and information on DSRs calculated by at this step.
#' @examples
#' add_standardized_rate(input_table, "Raw_incidence", keep_draft_cols=TRUE)
add_standardized_rate <- function(
    input_tab, rate_column, rate_suffix=str_remove(rate_column, "Raw_"),
    grouping_cols = c("Age", "Sex", "Year"), extra_grouping_cols = NULL,
    keep_draft_cols = FALSE
  ) {
  
  if (!is.null(extra_grouping_cols)) grouping_cols <- c(extra_grouping_cols, grouping_cols)
  
  # To allow the column name for crude numbers to be set flexibily
  .renamer_in <- function(s) {
    gsub(paste("^", rate_column, "$", sep=""), "Raw_rate_to_calculate_for", s)
  }
  
  # Some further flexibility with the naming of DSR and CI columns
  .renamer_out <- function(s) {
    gsub("rate_to_calculate_for", rate_suffix, s)
  }
  
  # Formatting input to have standard column names, no missing data and collapsed strata
  tmp_tab <- input_tab %>%
    rename_with(.renamer_in) %>%
    mutate(
      Raw_rate_to_calculate_for     = ifelse(
        is.na(Raw_rate_to_calculate_for), 0, Raw_rate_to_calculate_for
      )
    ) %>%
    # The whole point of the standardization porcess is to correct size of each age group;
    # The "Total" group makes no sense in this context and has to be avoided
    filter(Age != "Total") %>%
    group_by(across(all_of(grouping_cols))) %>%
    summarise(
      Raw_rate_to_calculate_for     = sum(Raw_rate_to_calculate_for, na.rm=TRUE),
      Population                    = sum(Population, na.rm=TRUE),
      Std_size                      = unique(Std_size)
    ) %>%
    ungroup()
  
  # Calculate Age-specific rates for each age group
  out_tab <- tmp_tab %>%
    mutate(
      Std_rate_to_calculate_for     = Raw_rate_to_calculate_for / Population * 100000,
      Std_rate_to_calculate_for     = ifelse(
        Population == 0, NA_real_, Std_rate_to_calculate_for
      ),
      Spc_rate_to_calculate_for     = Std_rate_to_calculate_for * Std_size / 100000,
      CI_margin                     = calculate_esp_confidence_margin(
        Std_rate_to_calculate_for, Raw_rate_to_calculate_for, Population
      ),
      CI_rate_to_calculate_for_lo   = Std_rate_to_calculate_for - CI_margin,
      CI_rate_to_calculate_for_hi   = Std_rate_to_calculate_for + CI_margin
    )
  
  # Summarize for total population
  out_tab <- out_tab %>%
    group_by(across(all_of(setdiff(grouping_cols, "Age")))) %>%
    summarise(
      Std_rate_to_calculate_for     = sum(Spc_rate_to_calculate_for, na.rm=TRUE),
      Raw_rate_to_calculate_for     = sum(Raw_rate_to_calculate_for, na.rm=TRUE),
      Population                    = sum(Population, na.rm=TRUE),
      Std_size                      = sum(Std_size, na.rm=TRUE)
    ) %>%
    ungroup() %>%
    mutate( 
      Age                           = "Total",
      Spc_rate_to_calculate_for     = Raw_rate_to_calculate_for / Population * 100000,
      CI_margin                     = calculate_esp_confidence_margin(
        Std_rate_to_calculate_for, Raw_rate_to_calculate_for, Population
      ),
      CI_rate_to_calculate_for_lo   = Std_rate_to_calculate_for - CI_margin,
      CI_rate_to_calculate_for_hi   = Std_rate_to_calculate_for + CI_margin
    ) %>%
    bind_rows(out_tab)
  
  # Remove unnecessary columns if not explicitly requested
  if (!keep_draft_cols) out_tab <- select(out_tab, -Std_size, -CI_margin)
  
  rename_with(out_tab, .renamer_out)
}