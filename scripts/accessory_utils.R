#############################################################
# Shared utility functions that facilitate                  #
# layer-wise execution of calculations                      #
#############################################################

# Importing tidyverse packages, in case it has not been initialized yet
library(tidyr)
library(dplyr)
library(purrr)

# Turn off an annoying warning related to summarizing groups
options(dplyr.summarise.inform = FALSE)

# Error handling for strata where statistic cannot be calculated
execute_model_safely <- function(
    in_tab, stat_fun, grouping_vars, err_fun=NULL, debug_mode=FALSE, ...
){
  
  if(is.null(err_fun)) {
    err_fun <- function(x) {
      x %>%
        select(one_of(grouping_vars)) %>%
        distinct()
    }
  }
  
  if (debug_mode) {
    stat_fun(in_tab, grouping_vars=grouping_vars, ...)
  } else {
    tryCatch(
      expr = {
        stat_fun(in_tab, grouping_vars=grouping_vars, ...)
      },
      error = function(e){
        err_fun(in_tab)
      }
    )
  }
}

# Convenience function to apply the statistical calculation to individual strata
calculate_model_across_layers <- function(in_tab, stat_fun, grouping_vars, ...) {
  
  in_tab %>%
    group_by(across(all_of(grouping_vars))) %>%
    group_split() %>%
    purrr::map_dfr(
      execute_model_safely, stat_fun=stat_fun, grouping_vars=grouping_vars, ...
    )
}

# Shortcut to calculate estimates for age groups
execute_model_on_age_bins <- function(
  input_tab, age_bins, stat_fun, add_total_age=TRUE, ...
) {
  
  age_bin_ends <- c(age_bins[-1] - 1, max(as.numeric(input_tab$Age)))
  
  out_tab <- data.frame(
      x=age_bins, y=age_bin_ends
    ) %>%
    purrr::pmap_dfr(
      function(x, y) {
        input_tab %>%
          filter(Age %in% seq(x, y)) %>%
          stat_fun(...) %>%
          mutate(
            Age = paste(x, y, sep="-")
          )
      }
    )
  
  if(add_total_age) {
    out_tab <- input_tab %>%
      stat_fun(...) %>%
      mutate(
        Age = "Total"
      ) %>%
      bind_rows(out_tab)
  }
  
  out_tab
}
