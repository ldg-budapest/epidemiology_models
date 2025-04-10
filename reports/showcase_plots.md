# Setup

## Library imports

## Example dataset

A realistic dataset of colorectal and lung cancer mortality is used as
an example input. The important difference compared to the actual data
that can be downloaded is the 1-year granularity of age (instead of
5-year groups).

``` r
example_dataset <- "../example_data/crc_lung_mortality.csv" %>%
  read.csv(check.names=FALSE) %>%
  pivot_longer(
    one_of("Colorectal (C18)", "Lung (C33-34)"),
    names_to = "Diagnosis", values_to = "N_cases"
  ) %>%
  mutate(
    Age = as.character(Age)
  )

# Commonly, numbers for both sexes merged are also present
example_dataset <- example_dataset %>%
  group_by(Age, Period, Diagnosis) %>%
  summarise(
    N_cases = sum(N_cases, na.rm=TRUE),
    Population = sum(Population, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Sex = "Total"
  ) %>%
  bind_rows(example_dataset)

# Adding the total age group to show it does not interfere with models
example_dataset <- example_dataset %>%
  group_by(Sex, Period, Diagnosis) %>%
  summarise(
    N_cases = sum(N_cases, na.rm=TRUE),
    Population = sum(Population, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Age = "Total"
  ) %>%
  select(Diagnosis, Age, Sex, Period, N_cases, Population) %>%
  bind_rows(example_dataset)

example_dataset %>%
  filter(Diagnosis == "Lung (C33-34)", Period == "2018") %>%
  filter(Sex == "Total", Age %in% seq(88, 90)) %>%
  head() %>%
  knitr::kable()
```

| Diagnosis     | Age | Sex   | Period | N_cases | Population |
|:--------------|:----|:------|-------:|--------:|-----------:|
| Lung (C33-34) | 88  | Total |   2018 |      68 |    22626.0 |
| Lung (C33-34) | 89  | Total |   2018 |      58 |    18226.5 |
| Lung (C33-34) | 90  | Total |   2018 |      96 |    62032.0 |

# Fundamental charts

## Standardized rates

The table output of the standardization step is expected to look as
below:

``` r
source("../scripts/standardized_rates.R")

standardized_table <- example_dataset %>%
  calculate_standardized_rate(
    extra_grouping_cols="Diagnosis",
    standard_population = "2011"
  )

standardized_table %>%
  filter(Period == "2021", Sex != "Total") %>%
  select(Age, Sex, Diagnosis, value=Std_rate) %>%
  mutate(
    value = round(value, 2)
  ) %>%
  knitr::kable()
```

| Age   | Sex    | Diagnosis        | value |
|:------|:-------|:-----------------|------:|
| Total | Female | Colorectal (C18) | 26.73 |
| Total | Male   | Colorectal (C18) | 33.51 |
| Total | Female | Lung (C33-34)    | 58.61 |
| Total | Male   | Lung (C33-34)    | 86.41 |

Graphical representation of the data would look something like:

``` r
standardized_table %>%
  ggplot(aes(x=Period, y=Std_rate, color=Sex, group=Sex)) +
  geom_line() +
  geom_point() +
  facet_wrap("Diagnosis") +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  labs(x="", y="Incidence rate projected to 2011", color="")
```

![](C:\Users\szatamas\OneDrive%20-%20Merck%20Sharp%20&%20Dohme%20LLC\Documents\Code_repos\Analyses\epidemiology_models\reports\showcase_plots_files/figure-markdown_github/unnamed-chunk-4-1.png)

## Average annual change

``` r
source("../scripts/annual_rate_change.R")

change_table <- example_dataset %>%
  filter(Period %in% seq(2011, 2019)) %>%
  filter(Age != "Total") %>% #, Sex = "Total") %>%
  calculate_poisson_rate(c("Sex", "Diagnosis")) %>%
  mutate(
    Age = "Total"
  )
  
change_table <- data.frame(
    x=seq(0, 90, 10), y=seq(9, 100, 10)
  ) %>%
  purrr::pmap_dfr(
    function(x, y) {
      example_dataset %>%
        filter(Period %in% seq(2011, 2019)) %>%
        filter(Age %in% seq(x, y)) %>% #, Sex = "Total") %>%
        calculate_poisson_rate(c("Sex", "Diagnosis")) %>%
        mutate(
          Age = paste(x, y, sep="-")
        )
    }
  ) %>%
  bind_rows(change_table)

change_table %>%
  filter(Sex != "Total") %>%
  ggplot(aes(x=estimate, y=Age, color=Sex)) +
  geom_errorbarh(
    aes(xmin=CI_lo, xmax=CI_hi), position=position_dodge(width=0.6)
  ) +
  geom_point(position=position_dodge(width=0.6)) +
  facet_wrap("Diagnosis") +
  scale_x_continuous(labels=scales::percent) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  labs(x="", y="", color="")
```

![](C:\Users\szatamas\OneDrive%20-%20Merck%20Sharp%20&%20Dohme%20LLC\Documents\Code_repos\Analyses\epidemiology_models\reports\showcase_plots_files/figure-markdown_github/unnamed-chunk-5-1.png)

## Impact of indidual years

As a first step, as well as a sanity check, calculate expected numbers
first.

``` r
source("../scripts/expected_case_numbers.R")
source("../scripts/risk_change_covid_years.R")

expectation_table <- example_dataset %>%
  filter(Age != "Total", Sex != "Total") %>%
  calculate_poisson_expectation(
    grouping_vars = c("Diagnosis", "Sex")
  ) %>%
  mutate(
    Age = "Total"
  )
  
expectation_table <- data.frame(
    x=seq(0, 90, 10), y=seq(9, 100, 10)
  ) %>%
  purrr::pmap_dfr(
    function(x, y) {
      example_dataset %>%
        filter(Age %in% seq(x, y), Sex != "Total") %>%
        calculate_poisson_expectation(
          grouping_vars = c("Diagnosis", "Sex")
        ) %>%
        mutate(
          Age = paste(x, y, sep="-")
        )
    }
  ) %>%
  bind_rows(expectation_table)

expectation_table %>%
  filter(Age == "Total") %>%
  mutate(
    Period = as.numeric(Period)
  ) %>%
  ggplot(aes(x=Period)) +
  geom_col(aes(y=Expected_numbers), fill="grey") +
  geom_line(aes(y=N_cases, group=Sex), color="red") +
  facet_grid("Sex~Diagnosis")
```

![](C:\Users\szatamas\OneDrive%20-%20Merck%20Sharp%20&%20Dohme%20LLC\Documents\Code_repos\Analyses\epidemiology_models\reports\showcase_plots_files/figure-markdown_github/unnamed-chunk-6-1.png)

As a next step, impact can be estimated as risk differences.

``` r
riskdiff_table <- expectation_table %>%
  filter(Period %in% seq(2020, 2021)) %>%
  calculate_risk_difference()

riskdiff_table %>%
  filter(!Age %in% c("0-9", "10-19", "20-29", "30-39", "90-99")) %>%
  ggplot(aes(x=RR, y=Age, color=Sex)) +
  geom_errorbarh(
    aes(xmin=lower, xmax=upper), position=position_dodge(width=0.6)
  ) +
  geom_point(position=position_dodge(width=0.6)) +
  facet_grid("Period~Diagnosis") +
  scale_x_continuous(labels=scales::percent, limits=c(-1,1)) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  labs(x="", y="", color="")
```

![](C:\Users\szatamas\OneDrive%20-%20Merck%20Sharp%20&%20Dohme%20LLC\Documents\Code_repos\Analyses\epidemiology_models\reports\showcase_plots_files/figure-markdown_github/unnamed-chunk-7-1.png)

An alternative would be to use the same model for extrapolation and
assessment of the impact. The resulting change estimates should be
similar to the ones above.

``` r
yearly_impact_table <- data.frame(
    x=seq(0, 90, 10), y=seq(9, 100, 10)
  ) %>%
  purrr::pmap_dfr(
    function(x, y) {
      example_dataset %>%
        filter(Period < 2022) %>%
        filter(Age %in% seq(x, y)) %>% #, Sex = "Total") %>%
        group_split(Diagnosis, Sex) %>%
        map_dfr(
          calculate_impact_of_year,
          grouping_vars = c("Diagnosis", "Sex"),
          impacted_years = c(2020, 2021)
        ) %>%
        mutate(
          Age = paste(x, y, sep="-")
        )
    }
  )

yearly_impact_table %>%
  filter(Sex != "Total") %>%
  filter(!Age %in% c("0-9", "10-19", "20-29", "30-39", "90-99")) %>%
  filter(Modelparam %in% c("Period_2020", "Period_2021")) %>%
  mutate(
    Modelparam = gsub("Period_", "", Modelparam)
  ) %>%
  ggplot(aes(x=estimate, y=Age, color=Sex)) +
  geom_errorbarh(
    aes(xmin=CI_lo, xmax=CI_hi), position=position_dodge(width=0.6)
  ) +
  geom_point(position=position_dodge(width=0.6)) +
  facet_grid("Modelparam~Diagnosis") +
  scale_x_continuous(labels=scales::percent, limits=c(-1,1)) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  labs(x="", y="", color="")
```

![](C:\Users\szatamas\OneDrive%20-%20Merck%20Sharp%20&%20Dohme%20LLC\Documents\Code_repos\Analyses\epidemiology_models\reports\showcase_plots_files/figure-markdown_github/unnamed-chunk-8-1.png)

# End
