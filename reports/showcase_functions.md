# Setup

## Library imports

## Example dataset

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
  bind_rows(example_dataset)

example_dataset %>%
  head() %>%
  knitr::kable()
```

| Sex    | Period | Diagnosis        | N_cases | Population | Age   |
|:-------|-------:|:-----------------|--------:|-----------:|:------|
| Female |   2011 | Colorectal (C18) |    1511 |    5233914 | Total |
| Female |   2011 | Lung (C33-34)    |    2974 |    5233914 | Total |
| Female |   2012 | Colorectal (C18) |    1570 |    5200052 | Total |
| Female |   2012 | Lung (C33-34)    |    3132 |    5200052 | Total |
| Female |   2013 | Colorectal (C18) |    1539 |    5183410 | Total |
| Female |   2013 | Lung (C33-34)    |    3173 |    5183410 | Total |

``` r
age_10_dataset <- example_dataset %>%
  filter(Age != "Total") %>%
  mutate(
    Age = as.numeric(Age),
    Age_c = cut(Age, seq(-1, 100, 10)),
    Age_d = gsub(",.*", "", Age_c),
    Age_d = gsub("\\(", "", Age_d),
    Age_c = gsub(".*,", "", Age_c),
    Age_c = gsub("\\]", "", Age_c),
    Age = paste(as.numeric(Age_d)+1, "-", Age_c, sep=""),
    Age = ifelse(Age == "90-99", "90-x", Age)
  ) %>%
  group_by(Age, Sex, Period, Diagnosis) %>%
  summarise(
    Population = sum(Population, na.rm=TRUE),
    N_cases = sum(N_cases, na.rm=TRUE)
  ) %>%
  ungroup()

# Re-adding the total age group
age_10_dataset <- age_10_dataset %>%
  group_by(Sex, Period, Diagnosis) %>%
  summarise(
    N_cases = sum(N_cases, na.rm=TRUE),
    Population = sum(Population, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Age = "Total"
  ) %>%
  bind_rows(age_10_dataset)

age_10_dataset %>%
  head() %>%
  knitr::kable()
```

| Sex    | Period | Diagnosis        | N_cases | Population | Age   |
|:-------|-------:|:-----------------|--------:|-----------:|:------|
| Female |   2011 | Colorectal (C18) |    1511 |    5233914 | Total |
| Female |   2011 | Lung (C33-34)    |    2974 |    5233914 | Total |
| Female |   2012 | Colorectal (C18) |    1570 |    5200052 | Total |
| Female |   2012 | Lung (C33-34)    |    3132 |    5200052 | Total |
| Female |   2013 | Colorectal (C18) |    1539 |    5183410 | Total |
| Female |   2013 | Lung (C33-34)    |    3173 |    5183410 | Total |

# Functionality

## Standardized rates

``` r
source("../scripts/standardized_rates.R")
```

By default, rate is standardized as the number of cases in 100,000
persons.

``` r
standardized_table <- age_10_dataset %>%
  calculate_standardized_rate(extra_grouping_cols="Diagnosis")

standardized_table %>%
  filter(Period == "2021", Sex == "Total") %>%
  select(Age, name=Diagnosis, value=Std_rate) %>%
  mutate(
    value = round(value, 2)
  ) %>%
  pivot_wider() %>%
  tail(8) %>%
  knitr::kable()
```

| Age   | Colorectal (C18) | Lung (C33-34) |
|:------|-----------------:|--------------:|
| 30-39 |             1.20 |          0.88 |
| 40-49 |             6.51 |          8.41 |
| 50-59 |            20.80 |         74.46 |
| 60-69 |            69.62 |        245.90 |
| 70-79 |           128.61 |        302.89 |
| 80-89 |           205.97 |        247.96 |
| 90-x  |           224.73 |        141.55 |
| Total |            34.19 |         81.02 |

The same function can be used to calculate direct standardized rates

``` r
standardized_table <- age_10_dataset %>%
  calculate_standardized_rate(
    extra_grouping_cols="Diagnosis", standard_population="esp2013"
  )

standardized_table %>%
  select(Period, Sex, Diagnosis, value=Std_rate) %>%
  mutate(
    value = round(value, 2)
  ) %>%
  pivot_wider(names_from=c("Diagnosis", "Sex")) %>%
  arrange(desc(Period)) %>%
  head() %>%
  knitr::kable()
```

| Period | Colorectal (C18)\_Female | Colorectal (C18)\_Male | Colorectal (C18)\_Total | Lung (C33-34)\_Female | Lung (C33-34)\_Male | Lung (C33-34)\_Total | NA_NA |
|----:|-----------:|----------:|-----------:|----------:|---------:|---------:|---:|
|   2023 |                    21.63 |                  44.74 |                   30.66 |                 57.08 |              111.36 |                78.98 |    NA |
|   2022 |                    22.86 |                  44.11 |                   31.20 |                 54.42 |              107.67 |                75.75 |    NA |
|   2021 |                    23.48 |                  44.76 |                   31.75 |                 54.22 |              110.71 |                76.83 |    NA |
|   2020 |                    22.78 |                  42.81 |                   30.58 |                 55.62 |              115.43 |                79.57 |    NA |
|   2019 |                    22.53 |                  44.38 |                   31.07 |                 57.80 |              119.71 |                82.73 |    NA |
|   2018 |                    23.23 |                  44.73 |                   31.47 |                 56.79 |              128.48 |                85.82 |    NA |

The 10-year granularity should not differ much from single-year
granularity:

``` r
standardized_table <- example_dataset %>%
  calculate_standardized_rate(
    extra_grouping_cols="Diagnosis", standard_population="esp2013"
  )

standardized_table %>%
  select(Period, Sex, Diagnosis, value=Std_rate) %>%
  mutate(
    value = round(value, 2)
  ) %>%
  pivot_wider(names_from=c("Diagnosis", "Sex")) %>%
  arrange(desc(Period)) %>%
  head() %>%
  knitr::kable()
```

| Period | Colorectal (C18)\_Female | Colorectal (C18)\_Male | Colorectal (C18)\_Total | Lung (C33-34)\_Female | Lung (C33-34)\_Male | Lung (C33-34)\_Total |
|----:|------------:|-----------:|-----------:|----------:|---------:|----------:|
|   2023 |                    23.56 |                  49.34 |                   33.21 |                 58.47 |              115.26 |                80.85 |
|   2022 |                    24.92 |                  48.20 |                   33.72 |                 55.69 |              110.74 |                77.45 |
|   2021 |                    25.70 |                  48.55 |                   34.37 |                 55.72 |              113.71 |                78.68 |
|   2020 |                    24.79 |                  47.40 |                   33.23 |                 57.14 |              120.07 |                81.93 |
|   2019 |                    24.81 |                  49.09 |                   33.97 |                 59.58 |              124.14 |                85.25 |
|   2018 |                    25.54 |                  49.37 |                   34.36 |                 58.66 |              133.09 |                88.33 |

A convenience option is to standardize to one of the years also present
in the data by simply supplying that year as a parameter.

``` r
standardized_table <- age_10_dataset %>%
  calculate_standardized_rate(
    extra_grouping_cols="Diagnosis", standard_population="2011"
  )

standardized_table %>%
  select(Period, Sex, Diagnosis, value=Std_rate) %>%
  mutate(
    value = round(value, 2)
  ) %>%
  pivot_wider(names_from=c("Diagnosis", "Sex")) %>%
  arrange(desc(Period)) %>%
  head() %>%
  knitr::kable()
```

| Period | Colorectal (C18)\_Female | Colorectal (C18)\_Male | Colorectal (C18)\_Total | Lung (C33-34)\_Female | Lung (C33-34)\_Male | Lung (C33-34)\_Total |
|----:|------------:|-----------:|-----------:|----------:|---------:|----------:|
|   2023 |                    24.73 |                  33.34 |                   29.17 |                 61.70 |               86.51 |                74.36 |
|   2022 |                    25.91 |                  33.28 |                   29.69 |                 58.80 |               83.50 |                71.28 |
|   2021 |                    26.74 |                  33.59 |                   30.27 |                 58.58 |               86.13 |                72.43 |
|   2020 |                    25.86 |                  32.05 |                   29.04 |                 59.91 |               90.41 |                75.16 |
|   2019 |                    25.79 |                  33.53 |                   29.69 |                 62.32 |               94.62 |                78.37 |
|   2018 |                    26.62 |                  33.38 |                   30.02 |                 61.26 |              102.26 |                81.51 |

Please note, however that DSRs are applied to the total population and
not to separate age groups. Rates could be projected onto the population
of a single year, by explicitly providing weight tables for as bands as
below:

``` r
standardized_table <- purrr::map_dfr(
  list(c(50, 59), c(60, 69), c(70, 79)),
  function(x) {
    example_dataset %>%
    calculate_standardized_rate(
      extra_grouping_cols="Diagnosis",
      standard_population = .create_weight_table_from_population(
        filter(example_dataset, Age %in% seq(x[1], x[2])), "2011"
      )
    )%>%
    mutate(
      Age = paste(x[1], x[2], sep="-")
    )
  }
)

standardized_table %>%
  filter(Period == "2021", Sex != "Total") %>%
  select(Age, Sex, Diagnosis, value=Std_rate) %>%
  mutate(
    value = round(value, 2)
  ) %>%
  pivot_wider(names_from=c("Sex", "Diagnosis")) %>%
  tail(8) %>%
  knitr::kable()
```

| Age   | Female_Colorectal (C18) | Male_Colorectal (C18) | Female_Lung (C33-34) | Male_Lung (C33-34) |
|:-----|------------------:|----------------:|----------------:|--------------:|
| 50-59 |                   18.16 |                 25.58 |                59.69 |             101.46 |
| 60-69 |                   46.41 |                 94.59 |               177.40 |             321.10 |
| 70-79 |                   86.72 |                195.84 |               202.39 |             459.04 |

As a comparison, the raw rates in the above age groups in 2021 was:

``` r
standardized_table <- age_10_dataset %>%
  calculate_standardized_rate(extra_grouping_cols="Diagnosis")

standardized_table %>%
  filter(Period == "2021", Sex != "Total") %>%
  select(Age, Sex, Diagnosis, value=Std_rate) %>%
  mutate(
    value = round(value, 2)
  ) %>%
  pivot_wider(names_from=c("Sex", "Diagnosis")) %>%
  tail(8) %>%
  knitr::kable()
```

| Age   | Female_Colorectal (C18) | Male_Colorectal (C18) | Female_Lung (C33-34) | Male_Lung (C33-34) |
|:-----|------------------:|----------------:|----------------:|--------------:|
| 30-39 |                    1.64 |                  0.78 |                 0.66 |               1.09 |
| 40-49 |                    6.52 |                  6.50 |                 6.27 |              10.50 |
| 50-59 |                   17.60 |                 24.13 |                55.91 |              93.75 |
| 60-69 |                   47.46 |                 97.50 |               180.99 |             327.59 |
| 70-79 |                   85.99 |                195.40 |               202.68 |             459.91 |
| 80-89 |                  168.72 |                290.09 |               165.65 |             433.83 |
| 90-x  |                  198.91 |                301.48 |               134.56 |             162.34 |
| Total |                   29.89 |                 38.87 |                64.66 |              98.79 |

## Average yearly change

``` r
source("../scripts/annual_rate_change.R")
```

A simple case if we want to see change for the total population and put
age and sex as co-variates.

``` r
standardized_table <- age_10_dataset %>%
  filter(Period %in% seq(2011, 2019)) %>%
  filter(Age != "Total", Sex != "Total") %>%
  calculate_poisson_rate(
    c("Diagnosis")
  )

standardized_table %>%
  knitr::kable()
```

| Diagnosis        |   estimate |      CI_lo |      CI_hi |   p_value |
|:-----------------|-----------:|-----------:|-----------:|----------:|
| Colorectal (C18) | -0.0146177 | -0.0206011 | -0.0086343 | 0.0000017 |
| Lung (C33-34)    | -0.0100003 | -0.0206381 |  0.0006374 | 0.0653984 |

This could be done separately for age groups in the merged dataset.

``` r
standardized_table <- age_10_dataset %>%
  filter(Period %in% seq(2011, 2019)) %>%
  filter(Sex == "Total") %>%
  calculate_poisson_rate(
    c("Age", "Diagnosis")
  )

standardized_table %>%
  select(Age, name=Diagnosis, value=estimate) %>%
  mutate(
    value = round(value, 3)
  ) %>%
  pivot_wider() %>%
  tail(8) %>%
  knitr::kable()
```

| Age   | Colorectal (C18) | Lung (C33-34) |
|:------|-----------------:|--------------:|
| 30-39 |            0.017 |        -0.042 |
| 40-49 |           -0.025 |        -0.109 |
| 50-59 |           -0.036 |        -0.055 |
| 60-69 |           -0.005 |         0.004 |
| 70-79 |           -0.010 |         0.012 |
| 80-89 |           -0.017 |         0.005 |
| 90-x  |           -0.019 |        -0.016 |
| Total |           -0.001 |         0.002 |

Better still, if this is calculated from the 1-year granularity of age
groups:

``` r
standardized_table <- example_dataset %>%
  filter(Period %in% seq(2011, 2019)) %>%
  filter(Age %in% seq(60, 69)) %>%
  calculate_poisson_rate(
    c("Diagnosis")
  )

standardized_table %>%
  mutate(
    Age = "60-69"
  ) %>%
  select(Age, name=Diagnosis, value=estimate) %>%
  mutate(
    value = round(value, 3)
  ) %>%
  pivot_wider() %>%
  tail(8) %>%
  knitr::kable()
```

| Age   | Colorectal (C18) | Lung (C33-34) |
|:------|-----------------:|--------------:|
| 60-69 |           -0.007 |         0.003 |

## Expected case numbers

``` r
source("../scripts/expected_case_numbers.R")
```

One possible approach is to use the Poisson-model created previously and
predict case numbers based on the fit model.

``` r
standardized_table <- example_dataset %>%
  filter(Age %in% seq(40, 79), Sex != "Total") %>%
  mutate(
    Age_bin = case_when(
      Age %in% seq(40, 49) ~ "40-49",
      Age %in% seq(50, 59) ~ "50-59",
      Age %in% seq(60, 69) ~ "60-69",
      Age %in% seq(70, 79) ~ "70-79",
      TRUE ~ NA_character_
    )
  ) %>%
  group_split(Diagnosis, Sex, Age_bin) %>%
  map_dfr(
    calculate_poisson_expectation,
    grouping_vars = c("Diagnosis", "Sex", "Age_bin")
  )

standardized_table %>%
  filter(Period == "2021", Sex != "Total") %>%
  select(Age_bin, Sex, Diagnosis, value=Expected_numbers) %>%
  mutate(
    value = round(value, 2)
  ) %>%
  pivot_wider(names_from=c("Sex", "Diagnosis")) %>%
  tail(8) %>%
  knitr::kable()
```

| Age_bin | Female_Colorectal (C18) | Male_Colorectal (C18) | Female_Lung (C33-34) | Male_Lung (C33-34) |
|:------|-----------------:|----------------:|---------------:|--------------:|
| 40-49   |                   37.33 |                 39.23 |                60.88 |              92.49 |
| 50-59   |                   81.76 |                160.61 |               463.86 |             667.86 |
| 60-69   |                  291.94 |                510.36 |              1372.08 |            2076.96 |
| 70-79   |                  472.01 |                614.82 |              1179.45 |            1637.65 |

As a reference, observed case numbers look as below.

``` r
standardized_table %>%
  filter(Period == "2021", Sex != "Total") %>%
  select(Age_bin, Sex, Diagnosis, value=N_cases) %>%
  mutate(
    value = round(value, 2)
  ) %>%
  pivot_wider(names_from=c("Sex", "Diagnosis")) %>%
  tail(8) %>%
  knitr::kable()
```

| Age_bin | Female_Colorectal (C18) | Male_Colorectal (C18) | Female_Lung (C33-34) | Male_Lung (C33-34) |
|:------|-----------------:|----------------:|---------------:|--------------:|
| 40-49   |                      51 |                    52 |                   49 |                 84 |
| 50-59   |                     113 |                   149 |                  359 |                579 |
| 60-69   |                     332 |                   542 |                 1266 |               1821 |
| 70-79   |                     462 |                   670 |                 1089 |               1577 |

As an alternative reference, expected case numbers calculated from
virtual numbers back-projected from rates.

``` r
standardized_table <- purrr::map_dfr(
  list(c(50, 59), c(60, 69), c(70, 79)),
  function(x) {
    example_dataset %>%
    calculate_standardized_rate(
      extra_grouping_cols="Diagnosis",
      standard_population = .create_weight_table_from_population(
        filter(example_dataset, Age %in% seq(x[1], x[2])), "2011"
      )
    )%>%
    mutate(
      Age = paste(x[1], x[2], sep="-")
    )
  }
) %>%
  inner_join(age_10_dataset) %>%
  mutate(
    Projected_cases = round(Population / 100000 * Std_rate, 1)
  )

standardized_table %>%
  filter(Period == "2021", Sex != "Total") %>%
  head(8) %>%
  select(Sex, Age, Diagnosis, N_cases, Projected_cases) %>%
  knitr::kable()
```

| Sex    | Age   | Diagnosis        | N_cases | Projected_cases |
|:-------|:------|:-----------------|--------:|----------------:|
| Female | 50-59 | Colorectal (C18) |     113 |           116.6 |
| Male   | 50-59 | Colorectal (C18) |     149 |           158.0 |
| Female | 50-59 | Lung (C33-34)    |     359 |           383.3 |
| Male   | 50-59 | Lung (C33-34)    |     579 |           626.6 |
| Female | 60-69 | Colorectal (C18) |     332 |           324.6 |
| Male   | 60-69 | Colorectal (C18) |     542 |           525.8 |
| Female | 60-69 | Lung (C33-34)    |    1266 |          1240.9 |
| Male   | 60-69 | Lung (C33-34)    |    1821 |          1784.9 |

## Impact of COVID years

``` r
source("../scripts/risk_change_covid_years.R")
```

Impact can be measured as the risk difference during pandemic years
compared to expectations based on some projection (average of base years
or calculated form trends).  
As an example, the risk difference for the 60-69 age group is shown
below.

``` r
expectation_table <- example_dataset %>%
  filter(Age %in% seq(60, 69), Sex == "Total") %>%
  mutate(
    Age_bin = case_when(
      Age %in% seq(60, 69) ~ "60-69",
      TRUE ~ NA_character_
    )
  ) %>%
  group_split(Diagnosis, Sex, Age_bin) %>%
  map_dfr(
    calculate_poisson_expectation,
    grouping_vars = c("Diagnosis", "Age_bin")
)

riskdiff_table <- expectation_table %>%
  filter(Period %in% seq(2020, 2022)) %>%
  select(-Age_bin) %>%
  calculate_risk_difference()

riskdiff_table %>%
  knitr::kable()
```

| Diagnosis        | Period |         RR |      lower |      upper |
|:-----------------|-------:|-----------:|-----------:|-----------:|
| Colorectal (C18) |   2020 | -0.0223881 | -0.1267392 |  0.0722988 |
| Colorectal (C18) |   2021 |  0.0812357 | -0.0111115 |  0.1651486 |
| Colorectal (C18) |   2022 |  0.0599520 | -0.0362844 |  0.1472513 |
| Lung (C33-34)    |   2020 | -0.0648402 | -0.1167034 | -0.0153857 |
| Lung (C33-34)    |   2021 | -0.1149984 | -0.1704338 | -0.0621886 |
| Lung (C33-34)    |   2022 | -0.1500000 | -0.2081899 | -0.0946127 |

An even more robust solution is to use the same Poisson model that
yielded expected numbers and add the post-COVID years as a dummy
variable.

``` r
covid_impact_table <- example_dataset %>%
  filter(Age %in% seq(60, 69), Sex == "Total") %>%
  mutate(
    Age_bin = case_when(
      Age %in% seq(60, 69) ~ "60-69",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(Period < 2022) %>%
  group_split(Diagnosis, Sex, Age_bin) %>%
  map_dfr(
    calculate_impact_of_year,
    grouping_vars = c("Diagnosis", "Age_bin"),
    impacted_years = c(2020, 2021)
)

covid_impact_table %>%
  knitr::kable()
```

| Diagnosis        | Age_bin | Modelparam  |   estimate |    pvalue |      CI_lo |      CI_hi |
|:--------------|:-------|:----------|---------:|---------:|---------:|---------:|
| Colorectal (C18) | 60-69   | Age         |  0.0713939 | 0.0000000 |  0.0637617 |  0.0790261 |
| Colorectal (C18) | 60-69   | Period      | -0.0069288 | 0.2342358 | -0.0183455 |  0.0044878 |
| Colorectal (C18) | 60-69   | Period_2020 | -0.0220790 | 0.6594334 | -0.1202739 |  0.0761159 |
| Colorectal (C18) | 60-69   | Period_2021 |  0.0843881 | 0.1139306 | -0.0202429 |  0.1890192 |
| Lung (C33-34)    | 60-69   | Age         |  0.0354292 | 0.0000000 |  0.0294571 |  0.0414012 |
| Lung (C33-34)    | 60-69   | Period      |  0.0032173 | 0.4746507 | -0.0056029 |  0.0120376 |
| Lung (C33-34)    | 60-69   | Period_2020 | -0.0631912 | 0.1246199 | -0.1438417 |  0.0174594 |
| Lung (C33-34)    | 60-69   | Period_2021 | -0.1096369 | 0.0230479 | -0.2041897 | -0.0150842 |

# End
