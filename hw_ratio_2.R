---
title: "new ratio 3"
output:
  pdf_document: default
  html_document: default
date: "2025-03-13"
---

```{r}
set.seed(161)

library(dplyr)
library(survey)

# Load the dataset
fv_data <- read.csv("/Users/saminaniazinuzhat/Downloads/California2006GOVfv.csv")

# Check column names
colnames(fv_data)
```

```{r}
# Create Stratum Variable
fv_data <- fv_data %>%
  mutate(Stratum = case_when(
    Votes < 100 ~ "Low Votes",
    Votes >= 100 & Votes < 500 ~ "Medium Votes",
    Votes >= 500 ~ "High Votes",
    TRUE ~ "Other"
  ))

# Check strata distribution
table(fv_data$Stratum)

# Calculate sample sizes for stratified sampling
strata_sizes <- fv_data %>%
  count(Stratum) %>%
  mutate(sample_size = round((n / sum(n)) * 1500))
```

```{r}
# Create Stratum Variable
fv_data <- fv_data %>%
  mutate(Stratum = case_when(
    Votes < 100 ~ "Low Votes",
    Votes >= 100 & Votes < 500 ~ "Medium Votes",
    Votes >= 500 ~ "High Votes"
  ))

# Check strata distribution
table(fv_data$Stratum)

# Calculate sample sizes for stratified sampling
strata_sizes <- fv_data %>%
  count(Stratum, name = "N") %>%
  mutate(sample_size = round((N / sum(N)) * 1500))

# Perform Stratified Sampling (Fix: Using `filter` and `sample_n()`)
stratified_sample <- fv_data %>%
  inner_join(strata_sizes, by = "Stratum") %>%  # Attach sample sizes
  group_by(Stratum) %>%
  sample_n(size = first(sample_size), replace = FALSE) %>%  # Use first() to get single value
  ungroup()

# Compute Combined Ratio Estimate
ratio_estimator_combined <- sum(stratified_sample$Nfraudmean) / sum(stratified_sample$Votes)

# Compute Separate Ratio Estimates for Each Stratum
ratio_estimators_separate <- stratified_sample %>%
  group_by(Stratum) %>%
  summarise(Ratio = sum(Nfraudmean) / sum(Votes), .groups = "drop")

# Define Survey Design for Ratio Estimation
design <- svydesign(id = ~1, strata = ~Stratum, data = stratified_sample)

# Compute Ratio Estimation and Confidence Interval
ratio_est <- svyratio(~Nfraudmean, ~Votes, design)
conf_int <- confint(ratio_est)

# Simple Random Sampling (SRS) for Comparison
srs_sample <- fv_data %>% sample_n(1500, replace = FALSE)
srs_ratio_est <- sum(srs_sample$Nfraudmean) / sum(srs_sample$Votes)
srs_se <- sd(srs_sample$Nfraudmean) / sqrt(nrow(srs_sample))

# Print Results
print(list(
  "Combined Ratio Estimate" = ratio_estimator_combined,
  "Separate Ratio Estimates" = ratio_estimators_separate,
  "Stratified Ratio SE" = sqrt(vcov(ratio_est)),
  "SRS Estimate" = srs_ratio_est,
  "SRS SE" = srs_se,
  "Confidence Interval" = conf_int
))

```
