
# 2.  Use ratio and regression estimation based on auxiliary variable "Votes" to
# estimate the mean number of eforensics-fraudulent votes (variable
# "Nfraudmean"), for one of the populations that have data in a file *fv.csv.
# Design for a bound of B=5 (when designing the sample you may treat population
# variables "NVoters", "NValid" and "Votes" as known).  Compare the bounds on the
# error of estimation achieved using the two methods.
 
# work with California 2006 data



```{r}
library(dplyr)
library(survey)

set.seed(161)
dat <- read.csv("/Users/saminaniazinuzhat/Downloads/California2006GOVfv.csv", row.names=1)

dat <- dat %>%
  mutate(stratum = case_when(
    cname %in% c("Group1_Stratum1", "Group2_Stratum1") ~ "Stratum1",
    cname %in% c("Group1_Stratum2", "Group2_Stratum2") ~ "Stratum2",
    TRUE ~ "Other"
  ))

small_strata <- dat %>%
  count(stratum) %>%
  filter(n < 35) %>%
  pull(stratum)

dat <- dat %>%
  mutate(stratum_combined = ifelse(stratum %in% small_strata, "Combined", stratum))

# Population sizes
N <- nrow(dat)
Ns <- table(dat$stratum_combined)

# Sample sizes (n = 1500 total)
n <- 1500
ns <- round((n/N) * Ns)
ns[which.max(ns)] <- ns[which.max(ns)] + (n - sum(ns)) # Adjust to get exact n

# Take stratified sample
sampled_data <- dat %>%
  group_by(stratum_combined) %>%
  sample_n(size = ns[as.character(first(stratum_combined))], replace = FALSE) %>%
  ungroup()

ybar <- tapply(sampled_data$Nfraudmean, sampled_data$stratum_combined, mean)
xbar <- tapply(sampled_data$Votes, sampled_data$stratum_combined, mean)
mux <- tapply(dat$Votes, dat$stratum_combined, mean)

r_separate <- ybar / xbar
hatmuyRS <- sum((Ns/N) * r_separate * mux)

sr2 <- sapply(names(ybar), function(j) {
  sj <- sampled_data[sampled_data$stratum_combined == j, ]
  sum((sj$Nfraudmean - r_separate[j] * sj$Votes)^2) / (ns[j] - 1)
})

hatvarmuyRS <- sum((Ns/N)^2 * ((Ns - ns)/Ns) * sr2/ns)
boundmuyRS <- 2 * sqrt(hatvarmuyRS)

ybarst <- sum((Ns/N) * ybar)
xbarst <- sum((Ns/N) * xbar)
r_combined <- ybarst / xbarst
hatmuyRC <- r_combined * mean(dat$Votes)

sr2_combined <- sapply(names(ybar), function(j) {
  sj <- sampled_data[sampled_data$stratum_combined == j, ]
  sum((sj$Nfraudmean - r_combined * sj$Votes)^2) / (ns[j] - 1)
})

varhatmuyRC <- sum((Ns/N)^2 * ((Ns - ns)/Ns) * sr2_combined/ns)
boundmuyRC <- 2 * sqrt(varhatmuyRC)

srs_sample <- sample_n(dat, 1500)
ybarSRS <- mean(srs_sample$Nfraudmean)
varybarSRS <- var(srs_sample$Nfraudmean)/1500 * (N - 1500)/N
SRSbound <- 2 * sqrt(varybarSRS)

# Results
cat("\nEstimated Mean (Separate Ratio Estimation):", hatmuyRS)
cat("\nError Bound (Separate Ratio Estimation):", boundmuyRS)

cat("\n\nEstimated Mean (Combined Ratio Estimation):", hatmuyRC)
cat("\nError Bound (Combined Ratio Estimation):", boundmuyRC)

cat("\n\nEstimated Mean (SRS):", ybarSRS)
cat("\nError Bound (SRS):", SRSbound)
```
