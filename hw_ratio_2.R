---
output:
  pdf_document: default
  html_document: default
---
# 2.  Use ratio and regression estimation based on auxiliary variable "Votes" to
# estimate the mean number of eforensics-fraudulent votes (variable
# "Nfraudmean"), for one of the populations that have data in a file *fv.csv.
# Design for a bound of B=5 (when designing the sample you may treat population
# variables "NVoters", "NValid" and "Votes" as known).  Compare the bounds on the
# error of estimation achieved using the two methods.
 
# work with California 2006 data



```{r}
library(dplyr)
set.seed(640)
fv <- read.csv("/Users/saminaniazinuzhat/Downloads/California2006GOVfv.csv")

N <- nrow(fv)  # Population size
X_pop <- mean(fv$Votes)  # Population mean of auxiliary variable


S_y <- sd(fv$Nfraudmean)

n_ratio <- ceiling((N * S_y / 5)^2 / (N + (S_y / 5)^2))  # Bound B = 5
n_ratio <- min(n_ratio, N)  # Ensure sample size does not exceed population size

sample_data <- sample_n(fv, n_ratio)

# sample means
y_bar <- mean(sample_data$Nfraudmean)  # Sample mean of Nfraudmean
x_bar <- mean(sample_data$Votes)  # Sample mean of auxiliary variable

# Ratio Estimator
B_ratio <- y_bar / x_bar  # Ratio of means
y_hat_ratio <- B_ratio * X_pop  # Estimate of mean Nfraudmean
se_ratio <- S_y / sqrt(n_ratio)  # Standard error
bound_ratio <- 1.96 * se_ratio  # 95% confidence bound

# Regression Estimator
reg_model <- lm(Nfraudmean ~ Votes, data = sample_data)
B_reg <- coef(reg_model)[2]  # Regression coefficient
y_hat_reg <- coef(reg_model)[1] + B_reg * X_pop  # Estimate of mean Nfraudmean
se_reg <- summary(reg_model)$sigma / sqrt(n_ratio)  # Standard error
bound_reg <- 1.96 * se_reg  # 95% confidence bound
cat("Ratio Estimation: Estimate =", y_hat_ratio, ", Bound =", bound_ratio, "\n")
cat("Regression Estimation: Estimate =", y_hat_reg, ", Bound =", bound_reg, "\n")

```
