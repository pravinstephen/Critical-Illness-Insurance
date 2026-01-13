
## Setup

library(readr)
critical_illness <- read_csv("critical_illness.csv")
install.packages("tidyverse")  
library(tidyverse)
ci <- critical_illness

## A suitable probability distribution for the claim frequency. 
## Estimating the parameters of this model separately for policyholders living with a chronic condition and for policyholder living without a chronic condition. 
## Providing confidence intervals for the estimated parameters.


# Summary of mean and variance of ClaimCount by chronic status
freq_summary <- ci %>%
  group_by(ChronicCondition) %>%
  summarise(
    n    = n(),
    mean = mean(ClaimCount),
    var  = var(ClaimCount)
  )

freq_summary

# Dispersion statistic to check Poisson assumption
dispersion_stat <- function(data) {
  mu <- mean(data$ClaimCount)
  sum((data$ClaimCount - mu)^2 / mu) / (nrow(data) - 1)
}

ci %>%
  group_by(ChronicCondition) %>%
  summarise(dispersion = dispersion_stat(cur_data()))

# 95% CI for Poisson lambda using CLT
poisson_ci <- function(lambda_hat, n, z = 1.96) {
  se <- sqrt(lambda_hat / n)
  c(lower = lambda_hat - z * se,
    upper = lambda_hat + z * se)
}

lambda_no  <- freq_summary$mean[freq_summary$ChronicCondition == "No"]
n_no       <- freq_summary$n[freq_summary$ChronicCondition == "No"]
lambda_yes <- freq_summary$mean[freq_summary$ChronicCondition == "Yes"]
n_yes      <- freq_summary$n[freq_summary$ChronicCondition == "Yes"]

ci_lambda_no  <- poisson_ci(lambda_no,  n_no)
ci_lambda_yes <- poisson_ci(lambda_yes, n_yes)

ci_lambda_no
ci_lambda_yes

# Proportion of zeros by chronic status
ci %>%
  group_by(ChronicCondition) %>%
  summarise(
    prop_zero = mean(ClaimCount == 0),
    prop_one  = mean(ClaimCount == 1),
    prop_two  = mean(ClaimCount == 2)
  )

nrow(ci)

## The estimated claim rates and claim amounts across age groups and chronic condition status
  

# 1) Claim rates by chronic condition
rate_chronic <- ci %>%
  group_by(ChronicCondition) %>%
  summarise(
    n       = n(),
    rate    = mean(AnyClaim),
    se      = sqrt(rate * (1 - rate) / n),
    lower   = rate - 1.96 * se,
    upper   = rate + 1.96 * se
  )

rate_chronic


# Two-sample test for equality of proportions (AnyClaim ~ ChronicCondition)
prop_test <- prop.test(
  x = c(sum(ci$AnyClaim[ci$ChronicCondition == "No"]),
        sum(ci$AnyClaim[ci$ChronicCondition == "Yes"])),
  n = c(sum(ci$ChronicCondition == "No"),
        sum(ci$ChronicCondition == "Yes"))
)

prop_test


# Claim rates by AgeGroup and ChronicCondition
rate_age_chronic <- ci %>%
  group_by(AgeGroup, ChronicCondition) %>%
  summarise(
    n    = n(),
    rate = mean(AnyClaim),
    .groups = "drop"
  )

rate_age_chronic

# Claim rates by Gender and ChronicCondition
rate_gender_chronic <- ci %>%
  group_by(Gender, ChronicCondition) %>%
  summarise(
    n    = n(),
    rate = mean(AnyClaim),
    .groups = "drop"
  )

rate_gender_chronic

# 2) Severity analysis (ClaimAvg) among claim-making policyholders only
claim_makers <- ci %>%
  filter(AnyClaim == 1, !is.na(ClaimAvg))

# Severity summary by chronic condition
severity_chronic <- claim_makers %>%
  group_by(ChronicCondition) %>%
  summarise(
    n      = n(),
    mean   = mean(ClaimAvg),
    sd     = sd(ClaimAvg),
    median = median(ClaimAvg),
    .groups = "drop"
  )

severity_chronic


# Two-sample t-test for difference in mean severity
# (ACTL2131-appropriate version: does not assume equal variances)
t_severity <- t.test(ClaimAvg ~ ChronicCondition,
                     data = claim_makers,
                     var.equal = FALSE)  # separate variances (standard ACTL approach)

t_severity

# Optional: t-test on log-severity to reduce skewness
t_severity_log <- t.test(log(ClaimAvg) ~ ChronicCondition,
                         data = claim_makers,
                         var.equal = FALSE)

t_severity_log

# Severity by AgeGroup and ChronicCondition (optional)
severity_age_chronic <- claim_makers %>%
  group_by(AgeGroup, ChronicCondition) %>%
  summarise(
    n    = n(),
    mean = mean(ClaimAvg),
    .groups = "drop"
  )

severity_age_chronic

## Any statistically significant difference in claim frequency or claim severity between smokers and non-smokers 
## The interaction between smoking status and chronic condition


# Subgroup means and counts
subgroup <- ci %>%
  group_by(ChronicCondition, Smoker) %>%
  summarise(
    n          = n(),
    lambda_hat = mean(ClaimCount),
    .groups    = "drop"
  )

subgroup

# 95% CIs for each subgroup Poisson mean (using CLT)
poisson_ci <- function(lambda_hat, n, z = 1.96) {
  se <- sqrt(lambda_hat / n)
  c(lower = lambda_hat - z * se,
    upper = lambda_hat + z * se)
}

subgroup_ci <- subgroup %>%
  rowwise() %>%
  mutate(
    ci_lower = poisson_ci(lambda_hat, n)[1],
    ci_upper = poisson_ci(lambda_hat, n)[2]
  ) %>%
  ungroup()

subgroup_ci

##  potential data quality issues when analysing claim experience datasets


# Missingness checks
colSums(is.na(ci))

# Distribution of ChronicCondition and AnyClaim
table(ci$ChronicCondition)
table(ci$AnyClaim)

# Quick check of ClaimAvg range (to see if extreme values)
summary(ci$ClaimAvg)

# Check how many policies have ClaimCount > 0
sum(ci$ClaimCount > 0)

## The sample size which is needed to reach a certainty of 95% that people living with chronic diseases file claims of larger amounts compared to those who do not.


# Using claim-making policyholders only
claim_makers <- ci %>%
  filter(AnyClaim == 1, !is.na(ClaimAvg))

# Split into groups
sev_no  <- claim_makers$ClaimAvg[claim_makers$ChronicCondition == "No"]
sev_yes <- claim_makers$ClaimAvg[claim_makers$ChronicCondition == "Yes"]

# Group sizes
length(sev_no)   # should be 224
length(sev_yes)  # should be 78

# Sample means
mean_no  <- mean(sev_no)
mean_yes <- mean(sev_yes)

mean_no
mean_yes

# Pooled standard deviation
sd_pooled <- sqrt(
  ((length(sev_no) - 1) * var(sev_no) +
     (length(sev_yes) - 1) * var(sev_yes)) /
    (length(sev_no) + length(sev_yes) - 2)
)

sd_pooled

# Difference in means [delta = (chronic - non-chronic)]
delta <- mean_yes - mean_no
delta

# Required sample size per group for 5% sig + 95% power
z_alpha <- qnorm(0.975)  # 1 - alpha/2 = 0.975
z_beta  <- qnorm(0.95)   # 1 - beta = 0.95 (power 95%)

n_req <- 2 * (z_alpha + z_beta)^2 * sd_pooled^2 / delta^2
n_req





