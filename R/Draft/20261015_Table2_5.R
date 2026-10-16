# Remove data of individuals categorized "unspecified" sex, education and age
install.packages(c("brms", "rstan"))

library(bigrquery)
library(dplyr)
library(tidyverse)
library(readr)
library(gtsummary)
library(brms)
library(rstan)

#-------------------------------------------------------------------------- Get path to the bucket in the workspace (i.e., Google Cloud)
# 1. Specify the GCS file path and local file name
gcs_file_path <- "gs://inflammatory-disease-plus-mold-exposure-5-wb-meteoric-aubergine/20261008_df_cohort_finalized.csv"
system(sprintf("gsutil cp %s .", gcs_file_path))
df_cohort <- read.csv("20261008_df_cohort_finalized.csv")

gcs_file_path <- "gs://inflammatory-disease-plus-mold-exposure-5-wb-meteoric-aubergine/20261009_df_cohort_modified.csv" #see: "20261008_Table1.R"
system(sprintf("gsutil cp %s .", gcs_file_path))
df_cohort_modified <- read.csv("20261009_df_cohort_modified.csv")

#View(df_cohort)
#View(df_cohort_modified)


#------------------------------------------------------

df_cohort_modified <- df_cohort_modified %>%
  mutate(survey_datetime.y = as.POSIXct(survey_datetime.y,format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    condition_start_datetime_Rheumatoid_EHS = as.POSIXct(condition_start_datetime_Rheumatoid_EHS, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    condition_start_datetime_Sclerosis_EHS = as.POSIXct(condition_start_datetime_Sclerosis_EHS, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    condition_start_datetime_Psoriasis_EHS = as.POSIXct(condition_start_datetime_Psoriasis_EHS, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    Self_Report_date_rheumatoid_multiple_sclerosis = as.POSIXct(Self_Report_date_rheumatoid_multiple_sclerosis, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    Self_Report_date_rheumatoid = as.POSIXct(Self_Report_date_rheumatoid, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))


df_RM_EHS <- df_cohort_modified %>%
  filter(!is.na(survey_datetime.y), !is.na(condition_start_datetime_Rheumatoid_EHS), survey_datetime.y < condition_start_datetime_Rheumatoid_EHS) 
nrow(df_RM_EHS)                          #269
table(df_cohort_modified$Rheumatoid_EHS) #1989

#-----------------------------------------------------
df_cohort_1 <- df_cohort_modified %>%
  filter(Sex_at_birth != "unspecified") %>%
  filter(Grade_school != "unspecified") %>%
  filter(Age_group_char != "unspecified")

df <- df_cohort_1

names(df)

# Exposure: Yes = 1, None = 0
df$exposure_group <- ifelse(df$exposure_group == "Exposed (Mold)", 1L, 0L)

# Outcome: Yes = 1, None = 0
df$Multiple_Sclerosis_EHS <- ifelse(df$Multiple_Sclerosis_EHS == "Yes", 1L, 0L)
df$multiple_sclerosis_self_Report <- ifelse(df$multiple_sclerosis_self_Report == "yes", 1L, 0L)
df$Rheumatoid_EHS <- ifelse(df$Rheumatoid_EHS == "Yes", 1L, 0L)
df$rheumatoid_self_Report <- ifelse(df$rheumatoid_self_Report == "yes", 1L, 0L)
df$Psoriasis <- ifelse(df$Psoriasis == "Yes", 1L, 0L)
#--------------------------------------------------------------------------------------------------------------Logistic Regression (Outcome ~ Exposure (without covariates))

#------------------------------------------------------------------------------(1-A) EHS: Multiple Sclerosis
fit_MS_EHS <- brm(
  formula = Multiple_Sclerosis_EHS ~ exposure_group,
  data = df,
  family = bernoulli(link = "logit"),
  prior = c(
    prior(normal(0, 3.5), class = "Intercept"),
    prior(normal(0, 3.5), class = "b")
  ),
  chains = 4,
  iter = 3000,
  warmup = 1500,
  seed = 123
)

summary(fit_MS_EHS)
plot(fit_MS_EHS)
# Posterior distribution
# draws <- posterior::as_draws_df(fit)
# pars <- c("b_Intercept", "exposure_group")
# result <- t(sapply(pars, function(p) {
#   x <- draws[[p]]
#   c(
#     Mean = mean(x),
#     Median = median(x),
#     Q2.5 = quantile(x, 0.025),
#     Q97.5 = quantile(x, 0.975)
#   )
# }))
# print(result)

#------------------------------------------------------------------------------(1-B) Self-report: Multiple Sclerosis
fit_MS_Self <- brm(
  formula = multiple_sclerosis_self_Report ~ exposure_group,
  data = df,
  family = bernoulli(link = "logit"),
  prior = c(
    prior(normal(0, 3.5), class = "Intercept"),
    prior(normal(0, 3.5), class = "b")
  ),
  chains = 4,
  iter = 3000,
  warmup = 1500,
  seed = 123
)

summary(fit_MS_Self)
plot(fit_MS_Self) #Posterior distribution


#------------------------------------------------------------------------------(2-A) EHS: Rheumatiod
fit_RM_EHS <- brm(
  formula = Rheumatoid_EHS ~ exposure_group,
  data = df,
  family = bernoulli(link = "logit"),
  prior = c(
    prior(normal(0, 3.5), class = "Intercept"),
    prior(normal(0, 3.5), class = "b")
  ),
  chains = 4,
  iter = 3000,
  warmup = 1500,
  seed = 123
)

summary(fit_RM_EHS)
plot(fit_RM_EHS) #Posterior distribution


#------------------------------------------------------------------------------(2-B) Self-report: Rheumatiod
fit_RM_Self <- brm(
  formula = rheumatoid_self_Report ~ exposure_group,
  data = df,
  family = bernoulli(link = "logit"),
  prior = c(
    prior(normal(0, 3.5), class = "Intercept"),
    prior(normal(0, 3.5), class = "b")
  ),
  chains = 4,
  iter = 3000,
  warmup = 1500,
  seed = 123
)

summary(fit_RM_Self)
plot(fit_RM_Self) #Posterior distribution



#------------------------------------------------------------------------------(3) EHS: Psoriasis
fit_PS_EHS <- brm(
  formula = Psoriasis_EHS ~ exposure_group,
  data = df,
  family = bernoulli(link = "logit"),
  prior = c(
    prior(normal(0, 3.5), class = "Intercept"),
    prior(normal(0, 3.5), class = "b")
  ),
  chains = 4,
  iter = 3000,
  warmup = 1500,
  seed = 123
)

summary(fit_PS_EHS)
plot(fit_PS_EHS) #Posterior distribution



#--------------------------------------------------------------------------------------------Result



summary(fit_MS_EHS)
summary(fit_MS_Self)
summary(fit_RM_EHS)
summary(fit_RM_Self)
summary(fit_PS_EHS)

plot(fit_MS_EHS)
plot(fit_MS_Self) #Posterior distribution
plot(fit_RM_EHS) #Posterior distribution
plot(fit_RM_Self) #Posterior distribution
plot(fit_PS_EHS)


