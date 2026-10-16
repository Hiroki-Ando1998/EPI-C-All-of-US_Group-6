


library(bigrquery)
library(dplyr)
library(tidyverse)
library(readr)
library(gtsummary)

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

#----------------------------------------------------------------------------------------Remove some data

df_cohort_1 <- df_cohort_modified %>%
  filter(Sex_at_birth != "unspecified") %>%
  filter(Grade_school != "unspecified") %>%
  filter(Age_group_char != "unspecified") %>%
  filter(Health_insurance != "Unspecified")


#----------------------------------------------------------------------------------------Tranform data for the logistic regression
df <- df_cohort_1
#names(df)

df$exposure_group <- ifelse(df$exposure_group == "Exposed (Mold)", 1, 0)
df <- df %>% mutate(sex_male = ifelse(Sex_at_birth == "male", 1, 0))
df <- df %>% mutate(Health_insurance = ifelse(Health_insurance == "No", 1, 0))
df$Age_group <- factor(df$Age_group_char,
                       levels = c(
                         "18–25 years (Born 2000–2007)", # 参照群
                         "26–41 years (Born 1985–1999)", 
                         "42–60 years (Born 1965–1984)", 
                         "≥61 years (Born before 1965)"
                       ))

df$Grade_school <- factor(df$Grade_school,
                          levels = c(
                            "Less than High School", # 参照群
                            "High School grad or equivalent", 
                            "Some college/associate", 
                            "college degreee or higher"  
                          ))

df$Employment_status <- factor(df$Employment_status,
                               levels = c(
                                 "Employed", # 参照群
                                 "Unemployed", 
                                 "Retired", 
                                 "Other"  
                               ))

df$Household_income <- factor(df$Household_income,
                              levels = c(
                                "<25k", # 参照群
                                "25-49k", 
                                "50-74k",
                                "75-99k",
                                "More than 100k",
                                "unspecified"  
                              ))

df$House_own_rent <- factor(df$House_own_rent,
                            levels = c(
                              "Own", # 参照群
                              "Rent", 
                              "unspecified"
                            ))



# Outcome: Yes = 1, None = 0
df$Multiple_Sclerosis_EHS <- ifelse(df$Multiple_Sclerosis_EHS == "Yes", 1, 0)
df$multiple_sclerosis_self_Report <- ifelse(df$multiple_sclerosis_self_Report == "yes", 1, 0)
df$Rheumatoid_EHS <- ifelse(df$Rheumatoid_EHS == "Yes", 1, 0)
df$rheumatoid_self_Report <- ifelse(df$rheumatoid_self_Report == "yes", 1, 0)
df$Psoriasis <- ifelse(df$Psoriasis == "Yes", 1, 0)


# -------------------------------------------------------------------------------------------Frequentist logistic regression model
fit_glm_unajusted <- glm(
  Multiple_Sclerosis_EHS ~ exposure_group,
  data = df,
  family = binomial(link = "logit")
)


fit_glm_adjusted <- glm(
  Multiple_Sclerosis_EHS ~ exposure_group + sex_male + Health_insurance + 
    Age_group + Grade_school + Employment_status + Household_income + House_own_rent,
  data = df,
  family = binomial(link = "logit")
)



#----------------------------(1) Brief summary of Result
summary(fit_glm_unajusted)
summary(fit_glm_adjusted)

#----------------------------(2) Parameter & 95%CI
coef_adjusted <- cbind(
  Estimate = coef(fit_glm_adjusted), 
  confint.default(fit_glm_adjusted)
)

print(round(coef_adjusted, 3))


##----------------------------(3) Odds ratio & 95% CI
adj_OR <- exp(cbind(OR = coef(fit_glm_adjusted), confint.default(fit_glm_adjusted)))
print(round(adj_OR, 3))



