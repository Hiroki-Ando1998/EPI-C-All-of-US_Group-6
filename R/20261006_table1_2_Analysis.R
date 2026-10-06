





library(bigrquery)
library(dplyr)
library(tidyverse)
library(readr)
library(tidyverse)

#-------------------------------------------------------------------------- Get path to the bucket in the workspace (i.e., Google Cloud)
# 1. Specify the GCS file path and local file name
gcs_file_path <- "gs://inflammatory-disease-plus-mold-exposure-5-wb-meteoric-aubergine/20261006_df_cohort_finalized.csv"
system(sprintf("gsutil cp %s .", gcs_file_path))


#2. Read the CSV file into a data frame
df_cohort <- read.csv("20261006_df_cohort_finalized.csv")
View(df_cohort)




#---------------------------------------------------------------------------(Table 1)

#---------------------------------------------------------------------------(3-A) survay data
df_Yes <- df_cohort %>% filter(exposure_group == "Exposed (Mold)")
df_No <- df_cohort %>% filter(exposure_group == "Unexposed")

nrow(df_Yes) #the numner of peopled exposed to household mold
nrow(df_No) # the numner of peopled not exposed to household mold



col_names <- colnames(df_Yes)
print(col_names)

#"Are you covered by health insurance or some other kind of health care plan?"
table(df_survay_Yes$Health_insurance)
table(df_survay_No$Health_insurance)

# "Do you own or rent the place where you live?")
table(df_survay_Yes$House_own_rent)
table(df_survay_No$House_own_rent)


# "What is the highest grade or year of school you completed?")
table(df_survay_Yes$Grade_school)
table(df_survay_No$Grade_school)


# "What is your annual household income from all sources?")
table(df_survay_Yes$Household_income)
table(df_survay_No$Household_income)

# "What was your biological sex assigned at birth?")
table(df_survay_Yes$Biological_sex)
table(df_survay_No$Biological_sex)

# "What is your current employment status? Please select 1 or more of these categories.")
table(df_survay_Yes$Employment_status)
table(df_survay_No$Employment_status)




#Outcome: Rheumatoid
table(df_survay_Yes$Rheumatoid)
table(df_survay_No$Rheumatoid)

#Outcome: Psoriasis
table(df_survay_Yes$Psoriasis)
table(df_survay_No$Psoriasis)


#Outcome: Multiple_Sclerosis
table(df_survay_Yes$Multiple_Sclerosis)
table(df_survay_No$Multiple_Sclerosis)


