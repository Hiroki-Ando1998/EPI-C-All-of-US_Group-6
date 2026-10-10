
install.packages(c("bigrquery", "tidyverse", "gtsummary"))


library(bigrquery)
library(dplyr)
library(tidyverse)
library(readr)
library(gtsummary)

#-------------------------------------------------------------------------- Get path to the bucket in the workspace (i.e., Google Cloud)
# 1. Specify the GCS file path and local file name
gcs_file_path <- "gs://inflammatory-disease-plus-mold-exposure-5-wb-meteoric-aubergine/20261008_df_cohort_finalized.csv"
system(sprintf("gsutil cp %s .", gcs_file_path))


#2. Read the CSV file into a data frame
df_cohort <- read.csv("20261008_df_cohort_finalized.csv")
View(df_cohort)




#--------------------------------------------------------------------------------------------------Rpakcage: Creating Table 1

# Data preprocessing (Create age groups and extract the 1st digit of postal code)

# 2. Data preprocessing (Create age groups as an ordered factor)
df_table1 <- df_cohort %>%
  mutate(
    # Create age categories based on birth date
    Age_group_char = case_when(
      Date_of_birth >= "2007-01-01" ~ "<26 years (Born 2007 or later)",
      Date_of_birth >= "2000-01-01" & Date_of_birth < "2007-01-01" ~ "18–25 years (Born 2000–2007)",     
      Date_of_birth >= "1985-01-01" & Date_of_birth < "2000-01-01" ~ "26–41 years (Born 1985–1999)",
      Date_of_birth >= "1965-01-01" & Date_of_birth < "1985-01-01" ~ "42–60 years (Born 1965–1984)",
      Date_of_birth < "1965-01-01" ~ "≥61 years (Born before 1965)",
      TRUE ~ NA_character_
    ),
    # Set the explicit order of categories (Youngest to Oldest)
    Age_group = factor(
      Age_group_char,
      levels = c(
        "<18 years (Born 2007 or later)",
        "<18-25 years (Born 2000-2007)",
        "26–41 years (Born 1985–1999)",
        "42–60 years (Born 1965–1984)",
        "≥61 years (Born before 1965)"
      )
    ),
    # Extract the first digit of the postal code
    Postal_code_1st = substr(Postal_code, 1, 1)
  )


df_cohort_modified <- df_table1 %>%
  mutate(Sex_at_birth = case_when(
    Sex_at_birth == "Male"   ~ "male",
    Sex_at_birth == "Female" ~ "female",
    TRUE                     ~ "unspecified"
  )) %>%
  mutate(Race = case_when(
    Race == "White" ~ "White",
    Race %in% c("Black or African American") ~ "Black",
    Race == "Asian" ~ "Asian",
    Race %in% c("I prefer not to answer", "None Indicated", "None of these", "Skip") ~ "unspecified",    
    TRUE ~ "Other"
  )) %>%
  mutate(Grade_school = case_when(
    Grade_school %in% c("One Through Four", "Five Through Eight", "Nine Through Eleven") ~ "Less than High School",
    Grade_school == "Twelve Or GED" ~ "High School grad or equivalent",
    Grade_school == "College One to Three" ~ "Some college/associate",
    Grade_school %in% c("College Graduate", "Advanced Degree") ~ "college degreee or higher",    
    TRUE ~ "unspecified"
  ))%>%
  mutate(Postal_code_1st = case_when(
    Postal_code_1st %in% c("0", "1") ~ "Northeast",
    Postal_code_1st %in% c("4", "5", "6") ~ "Midwest",
    Postal_code_1st %in% c("2", "3", "7") ~ "South",
    TRUE ~ "West"
  ))%>%
  mutate(Health_insurance = case_when(
    Health_insurance == "Yes" ~ "Yes",
    Health_insurance == "No" ~ "No",
    TRUE ~ "Unspecified"
  ))%>%
  mutate(Employment_status = case_when(
    Employment_status %in% c("Employed For Wages", "Self Employed") ~ "Employed",
    Employment_status %in% c("Unable To Work", "Out Of Work One Or More", "Homemaker", "Out Of Work Less Than One") ~ "Unemployed",
    Employment_status == "Retired" ~ "Retired",
    TRUE ~ "Other"
  ))%>%
  mutate(Household_income = case_when(
    Household_income %in% c("less 10k", "10k 25k") ~ "<25k",
    Household_income %in% c("25k 35k", "35k 50k") ~ "25-49k",
    Household_income %in% c("50k 75k") ~ "50-74k",
    Household_income %in% c("75k 100k") ~ "75-99k",
    Household_income %in% c("100k 150k", "150k 200k", "more 200k") ~ "More than 100k",
    TRUE ~ "unspecified"
  ))%>%
  mutate(House_own_rent = case_when(
    House_own_rent %in% c("Own") ~ "Own",
    House_own_rent %in% c("Rent") ~ "Rent",
    TRUE ~ "unspecified"
  ))%>%
  mutate(Rheumatoid_EHS = case_when(
    Rheumatoid_EHS %in% c("None") ~ "None",
    TRUE ~ "Yes"
  ))%>%
  mutate(Multiple_Sclerosis_EHS = case_when(
    Multiple_Sclerosis_EHS %in% c("None") ~ "None",
    TRUE ~ "Yes"
  ))%>%
  mutate(Psoriasis_EHS = case_when(
    Psoriasis_EHS %in% c("None") ~ "None",
    TRUE ~ "Yes"
  ))


# #------------------------Save the created datafrmaework in Bucket of the workbench
# 
# # 1. Specify the target bucket path
# my_target_bucket <- "gs://inflammatory-disease-plus-mold-exposure-5-wb-meteoric-aubergine/"
# 
# # 2. Save the data frame as a local temporary CSV file
# write.csv(df_cohort_modified, "20261009_df_cohort_modified.csv", row.names = FALSE)
# 
# # 3. Transfer (copy) the CSV file to the specified bucket
# system(sprintf("gsutil cp 20261009_df_cohort_modified.csv %s", my_target_bucket))
# 
# # 4. Verify that the file was successfully uploaded to the bucket
# system(sprintf("gsutil ls %s", my_target_bucket))




#---------------------------------------------------------------------------------------------Table 1

table1_modified <- df_cohort_modified %>%
  # Select variables to include in Table 1
  select(
    exposure_group,       # Grouping variable (Exposed vs Unexposed)
    Age_group,
    Race,
    Sex_at_birth,
    Postal_code_1st,
    Health_insurance,
    House_own_rent,
    Grade_school,
    Household_income,
    Employment_status,
    Rheumatoid_EHS,
    rheumatoid_self_Report,
    Multiple_Sclerosis_EHS,
    multiple_sclerosis_self_Report,
    Psoriasis_EHS,
  ) %>%
  # Generate summary table
  tbl_summary(
    by = exposure_group, # Compare across exposure groups
    missing = "ifany",   # Display missing values (NA) only if present
    label = list(        # Define variable labels for display
      Age_group ~ "Age group",
      Race ~ "Race/Ethnicity",
      Sex_at_birth ~ "Sex at birth",
      Postal_code_1st ~ "Postal code (1st digit)",
      Health_insurance ~ "Health insurance coverage", #"Are you covered by health insurance or some other kind of health care plan?"
      House_own_rent ~ "Housing status (Own/Rent)",   # "Do you own or rent the place where you live?")
      Grade_school ~ "Education level",               # "What is the highest grade or year of school you completed?")
      Household_income ~ "Annual household income",   # "What is your annual household income from all sources?")
      Employment_status ~ "Employment status",        # "What is your current employment status? Please select 1 or more of these categories.")
      Psoriasis_EHS ~ "Psoriasis_EHS",
      Rheumatoid_EHS ~ "Rheumatoid arthritis_EHS",
      rheumatoid_self_Report ~ "Rheumatoid arthritis_Self_Report",        #"Including yourself, who in your family has had rheumatoid arthritis (RA)? self."
      Multiple_Sclerosis_EHS ~ "Multiple sclerosis_EHS",
      multiple_sclerosis_self_Report ~ "Multiple sclerosis_Self_Report"   #"Including yourself, who in your family has had multiple sclerosis (MS)? self."
    )
  ) %>%
  #add_p() %>%             # Calculate p-values for group comparisons (Chi-square, Fisher's exact test, etc.)
  add_overall() %>%       # Add an Overall column
  bold_labels()           # Make variable names bold


#----------------------------------------------------------------------------------------------------------------------------Final Output
table1_modified




#---------------------------------------------------------------Preliminary Table 1
table1 <- df_table1 %>%
  # Select variables to include in Table 1
  select(
    exposure_group,       # Grouping variable (Exposed vs Unexposed)
    Age_group,
    Race,
    Sex_at_birth,
    Postal_code_1st,
    Health_insurance,
    House_own_rent,
    Grade_school,
    Household_income,
    Employment_status,
    Rheumatoid_EHS,
    rheumatoid_self_Report,
    Multiple_Sclerosis_EHS,
    multiple_sclerosis_self_Report,
    Psoriasis_EHS,
  ) %>%
  # Generate summary table
  tbl_summary(
    by = exposure_group, # Compare across exposure groups
    missing = "ifany",   # Display missing values (NA) only if present
    label = list(        # Define variable labels for display
      Age_group ~ "Age group",
      Race ~ "Race/Ethnicity",
      Sex_at_birth ~ "Sex at birth",
      Postal_code_1st ~ "Postal code (1st digit)",
      Health_insurance ~ "Health insurance coverage", #"Are you covered by health insurance or some other kind of health care plan?"
      House_own_rent ~ "Housing status (Own/Rent)",   # "Do you own or rent the place where you live?")
      Grade_school ~ "Education level",               # "What is the highest grade or year of school you completed?")
      Household_income ~ "Annual household income",   # "What is your annual household income from all sources?")
      Employment_status ~ "Employment status",        # "What is your current employment status? Please select 1 or more of these categories.")
      Psoriasis_EHS ~ "Psoriasis_EHS",
      Rheumatoid_EHS ~ "Rheumatoid arthritis_EHS",
      rheumatoid_self_Report ~ "Rheumatoid arthritis_Self_Report",        #"Including yourself, who in your family has had rheumatoid arthritis (RA)? self."
      Multiple_Sclerosis_EHS ~ "Multiple sclerosis_EHS",
      multiple_sclerosis_self_Report ~ "Multiple sclerosis_Self_Report"   #"Including yourself, who in your family has had multiple sclerosis (MS)? self."
    )
  ) %>%
  #add_p() %>%             # Calculate p-values for group comparisons (Chi-square, Fisher's exact test, etc.)
  add_overall() %>%       # Add an Overall column
  bold_labels()           # Make variable names bold




#   as_tibble() %>%
#   write.csv("Table1_Baseline_Characteristics.csv", row.names = FALSE)

