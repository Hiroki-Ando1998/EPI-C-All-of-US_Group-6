
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



# Create Table 1 using gtsummary
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

# 4. Display the table in the console
table1


# 5. Export options (Word / CSV)
# Export as a Word document (.docx)
# table1 %>%
#   as_flex_table() %>%
#   flextable::save_as_docx(path = "Table1_Baseline_Characteristics.docx")
# 
# # Export as a CSV file
# table1 %>%
#   as_tibble() %>%
#   write.csv("Table1_Baseline_Characteristics.csv", row.names = FALSE)

