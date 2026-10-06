
install.packages(c("bigrquery", "tidyverse", "gtsummary"))


library(bigrquery)
library(dplyr)
library(tidyverse)
library(readr)
library(tidyverse)
library(gtsummary)

#-------------------------------------------------------------------------- Get path to the bucket in the workspace (i.e., Google Cloud)
# 1. Specify the GCS file path and local file name
gcs_file_path <- "gs://inflammatory-disease-plus-mold-exposure-5-wb-meteoric-aubergine/20261006_df_cohort_finalized.csv"
system(sprintf("gsutil cp %s .", gcs_file_path))


#2. Read the CSV file into a data frame
df_cohort <- read.csv("20261006_df_cohort_finalized.csv")
View(df_cohort)




#--------------------------------------------------------------------------------------------------Rpakcage: Creating Table 1

# Data preprocessing (Create age groups and extract the 1st digit of postal code)

# 2. Data preprocessing (Create age groups as an ordered factor)
df_table1 <- df_cohort %>%
  mutate(
    # Create age categories based on birth date
    Age_group_char = case_when(
      Date_of_birth >= "2000-01-01" ~ "<26 years (Born 2000 or later)",
      Date_of_birth >= "1985-01-01" & Date_of_birth < "2000-01-01" ~ "26–41 years (Born 1985–1999)",
      Date_of_birth >= "1965-01-01" & Date_of_birth < "1985-01-01" ~ "42–60 years (Born 1965–1984)",
      Date_of_birth < "1965-01-01" ~ "≥61 years (Born before 1965)",
      TRUE ~ NA_character_
    ),
    # Set the explicit order of categories (Youngest to Oldest)
    Age_group = factor(
      Age_group_char,
      levels = c(
        "<26 years (Born 2000 or later)",
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
    Rheumatoid,
    Psoriasis,
    Multiple_Sclerosis
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
      Health_insurance ~ "Health insurance coverage",
      House_own_rent ~ "Housing status (Own/Rent)",
      Grade_school ~ "Education level",
      Household_income ~ "Annual household income",
      Employment_status ~ "Employment status",
      Rheumatoid ~ "Rheumatoid arthritis",
      Psoriasis ~ "Psoriasis",
      Multiple_Sclerosis ~ "Multiple sclerosis"
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



























#-------------------------------------------------------------------------------------------------------------------------Manual: creating Table 1 (Not need to copy!!!)

df_Yes <- df_cohort %>% filter(exposure_group == "Exposed (Mold)")
df_No <- df_cohort %>% filter(exposure_group == "Unexposed")


nrow(df_Yes) #the numner of peopled exposed to household mold
nrow(df_No) # the numner of peopled not exposed to household mold


col_names <- colnames(df_Yes)
print(col_names)
#------------------------------------------------------------------------------------------------------Age

#people born before 1955 (> almost 55 years old)
sum(df_Yes$Date_of_birth < "1965-01-01", na.rm = TRUE)
sum(df_No$Date_of_birth < "1965-01-01", na.rm = TRUE)

#people born after 1985 and before 2000 (almost 18-35 years old)
sum(df_Yes$Date_of_birth > "1985-01-01" & df_Yes$Date_of_birth < "2000-01-01", na.rm = TRUE)
sum(df_No$Date_of_birth > "1985-01-01" & df_No$Date_of_birth < "2000-01-01", na.rm = TRUE)

#people born after 2000 (< almost 18 years old)
sum(df_Yes$Date_of_birth > "2000-01-01", na.rm = TRUE)
sum(df_No$Date_of_birth > "2000-01-01", na.rm = TRUE)



#------------------------------------------------------------------------------------------------------Basic data

# Race
table(df_Yes$Race)
table(df_No$Race)


# Sex_at_birth
table(df_Yes$Sex_at_birth)
table(df_No$Sex_at_birth)


# Postal code (The first digit of the postal code)
table(substr(df_Yes$Postal_code, 1, 1))
table(substr(df_No$Postal_code, 1, 1))

#-------------------------------------------------------------------------------- survay data

#"Are you covered by health insurance or some other kind of health care plan?"
table(df_Yes$Health_insurance)
table(df_No$Health_insurance)

# "Do you own or rent the place where you live?")
table(df_Yes$House_own_rent)
table(df_No$House_own_rent)


# "What is the highest grade or year of school you completed?")
table(df_Yes$Grade_school)
table(df_No$Grade_school)


# "What is your annual household income from all sources?")
table(df_Yes$Household_income)
table(df_No$Household_income)

# "What was your biological sex assigned at birth?")
table(df_Yes$Biological_sex)
table(df_No$Biological_sex)

# "What is your current employment status? Please select 1 or more of these categories.")
table(df_Yes$Employment_status)
table(df_No$Employment_status)







#-------------------------------------------------------------------Outcome data
#Outcome: Rheumatoid
table(df_Yes$Rheumatoid)
table(df_No$Rheumatoid)

#Outcome: Psoriasis
table(df_Yes$Psoriasis)
table(df_No$Psoriasis)


#Outcome: Multiple_Sclerosis
table(df_Yes$Multiple_Sclerosis)
table(df_No$Multiple_Sclerosis)











