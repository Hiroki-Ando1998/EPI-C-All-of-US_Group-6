
library(bigrquery)
library(dplyr)
library(tidyverse)
library(readr)
library(tidyverse)

#---------------------------------------------------------------------------------------(0) Preliminary step: Get path to the bucket in the workspace (i.e., Google Cloud)
# 0. Get the workspace project ID (automatically loaded from environment variables)
project_id <- Sys.getenv("GOOGLE_PROJECT")
print(project_id)

# 1. Get the path to the workspace bucket
bucket_path <- Sys.getenv("WORKSPACE_BUCKET")

# 2. Display the list of files (full paths) in the bucket
file_list <- system(sprintf("gsutil ls %s", bucket_path), intern = TRUE)
print(file_list)

# List contents of the target bucket using a wildcard (*)

target_bucket <- "gs://inflammatory-disease-plus-mold-exposure-4-wb-meteoric-aubergine/*"

# Retrieve the file list
file_list <- system(sprintf("gsutil ls %s", target_bucket), intern = TRUE)
print(file_list)


#-------------------------------------------------------------------------(1) Survey data
survey_files <- file_list[grep("surveyOccurrence", file_list)]
print(paste("Found", length(survey_files), "survey files."))


# 2. Download and merge all partitioned files into a single data frame
survey_df_all <- survey_files %>% 
  map_dfr(~ {
    local_tmp <- tempfile(fileext = ".csv.gz")
    system(sprintf("gsutil cp %s %s", .x, local_tmp))
    read_csv(local_tmp, show_col_types = FALSE)
  })
#View(survey_df_all)


# (supplement analysis) Group by person_id and extract records with multiple distinct survey dates
survey_df_diff_time <- survey_df_all %>%
  filter(question == 40192402) %>% #Think about the place you live
  group_by(person_id) %>%
  filter(n_distinct(survey_datetime) > 1) %>%
  arrange(person_id, survey_datetime) %>% # sort by person
# There are no people answering the exposure question at multiple times......

  
#-------------------------------------------------------------------------(2)Extract participants who answered positive for household mold exposure
# see: Data brower (https://databrowser.researchallofus.org/survey/social-factors-of-health/mold)
# 40192479: Mold
# 40192392: None of the above
# 40192444: Water leaks
# 40192469: Bug infestation
# 903096: Skip
# 40192434: Inadequate heat
# 40192468: No or not working smoke detector
# 40192393: Lead paint or pipes
# 40192495: Oven or stove not working


mold_yes <- survey_df_all %>% filter(answer_concept_id == 40192479 & question == 40192402) %>% arrange(person_id, survey_datetime) # sort by person
mold_yes_id <- mold_yes %>% pull(person_id) %>% unique()

mold_not <- survey_df_all %>% filter(answer_concept_id != 40192479 & question == 40192402) %>% arrange(person_id, survey_datetime) # sort by person
mold_not_id <- mold_not %>% pull(person_id) %>% unique()

View(mold_yes)
View(mold_not)




#---------------------------------------------------------------------------------(2) Preliminary step for making Table 1

# 1. Load demographic baseline data (data_person)
target_file_12 <- file_list[12] #20261002_032143_1819351146_data_person-000000000000.csv.gz(see: (0) Preliminary step)
local_file_12 <- "./survey_data.csv.gz"
system(sprintf("gsutil cp %s %s", target_file_12, local_file_12))
person_df <- read_csv(local_file_12)
data.frame(person_df)


# Compare total row count of person_df with the number of unique person_ids. If these two numbers match, there are no duplicates.
nrow(person_df)
length(unique(person_df$person_id))



# 2. Assign flags for Exposed vs. Unexposed groups
cohort_demographics <- person_df %>% 
  mutate(
    exposure_group = if_else(person_id %in% mold_yes_id, "Exposed (Mold)", "Unexposed")
  )

data.frame(cohort_demographics)
View(cohort_demographics)




#--------------------------------------------------------------------------------------------------(3) Table 1
df_Y <- cohort_demographics %>% filter(exposure_group == "Exposed (Mold)")
df_N <- cohort_demographics %>% filter(exposure_group == "Unexposed")

nrow(df_Y) #Sample size
nrow(df_N) #Sample size

#View(df_Y)
#colnames(df_Y)

#(1) Date of birth
#table(df_Y$date_of_birth)
#table(df_N$date_of_birth)

#(2) Ethnicity
table(df_Y$T_DISP_ethnicity)
table(df_N$T_DISP_ethnicity)

#(3) Gender
table(df_Y$T_DISP_gender)
table(df_N$T_DISP_gender)






