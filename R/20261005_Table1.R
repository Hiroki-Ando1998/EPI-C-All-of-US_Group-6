
install.packages("bigrquery", "tidyverse")

library(bigrquery)
library(dplyr)
library(tidyverse)
library(readr)

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

target_bucket <- "gs://inflammatory-disease-plus-mold-exposure-5-wb-meteoric-aubergine/*"

# Retrieve the file list
file_list <- system(sprintf("gsutil ls %s", target_bucket), intern = TRUE)
print(file_list)



#-------------------------------------------------------------------------(1-A) Observation data (Postal code [Location], Tobacco smoking status)
observation_files <- file_list[grep("observationOccurrence", file_list)]
print(paste("Found", length(observation_files), "observation files."))


# 2. Download and merge all partitioned files into a single data frame
observation_df_all_0 <- observation_files %>% 
  map_dfr(~ {
    local_tmp <- tempfile(fileext = ".csv.gz")
    system(sprintf("gsutil cp %s %s", .x, local_tmp))
    read_csv(local_tmp, show_col_types = FALSE)
  })
observation_df_all <- observation_df_all_0 %>% arrange(person_id)
#View(observation_df_all)



Postal_code <- observation_df_all %>% filter(observation_df_all$T_DISP_standard_concept_name == "Postal code [Location]")
Smoking_status <- observation_df_all %>% filter(observation_df_all$T_DISP_standard_concept_name == "Tobacco smoking status")



#-------------------------------------------------------------------------(1-B) Conditional data
condition_files <- file_list[grep("conditionOccurrence", file_list)]
print(paste("Found", length(condition_files), "condition files."))


# 2. Download and merge all partitioned files into a single data frame
condition_df_all_0 <- condition_files %>% 
  map_dfr(~ {
    local_tmp <- tempfile(fileext = ".csv.gz")
    system(sprintf("gsutil cp %s %s", .x, local_tmp))
    read_csv(local_tmp, show_col_types = FALSE)
  })
condition_df_all <- condition_df_all_0 %>% arrange(person_id, condition_start_datetime)

#View(condition_df_all)


#-------------------------------------------------------------------------(1-C) Visit occurrence data
visit_files <- file_list[grep("visitOccurrence", file_list)]
print(paste("Found", length(visit_files), "visit files."))


# 2. Download and merge all partitioned files into a single data frame
visit_df_all_0 <- visit_files %>% 
  map_dfr(~ {
    local_tmp <- tempfile(fileext = ".csv.gz")
    system(sprintf("gsutil cp %s %s", .x, local_tmp))
    read_csv(local_tmp, show_col_types = FALSE)
  })
visit_df_all <- visit_df_all_0 %>% arrange(person_id)

#View(visit_df_all)


#------------------------------------------------------------------------(1-D) Peson-id (demographic baseline data)
target_file_120 <- file_list[120] #20261005_085343_1511757297_data_person-000000000000.csv.gz(see: (0) Preliminary step)
local_file_120 <- "./survey_data.csv.gz"
system(sprintf("gsutil cp %s %s", target_file_120, local_file_120))
person_df <- read_csv(local_file_120)
data.frame(person_df)


# Compare total row count of person_df with the number of unique person_ids. If these two numbers match, there are no duplicates.
nrow(person_df)
length(unique(person_df$person_id))


  
#-------------------------------------------------------------------------(2)Survay data: Extract participants who answered positive for household mold exposure
survey_files <- file_list[grep("surveyOccurrence", file_list)]
print(paste("Found", length(survey_files), "survey files."))


# 2. Download and merge all partitioned files into a single data frame
survey_df_all_0 <- survey_files %>% 
  map_dfr(~ {
    local_tmp <- tempfile(fileext = ".csv.gz")
    system(sprintf("gsutil cp %s %s", .x, local_tmp))
    read_csv(local_tmp, show_col_types = FALSE)
  })
#View(survey_df_all)

# People who provided a report before 2024
survey_df_all <- survey_df_all_0 %>%
  filter(as.numeric(substr(survey_datetime, 1, 4)) < 2024) %>%
  arrange(person_id)
nrow(survey_df_before)

#table(survey_df_all$T_DISP_question)
Question_exposure <- survey_df_all %>% filter(T_DISP_question == "Think about the place you live. Do you have problems with any of the following? Select all that apply.")
Question_1 <- survey_df_all %>% filter(T_DISP_question == "Including yourself, who in your family has had multiple sclerosis (MS)? Select all that apply.")
Question_2 <- survey_df_all %>% filter(T_DISP_question == "Including yourself, who in your family has had rheumatoid arthritis (RA)? Select all that apply.")
Question_3 <- survey_df_all %>% filter(T_DISP_question == "Are you covered by health insurance or some other kind of health care plan?")
Question_4 <- survey_df_all %>% filter(T_DISP_question == "Do you own or rent the place where you live?")
Question_5 <- survey_df_all %>% filter(T_DISP_question == "What is the highest grade or year of school you completed?")
Question_6 <- survey_df_all %>% filter(T_DISP_question == "What is your annual household income from all sources?")
Question_7 <- survey_df_all %>% filter(T_DISP_question == "What is your current employment status? Please select 1 or more of these categories.")
Question_8 <- survey_df_all %>% filter(T_DISP_question == "What was your biological sex assigned at birth?")


# (supplement analysis) Group by person_id and extract records with multiple distinct survey dates
#survey_df_diff_time <- survey_df_all %>%
#filter(question == 40192402) %>% #Think about the place you live
#group_by(person_id) %>%
#filter(n_distinct(survey_datetime) > 1) %>%
#arrange(person_id, survey_datetime) %>% # sort by person
# There are no people answering the exposure question at multiple times......




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

mold_question <- survey_df_all %>% filter(question == 40192402)
mold_question_id <- mold_question %>% pull(person_id) %>% unique()

mold_yes <- mold_question %>% filter(answer_concept_id == 40192479) %>% arrange(person_id, survey_datetime) 
mold_yes_id <- mold_yes %>% pull(person_id) %>% unique()

mold_not <- mold_question %>% filter(answer_concept_id != 40192479) %>% arrange(person_id, survey_datetime) 
mold_not_id <- mold_not %>% pull(person_id) %>% unique()

View(mold_yes)
View(mold_not)


#----------------------------------------------------------------------------------------------------------(3) Creating cohort 
#(A-1) People who have visted at least two times and answered the mold question (cohort)
common_person_ids_0 <- intersect(visit_df_all_0$person_id, mold_question$person_id) %>% sort()
common_person_ids_1 <- data.frame(person_id = common_person_ids_0)

#(A-2) Among the cohort, people who provided the information on both postal_code and smoking status
id_list_0 <- list(
  common_person_ids_1$person_id,
  Postal_code$person_id,
  Smoking_status$person_id
)

common_person_ids_2 <- Reduce(intersect, id_list_0) %>% sort()
common_person_ids_3 <- data.frame(person_id = common_person_ids_2)

#(A-3) Among the cohort, people who provided the additonal information on covariates furthermore
id_list_1 <- list(
  common_person_ids_3$person_id,
  #Question_1$person_id, Question_2$person_id,
  Question_3$person_id, Question_4$person_id,
  Question_5$person_id, Question_6$person_id,
  Question_7$person_id, Question_8$person_id
)

common_person_ids_4 <- Reduce(intersect, id_list_1) %>% sort()
common_person_ids_5 <- data.frame(person_id = common_person_ids_4)

nrow(common_person_ids_5)
nrow(common_person_ids_3)


#(B) Among the cohort, people who have developed rheumatoid arthritis, psoriasis, or multiple sclerosis
case_person_ids_0 <- intersect(common_person_ids_5$person_id, condition_df_all$person_id) %>% sort()
case_person_ids <- data.frame(person_id = case_person_ids_0)

#(C) Among the cohort, people who have not developed the diseases
# common_person_ids にあって、condition_df_all にはない person_id を抽出
control_person_ids_0 <- setdiff(common_person_ids_5$person_id, condition_df_all$person_id) %>% sort()
control_person_ids <- data.frame(person_id = control_person_ids_0)


nrow(common_person_ids_5)
nrow(case_person_ids) + nrow(control_person_ids)
nrow(case_person_ids)
nrow(control_person_ids)




#---------------------------------------------------------------------------------(2) Preliminary step for creating table 1

#(a) Personal data: Assign flags for Exposed vs Unexposed groups among cohort
person_df_filtered <- person_df %>% filter(person_id %in% common_person_ids_5$person_id) %>% arrange(person_id)
cohort_demographics <- person_df_filtered %>% 
  mutate(exposure_group = if_else(person_id %in% mold_yes_id, "Exposed (Mold)", "Unexposed"))


#(b) Survay Data (e.g., income): Assign flags for Exposed vs Unexposed groups among cohort
survay_df_filtered <- survey_df_all %>% filter(person_id %in% common_person_ids_5$person_id) %>% arrange(person_id)
cohort_survay <- survay_df_filtered %>% 
  mutate(exposure_group = if_else(person_id %in% mold_yes_id, "Exposed (Mold)", "Unexposed")) 

#(c) Observation Data (e.g., smoking): Assign flags for Exposed vs Unexposed groups among cohort
observation_df_filtered <- observation_df_all %>% filter(person_id %in% common_person_ids_5$person_id) %>% arrange(person_id)
cohort_observation <- observation_df_filtered %>% 
  mutate(exposure_group = if_else(person_id %in% mold_yes_id, "Exposed (Mold)", "Unexposed"))


data.frame(cohort_demographics)
data.frame(cohort_survay)
data.frame(cohort_observation)

nrow(cohort_demographics)
nrow(cohort_survay)
nrow(cohort_observation)

#View(cohort_demographics)

#--------------------------------------------------------------------------------------------------(3) Table 1
#(1) survay data
df_survay_Y <- cohort_survay %>% filter(exposure_group == "Exposed (Mold)")
df_survay_N <- cohort_survay %>% filter(exposure_group == "Unexposed")


#table(survey_df_all$T_DISP_question)
#Question_1_Y <- df_survay_Y %>% filter(T_DISP_question == "Including yourself, who in your family has had multiple sclerosis (MS)? Select all that apply.")
#Question_2_Y <- df_survay_Y %>% filter(T_DISP_question == "Including yourself, who in your family has had rheumatoid arthritis (RA)? Select all that apply.")
Question_3_Y <- df_survay_Y %>% filter(T_DISP_question == "Are you covered by health insurance or some other kind of health care plan?")
Question_4_Y <- df_survay_Y %>% filter(T_DISP_question == "Do you own or rent the place where you live?")
Question_5_Y <- df_survay_Y %>% filter(T_DISP_question == "What is the highest grade or year of school you completed?")
Question_6_Y <- df_survay_Y %>% filter(T_DISP_question == "What is your annual household income from all sources?")
Question_7_Y <- df_survay_Y %>% filter(T_DISP_question == "What is your current employment status? Please select 1 or more of these categories.")
Question_8_Y <- df_survay_Y %>% filter(T_DISP_question == "What was your biological sex assigned at birth?")

#Question_1_N <- df_survay_N %>% filter(T_DISP_question == "Including yourself, who in your family has had multiple sclerosis (MS)? Select all that apply.")
#Question_2_N <- df_survay_N %>% filter(T_DISP_question == "Including yourself, who in your family has had rheumatoid arthritis (RA)? Select all that apply.")
Question_3_N <- df_survay_N %>% filter(T_DISP_question == "Are you covered by health insurance or some other kind of health care plan?")
Question_4_N <- df_survay_N %>% filter(T_DISP_question == "Do you own or rent the place where you live?")
Question_5_N <- df_survay_N %>% filter(T_DISP_question == "What is the highest grade or year of school you completed?")
Question_6_N <- df_survay_N %>% filter(T_DISP_question == "What is your annual household income from all sources?")
Question_7_N <- df_survay_N %>% filter(T_DISP_question == "What is your current employment status? Please select 1 or more of these categories.")
Question_8_N <- df_survay_N %>% filter(T_DISP_question == "What was your biological sex assigned at birth?")

#Check
nrow(common_person_ids_5)
nrow(Question_3_Y) + nrow(Question_3_N)
nrow(Question_4_Y) + nrow(Question_4_N)
nrow(Question_5_Y) + nrow(Question_5_N)
nrow(Question_6_Y) + nrow(Question_6_N)
nrow(Question_7_Y) + nrow(Question_7_N)
nrow(Question_8_Y) + nrow(Question_8_N)



# "Are you covered by health insurance or some other kind of health care plan?")
table(Question_3_Y$T_DISP_answer) # Y: exposed to household mold
table(Question_3_N$T_DISP_answer) # N: Not exposed


# "Do you own or rent the place where you live
table(Question_4_Y$T_DISP_answer)
table(Question_4_N$T_DISP_answer)


# What is the highest grade or year of school you completed
table(Question_5_Y$T_DISP_answer)
table(Question_5_N$T_DISP_answer)


# What is your annual household income from all sources?
table(Question_6_Y$T_DISP_answer)
table(Question_6_N$T_DISP_answer)

# What is your current employment status? Please select 1 or more of these categories
table(Question_7_Y$T_DISP_answer)
table(Question_7_N$T_DISP_answer)

# What was your biological sex assigned at birth?
table(Question_8_Y$T_DISP_answer)
table(Question_8_N$T_DISP_answer)

