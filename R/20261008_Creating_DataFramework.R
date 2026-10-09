

install.packages(c("bigrquery", "tidyverse"))

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
control_person_ids_0 <- setdiff(common_person_ids_5$person_id, condition_df_all$person_id) %>% sort()
control_person_ids <- data.frame(person_id = control_person_ids_0)


nrow(common_person_ids_5)
nrow(case_person_ids) + nrow(control_person_ids)
nrow(case_person_ids)
nrow(control_person_ids)





#---------------------------------------------------------------------------------------------------------------(4) Creating Data Framework
person_df_filtered <- person_df %>% filter(person_id %in% common_person_ids_5$person_id) %>% arrange(person_id)
survay_df_filtered <- survey_df_all %>% filter(person_id %in% common_person_ids_5$person_id) %>% arrange(person_id)
observation_df_filtered <- observation_df_all %>% filter(person_id %in% common_person_ids_5$person_id) %>% arrange(person_id)
condition_df_filtered <- condition_df_all %>% filter(person_id %in% common_person_ids_5$person_id) %>% arrange(person_id)





#-----------------------------------------------------------------------------(4-a) Personal data

#View(person_df_filtered)
person_df_filtered_1 <- person_df_filtered %>% select(person_id, date_of_birth, T_DISP_race, T_DISP_sex_at_birth)
colnames(person_df_filtered_1) <- c("person_id", "Date_of_birth", "Race", "Sex_at_birth")



#-----------------------------------------------------------------------------(4-b) postal code data

postal_code_1 <- observation_df_filtered %>% filter(T_DISP_standard_concept_name == "Postal code [Location]") %>%
  select(person_id, observation_datetime , value_as_string)
colnames(postal_code_1) <- c("person_id", "Observation_time", "Postal_code")



#------------------------------------------------------------------------------(4-c) Condition data


Question_3_1 <- survay_df_filtered %>% filter(T_DISP_question == "Are you covered by health insurance or some other kind of health care plan?") %>%
  select(person_id, survey_datetime, T_DISP_answer)
colnames(Question_3_1) <- c("person_id", "survey_datetime", "Health_insurance")

Question_4_1 <- survay_df_filtered %>% filter(T_DISP_question == "Do you own or rent the place where you live?") %>%
  select(person_id, T_DISP_answer)
colnames(Question_4_1) <- c("person_id", "House_own_rent")


Question_5_1 <- survay_df_filtered %>% filter(T_DISP_question == "What is the highest grade or year of school you completed?") %>%
  select(person_id, T_DISP_answer)
colnames(Question_5_1) <- c("person_id", "Grade_school")

Question_6_1 <- survay_df_filtered %>% filter(T_DISP_question == "What is your annual household income from all sources?") %>%
  select(person_id, T_DISP_answer)
colnames(Question_6_1) <- c("person_id", "Household_income")

Question_7_1 <- survay_df_filtered %>% filter(T_DISP_question == "What was your biological sex assigned at birth?") %>%
  select(person_id, T_DISP_answer)
colnames(Question_7_1) <- c("person_id", "Biological_sex")



Question_8_1_0 <- survay_df_filtered %>% filter(T_DISP_question == "What is your current employment status? Please select 1 or more of these categories.") %>%
  select(person_id, T_DISP_answer)
colnames(Question_8_1_0) <- c("person_id", "Employment_status")

Question_8_1 <- Question_8_1_0 %>%
  group_by(person_id) %>%
  mutate(
    # Define priority ranks for employment statuses (1 = highest priority)
    rank = case_when(
      Employment_status == "Student"                   ~ 1,
      Employment_status == "Unable To Work"            ~ 2,
      Employment_status == "Retired"                   ~ 3,
      Employment_status == "Employed For Wages"        ~ 4,
      Employment_status == "Homemaker"                 ~ 5,
      Employment_status == "Self Employed"             ~ 6,
      Employment_status == "Out Of Work Less Than One" ~ 7,
      TRUE                                            ~ 99 # All other statuses
    )
  ) %>%
  # Keep only the row(s) with the highest priority rank for each person_id
  filter(rank == min(rank)) %>%
  select(-rank) %>% # Remove the temporary rank column
  ungroup()



df_1 <- person_df_filtered_1 %>%
  left_join(postal_code_1, by = "person_id") %>%
  left_join(Question_3_1, by = "person_id") %>%
  left_join(Question_4_1, by = "person_id") %>%
  left_join(Question_5_1, by = "person_id") %>%
  left_join(Question_6_1, by = "person_id") %>%
  left_join(Question_7_1, by = "person_id") %>%
  left_join(Question_8_1, by = "person_id")




#-------------------------------------------------------------------------(4-D) outcome data

# Self-Report Data
multiple_sclerosis_self <- survay_df_filtered %>% filter(T_DISP_question == "Including yourself, who in your family has had multiple sclerosis (MS)? Select all that apply.") %>%
  select(person_id, survey_datetime, T_DISP_answer) %>%
  mutate(T_DISP_answer = if_else(T_DISP_answer == "Including yourself, who in your family has had multiple sclerosis (MS)? - Self", "yes", T_DISP_answer))
colnames(multiple_sclerosis_self) <- c("person_id", "Self_Report_date_rheumatoid_multiple_sclerosis", "multiple_sclerosis_self_Report")

df_merged_2 <- df_1 %>%
  left_join(multiple_sclerosis_self, by = "person_id") %>%
  mutate(
    # 日時列が Date/POSIXct 型の場合は as.character() に変換してから "none" に置換
    Self_Report_date_rheumatoid_multiple_sclerosis = as.character(Self_Report_date_rheumatoid_multiple_sclerosis),
    Self_Report_date_rheumatoid_multiple_sclerosis = coalesce(Self_Report_date_rheumatoid_multiple_sclerosis, "none"),
    
    # Rheumatoid 列の NA を "none" に置換（文字型の場合）
    multiple_sclerosis_self_Report = as.character(multiple_sclerosis_self_Report),
    multiple_sclerosis_self_Report = coalesce(multiple_sclerosis_self_Report, "none")
  )


rheumatoid_self <- survay_df_filtered %>% filter(T_DISP_question == "Including yourself, who in your family has had rheumatoid arthritis (RA)? Select all that apply.") %>%
  select(person_id, survey_datetime, T_DISP_answer) %>%
  mutate(T_DISP_answer = if_else(T_DISP_answer == "Including yourself, who in your family has had rheumatoid arthritis (RA)? - Self" , "yes", T_DISP_answer))
colnames(rheumatoid_self) <- c("person_id", "Self_Report_date_rheumatoid", "rheumatoid_self_Report")  
  
df_merged_3 <- df_merged_2 %>%
  left_join(rheumatoid_self, by = "person_id") %>%
  mutate(
    # 日時列が Date/POSIXct 型の場合は as.character() に変換してから "none" に置換
    Self_Report_date_rheumatoid = as.character(Self_Report_date_rheumatoid),
    Self_Report_date_rheumatoid = coalesce(Self_Report_date_rheumatoid, "none"),
    
    # Rheumatoid 列の NA を "none" に置換（文字型の場合）
    rheumatoid_self_Report = as.character(rheumatoid_self_Report),
    rheumatoid_self_Report = coalesce(rheumatoid_self_Report, "none")
  )

#(A)------Filter conditions containing "rheumatoid" (case-insensitive)

# Create a dataframe with unique person_ids (# Remove duplicates, keeping the first occurrence )
rheumatoid_unique <- condition_df_filtered %>%
  filter(str_detect(T_DISP_standard_concept_name, regex("rheumatoid", ignore_case = TRUE))) %>%
  distinct(person_id, .keep_all = TRUE) %>%
  select(person_id, condition_start_datetime, T_DISP_standard_concept_name)
colnames(rheumatoid_unique) <- c("person_id", "condition_start_datetime_Rheumatoid_EHS", "Rheumatoid_EHS")


df_merged_4 <- df_merged_3 %>%
  left_join(rheumatoid_unique, by = "person_id") %>%
  mutate(
    # 日時列が Date/POSIXct 型の場合は as.character() に変換してから "none" に置換
    condition_start_datetime_Rheumatoid_EHS = as.character(condition_start_datetime_Rheumatoid_EHS),
    condition_start_datetime_Rheumatoid_EHS = coalesce(condition_start_datetime_Rheumatoid_EHS, "none"),
    
    # Rheumatoid 列の NA を "none" に置換（文字型の場合）
    Rheumatoid_EHS = as.character(Rheumatoid_EHS),
    Rheumatoid_EHS = coalesce(Rheumatoid_EHS, "none")
  )



#(B)------Filter conditions containing "psoriasis" (case-insensitive)
psoriasis_unique <- condition_df_filtered %>%
  filter(str_detect(T_DISP_standard_concept_name, regex("psoriasis", ignore_case = TRUE))) %>%
  distinct(person_id, .keep_all = TRUE) %>%
  select(person_id, condition_start_datetime, T_DISP_standard_concept_name)
colnames(psoriasis_unique) <- c("person_id", "condition_start_datetime_Psoriasis_EHS", "Psoriasis_EHS")


df_merged_5 <- df_merged_4 %>%
  left_join(psoriasis_unique, by = "person_id") %>%
  mutate(
    condition_start_datetime_Psoriasis_EHS = as.character(condition_start_datetime_Psoriasis_EHS),
    condition_start_datetime_Psoriasis_EHS = coalesce(condition_start_datetime_Psoriasis_EHS, "none"),
    
    Psoriasis_EHS = as.character(Psoriasis_EHS),
    Psoriasis_EHS = coalesce(Psoriasis_EHS, "none")
  )




#(C)------Filter conditions containing "multiple sclerosis" (case-insensitive)
sclerosis_unique <- condition_df_filtered %>%
  filter(str_detect(T_DISP_standard_concept_name, regex("sclerosis", ignore_case = TRUE))) %>%
  distinct(person_id, .keep_all = TRUE) %>%
  select(person_id, condition_start_datetime, T_DISP_standard_concept_name)
colnames(sclerosis_unique) <- c("person_id", "condition_start_datetime_Sclerosis_EHS", "Multiple_Sclerosis_EHS")


df_merged_6 <- df_merged_5 %>%
  left_join(sclerosis_unique, by = "person_id") %>%
  mutate(
    condition_start_datetime_Sclerosis_EHS = as.character(condition_start_datetime_Sclerosis_EHS),
    condition_start_datetime_Sclerosis_EHS = coalesce(condition_start_datetime_Sclerosis_EHS, "none"),
    
    Multiple_Sclerosis_EHS = as.character(Multiple_Sclerosis_EHS),
    Multiple_Sclerosis_EHS = coalesce(Multiple_Sclerosis_EHS, "none")
  )



#-------------------------------------------------------------------------(4-E) Assign flags for Exposed vs Unexposed groups among cohort
# Pick up surveydate among people exposed to household mold
mold_yes <- mold_question %>% filter(answer_concept_id == 40192479) %>% arrange(person_id, survey_datetime) 
mold_yes_filtered <- mold_yes %>% semi_join(df_merged_6, by = "person_id") %>% select(person_id, survey_datetime)

df_not_in_mold <- df_merged_6 %>% anti_join(mold_yes_filtered, by = "person_id") %>% select(person_id)
mold_no_filtered <- mold_question %>% semi_join(df_not_in_mold, by = "person_id") %>% 
  select(person_id, survey_datetime) %>% distinct(person_id, .keep_all = TRUE)


mold_merged <- rbind(mold_yes_filtered, mold_no_filtered)
df_merged_7 <- df_merged_6 %>% left_join(mold_merged, by = "person_id")






#-------------------------Finalized data file
df_cohort_finalized <- df_merged_7 %>% mutate(exposure_group = if_else(person_id %in% mold_yes_filtered$person_id, "Exposed (Mold)", "Unexposed"))
View(df_cohort_finalized)

#df_Yes <- df_cohort_finalized %>% filter(exposure_group == "Exposed (Mold)")
#df_No <- df_cohort_finalized %>% filter(exposure_group == "Unexposed")

#nrow(df_Yes)
#nrow(df_No)


#------------------------Save the created datafrmaework in Bucket of the workbench

# 1. Specify the target bucket path
my_target_bucket <- "gs://inflammatory-disease-plus-mold-exposure-5-wb-meteoric-aubergine/"

# 2. Save the data frame as a local temporary CSV file
write.csv(df_cohort_finalized, "20261008_df_cohort_finalized.csv", row.names = FALSE)

# 3. Transfer (copy) the CSV file to the specified bucket
system(sprintf("gsutil cp 20261008_df_cohort_finalized.csv %s", my_target_bucket))

# 4. Verify that the file was successfully uploaded to the bucket
system(sprintf("gsutil ls %s", my_target_bucket))
