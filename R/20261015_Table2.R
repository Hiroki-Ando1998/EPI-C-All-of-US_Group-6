
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


