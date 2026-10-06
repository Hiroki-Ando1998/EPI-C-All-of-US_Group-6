


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
