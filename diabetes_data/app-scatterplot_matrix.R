library(pairsD3)
library(shiny)
library(dplyr)

df_ids <- read.csv(("dataset_diabetes/IDs_mapping.csv"))
df <- read.csv("dataset_diabetes/diabetic_data.csv")

df_subset <- subset(df, (gender %in% c("Male", "Female")) & (race %in% c("AfricanAmerican", "Asian", "Caucasian",       "Hispanic")), select = c("race", "gender", "age", "time_in_hospital", "num_lab_procedures", "num_medications", "number_diagnoses"))

# Drop unused levels
df_subset[] <- lapply(df_subset, function(x) if(is.factor(x)) factor(x) else x)

labs <- c("Time in Hospital", "Number of Lab Procedures", "Number of Medications", "Number of Diagnoses")

original <- c("time_in_hospital", "num_lab_procedures", "num_medications", "number_diagnoses")

sp <- split(df_subset, list(df_subset$race, df_subset$gender, df_subset$age))
samples <- lapply(sp, function(x) x[sample(1:nrow(x), 5, TRUE),])
out <- do.call(rbind, samples)

sample_df <- out[, c(4, 5, 6, 7, 1, 2, 3)]


shinypairs(sample_df, labels = labs)