# Programming in R Assignment 2 
library(tidyverse)

# Information about the dataset ----
  # dat_diag is AdmissionsDiagnosesCorePopulatedTable
  # dat_admis is AdmissionsCorePopulatedTable
  # dat_pts is PatientCorePopulatedTable
  # dat_lab is LabsCorePopulatedTable

# 1: Load the datasets into memory using load. ---- 
load("MSBBSS04_10k_dataset.RData")

head(dat_lab)
dim(dat_lab)

# 2. From the lab values dataset, select per patient, admission and LabName ----
# only the last measurement entry. I.e. the row with the latest LabDateTime.

# use max function to select the latest observation 
# after grouping by patient, admission and labname and select the 
dat_time <- dat_lab |> 
  group_by(PatientID, AdmissionID, LabName) |>
  filter(LabDateTime == max(LabDateTime)) |>
  ungroup()

# To check whether the code was correct, we can compare the unique before and after 
unique_obs_before <- dat_lab |> 
  select(PatientID, AdmissionID, LabName) |>
  unique() |> 
  nrow()

unique_obs_after <- nrow(select_time)

all.equal(unique_obs_before, unique_obs_after)

# 3. Part 1: Unite PatientID and AdmissionID into one ID variable ----
# don’t drop the PatientID and AdmissionID yet. 

admis <- dat_admis |> unite("ID", PatientID, AdmissionID, sep = "n", remove = F)

# 3. Part 2: Merge the four datasets using a join function. ----

# PSEUDOCODE for joining datasets 
# Ref.: Using the steps from this tutorial: https://r4ds.hadley.nz/joins.html 
# Step 1: Find (primary or compound) keys to join -----
  # Definition of keys:
      # Keys are combinations of variables that make each row unique. We want to 
      # join "unique rows" from one data frame with the "unique row" of another by their keys.   
          # "A 'primary' key is a variable or set of variables that uniquely identifies each observation. 
          # When more than one variable is needed, the key is called a 'compound' key." 
  # Find the same variables in different data frames
    # We look for equal column names 
  # The procedure: Use count() + filter(n > 1) 
    # We can use count to determine the number of rows with a combination of variables
    # and then filter to see if it's is not unique (larger than 0) 
    # if the result is > 0, it means that the combination of variables repeats and cannot 
    # be used as key. If so, we need to add another variable or find another combination. 
  # Primary x compound keys 
    # dat_pts has only one column in common: "PatientID", which will work as a primary key to join another
    # For 
# Step 2: Use left joint 
  # General idea 
    # Why to use left join? 
      # because it preserves the original data frame and only adds new columns to the right 
      # in the tutorial: "the output will always have the same rows as x, the data frame you’re joining to"
  # Consequences
    # Joining dat_pts, dat_pts and dat_pts will not create NAs
    # it will create a long format 

sapply(list(dat_pts, dat_admis, dat_diag, dat_lab), names)
sapply(list(dat_pts, dat_admis, dat_diag, dat_lab), dim)
# Looking at the names, we can see that some variables repeat across df: 
# PatientID + AdmissionID repeat in dat_admis, dat_diag, dat_time 

# Defining keys 
  # confirming which variables can work as key 

  # dat_pts can be added to any one of the other dfs using PatientID as primary key 
  dat_pts |> count(PatientID) |> filter(n > 1) 
  
  # dat_admis and dat_diag can be added to each other or to dat_time using 
  # Patient ID and AdmissionID as compound keys 
  admis |> count(PatientID, AdmissionID) |> filter(n > 1)
  dat_diag |> count(PatientID, AdmissionID) |> filter(n > 1) 
  
  # dat_time does not have an unique combination for PatientID and AdmissionID
  dat_time |> count(PatientID, AdmissionID) |> filter(n > 1) 
  dat_time |> count(PatientID, AdmissionID, LabName) |> filter(n > 1) 
  
  # Patient ID + Admission ID alone can key for dat_time, however, each combination will 
  # add many new rows, as a result: 
  # nrow(complete) = nrow(dat_time) + nrow(semicomplete)
  # in future exercises, we will transform it into a wider format 

# joining left 
  semicomplete <- admis |> 
    left_join(dat_diag, join_by(PatientID, AdmissionID)) |>
    left_join(dat_pts, join_by(PatientID)) 

  all.equal(nrow(admis), nrow(semicomplete))
  # all rows were preserved and no new rows were created 
  
  complete <- semicomplete |> 
    left_join(dat_time, join_by(PatientID, AdmissionID))
  
  all.equal(nrow(complete), nrow(dat_time))
  # now the final number of rows is the number of rows that dat_time already had 
  # the data frame has a format long 
  
  sapply(list(dat_pts, admis, dat_diag, semicomplete, dat_time, complete), nrow)


# 4. Construct the outcome: identify patients with “malignant neoplasm” in their primary diagnosis. Give
# them the value 1 or “case”, where the variable name is simply “outcome”. The value 0 or “control” is
# for all other patients.
  
# check NA, if there are NA then we need to consider what to do in the ifelse  
  sum(is.na(complete$PrimaryDiagnosisDescription)) # no NA 

# ifelse creates a rule that is the string detector finds a text with "malignant neoplasm", then 
  # it assigns 1 to the new variable outcome. Otherwise, it assigns 0. 
  data1 <- complete |> 
    mutate(outcome = ifelse(str_detect(str_to_lower(PrimaryDiagnosisDescription), "malignant neoplasm"), 1, 0))
View(data1)
  dim(data1)
  
# 5. You may notice that patients may have several admissions, so this data is in a long format. We will
# take the easy way out and only retain data from the first admission. Filter the rows accordingly, you
# may use something like AdmissionID == 1.

data2 <- data1 |> 
  filter(AdmissionID == 1)


data1 |> select(PatientID, AdmissionID) |> unique() |> nrow() 
data2 |> select(PatientID, AdmissionID) |> unique() |> nrow() # this combination went back to the number in dat_pts 


# 6. You may notice that the data are still in long format (with duplicate IDs): each patient has multiple lab
# values measured. Convert these measurements to a wide format, giving each LabName its own column.
# In the names of these columns, change all spaces and forward slashes to an underscore and remove all
# colons.

# create a function that can clean the names: 
clean_names <- function(x) {
  x |>
    gsub(pattern = " ", replacement = "_") |> 
    gsub(pattern = ":", replacement = "") |> 
    gsub(pattern = "-", replacement = "_")
}

# lets clean the names of column LabName: 
data3 <- data2 |> 
  mutate(LabName = clean_names(LabName))
head(data3$LabName)

# finally, we perform the pivot wider using the names from LabName and the values from LabValue
# the result is a dataset with a column per exame, so we can see all the exams that each
# patient has 

data4 <- pivot_wider(data3,
  names_from = LabName,
  values_from = LabValue
)
View(data4)


# 7. Calculate the patient age in years and length of stay (LOS). The length of stay is defined as
# the duration from admission start to end in days. You may use lubridate::interval and
# lubridate::dyears/ddays, e.g. something like LOS = interval(start date, end date)/ddays(1)
# to get a numeric value for the number of days.
# 8. Create a new dataset with all unnecessary variables removed, only keep those relevant to the analysis
# described in the next points.
# 1
# 9. Report descriptive analyses (mean, median, SD, IQR, min, max, boxplots) on and test whether there is
# a difference in age between cases and controls. Do the same for LOS.
# 10. Report a table with counts of cases and controls across the sex categories and race categories. Perform
# tests on differences in counts (hint: first use table()).
# 11. Make boxplots of the lab values comparing cases and controls. Put the boxplots in one figure.
# 12. Using words, describe any remarkable patterns in the dataset in terms of normality of the distribution,
# skewness, missingness, rare categories, outliers.
# 13. Perform the appropriate regression (linear, logistic, Cox, etc.) to investigate the association between
# the outcome (case/control) and the lab values, corrected for age, gender, and LOS. Which variables are
# found to be significantly
