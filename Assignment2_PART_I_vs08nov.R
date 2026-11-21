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
dat_time<- dat_lab |> 
  group_by(PatientID, AdmissionID, LabName) |>
  filter(LabDateTime == max(LabDateTime)) |>
  ungroup()

# To check whether the code was correct, we can compare the unique before and after 
unique_obs_before <- dat_lab |> 
  select(PatientID, AdmissionID, LabName) |>
  unique() |> 
  nrow()

unique_obs_after <- nrow(dat_time)

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
  
  data1 <- data1 |> 
    mutate(outcome_f = factor(outcome, levels = c(0, 1), labels = c("Control", "Case"))) # give names to the factors
  
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
    gsub(pattern = "-", replacement = "_")|>
    gsub(pattern = "/", replacement = "_")
}

# lets clean the names of column LabName: 
data3 <- data2 |> 
  mutate(LabName = clean_names(LabName))
head(data3$LabName)

# finally, we perform the pivot wider using the names from LabName and the values from LabValue
# the result is a dataset with a column per exame, so we can see all the exams that each
# patient has 

data4 <- data3 |> 
  select(-LabUnits, -LabDateTime) |> # remove units and dates/times so each patient gets only 1 row
  pivot_wider(
    names_from = LabName,
    values_from = LabValue
  )


# 7. Calculate the patient age in years and length of stay (LOS). The length of stay is defined as
# the duration from admission start to end in days. You may use lubridate::interval and
# lubridate::dyears/ddays, e.g. something like LOS = interval(start date, end date)/ddays(1)
# to get a numeric value for the number of days.


# PSEUDOCODE for calculating patient age in years and length of stay (LOS)

# Step 1: We first check the format of date variables 

# Step 2: We calculate patient age at admission
#We use a time interval from birth date to admission start date using interval() to get a numeric value

# Step 3: We calculate length of stay (LOS)
# We use a time interval from admission start date to admission end date using interval() to get a numeric value

# Step 4: We use summary() to check for negative values, NAs, or unrealistic numbers

data5 <- data4 |> 
  mutate(
    age = interval(PatientDateOfBirth, AdmissionStartDate) / dyears(1), # age at admission in years
    LOS = interval(AdmissionStartDate, AdmissionEndDate) / ddays(1)     # length of hospital stay in days
  )

summary(data5$age)
summary(data5$LOS)




# 8. Create a new dataset with all unnecessary variables removed, only keep those relevant to the analysis
# described in the next points.

# PSEUDOCODE for selecting relevant variables
# Step 1: We see that for the next points we will need the following variables: age, LOS, race(PatientRace), gender (PatientGender), lab values, We also keep ID


# Step 2: # We use select() to keep only the necessary variables

relev_var <- data5 |> 
  select(
    ID, outcome, outcome_f,age, LOS, PatientGender, PatientRace,
    CBC_ABSOLUTE_NEUTROPHILS:URINALYSIS_SPECIFIC_GRAVITY # select all columns from the first lab test to the last one
  )

dim(relev_var)





# 9. Report descriptive analyses (mean, median, SD, IQR, min, max, boxplots) on and test whether there is a difference in age between cases and controls. Do the same for LOS..


# PSEUDOCODE for comparing age and LOS between Cases and Controls

# Step 1: We create a summary table with mean, sd, median, IQR,, min, max for age and LOS
# 
# Step 2: We run t.test() and wilcox.test(). Due to the large sample size (N = 10,000), we can safely run a t.test(). However, we also performed a wilcox.test() to ensure robustness against skewness and outliers.
# Step 3: We created the boxplots



# Step 1
table9 <- relev_var |>
  group_by(outcome_f) |> # divide the dataset into Control and Case groups to use them in summarise()
  summarise(
    n          = n(),                         
    age_mean   = mean(age, na.rm = TRUE),     
    age_sd     = sd(age, na.rm = TRUE),       
    age_median = median(age, na.rm = TRUE),   
    age_iqr    = IQR(age, na.rm = TRUE),      
    age_min    = min(age, na.rm = TRUE),      
    age_max    = max(age, na.rm = TRUE),      
    los_mean   = mean(LOS, na.rm = TRUE),     
    los_sd     = sd(LOS, na.rm = TRUE),       
    los_median = median(LOS, na.rm = TRUE),   
    los_iqr    = IQR(LOS, na.rm = TRUE),      
    los_min    = min(LOS, na.rm = TRUE),      
    los_max    = max(LOS, na.rm = TRUE)       
  )
print(table9, width = Inf) 

# Step 2. Hypothesis tests for age : Null hypothesis: There are no differences between Cases and Control 
t.test(age ~ outcome_f, data = relev_var) 
wilcox.test(age ~ outcome_f, data = relev_var) 

#Hypothesis tests for LOS : Null hypothesis: There are no differences between Cases and Control

t.test(LOS ~ outcome_f, data = relev_var)      # parametric test comparing mean LOS
wilcox.test(LOS ~ outcome_f, data = relev_var) # non-parametric test comparing LOS distributions

# 9.3.Boxplots
par(mfrow = c(1, 2)) # split plot area into 1 row and 2 columns
boxplot(age ~ outcome_f, data = relev_var,
        main = "Age by Outcome", xlab = "", ylab = "Age (years)",
        col = c("lightblue", "salmon")) # boxplot comparing age between Control and Case
boxplot(LOS ~ outcome_f, data = relev_var,
        main = "Length of Stay by Outcome", xlab = "", ylab = "LOS (days)",
        col = c("lightblue", "salmon")) # boxplot comparing LOS between Control and Case
par(mfrow = c(1, 1)) # reset plot area back to a single plot





# 10. Contingency tables and chi-square tests for Gender and Race ----

# PSEUDOCODE for comparing Gender and Race proportions between cases and controls
# Step 1: Analyze PatientGender vs Outcome
# Build a 2x2 frequency table using table()
# Calculate column percentages using prop.table(..., margin = 2)
# Run chisq.test() to check if case/control proportions differ by sex
# Step 2: Analyze PatientRace vs Outcome
# Build a frequency table for race vs outcome
# Calculate column percentages
# Set a random seed with set.seed() for reproducibility
# Run chisq.test(..., simulate.p.value = TRUE) to avoid warnings from small race counts

# 10.1. Gender vs. Outcome
tab_sex <- table(Sex = relev_var$PatientGender, Outcome = relev_var$outcome_f) # count patients by sex and outcome
tab_sex                                                                        # display raw counts
round(prop.table(tab_sex, margin = 2) * 100, 1)                                # show column percentages (rounded to 1 decimal)
chisq.test(tab_sex)                                                            # chi-square test of independence

# 10.2. Race vs. Outcome
tab_race <- table(Race = relev_var$PatientRace, Outcome = relev_var$outcome_f) # count patients by race and outcome
tab_race                                                                       # display raw counts
round(prop.table(tab_race, margin = 2) * 100, 1)                               # show column percentages (rounded to 1 decimal)

set.seed(123)                                                                  # fix random seed so simulated p-value is reproducible
chisq.test(tab_race, simulate.p.value = TRUE)                                  # chi-square test with Monte Carlo simulation





# 10. Report a table with counts of cases and controls across the sex categories and race categories. Perform
# tests on differences in counts (hint: first use table()).
# 11. Make boxplots of the lab values comparing cases and controls. Put the boxplots in one figure.
# 12. Using words, describe any remarkable patterns in the dataset in terms of normality of the distribution,
# skewness, missingness, rare categories, outliers.
# 13. Perform the appropriate regression (linear, logistic, Cox, etc.) to investigate the association between
# the outcome (case/control) and the lab values, corrected for age, gender, and LOS. Which variables are
# found to be significantly
