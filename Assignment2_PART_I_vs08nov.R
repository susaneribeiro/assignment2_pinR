# Programming in R Assignment 2 
library(tidyverse)
library(lubridate)
set.seed(3)
# Information about the dataset ----
  # dat_diag is AdmissionsDiagnosesCorePopulatedTable
  # dat_admis is AdmissionsCorePopulatedTable
  # dat_pts is PatientCorePopulatedTable
  # dat_lab is LabsCorePopulatedTable

# 1: Load the datasets into memory using load. ---- 
load("MSBBSS04_10k_dataset.RData")

head(dat_lab)
dim(dat_lab)
class(dat_lab$LabDateTime) # check if true chronological date-times

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
    gsub(pattern = "/", replacement = "_") # I changed it to forward slash as I still see them in data4.
}

# lets clean the names of column LabName: 
data3 <- data2 |> 
  mutate(LabName = clean_names(LabName))
head(data3$LabName)
unique(data3$LabName) #check for forward slash.

# finally, we perform the pivot wider using the names from LabName and the values from LabValue
# the result is a dataset with a column per exame, so we can see all the exams that each
# patient has 

data4 <- data3 |>
  select(-LabDateTime, -LabUnits) |> # to get to "one row per patient"
  pivot_wider(
    names_from = LabName,
    values_from = LabValue
  )
View(data4)


# 7. Calculate the patient age in years and length of stay (LOS). The length of stay is defined as
# the duration from admission start to end in days. You may use lubridate::interval and
# lubridate::dyears/ddays, e.g. something like LOS = interval(start date, end date)/ddays(1)
# to get a numeric value for the number of days.

data5 <- data4 |>
  mutate(
    age = interval(as.POSIXct(PatientDateOfBirth), as.POSIXct(AdmissionStartDate)) / dyears(1),
    LOS = interval(as.POSIXct(AdmissionStartDate), as.POSIXct(AdmissionEndDate)) / ddays(1)
  ) # I checked "LabDateTime" before and it is already in the format of POSIXct, but why risk it by being lazy? Better safe than sorry. 

summary(data5$age) # Weirdly young? Good news, now we have something to say in Task 12.
summary(data5$LOS)


data5 |>
  select(PatientDateOfBirth, AdmissionStartDate, age, AdmissionEndDate, LOS) |>
  tail(5) # Looks reasonable to me...


# 8. Create a new dataset with all unnecessary variables removed, only keep those relevant to the analysis
# described in the next points.

lab_cols <- unique(data3$LabName)

data8 <- data5 |>
  select(
    PatientID,
    outcome,
    age,
    LOS,
    PatientGender,
    PatientRace,
    all_of(lab_cols)
  ) |>
  mutate(
    outcome_f = factor(outcome, levels = c(0, 1), labels = c("Control", "Case")), # Binary outcome? Force of habit.
    PatientGender = as.factor(PatientGender),
    PatientRace = as.factor(PatientRace)
  )

dim(data8)
sum(complete.cases(data8))

# 9. Report descriptive analyses (mean, median, SD, IQR, min, max, boxplots) on and test whether there is
# a difference in age between cases and controls. Do the same for LOS.

# 9.1. Summary table for Age and LOS by Case/Control, and this is task 9, so... table9 it is.
table9 <- data8 |>
  group_by(outcome_f) |>
  summarise(
    n = n(),
    age_mean = mean(age, na.rm = TRUE),
    age_sd = sd(age, na.rm = TRUE),
    age_median = median(age, na.rm = TRUE),
    age_iqr = IQR(age, na.rm = TRUE),
    age_min = min(age, na.rm = TRUE),
    age_max = max(age, na.rm = TRUE),
    los_mean = mean(LOS, na.rm = TRUE),
    los_sd = sd(LOS, na.rm = TRUE),
    los_median = median(LOS, na.rm = TRUE),
    los_iqr = IQR(LOS, na.rm = TRUE),
    los_min = min(LOS, na.rm = TRUE),
    los_max = max(LOS, na.rm = TRUE)
  )
print(table9, width = Inf)

# 9.2. Hypothesis tests
t.test(age ~ outcome_f, data = data8) # Normality assumption. Watchout for the outliers.
wilcox.test(age ~ outcome_f, data = data8) # Why? No assumption on normality, and rank-sum dampens the outliers pulls.

t.test(LOS ~ outcome_f, data = data8)
wilcox.test(LOS ~ outcome_f, data = data8)

# 9.3. Side-by-side boxplots
par(mfrow = c(1, 2))
boxplot(age ~ outcome_f, data = data8,
        main = "Age by Outcome", xlab = "", ylab = "Age (years)",
        col = c("lightblue", "salmon"))
boxplot(LOS ~ outcome_f, data = data8,
        main = "Length of Stay by Outcome", xlab = "", ylab = "LOS (days)",
        col = c("lightblue", "salmon"))
par(mfrow = c(1, 1))
# Why salmon and ocean blue? Because we talked about the salmon farming industry at Wednesday's Post-Exam beer.


# 10. Report a table with counts of cases and controls across the sex categories and race categories. Perform
# tests on differences in counts (hint: first use table()).

# Gender vs. Outcome
tab_sex <- table(Sex = data8$PatientGender, Outcome = data8$outcome_f)
tab_sex
round(prop.table(tab_sex, margin = 2) * 100, 1) #See the table in a different view (It sums up to 100% by rows and columns)
chisq.test(tab_sex) # We want to know if the proportion of cases and controls differs significantly across Sex category.

# Null hypothesis H_0:patient sex and diagnosis outcome (case vs. control) are statistically independent.
# In other words, H_0 is claiming that the proportion of patients with confirmed case is identical across sex categories, and any small difference in the sample is due to random chance.
# p_value = 0.7491 > 0.05, we fail to reject the H_0. The cases and controls proportions do not differ by sex category.


# Race vs. Outcome
tab_race <- table(Race = data8$PatientRace, Outcome = data8$outcome_f)
tab_race
round(prop.table(tab_race, margin = 2) * 100, 1) # 2 means splitting by columns
chisq.test(tab_race) # We want to know if the proportion of cases and controls differs significantly across Race category.

# Null hypothesis H_0:patient Race and diagnosis outcome (case vs. control) are statistically independent.
# In other words, H_0 is claiming that the proportion of patients with confirmed case is identical across Race categories, and any small difference in the sample is due to random chance.
# p_value = 0.1033 > 0.05, we fail to reject the H_0. The cases and controls proportions do not differ by race category.
# I didn't set seed here, so the p-values might be different when run by others.

# 11. Make boxplots of the lab values comparing cases and controls. Put the boxplots in one figure.

# Task 11: Boxplots of lab values comparing cases and controls
lab_cols <- setdiff(
  names(data8),
  c("PatientID", "outcome", "outcome_f", "age", "LOS", "PatientGender", "PatientRace")
)

plot11 <- data8 |>
  select(outcome_f, all_of(lab_cols)) |>
  pivot_longer(
    cols = all_of(lab_cols),
    names_to = "LabName",
    values_to = "LabValue"
  ) |>
  filter(!is.na(LabValue)) |>
  ggplot(aes(x = outcome_f, y = LabValue, fill = outcome_f)) +
  geom_boxplot(outlier.size = 0.6, alpha = 0.85) +
  facet_wrap(~ LabName, scales = "free_y", ncol = 5) +
  scale_fill_manual(values = c("Control" = "lightblue", "Case" = "salmon")) +
  labs(
    title = "Distribution of Lab Values by Outcome (Case vs. Control)",
    x = NULL,
    y = "Lab Value",
    fill = "Group"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "top",
    strip.text = element_text(size = 8, face = "bold")
  )

plot11


# Can't really see anything, huh? GGsave comes to rescue!

ggsave("plot11_lab_boxplots.png", plot = plot11, width = 14, height = 15, dpi = 300)

# 12. Using words, describe any remarkable patterns in the dataset in terms of normality of the distribution,
# skewness, missingness, rare categories, outliers.

# Task 12: Diagnostic summary for normality, skewness, missingness, rare categories, and outliers
num_cols <- c("age", "LOS", lab_cols)

table12 <- data8 |>
  select(all_of(num_cols)) |>
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") |>
  group_by(Variable) |>
  summarise(
    n_present = sum(!is.na(Value)),
    missing_pct = round(mean(is.na(Value)) * 100, 1),
    mean = round(mean(Value, na.rm = TRUE), 2),
    median = round(median(Value, na.rm = TRUE), 2),
    skewness = round(
      mean((Value - mean(Value, na.rm = TRUE))^3, na.rm = TRUE) /
        (sd(Value, na.rm = TRUE)^3),
      2
    ),
    n_outliers = sum(
      Value < (quantile(Value, 0.25, na.rm = TRUE) - 1.5 * IQR(Value, na.rm = TRUE)) |
        Value > (quantile(Value, 0.75, na.rm = TRUE) + 1.5 * IQR(Value, na.rm = TRUE)),
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  arrange(desc(missing_pct))

print(table12, n = Inf, width = Inf)


# Check categorical proportions and complete-case row count
round(prop.table(table(data8$PatientGender, useNA = "ifany")) * 100, 1)
round(prop.table(table(data8$PatientRace, useNA = "ifany")) * 100, 1)
sum(complete.cases(data8))

# Comments:
# 12.1. Normality, Skewness, and one stand-out variable
## From earlier step, we already discovered that None of the continuous variables follow a normal bell curve.
## We can see this from both age (18.0 to 28.0 years; mean = 23.1, median = 23.1, skewness = -0.03) and length of stay (LOS: 2.01 to 20.0 days; mean = 10.9, median = 10.9, skewness = 0.03) follow non-normal distributions

## Here in the table12, we see that all these varying lab tests have almost identical means and medians. The skewness bounded around -0.15 and 0.04.

## URINALYSIS_SPECIFIC_GRAVITY stands out: all 9,988 non-missing values equal 1.0 (mean = 1, median = 1, skewness = NaN).

# 12.2. Missingness
## Total size is N = 10,000 patients (8,602 Controls and 1,398 Cases; see table9).

## Demographics (age, PatientGender, PatientRace), LOS, and outcome have 0% explicit NA missingness (see table12 rows 36-37 for age/LOS, table9 for outcome, and task 12 last three lines). 
## However, 13.0% of patients have PatientRace recorded as "Unknown" (n = 1,300; see tab_race in Task 10).

## Lab variables have minimal missingness—only 0.1% to 0.2% per test, with 9,979 to 9,994 non-missing records out of 10,000 (see table12, rows 1-35).

## Across all columns simultaneously, 9,662 out of 10,000 patients (96.6%) have complete non-missing records (task 12 last line).

# 12.3 Rare Categories:
## Outcome is moderately imbalanced: 1,398 Cases (14.0%) vs. 8,602 Controls (86.0%) (see table9).

## Neither PatientGender (52.0% Female, 48.0% Male; see Task 12 last three lines) nor PatientRace (49.0% White, 23.0% Asian, 15.0% African American, 13.0% Unknown; see Task 12 last three lines) contains ultra-rare categories (<5%).

# 12.4 4. Outliers:
## All 37 continuous variables (age, LOS, and all 35 lab tests) have 0 IQR-based outliers (see n_outliers in table12 and plot11). 



# 13. Perform the appropriate regression (linear, logistic, Cox, etc.) to investigate the association between
# the outcome (case/control) and the lab values, corrected for age, gender, and LOS. Which variables are
# found to be significantly

# Task 13: Multivariable logistic regression predicting malignant neoplasm (outcome = 1)
# Exclude PatientID, outcome_f (duplicate factor of outcome), and
# URINALYSIS_SPECIFIC_GRAVITY (zero-variance constant = 1.0 found in Task 12).

# Binary outcome, predicting cases, logistic comes to mind, just a hunch.
# Excluding PatientID, outcome_f (duplicate factor of outcome), and that one weird variable found in previous step: URINALYSIS_SPECIFIC_GRAVITY (zero-variance constant = 1.0).

data13 <- data8 |>
  select(-PatientID, -outcome_f, -URINALYSIS_SPECIFIC_GRAVITY)

model13 <- glm(outcome ~ ., data = data13, family = binomial(link = "logit"))

# Verify sample size used in complete-case regression (should match 9,662 from Task 12)
nobs(model13)

# Build summary table of Log-Odds, Odds Ratios (OR), 95% CIs, and p-values
ci13 <- confint.default(model13)

table13 <- tibble(
  Predictor = names(coef(model13)),
  LogOdds   = round(coef(model13), 4),
  OR        = round(exp(coef(model13)), 4),
  CI_lower  = round(exp(ci13[, 1]), 4),
  CI_upper  = round(exp(ci13[, 2]), 4),
  p_value   = signif(summary(model13)$coefficients[, 4], 3)
) |>
  filter(Predictor != "(Intercept)") |>
  arrange(p_value)

print(table13, n = Inf, width = Inf)

# Filter to statistically significant predictors (p < 0.05)
sig13 <- table13 |>
  filter(p_value < 0.05)

print(sig13, n = Inf, width = Inf)

# Findings:
# 1. Model Specification & Sample Size:
## A multivariable logistic regression model (model13) was fitted on data13 to predict cases (outcome = 1) across predictors (lab tests, age, LOS, PatientGender, and PatientRace).

## PatientID, outcome_f (duplicate factor), and URINALYSIS_SPECIFIC_GRAVITY (see table12 row 35) were excluded prior to fitting.

# 2. Statistically Significant Predictors (p < 0.05; see sig13):
## URINALYSIS_PH: Odds Ratio = 1.08 (95% CI: 1.01-1.16, p = 0.0226). Holding all other variables constant, each 1-unit increase in urine pH is associated with an 8% increase in the odds of malignant neoplasm.

## URINALYSIS_RED_BLOOD_CELLS: Odds Ratio = 1.06 ((95% CI: 1.00–1.12, p = 0.0446). Holding all other variables constant, each 1-unit increase in urinalysis red blood cell count is associated with a 6% increase in the odds of malignant neoplasm.

# 3. Age, Gender, and LOS:
## Consistent with bivariate tests in Tasks 9 and 10, age (OR = 1.01, p = 0.240), LOS (OR = 1.00, p = 0.648), PatientGender (Male vs. Female: OR = 0.99, p = 0.926), and
## PatientRace (White p = 0.152, Asian p = 0.663, Unknown p = 0.799 vs. African American) are not significantly associated with cases after adjustment.

