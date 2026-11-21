# Assignment 2 - part 2 - updated version
# Prof. Rebecca Stelatto 
# Students: Kyle, Nassi, Susane 

# Changes between current and previous version (4 differences): 
  # 1. This new version contains more comments with explanations
  # 2. The "for" is wrapped inside the function "simulate_power" to run for 1,000 reps
  # 3. The rnorm inside the for should have the argument n = 50 obs. 
  #       not 1,000 inside the for loop:
  #       Before:  control <- rnorm(n, mean = 150, sd = 15) was changed to
  #       Now:     control <- rnorm(group_size, mean = 150, sd = 15)
  # 4. This line was added to calculate the power: 
  #         power <- mean(vector < 0.05 )


# Background information --------------------------
    # randomized experiment 
    # two groups: experimental + control 
    # 50 participants per group, randomly assigned 
  # Y : test score 
  # exposure X: training on learning strategies before exam 
    # control group: mean score of 150, sd = 15 
    # experimental group: 160, sd = 15 
  # Goal of the simulation: 
    # Use a t.test to calculate the power of the difference of mean_diff = 10

#Exercise 1 ------------------------------------------------
# Step 1: Understand the data: normal data, mean, sd 
# Step 2: Data simulation with rnorm 
set.seed(1111)
n <- 1000
group_size <- 50
control <- rnorm(group_size, mean = 150, sd = 15)
experimental <- rnorm(group_size, mean = 160, sd = 15)

# Step 3: p-value from the t.test. 
  # H0: mu_experimental = mu_control (mu_exp - mu_control = 0)
  # H1: mu_experimental != mu_control (two.sided test)
  # We chose a two.sided test because it is more conservative: 
  # we can also detect a decrease in scores due to training. 

t.test(control, experimental)$p.value

# Conclusion: 
  # p-value = 0.001017028 is significant, we can reject the null hypothesis
  # the groups' means are significantly different 

# Exercise 2 --------------------------------------------------
# --- CORRECTION #1 : Create a function around the for loop with arguments n and group_size
simulate_power <- function(n = 1000, group_size = 50){
vector <- numeric(n)
t.statistic <- numeric(n)

for (i in 1:n){
  # generate data 
  # --- CORRECTION #2: n = group_size instead of n (otherwise we'd create sample sizes with 1000 obs.)
  control <- rnorm(group_size, mean = 150, sd = 15)
  experimental <- rnorm(group_size, mean = 160, sd = 15)
  # t-test 1000 times (n = 1000)
  vector[i] <- t.test(control, experimental)$p.value
  t.statistic[i] <- t.test(control, experimental)$statistic
  }
head(vector)
head(t.statistic)

# Power is the probability of  rejecting the null hypothesis when it is false 

#                 R E A L I T Y
#            | H0 is true | H0 is false |
#            |------------|-------------|
# Reject H0  | Type I err |  POWER!!    | 
# Not Rej H0 |   Correct |  Type II err | 

# So our idea is to calculate: 
########## the probability of getting a significant p-value

# --- CORRECTION 3: power = proportion of times estimated p-value < .05
# calculate proportion of significant p-values
power <- mean(vector < 0.05 )

list(power = power)
}

simulate_power(n = 1000, group_size = 50)
# power = .904 
power.t.test(n = 50, delta = 10, sd = 15)
# power = 0.9099633 

# Conclusion: 
# The power simulated with the Monte Carlo algorithm with 1000 repetitions (.904) 
# was very close to the power obtained with the function power.t.test (.9099). 
# Both methods suggest that with 50 participants per group, the chance of
# detecting a true effect is around 91%.

