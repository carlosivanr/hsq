# t_to_relapse_R


library(tidyverse)
library(gtsummary)
library(survival)


# Load data
first_relapse <- read_csv('C:/Users/rodrica2/OneDrive - The University of Colorado Denver/Documents/DFM/projects/hsq/analyses_sas/data/first_relapse.csv')


hsq_data <- read_csv('C:/Users/rodrica2/OneDrive - The University of Colorado Denver/Documents/DFM/projects/hsq/analyses_sas/data/hsq_data.csv')

# Load the training data


# Model data



# Kaplan Meier ----------------------------------------------------------------

# Define the survival object
surv_obj <- Surv(time = first_relapse$time, event = first_relapse$event)

# Fit Kaplan-Meier survival model
km_fit <- survfit(surv_obj ~ arm, data = first_relapse)

# Summary of results
summary(km_fit)

# Plot Kaplan-Meier curves
# Extract survival estimates into a tidy dataframe
km_data <- data.frame(
  time = km_fit$time,
  surv = km_fit$surv,
  strata = factor(rep(names(km_fit$strata), km_fit$strata))
)

# Create ggplot survival plot
ggplot(km_data, aes(x = time, y = surv, color = strata, group = strata)) +
  geom_step() +  # Kaplan-Meier curve is a step function
  labs(title = "Kaplan-Meier Analysis",
       x = "Time", y = "Survival Probability") +
  theme_minimal()


# Unadjusted ------------------------------------------------------------------
# Fit the Cox proportional hazards model
cox_fit <- coxph(surv_obj ~ arm, data = first_relapse)

# Summary of model results
summary(cox_fit)

# Display table
# when exponentiate = TRUE, the result is HR not log(HR)
# HR is interpreted as the intervention has 85% of the risk compared to the
# reference group. Therefore the intervention reduces the likelihood of relapse.
# The intervention group is 15% less likely to experience relapse.
tbl_regression(cox_fit, exponentiate = TRUE)


# Adjusted --------------------------------------------------------------------
# Fit adjusted Cox model
cox_adjusted <- coxph(surv_obj ~ arm * gender + age , data = first_relapse)

# Summary of results
summary(cox_adjusted)

# Display table
tbl_regression(cox_adjusted, exponentiate = TRUE)
