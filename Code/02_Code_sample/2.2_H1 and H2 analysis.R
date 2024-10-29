# T-test and check and balances
# Load necessary libraries
#install.packages("tidyr")
#install.packages("rcompanion")
library(dplyr)
library(tidyr)
library(ggplot2)
library(rcompanion)


# Assume Cleaned_v6_t_test is already loaded as data_t
data_t <- Data_v1

#Calculating the average deviation and confidence elicitation per Standard deviation condition per participant

#Absolute deviation
data_t$Average_Absolute_Deviation_LSD <- (data_t$Absolute_Deviation_Mean_400+data_t$Absolute_Deviation_Mean_80)/2
data_t$Average_Absolute_Deviation_HSD <- (data_t$Absolute_Deviation_Mean_100+data_t$Absolute_Deviation_Mean_300)/2

#Confidence elicitation
data_t$Average_Confidence_LSD <- (data_t$Confidence_Mean_400_Percentage+data_t$Confidence_Mean_80_Percentage)/2
data_t$Average_Confidence_HSD <- (data_t$Confidence_Mean_300_Percentage+data_t$Confidence_Mean_100_Percentage)/2

# Filter the control group data. This is done as the H1 and H2 are part of a replication, thus the condition of the control group resemble te original study
control_data_t <- data_t %>% filter(Group == "Control")

#Testing for Normality
shapiro.test(control_data_t$Average_Absolute_Deviation_HSD)
shapiro.test(control_data_t$Average_Absolute_Deviation_LSD)
shapiro.test(control_data_t$Average_Confidence_HSD)
shapiro.test(control_data_t$Average_Confidence_LSD)

#ABSOLUTE DEVIATION 
# Perform the Wilcoxon Signed-Rank Test for paired data (as it is not normaly distributed)
wilcox_test_absolute_deviation <- wilcox.test(control_data_t$Average_Absolute_Deviation_HSD, 
                                              control_data_t$Average_Absolute_Deviation_LSD, 
                                              paired = TRUE)
print(wilcox_test_absolute_deviation)

#CONFIDENCE
# Perform the paired t-test for Confidence (as it is normal)

print(t_test_result_Confidence <- t.test(control_data_t$Average_Confidence_HSD,control_data_t$Average_Confidence_LSD, paired = TRUE))

#SUMMARY STATISTICS

#Mean elicitation
means <- data.frame(
  Condition = c("High SD", "Low SD"),
  Average_Absolute_Deviation = c(mean(control_data_t$Average_Absolute_Deviation_HSD), mean(control_data_t$Average_Absolute_Deviation_LSD)),
  Average_Confidence = c(mean(control_data_t$Average_Confidence_HSD), mean(control_data_t$Average_Confidence_LSD))
)

#standard errors
se <- data.frame(
  Condition = c("High SD", "Low SD"),
  SE_Absolute_Deviation = c(sd(control_data_t$Average_Absolute_Deviation_HSD) / sqrt(nrow(control_data_t)), sd(control_data_t$Average_Absolute_Deviation_LSD) / sqrt(nrow(control_data_t))),
  SE_Confidence = c(sd(control_data_t$Average_Confidence_HSD) / sqrt(nrow(control_data_t)), sd(control_data_t$Average_Confidence_LSD) / sqrt(nrow(control_data_t)))
)


# Merge means and standard errors
data_summary <- merge(means, se, by = "Condition")

#EFFECT SIZE

# For Confidence
# Calculate the differences
differences_Confidence <- control_data_t$Average_Confidence_HSD - control_data_t$Average_Confidence_LSD

# Calculate mean and standard deviation of the differences
mean_diff_Confidence <- mean(differences_Confidence)
sd_diff_Confidence <- sd(differences_Confidence)

# Calculate Cohen's d
cohens_d_Confidence <- mean_diff_Confidence / sd_diff_Confidence

# Print Cohen's d
print(paste("Cohen's d for Average Confidence:", cohens_d_Confidence))





