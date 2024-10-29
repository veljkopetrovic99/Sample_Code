# This part of analysis aims to test H3 and H4 by using a random intercept model

# Load necessary libraries
#install.packages("lme4")
#install.packages("lmerTest")
#install.packages("dplyr")

library(dplyr)
library(lme4)
library(lmerTest)

# Loading the data
data <- Data_v2
describe(data)

#CREATING NECESSARY DUMMY VARIABLES

# Create dummy variables for Condition (SD)
data <- data %>%
  mutate(Condition_Dummy = ifelse(Condition == "High SD", 1, 0))

# Create dummy variables for Group
data <- data %>%
  mutate(Group_Dummy = ifelse(Group == "Treatment", 1, 0))

# Create dummy variables for Gender
data <- data %>%
  mutate(Gender_Male = ifelse(Gender == "Male", 1, 0),
         Gender_Female_Other = ifelse(Gender != "Male", 1, 0))

# Create dummy variables for Education
data <- data %>%
  mutate(Education_Bachelors = ifelse(Education == "Bachelors", 1, 0),
         Education_High_School_or_below = ifelse(Education == "High School or below", 1, 0),
         Education_Masters = ifelse(Education == "Masters", 1, 0),
         Education_PhD_or_equivalent = ifelse(Education == "PhD or equivalent", 1, 0))

# Create dummy variables for Income ranges
data <- data %>%
  mutate(Income_0_5000 = ifelse(Income >= 0 & Income <= 5000, 1, 0),
         Income_5001_15000 = ifelse(Income >= 5001 & Income <= 15000, 1, 0),
         Income_15001_30000 = ifelse(Income >= 15001 & Income <= 30000, 1, 0),
         Income_30001_45000 = ifelse(Income >= 30001 & Income <= 45000, 1, 0),
         Income_45001_60000 = ifelse(Income >= 45001 & Income <= 60000, 1, 0),
         Income_Above_60000 = ifelse(Income > 60000, 1, 0))

#Create dummy for each SD and Mean pair
data <- data %>%
  mutate(M_400_SD_10 = ifelse(True_Mean == 400 & SD == 10, 1, 0),
         M_80_SD_5 = ifelse(True_Mean == 80 & SD == 5, 1, 0),
         M_300_SD_60 = ifelse(True_Mean == 300 & SD == 60, 1, 0),
         M_100_SD_50 = ifelse(True_Mean == 100 & SD == 50, 1, 0))

#REGRESSION ANALYSIS
# H3 is tested by seeing seing a significant effect of the Group_dummy variable; H4 is tested by the interaction term Group_Dummy and Confidence_Percentage

random_intercept_model <- lmer(Deviation ~ M_400_SD_10 + M_80_SD_5 + M_300_SD_60 + Confidence_Percentage + Group_Dummy + Group_Dummy * Confidence_Percentage +
                                 Gender_Male +
                                 Education_High_School_or_below + Education_Bachelors + Education_PhD_or_equivalent +
                                 Income_0_5000  + Income_15001_30000 + Income_45001_60000 + Income_Above_60000 +
                                 Age + (1 | Number), 
                               data = data)
model_summary1 <- summary(random_intercept_model)
print(model_summary1)

