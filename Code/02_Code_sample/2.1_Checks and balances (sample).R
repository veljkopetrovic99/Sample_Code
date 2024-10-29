# This part of the analysis aims to see if there are any significant over representation of any control category (e.g gender) in the treatment 
#or control group. 

# Load necessary libraries
library(dplyr)
library(tidyr)
library(outliers)

data_checks = Data_v1
# Perform baseline comparisons

# Categorical Variables: Gender, Education, Income

#GENDER
# Chi-square test for Gender
gender_table <- table(data_checks$Gender, data_checks$Group)
chi_square_gender <- chisq.test(gender_table)
print(chi_square_gender)

# Fisher's Exact Test for Gender. This test was used as in some subcategories the threshold of 5 observations was not satisfied
fisher_test_gender <- fisher.test(gender_table)
print(fisher_test_gender)

#INCOME
# Chi-square test for Income
income_table <- table(data_checks$Income, data_checks$Group)
chi_square_income <- chisq.test(income_table)
print(chi_square_income)

# Fisher's Exact Test for Income
fisher_test_income <- fisher.test(income_table)
print(fisher_test_income)

#EDUCATION
# Chi-square test for Education
education_table <- table(data_checks$Education, data_checks$Group)
chi_square_education <- chisq.test(education_table)
print(chi_square_education)

# Fisher's Exact Test for Education
fisher_test_education <- fisher.test(education_table)
print(fisher_test_education)


# Continuous Variable: Age
# Independent t-test for Age
# checking for normality to guide which test to use 
shapiro.test(data_checks$Age)


# Alternative to t-test for non-normal distributions: Wilcoxon rank-sum test
# Wilcoxon rank-sum test for Age
wilcox_test_age <- wilcox.test(Age ~ Group, data = data_checks)
print(wilcox_test_age)

