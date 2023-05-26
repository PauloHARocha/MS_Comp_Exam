library(dplyr)
library(survey)
library(sjstats)
library(MASS)
library(ggplot2)

# AJ32 - FEEL DEPRESSED PAST 30 DAYS
# AF66 - FEEL DEPRESSED WORST MONTH
# AF79 - COMPLETED RECOMMENDED MENTAL HEALTH TREATMENT (NOT FOUND)
# AF86 - EVER THOUGHT TO COMMIT SUICIDE (NOT FOUND)
# AF80 - MAIN REASON QUIT MENTAL HEALTH TREATMENT (NOT FOUND)

# Unavailable codes
# -1:   INAPPLICABLE.
# -2:   PROXY SKIPPED.
# -5:   CHILD/HOUSEHOLD INFORMATION NOT COLLECTED FOR TEEN AND CHILD INTERVIEWS.
# -7:   REFUSED.
# -8:   DON’T KNOW.
# -9:   NOT ASCERTAINED.

# UR_RHP
# 1:Urban
# 2:Rural

# OVRWT (Overweight or obese)
# 1: Yes
# 2: No

#setwd to current folder
setwd("/home/jovyan/work/MS_Comp_Exam/")

# read csv file
df <- read.csv("data/adult_2015_2019_vars_interest.csv")

dim(df)
head(df)

# remove negative values from column AJ32, SRSEX, DISTRESS, UR_RHP
X <- df[which(df$AJ32 >= 0 & df$UR_RHP >= 0 & df$DISTRESS >= 0
& df$SRSEX >= 0 & df$AK28 >= 0 & df$OVRWT >= 0 & df$AE15 >= 0), ]
 
# create a column for DISTRESS higher then 5 (Binomial model outcome)
X$DISTRESS_BI <- ifelse(X$DISTRESS > 5, 1, 0)

# create categories for age SRAGE_P1
# [18, 26] –> 18-29 years
# [30, 40] –> 30 – 44 years
# [45,60] –> 45 – 64 years 
# [65, 80] -> 65 – 84 years
# [85] -> 85+ years
X$SRAGE_P1 <- ifelse(X$SRAGE_P1 == "18" | X$SRAGE_P1 == "26", "18-29",
X$SRAGE_P1)

X$SRAGE_P1 <- ifelse(X$SRAGE_P1 == "30" | X$SRAGE_P1 == "35" |
X$SRAGE_P1 == "40", "30-44", X$SRAGE_P1)

X$SRAGE_P1 <- ifelse(X$SRAGE_P1 == "45" | X$SRAGE_P1 == "50" |
X$SRAGE_P1 == "55" | X$SRAGE_P1 == "60", "45-64", X$SRAGE_P1)

X$SRAGE_P1 <- ifelse(X$SRAGE_P1 == "65" | X$SRAGE_P1 == "70" |
  X$SRAGE_P1 == "75" | X$SRAGE_P1 == "80", "65-84", X$SRAGE_P1)

X$SRAGE_P1 <- ifelse(X$SRAGE_P1 == "85", "85+", X$SRAGE_P1)

X$Age <- X$SRAGE_P1

print("Age")
table(X[X$YEAR_NAME == 2019, ]$Age)
table(X$Age)

# create categories for education
# Add a high school education (1-3)
# Add some college or bachelor education (4-7)
# Add master and phd education (8-9)

X$AHEDC_P1 <- ifelse(X$AHEDC_P1 == "1" | X$AHEDC_P1 == "2"
| X$AHEDC_P1 == "3", "HighSchool", X$AHEDC_P1)

X$AHEDC_P1 <- ifelse(X$AHEDC_P1 == "4" | X$AHEDC_P1 == "5" |
  X$AHEDC_P1 == "6" | X$AHEDC_P1 == "7", "SomeCollege/Bachelor", 
  X$AHEDC_P1)

X$AHEDC_P1 <- ifelse(X$AHEDC_P1 == "8" | X$AHEDC_P1 == "9", 
"Master/Phd", X$AHEDC_P1)

X$Education <- X$AHEDC_P1

print("Education")
table(X[X$YEAR_NAME == 2019, ]$Education)
table(X$Education)

# Create categories for smoked 100 cigaretes
# 1: Yes
# 2: No
X$AE15 <- ifelse(X$AE15 == "1", "Yes", "No")

X$Smoked100 <- X$AE15

print("Smoked100")
table(X[X$YEAR_NAME == 2019, ]$Smoked100)
table(X$Smoked100)

# Create categories for born in US
# 1: Yes
# 2: No
X$AH33NEW <- ifelse(X$AH33NEW == "1", "Yes", "No")

X$BornUS <- X$AH33NEW

print("BornUS")
table(X[X$YEAR_NAME == 2019, ]$BornUS)
table(X$BornUS)


# Create categories for AK28 – Feel safe in the neighborhood (1: All of the time, 2: Most, 3: Some, 4: None)
# 1: All_time
# 2: Most_time
# 3: Some_time
# 4: None
X$AK28 <- ifelse(X$AK28 == "1", "All_time", X$AK28)
X$AK28 <- ifelse(X$AK28 == "2", "Most_time", X$AK28)
X$AK28 <- ifelse(X$AK28 == "3", "Some_time", X$AK28)
X$AK28 <- ifelse(X$AK28 == "4", "None", X$AK28)

X$FeelSafe <- X$AK28

print("FeelSafe")
table(X[X$YEAR_NAME == 2019, ]$FeelSafe)
table(X$FeelSafe)

# Create categories for OVRWT (Overweight or obese)
# 1: Yes
# 2: No
X$OVRWT <- ifelse(X$OVRWT == "1", "Yes", "No")

X$Overweight <- X$OVRWT

print("Overweight")
table(X[X$YEAR_NAME == 2019,]$Overweight)
table(X$Overweight)

# Create categories for SRSEX
# 1: Male
# 2: Female

X$SRSEX <- ifelse(X$SRSEX == "1", "Male", "Female")

X$Sex <- X$SRSEX

print("Sex")
table(X[X$YEAR_NAME == 2019, ]$Sex)
table(X$Sex)

# Create categories for UR_RHP (Urban or rural)
# 1: Urban
# 2: Rural
X$UR_RHP <- ifelse(X$UR_RHP == "1", "Urban", "Rural")

X$UrbanRural <- X$UR_RHP

print("UrbanRural")
table(X[X$YEAR_NAME == 2019, ]$UrbanRural)
table(X$UrbanRural)



# create categories for RACECN_P1 (Race)
# 1: OtherSingle
# 2: Native
# 3: Asian
# 4: AfricaAmerican
# 5: White
# 7: Multiple
X$RACECN_P1 <- ifelse(X$RACECN_P1 == "1", "OtherSingle", X$RACECN_P1)
X$RACECN_P1 <- ifelse(X$RACECN_P1 == "2", "Native", X$RACECN_P1)
X$RACECN_P1 <- ifelse(X$RACECN_P1 == "3", "Asian", X$RACECN_P1)
X$RACECN_P1 <- ifelse(X$RACECN_P1 == "4", "AfricanAmerican", X$RACECN_P1)
X$RACECN_P1 <- ifelse(X$RACECN_P1 == "5", "White", X$RACECN_P1)
X$RACECN_P1 <- ifelse(X$RACECN_P1 == "7", "Multiple", X$RACECN_P1)

X$Race <- X$RACECN_P1

print("Race")
table(X[X$YEAR_NAME == 2019, ]$Race)
table(X$Race, X$YEAR_NAME)



# create categories for UR_BG6 Rural and Urban - Claritas (By block group) (6 levels)
# 1: Urban
# 2: 2ndCity
# 3: Mixed
# 4: Suburban
# 5: Town
# 6: Rural
X$UR_BG6 <- ifelse(X$UR_BG6 == "1", "Urban", X$UR_BG6)
X$UR_BG6 <- ifelse(X$UR_BG6 == "2", "2ndCity", X$UR_BG6)
X$UR_BG6 <- ifelse(X$UR_BG6 == "3", "Mixed", X$UR_BG6)
X$UR_BG6 <- ifelse(X$UR_BG6 == "4", "Suburban", X$UR_BG6)
X$UR_BG6 <- ifelse(X$UR_BG6 == "5", "Town", X$UR_BG6)
X$UR_BG6 <- ifelse(X$UR_BG6 == "6", "Rural", X$UR_BG6)

X$UrbanRural6 <- X$UR_BG6

print("UrbanRural6")
table(X[X$YEAR_NAME == 2019, ]$UrbanRural6)
table(X$UrbanRural6)


# save the data
write.csv(X, "data/adult_2015_2019_vars_interest_final.csv", row.names = FALSE)