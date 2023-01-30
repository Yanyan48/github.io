### Treatment of Lead-Exposed Children Trial ###
#### Longitudinal Data Analysis ####

# The purpose of this script is to use some mixed effect models of clinical trial 
# data to evaluate the safety and efficacy of a new drug.

# # # # # # # # # # # # # # # # # # # # #
# Install required packages
# tidyverse for data wrangling 
# tidyr for changing the shape of a dataset
# lme4 for fitting and analyzing mixed models
# # # # # # # # # # # # # # # # # # # # #
install.packages("lme4") 
install.packages('Matrix')
library(tidyverse)
library(tidyr)
library(lme4)
#=====================
# STEP 1:COLLECT DATA
#=====================

# Upload Treatment of Lead-Exposed Children (TLC) datasets here
tlc_data <- read.table('/Users/anna/Desktop/ASU/Fall 22/LSC 598 II - Yue Wang/STP 560 Lectures/Module V/tlc-data.txt')
head(tlc_data)

#=====================
# STEP 2:WRANGLE DATA 
#=====================

# Add column names
colnames(tlc_data) <- c("ID", "Trt", "W0", "W1", "W4", "W6") 
head(tlc_data)

#===============================================
#STEP 3:PREPARE AND CONDUCT DESCRIPTIVE ANALYSIS
#===============================================

# Do the trajectory plot using the wide format
time <- c(0, 1, 4, 6) # time points
n <- dim(tlc_data)[1] # the number of subjects
# Getting to know the range of the data
max(tlc_data$W0, tlc_data$W1, tlc_data$W4, tlc_data$W6)
min(tlc_data$W0, tlc_data$W1, tlc_data$W4, tlc_data$W6)
# Covert the column Trt to numbers so that two treatment groups() can show different color
tlc_data %>% mutate(Trt.color = factor(Trt, levels = c("A", "P"), labels = c(1, 2)))-> tlc_data
# Plot the first subject
plot(time, as.numeric(tlc_data[1, 3:6]), type = 'l', ylim = c(0, 65), pch = 19, xlab = 'Week', ylab = 'Blood lead level')
# Add lines of the remaining subjects to this plot
for(i in 2:n){
  lines(time, as.numeric(tlc_data[i, 3:6]), pch = 19, col = tlc_data$Trt.color[i], ylim = c(0, 65))
}
legend("topleft", legend = c("New Agent(A)", "Placebo(P)"), lwd = 3,col = c("black", "red"))

## CONCLUSION:
# From the plot, we can see that the trajectories of New Agent are mostly separated 
# from the Placebo subjects. The blood lead level of the New Agent group is mostly 
# lower than the Placebo group.

#=======================================================================
#STEP4: FIX A MIXED EFFECT MODEL TO CONDUCT FURTHER DESCRIPTIVE ANALYSIS
#=======================================================================

# Fit a mixed effect model with a random intercept to examine whether the new 
#agent is effective in reducing the blood lead level.

# Reload the data and add column name
tlc_data <- read.table('/Users/anna/Desktop/ASU/Fall 22/LSC 598 II - Yue Wang/STP 560 Lectures/Module V/tlc-data.txt')
head(tlc_data)
colnames(tlc_data) <- c("ID", "Trt", "W0", "W1", "W4", "W6")
head(tlc_data)

# Convert it to a long format so that it is good for modeling
long_tlc_data <- tlc_data %>% gather(Week, Level,W0, W1, W4, W6) 
head(long_tlc_data, 20)

# Creat a numeric variable for time based on the Day variable
long_tlc_data_2 <- separate(long_tlc_data, Week, sep = 'W', into = c('No use', "time"))
head(long_tlc_data_2)

# Fit a mixed effect model with a random intercept
model1 = lmer(as.numeric(Level) ~ Trt + as.numeric(time) + (1 | ID), data = long_tlc_data_2)
summary(model1)

## CONCLUSION:
# From the data in the output of confint() function, the confidence intervals of 
# Trt is 3.4 to 7.7, which means we have 95% confidence there are differences 
# between two treatments. In addition, the time variables are significant, which 
# means we have 95% confidence the blood lead level does change along with time.

# Fit a mixed effect model with a random intercept and a random time variable
model2 = lmer(as.numeric(Level) ~ Trt + as.numeric(time) + (1 + as.numeric(time) | ID), data = long_tlc_data_2)
summary(model2)

## CONCLUSION:
# There is a warning message occurring on this model, which may indicate that the 
# model fitting process is numerically unstable and that a simpler model may need 
# to be considered. However, we still can get a similar result from this more 
# complicated model. We have 95% confidence that there are differences between two 
# treatments. And we have 95% confidence the blood lead level does change along with time.

# The end!
#=================================================
