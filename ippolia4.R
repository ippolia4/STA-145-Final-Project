##load packages 
library (readr)
library(dplyr)
library(ggplot2)
library(haven)
library(forcats)
library(psych)


##set working directory 
setwd("/courses/STA145/ippolia4")

#load the data
data <-read_delim("final_project.csv")

##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################
###Descriptive Statistics 
mean(data$monthly_listeners)
sd(data$monthly_listeners)
summary(data$monthly_listeners)
describe(data$monthly_listeners)

summary(data$length_of_song)
describe(data$length_of_song)
sd(data$length_of_song)

##################################################################################
####################   Figure 1: scatter plot             ####################   
##################################################################################
####Create Scatter Plot 
# showing the relationship between campaign spending and observed vote share 


linear_plot <- plot(data$length_of_song, data$monthly_listeners)
print(linear_plot)

# add x line and y line for means
meany <- mean(data$length_of_song)
meanx <- mean(data$monthly_listeners)

abline(v = meany, col = "black")
abline(h = meanx, col = "black")

##### STEP 2: Calculate linear regression line (i.e., slope) and add to scatter plot
linear_relationship <- lm(length_of_song ~ monthly_listeners, data = data)
summary(linear_relationship)
summary(linear_relationship)

# Add the linear regression line to the scatter plot
# NOTE: double check the scatter plot is currently in your utilities window!
abline(linear_relationship, col = "red")

##### STEP 3: Plot the residuals

# Plot the residuals
plot(data$monthly_listeners, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")

hist(residuals(linear_relationship))

plot(data$length_of_song, log10(data$monthly_listeners),
     xlab="Song Length",
     ylab="Log10 Monthly Listeners")

abline(lm(log10(monthly_listeners) ~ length_of_song, data=data), col="red")

##find correlation coefficient 
cor(data$length_of_song, data$monthly_listeners)
