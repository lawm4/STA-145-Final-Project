## Project:  STA 145, Fall 2026, Final Project
# Located:   Law TCNJ Google Drive
# File Name: lawm4_final_project
# Date:      2026_11_10
# Who:       Marin Law



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(haven)
library(forcats)
library(psych)


# Load data
data <- read.csv("raw_data.csv") 

linear_relationship <- lm(awards ~ content_rating, data = data) 

dataset <- read.csv("raw_data.csv")
class(dataset)   # must say "data.frame"
names(dataset)   # shows the real column names

###############################################################################
############### Table 1: Descriptive Statistics ###############################
###############################################################################

# generate summary statistics for awards
mean(data$awards)
sd (data$awards)
hist(data$awards)
summary(data$awards)


# generate summary statistics for content rating
table(data$content_rating)

dataset$content_rating <- as.numeric(dataset$content_rating)

# Figure 2: scatter plot with regression line
# Research question: does maturity rating predict awards?
# So awards = outcome (y), content_rating = predictor (x)

linear_relationship <- lm(awards ~ content_rating, data = dataset)
summary(linear_relationship)

plot(dataset$content_rating, dataset$awards,
     xlab = "Maturity Rating (1 = TV-Y to 7 = TV-MA)",
     ylab = "Awards Won",
     main = "Maturity Rating vs. Awards Won")
abline(h = mean(dataset$awards), col = "black")          # mean of y
abline(v = mean(dataset$content_rating), col = "black")  # mean of x
abline(linear_relationship, col = "red")


##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################
# Plot the residuals
plot(dataset$awards, residuals(linear_relationship))
plot(dataset$content_rating, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")


##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################
table(dataset$awards, dataset$content_rating)



              