##Install the relevant packages
install.packages("lme4")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("report")
install.packages("lmerTest")
install.packages("car")
install.packages("MuMIn")
install.packages("sjPlot")


##Call the relevant packages
library(lme4)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(report)
library(lmerTest)
library(car)
library(MuMIn)
library(sjPlot)


##Load data
# set the path of the folder containing the .csv files
path <- "~/Downloads/Final VR Data/"
# list all .csv files in the folder
csv_files <- list.files(path, pattern = "*.csv")
csv_files
# create an empty list to store the data frames
df_list <- list()
# loop through each .csv file and import into a data frame
for (file in csv_files) {
  file_path <- paste0(path, file)
  df <- read.csv(file_path, header = TRUE)
  df_list[[file]] <- df
}
# combine all data frames into one using rbind
VRdata <- do.call(rbind, df_list)


##Manipulate the data
# filter out any #N/A values in breath and leg variables, which should leave us with only the hill data (i.e., the experimental data)
filteredData <- VRdata[VRdata$breath != "#N/A",]
# check the structure of the df
str(filteredData)
# convert breath, leg and hours_exercise to numeric variables
filteredData$breath <- as.numeric(filteredData$breath)
filteredData$leg <- as.numeric(filteredData$leg)
filteredData$hours_exercise <- as.numeric(filteredData$hours_exercise)
head(filteredData)
# obtain mean values for each participant's power, observed_slope, time_elapsed, breath, leg, hours_exercise, STAI_anxiety for each session
meansData <- filteredData %>%
  group_by(participant, session, stage) %>%
  summarize(mean_breath = mean(breath),
            mean_leg = mean(leg),
            mean_observed_slope = mean(observed_slope),
            mean_power = mean(power),
            mean_time_elapsed = mean(time_elapsed),
            mean_hours_exercise = mean(hours_exercise),
            mean_STAI_anxiety = mean(STAI_anxiety)
  )
meansData <- data.frame(meansData)
head(meansData)


##Did participants apply more power in the high vs. low slope resistance conditions?
# this is fundamental to the study design: higher resistance should equal greater power across all slope resistances
# first, we need to change the variable 'slope_resistance' to factor
filteredData$slope_resistance <- as.factor(filteredData$slope_resistance)
str(filteredData)
# I will then create a box plot showing power at each level of slope resistance.
# basic box plot
power_observed_slope <- ggplot(filteredData, aes(x=slope_resistance, y=power))
power_observed_slope
# it's clear that this data is not normally distributed
# let's perform a Kruskal Wallis test to determine if power is different across slope resistances
kruskal.test(power ~ slope_resistance, data = filteredData)
#p = 2.2e-16, so we can confidentially conclude that power changes across the levels of slope resistance


##Now centre and scale the continuous data, which will facilitate comparisons, model convergence, and interpretation of the results
# select continuous variables to center and scale
vars_to_scale <- c("mean_power", "mean_observed_slope", "mean_time_elapsed", "mean_hours_exercise", "mean_STAI_anxiety")
# center and scale selected variables
meansData[, vars_to_scale] <- scale(meansData[, vars_to_scale])
# print scaled data frame
head(meansData)


##Create 2 distinct linear mixed effects models to assess contribution of predictor variables to breathlessness and leg fatigue
# model 1 (breathlessness)
breathModel <- lmer(mean_breath ~ mean_power + mean_observed_slope + mean_time_elapsed + mean_hours_exercise + mean_STAI_anxiety + (1 | participant), data = meansData, na.action = na.omit, REML = F)
summary(breathModel)
report(breathModel)
vif(breathModel)
# low variance inflation factor (~1) shows that there is no perfect colinearity (assumption)
qqPlot(residuals(breathModel))
# Q-Q plot shows that residuals are normally distributed (assumption)

# model 2 (leg fatigue)
legModel <- lmer(mean_leg ~ mean_power + mean_observed_slope + mean_time_elapsed + mean_hours_exercise + mean_STAI_anxiety + (1 | participant), data = meansData, na.action = na.omit, REML = F)
summary(legModel)
report(legModel)
vif(legModel)
# low variance inflation factor (~1) shows that there is no perfect colinearity (assumption)
qqPlot(residuals(legModel))
# Q-Q plot shows that residuals are normally distributed (assumption)