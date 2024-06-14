### Data inspection and visualization

#load libraries
library('ggplot2')
library(dplyr)
library(tidyr)

#import data
setwd("/Users/user/Documents/Uni/Master/RLDM/Archive")
rawdata <- as.data.frame(read.csv("dataset10.csv"))

#visual inspection
View(rawdata)
summary(rawdata$ID) #12 participants

#change condition and correct to factor
rawdata$condition <- as.factor(rawdata$condition)
class(rawdata$condition)
rawdata$correct <- as.factor(rawdata$correct)
class(rawdata$correct)

#distribution of response times
hist(rawdata$rt, xlim = range(0, 4000), breaks = 500)

### outliers
#calculate quartiles and interquartile range
Q1 <- quantile(rawdata$rt, 0.25)
Q3 <- quantile(rawdata$rt, 0.75)
IQR <- Q3 - Q1

#define lower and upper bounds for outliers (1.5 * IQR)
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

#remove outliers
filtered_data <- rawdata %>%
  filter(rt >= lower_bound & rt <= upper_bound)

###plots
##histogram for correct and incorrect
ggplot(filtered_data, aes(x = rt, fill = correct)) +
  geom_histogram(data = subset(filtered_data, correct == 1), 
                 fill = "blue", alpha = 0.5, bins = 30) +
  geom_histogram(data = subset(filtered_data, correct == 0), 
                 fill = "green", alpha = 0.5, bins = 30) +
  # scale_fill_manual(values = c("blue", "red"),
  #                   labels = c("Correct", "Incorrect")) +
  labs(x = "Reaction Time", y = "Frequency", 
       title = "Reaction Time Distribution for Correct and Incorrect Responses") +
  theme_minimal()

##same for condition 1 vs condition 2
ggplot(filtered_data, aes(x = rt, fill = condition)) +
  geom_histogram(data = subset(filtered_data, condition == 1), 
                 fill = "blue", alpha = 0.5, bins = 30) +
  geom_histogram(data = subset(filtered_data, condition == 2), 
                 fill = "green", alpha = 0.5, bins = 30) +
  labs(x = "Reaction Time", y = "Frequency", 
       title = "Reaction Time Distribution for Condition 1 and Condition 2") +
  theme_minimal()

##without colour
ggplot(filtered_data, aes(x = rt)) +
  geom_density(data = subset(filtered_data, condition == 1), 
               linetype = "solid", size = 1) +
  geom_density(data = subset(filtered_data, condition == 2), 
               linetype = "dashed", size = 1) +
  labs(x = "Reaction Time", y = "Density", 
       title = "Reaction Time Distribution for 'Alcohol Intoxication' and 'Hangover'") +
  theme_minimal() +
  theme(legend.position = "right")

##same but with line for the median included in the plot
#calculate medians for each condition
medians <- filtered_data %>%
  group_by(condition) %>%
  summarise(median_rt = median(rt))
#plotting the density of reaction times for each condition
ggplot(filtered_data, aes(x = rt, linetype = factor(condition))) +
  geom_density(size = 1) + # Add density plot with specified line size
  scale_linetype_manual(values = c("solid", "dashed"), 
                        labels = c("Condition 1", "Condition 2")) + # Specify line types and labels
  labs(x = "Reaction Time", y = "Density", 
       title = "Reaction Time Distribution for Condition 1 and Condition 2",
       linetype = "Condition") + # Add legend title
  theme_minimal() +
  theme(legend.position = "right") + # Ensure legend is positioned to the right
  geom_vline(data = medians, aes(xintercept = median_rt, color = factor(condition)), size = 1, show.legend = TRUE) + # Add vertical lines for medians
  geom_text(data = medians, aes(x = median_rt, y = Inf, label = paste("Median", round(median_rt, 2))), 
            angle = 90, vjust = -0.5, size = 3, show.legend = TRUE) # Add text labels for medians

##profile plot
filtered_data$condition <- factor(filtered_data$condition, levels = c(1, 2), labels = c("Intoxication", "Hangover"))
#first aggregate the data
agg_data <- filtered_data %>%
  group_by(condition, correct) %>%
  summarise(med_rt = median.default(rt, na.rm = TRUE))

#create the profile plot
ggplot(agg_data, aes(x = as.factor(correct), y = med_rt, group = condition)) +
  geom_line(aes(linetype = condition), size = 1) +
  geom_point(aes(shape = condition), size = 3) +
  labs(title = "Median Reaction Time by Accuracy and Condition",
       x = "Correct (0 = Incorrect, 1 = Correct)",
       y = "Median Reaction Time",
       linetype = "Condition",
       shape = "Condition") +
  theme_minimal() +
  scale_linetype_manual(values = c("solid", "dashed")) # Adjust line types

