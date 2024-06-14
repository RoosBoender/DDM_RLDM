### Model fitting

library('ggplot2')
library(dplyr)
library(tidyr)

#import the data (+filter out outliers)
rm(list = ls())
setwd("/Users/user/Documents/Uni/Master/RLDM/Archive")
rawdata <- as.data.frame(read.csv("dataset10.csv"))
Q1 <- quantile(rawdata$rt, 0.25)
Q3 <- quantile(rawdata$rt, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
filtered_data <- rawdata %>%
  filter(rt >= lower_bound & rt <= upper_bound)

setwd("/Users/user/Documents/Uni/Master/RLDM/framework")
source('helper_functions.r')

#create datafram to store parameters
df <- data.frame(ID=numeric(),
                 condition=numeric(),
                 s=numeric(),
                 A=numeric(),
                 ter=numeric(),
                 b=numeric(),
                 v1=numeric())

#initialize
n_subjects <- unique(filtered_data$ID)
conds <- unique(filtered_data$condition)

#start model fitting for each participant x condition using fit_data() function
for (subject in n_subjects) {
  for (condition in conds) {
    print(paste("participant", subject, "condition", condition))
    
    results <- if(condition == 1){
      print(fit_data(subset(filtered_data, ID==subject & condition==1)))
    } else {
      print(fit_data(subset(filtered_data, ID==subject & condition==2)))
    }
    df <- rbind(df, c(subject, condition, results))
  }
}

names(df) <-c("ID", "condition", "s", "A", "ter", "b", "v1")

##check which parameter(s) differ between conditions
# "s"		SD of drift rates
sd_test <- t.test(df$s ~ df$condition, paired = T)
print(sd_test)

means_s <- df %>%
  group_by(condition) %>%
  summarise(mean_sd = mean(s)) 
means_s

sd_s <- df %>%
  group_by(condition) %>%
  summarise(sd_sd = sd(s)) 
sd_s

# "A"		upper limit of starting point
A_test <- t.test(df$A ~ df$condition, paired = T)
print(A_test)

means_a <- df %>%
  group_by(condition) %>%
  summarise(mean_A = mean(A))
means_a

sd_a <- df %>%
  group_by(condition) %>%
  summarise(sd_A = sd(A))
sd_a

# "ter"	non-decision time
ter_test <- t.test(df$ter ~ df$condition, paired = T)
print(ter_test)

means_ter <- df %>%
  group_by(condition) %>%
  summarise(mean_ter = mean(ter)) 
print(means_ter)

sd_ter <- df %>%
  group_by(condition) %>%
  summarise(sd_ter = sd(ter)) 
sd_ter

# "b"		threshold
b_test <- t.test(df$b ~ df$condition, paired = T)
print(b_test)

means_b <- df %>%
  group_by(condition) %>%
  summarise(mean_b = mean(b))
means_b

sd_b <- df %>%
  group_by(condition) %>%
  summarise(sd_b = sd(b))
sd_b

# "v1"	drift rate
v1_test <- t.test(df$v1 ~ df$condition, paired = T)
print(v1_test)

means_v1 <- df %>%
  group_by(condition) %>%
  summarise(mean_v1 = mean(v1))
means_v1

sd_v1 <- df %>%
  group_by(condition) %>%
  summarise(sd_v1 = sd(v1))
sd_v1