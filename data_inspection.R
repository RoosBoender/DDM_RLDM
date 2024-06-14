### Inspect data 

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

#aggregate data rt median
median <- filtered_data %>%
  group_by(ID, condition) %>% #since it is paired-t
  summarise(median_rt = median(rt))

#paired t-test over RT data by condition
ttest <- t.test(median$median_rt ~ median$condition, paired = T) #compare rt between conditions
print(ttest)

#calculate accuracy for each condition
accuracy_data <- filtered_data %>%
  group_by(ID, condition) %>%
  summarize(accuracy = mean(correct)) %>%
  ungroup()

#effect condition on accuracy
contingency_table <- table(filtered_data$correct, filtered_data$condition)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)