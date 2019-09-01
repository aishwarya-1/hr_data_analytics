---
title: "Final"
author: "Aishwarya K"
date: "28/08/2019"
output: html_document
---


#Importing the data using data.table
library(data.table)
library(dplyr)

df1 = fread("/home/aishwarya/Documents/Academia/Sem 5/DA/Assignment1/hr-analytics-case-study/general_data.csv")
df2 = fread("/home/aishwarya/Documents/Academia/Sem 5/DA/Assignment1/hr-analytics-case-study/manager_survey_data.csv")
df3 = fread("/home/aishwarya/Documents/Academia/Sem 5/DA/Assignment1/hr-analytics-case-study/employee_survey_data.csv")

df1 = na.omit(df1)
df2 = na.omit(df2)
df3 = na.omit(df3)
df <- merge(df1, df2, by="EmployeeID")
data <- merge(df, df3, by="EmployeeID")


#Question 1a

hist(data$Age, 
     breaks = c(10, 20, 30, 40, 50, 60, 70, 80), 
     right=FALSE, 
     xlab="Age", 
     main="Number of people in different age groups",
     xlim = c(20, 70),
     ylim = c(0, 2000),
     col="cornflowerblue"
     )
cat("Right Skewed Graph, Unimodal")

#Question 1b

l <- tapply(data$MonthlyIncome, cut(data$Age, seq(19, 70, by=10)), mean)
cat("Max Monthly Income Average is in ")
which.max(l)
salary_max <- filter(data, Age <=49, Age>=40)
max_salary <- head(salary_max[unique(order(salary_max$MonthlyIncome, decreasing = TRUE)), ], n=5)
max_salary[c("EmployeeID", "MonthlyIncome", "TotalWorkingYears")]


#Question 1c

library(plyr)
z <- ddply(salary_max, .(JobRole), summarize,  Average=mean(MonthlyIncome))
par(mar=c(7,4,4,2)+0.1,mgp=c(4,1,0))
barplot(z$Average, names.arg = z$JobRole, ylab="Average Monthly Income", main = "Job Role Vs Average Salary", cex.names=0.6, las=2, col ="cornflowerblue")


#Question 2

library(dplyr)
female_data <- filter(data, Gender=="Female")
d1 <- female_data %>% select(MaritalStatus, EnvironmentSatisfaction)
l1 = unique(d1$EnvironmentSatisfaction)
new_data1 <- table(d1$MaritalStatus, d1$EnvironmentSatisfaction)
cols <- c("red", "blue", "green")
barplot(new_data1, main="Environment Satisfaction in Females", col=cols,
  xlab="Environment Satisfaction", ylim=c(0, 300), 
  beside=TRUE)
legend("topleft", 
       title="Marital Status", 
       legend=c(rownames(new_data1)),
       fill=cols
       )

#Question 3

library(dplyr)
d1 <- data %>% select(Attrition, Department)
z = list()
dept_list = unique(d1$Department)
for (variable in dept_list) {
  x <- filter(d1, Department==variable)
  n  = nrow(x)
  y <- filter(x, Attrition=="Yes")
  attrition = nrow(y)
  z[variable] = attrition/n
}
cat("Least Attrition is in department ")
which.min(z)        


#Question 4a

library(dplyr)

Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

d1 <- data %>% select(EducationField, MonthlyIncome)
par(mar=c(5,6,1,0)+0.1,mgp=c(4,1,0))
par(cex.axis=0.6)
boxplot(MonthlyIncome ~ EducationField, data = d1, ylab = "Monthly Income", main = "Box Plots for Monthly Income ", boxwex=0.3, las=2, cex.names=0.2)

education_fields = unique(data$EducationField)
d1 <- data %>% select(EducationField, MonthlyIncome, Gender)
for(variable in education_fields)
{
  x <- filter(d1, EducationField==variable)
  print(variable)
  print(summary(x$MonthlyIncome))
  cat("Modes = ", Modes(x$MonthlyIncome), "\n")
  cat("IQR = ", IQR(x$MonthlyIncome), "\n\n")
}

#Question 4b

d2 <- data %>% select(Gender, MonthlyIncome)
par(mar=c(5,6,0,0)+0.1,mgp=c(4,1,0))
boxplot(MonthlyIncome ~ Gender, data = d2, xlab = "Gender",
   ylab = "Monthly Income", main = "B", cex.names=0.3, las=2)
print("As seen from the box plot, the gender wage gap isn't prominent. The median pay for both male and female is slightly higher in women by 280. The Mean salary for men is greater than the mean salary for women by 724.")
men <- filter(d2, Gender=="Male")
summary(men$MonthlyIncome)
women <- filter(d2, Gender=="Female")
summary(women$MonthlyIncome)

#Question 4c

x <- filter(d1, EducationField=="Medical")
print(quantile(x$MonthlyIncome, c(.95)))

#Question 5

library(moments)
library(PerformanceAnalytics)
hist(data$MonthlyIncome)
print("Skewness is a measure of the symmetry in a distribution.  A symmetrical dataset will have a skewness equal to 0.  So, a normal distribution will have a skewness of 0. The histogram given below is positively skewed.")
skewness(data$MonthlyIncome)




print("Kurtosis is a measure of the combined sizes of the two tails.  It measures the amount of probability in the tails. The kurtosis for a standard normal distribution is three. positive kurtosis indicates a \"heavy-tailed\" distribution and negative kurtosis indicates a \"light tailed\" distribution.")
kurtosis(data$MonthlyIncome)


