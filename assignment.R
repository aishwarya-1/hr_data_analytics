---
title: "R Notebook"
output: html_notebook
---

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.

```{r}
#Importing the data using data.table
library(data.table)

df1 = fread("./hr-analytics-case-study/general_data.csv")
summary(df1)
```

```{r}
df2 = fread("./hr-analytics-case-study/manager_survey_data.csv")
summary(df2)
```

```{r}
df3 = fread("./hr-analytics-case-study/employee_survey_data.csv")
summary(df3)
```
```{r}
df1 = na.omit(df1)
df2 = na.omit(df2)
df3 = na.omit(df3)
```
```{r}
df <- merge(df1, df2, by="EmployeeID")
df
```

```{r}
data <- merge(df, df3, by="EmployeeID")
data
```

```{r}
hist(data$Age, 
     breaks = c(10, 20, 30, 40, 50, 60, 70, 80), 
     right=FALSE, 
     xlab="Age", 
     main="Histogram of Age",
     xlim = c(20, 70)
     )
paste("Right Skewed Graph, Unimodal")
```

```{r}
l <- tapply(data$MonthlyIncome, cut(data$Age, seq(19, 70, by=10)), mean)
which.max(l)
```

```{r}
library(dplyr)    
salary_max <- filter(data, Age <=49, Age>=40)
salary_max
```

```{r}
max_salary <- head(salary_max[order(salary_max$MonthlyIncome, decreasing = TRUE), ], n=5)
```

```{r}
max_salary[c("EmployeeID", "MonthlyIncome", "TotalWorkingYears")]  
```

```{r}
x <- unique(salary_max$JobRole)
?barplot
barplot(,names.arg=x)
```
```{r}
#salary_max % > %
	dplyr::group_by(salary_max$JobRole)
	summarise_at(vars(-salary_max$JobRole), funs(mean(., na.rm=TRUE)))

	
dplyr::count(salary_max, JobRole)

```
```{r}
library(plyr)
z <- ddply(salary_max, .(JobRole), summarize,  Average=mean(MonthlyIncome))
barplot(z$Average, names.arg = z$JobRole, xlab="Job Role", ylab="Average Monthly Income", main = "Job Role Vs Average Salary", cex.names=0.5, las=2)
```
```{r}
library(dplyr)
library(ggplot2)
female_data <- filter(data, Gender=="Female")
female_data <- select(female_data$EnvironmentSatisfaction, female_data$MaritalStatus)
female_data <- group_by(female_data, EnvironmentSatisfaction)
female_data
ggplot(female_data, aes(factor(EnvironmentSatisfaction), count(female_data), fill = MaritalStatus)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")
```
```{r}
library(plyr)
z <- ddply(data, .(Department), summarize,  Average=count(MonthlyIncome))
```

```{r}
library(dplyr)
d1 <- data %>% select(Attrition, Department)
d1
dept_list = unique(d1$Department)
for (variable in dept_list) {
  x <- filter(d1, Department==variable)
  n  = nrow(x)
  y <- filter(x, Attrition=="No")
  attrition = nrow(y)
  print(variable)
  print(attrition/n)
}
```
```{r}
library(dplyr)
female_data <- filter(data, Gender=="Female")
d1 <- female_data %>% select(MaritalStatus, EnvironmentSatisfaction)
l1 = unique(d1$EnvironmentSatisfaction)
```

```{r}
marital = unique(d1$MaritalStatus)
bars = list()

for (variable in l1) {
  x <- filter(d1, EnvironmentSatisfaction==variable)
  z = list()
  for(v in marital)
  {
      y <- filter(x, MaritalStatus==marital)
      z[marital] <- nrow(y)
      print(z[marital])
  }
  bars[variable] <- z
}
bars[1]
```
```{r}
new_data1 <- table(d1$MaritalStatus, d1$EnvironmentSatisfaction)
barplot(new_data1, main="Environment Satisfaction in Females",
  xlab="Environment Satisfaction", col=c("darkblue","red", "green"),
  legend = rownames(new_data1), beside=TRUE)
```
```{r}
library(dplyr)
d1 <- data %>% select(EducationField, MonthlyIncome, Gender)
boxplot(MonthlyIncome ~ EducationField, data = d1, xlab = "Education Fields",
   ylab = "Monthly Income", main = "B", cex.names=0.3, las=2)

boxplot(MonthlyIncome ~ EducationField, data = d1, xlab = "Education Fields",
   ylab = "Monthly Income", main = "B", cex.names=0.3, las=2)
```
```{r}
education_fields = unique(data$EducationField)
d1 <- data %>% select(EducationField, MonthlyIncome, Gender)
for(variable in education_fields)
{
  x <- filter(d1, EducationField==variable)
  boxplot(MonthlyIncome ~ EducationField, data = x, xlab = variable,
   ylab = "Monthly Income", main = "B", cex.names=0.3, las=2)
  print(variable)
  print(summary(x$MonthlyIncome))
  print(quantile(x$MonthlyIncome, c(.95)))
  #print("IQR")
  print(IQR(x$MonthlyIncome))
}
```
```{r}
library(dplyr)
d1 <- data %>% select(EducationField, MonthlyIncome, Gender)
boxplot(MonthlyIncome ~ EducationField, data = d1, xlab = "Education Fields",
   ylab = "Monthly Income", main = "B", cex.names=0.3, las=2)
```

```{r}

```

