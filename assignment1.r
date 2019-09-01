---
title: "R Notebook"
output: html_notebook
---

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code. 

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*.
When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.

```{r}
#Importing the data using data.table
library(data.table)
```

```{r}
df1 = fread("/home/aishwarya/Documents/Academia/Sem 5/DA/Assignment1/hr-analytics-case-study/general_data.csv")
summary(df1)
```

```{r}
df2 = fread("hr-analytics-case-study/manager_survey_data.csv")
summary(df2)
```

```{r}
df3 = fread("hr-analytics-case-study/employee_survey_data.csv")
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
barplot(names.arg=)
```