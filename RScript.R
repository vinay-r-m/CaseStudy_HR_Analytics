#install.packages("MASS")
#install.packages("e1071")
#install.packages("caret")
#install.packages("cowplot")
#install.packages("caTools")
#install.packages("lubridate")

## LIBRARY
library(car)
library(ggplot2)
library(MASS)
library(e1071)
library(caret)
library(cowplot)
library(caTools)
library(reshape2)
library(dplyr)
library(lubridate)

## user defined function

convert_empid_date_time <- function(df) {
  melted_df <- melt(df, id.vars = c("X"))
  melted_df <-
    rename(melted_df,
           EmployeeID = X,
           Date = variable,
           Time = value)
  melted_df$EmployeeID <- as.factor(melted_df$EmployeeID)
  melted_df$Date <- substring(melted_df$Date, 2)
  melted_df$Date <- as.Date(melted_df$Date, format = "%Y.%m.%d")
  return(melted_df)
}

is_emp_leave <- function(in_time) {
  if (is.na(in_time)) {
    return(1)
  } else{
    return(0)
  }
}


## DATA LOADING
employee_survey_data <-
  read.csv("employee_survey_Data.csv", stringsAsFactors = FALSE)

general_data <-
  read.csv("general_data.csv", stringsAsFactors = FALSE)

in_time <- read.csv("in_time.csv", stringsAsFactors = FALSE)

manager_survey_data <-
  read.csv("manager_survey_data.csv", stringsAsFactors = FALSE)

out_time <- read.csv("out_time.csv", stringsAsFactors = FALSE)


## DATA CLEANING
in_time <-
  in_time[, colSums(is.na(in_time)) < nrow(in_time)] # remove all the columns with only NA values
out_time <-
  out_time[, colSums(is.na(out_time)) < nrow(out_time)] # remove all the columns with only NA values

#Using the in time and out time and deriving variables "Average Working Hours of the Employee"
#Assumption :
#if an employee IN Time is NA considering them as Leave.
ncol(in_time) == ncol(out_time)
nrow(in_time) == nrow(out_time)
in_time <- convert_empid_date_time(in_time)
out_time <- convert_empid_date_time(out_time)

in_time <- rename(in_time, IN_Time = Time)
out_time <- rename(out_time, OUT_Time = Time)

emp_in_out_times_df <-
  merge(
    x = in_time,
    y = out_time,
    by = c("EmployeeID", "Date"),
    all = F
  )
rm(in_time, out_time) # remove the IN and OUT Times data set and clean the memory

sum(is.na(emp_in_out_times_df$IN_Time)) == sum(is.na(emp_in_out_times_df$OUT_Time))
emp_in_out_times_df$Leaves <-
  sapply(emp_in_out_times_df$IN_Time , is_emp_leave)

emp_annual_leaves <- aggregate(Leaves ~ EmployeeID,
                               data = emp_in_out_times_df,
                               sum)

emp_in_out_times_df <-
  emp_in_out_times_df[-which(is.na(emp_in_out_times_df$IN_Time)), ]

emp_in_out_times_df$IN_Time = as.POSIXlt(emp_in_out_times_df$IN_Time, format =
                                           "%Y-%m-%d %H:%M:%S")
emp_in_out_times_df$OUT_Time = as.POSIXlt(emp_in_out_times_df$OUT_Time, format =
                                            "%Y-%m-%d %H:%M:%S")
emp_in_out_times_df$work_duration <-
  as.numeric(
    difftime(
      emp_in_out_times_df$OUT_Time,
      emp_in_out_times_df$IN_Time,
      units = "hours"
    )
  )

emp_in_out_times_df$month <-
  month(emp_in_out_times_df$Date, label = TRUE, abbr = TRUE)
monthly_emp_average_workhour <-
  aggregate(work_duration ~ EmployeeID + month,
            data = emp_in_out_times_df,
            mean)
rm(emp_in_out_times_df)  # removing the temporarily created data

monthly_emp_average_workhour$work_duration <-
  signif(monthly_emp_average_workhour$work_duration, digits = 2)
monthly_emp_average_workhour <-
  reshape(
    monthly_emp_average_workhour,
    idvar = "EmployeeID",
    timevar = "month",
    direction = "wide"
  )
monthly_emp_average_workhour <-
  rename(
    monthly_emp_average_workhour,
    Jan_avg_workhours = work_duration.Jan,
    Feb_avg_workhours = work_duration.Feb,
    Mar_avg_workhours = work_duration.Mar,
    Apr_avg_workhours = work_duration.Apr,
    May_avg_workhours = work_duration.May,
    Jun_avg_workhours = work_duration.Jun,
    Jul_avg_workhours = work_duration.Jul,
    Aug_avg_workhours = work_duration.Aug,
    Sep_avg_workhours = work_duration.Sep,
    Oct_avg_workhours = work_duration.Oct,
    Nov_avg_workhours = work_duration.Nov,
    Dec_avg_workhours = work_duration.Dec
  )

employee_data <-
  merge(
    x = monthly_emp_average_workhour,
    y = emp_annual_leaves,
    by = c("EmployeeID"),
    all = F
  )
rm(monthly_emp_average_workhour, emp_annual_leaves) # clean the in and out times data and release the memory

length(unique(employee_data$EmployeeID)) == length(unique(employee_survey_data$EmployeeID))
setdiff(employee_data$EmployeeID, employee_survey_data$EmployeeID)
employee_data <-
  merge(
    x = employee_data,
    y = employee_survey_data,
    by = c("EmployeeID"),
    all = F
  )
rm(employee_survey_data) # clean the survey data and release the memory

length(unique(employee_data$EmployeeID)) == length(unique(manager_survey_data$EmployeeID))
setdiff(employee_data$EmployeeID, manager_survey_data$EmployeeID)
employee_data <-
  merge(
    x = employee_data,
    y = manager_survey_data,
    by = c("EmployeeID"),
    all = F
  )
rm(manager_survey_data) # clean the manager survey data and release the memory

length(unique(employee_data$EmployeeID)) == length(unique(general_data$EmployeeID))
setdiff(employee_data$EmployeeID, general_data$EmployeeID)
employee_data <-
  merge(
    x = general_data,
    y = employee_data,
    by = c("EmployeeID"),
    all = F
  )
rm(general_data) # clean the general data and release the memory
employee_data_1 <- employee_data

employee_data$Attrition <- as.factor(employee_data$Attrition)
employee_data$BusinessTravel <- as.factor(employee_data$BusinessTravel)
employee_data$Department <- as.factor(employee_data$Department)
employee_data$Education <- as.factor(employee_data$Education)
employee_data$EducationField <- as.factor(employee_data$EducationField)
employee_data$Gender <- as.factor(employee_data$Gender)
employee_data$JobLevel <- as.factor(employee_data$JobLevel)
employee_data$JobRole <- as.factor(employee_data$JobRole)
employee_data$MaritalStatus <- as.factor(employee_data$MaritalStatus)
employee_data$Over18 <- as.factor(employee_data$Over18) # can remove
employee_data$EnvironmentSatisfaction <- as.factor(employee_data$EnvironmentSatisfaction)
employee_data$JobSatisfaction <- as.factor(employee_data$JobSatisfaction)
employee_data$WorkLifeBalance <- as.factor(employee_data$WorkLifeBalance)
employee_data$JobInvolvement <- as.factor(employee_data$JobInvolvement)
employee_data$PerformanceRating <- as.factor(employee_data$PerformanceRating)

levels(employee_data$Department)
dummy <- model.matrix( ~ BusinessTravel - 1, data = employee_data)
dummy <- dummy[, -1]
employee_data <- cbind(employee_data, dummy)

dummy <- model.matrix( ~ Department - 1, data = employee_data)
dummy <- dummy[, -1]
employee_data <- cbind(employee_data, dummy)

dummy <- model.matrix( ~ Education - 1, data = employee_data)
dummy <- dummy[, -1]
employee_data <- cbind(employee_data, dummy)

dummy <- model.matrix( ~ EducationField - 1, data = employee_data)
dummy <- dummy[, -1]
employee_data <- cbind(employee_data, dummy)


dummy <- model.matrix( ~ JobLevel - 1, data = employee_data)
dummy <- dummy[, -1]
employee_data <- cbind(employee_data, dummy)

dummy <- model.matrix( ~ JobRole - 1, data = employee_data)
dummy <- dummy[, -1]
employee_data <- cbind(employee_data, dummy)


dummy <- model.matrix( ~ MaritalStatus - 1, data = employee_data)
dummy <- dummy[, -1]
employee_data <- cbind(employee_data, dummy)

levels(employee_data$EnvironmentSatisfaction)
dummy <- model.matrix( ~ EnvironmentSatisfaction - 1, data = employee_data)
dummy <- dummy[, -1]
employee_data <- cbind(employee_data, dummy)

nrow(employee_data)
View(employee_data)
str(employee_data)
