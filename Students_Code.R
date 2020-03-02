## Step 0: Save this R Script in your state_farm_project folder on your Desktop. ##

## Step 1: Load package ##
library("ggplot2")

## Step 2: Reading your dataset into R ##
library("readr")
urlfile <- "https://github.com/tmthiel2/STEM/raw/master/Students.csv"
All_Data<-as.data.frame(read_csv(url(urlfile)))

## Metadata for your data ##
# Variable : Description : Type of Variable : Predictor or Target 
# Student_ID : ID # of the student : ID : Neither - don't use id in model
# Honor_Roll : Has the student been on the honor roll before? : Categorical - Binary : Predictor
# Age : Age of the student : Continuous : Predictor
# Travel_Time : Time (in minutes) it takes the student to travel to school : Continuous : Predictor
# Study_Time : Time (in minutes) the student studies per week : Continuous : Predictor
# Failures : Number of past class failures : Continuous : Predictor
# School_Support : Does the student have school support? : Categorical - Binary : Predictor
# Family_Support : Does the student have family support? : Categorical - Binary : Predictor
# Absences : Number of absences the student had in the past year : Continuous : Predictor
# Final_Grade : Final grade of the student : Continuous : Target
# Sim : Random variable : Continuous : Predictor

## Step 3: Calculating the number of rows in your dataset ##
num_obs <- nrow(All_Data)
num_obs

## Step 5: Calculating the number of columns in your dataset ##
num_col <- ncol(All_Data)
num_col

## Step 7: Splitting your dataset into 60% training and 40% validation ##
p60 <- quantile(All_Data$Sim,0.6)
Training_Data <- All_Data[which(All_Data$Sim <= p60),]
Validation_Data <- All_Data[which(All_Data$Sim > p60),]

## Step 8: Run out the deciles to see the distributions of each numeric variable ##
Deciles_Age <- quantile(All_Data$Age, probs = seq(0, 1,length=11))
Deciles_Age

Deciles_Travel_Time <- quantile(All_Data$Travel_Time, probs = seq(0, 1,length=11))
Deciles_Travel_Time

Deciles_Study_Time <- quantile(All_Data$Study_Time, probs = seq(0, 1,length=11))
Deciles_Study_Time

Deciles_Failures <- quantile(All_Data$Failures, probs = seq(0, 1,length=11))
Deciles_Failures

Deciles_Absences <- quantile(All_Data$Absences, probs = seq(0, 1,length=11))
Deciles_Absences

Deciles_Final_Grade <- quantile(All_Data$Final_Grade , probs = seq(0, 1,length=11))
Deciles_Final_Grade 

Deciles_Sim <- quantile(All_Data$Sim, probs = seq(0, 1,length=11))
Deciles_Sim

## Step 10: Calculating the frequencies on categorical variables ##
HonorRoll_Freq <- table(All_Data$Honor_Roll)
HonorRoll_Freq

School_Support_Freq <- table(All_Data$School_Support)
School_Support_Freq

Family_Support_Freq <- table(All_Data$Family_Support)
Family_Support_Freq

## Step 12: Plotting the numeric variables ##
ggplot(data = All_Data) + geom_point(aes(x = Age, y = Final_Grade), colour = 'blue')

ggplot(data = All_Data) + geom_point(aes(x = Travel_Time, y = Final_Grade), colour = 'purple')

ggplot(data = All_Data) + geom_point(aes(x = Study_Time, y = Final_Grade), colour = 'brown')

ggplot(data = All_Data) + geom_point(aes(x = Failures, y = Final_Grade), colour = 'orange')

ggplot(data = All_Data) + geom_point(aes(x = Absences, y = Final_Grade), colour = 'red')

ggplot(data = All_Data) + geom_point(aes(x = Sim, y = Final_Grade), colour = 'black')

## Step 14: Plotting the categorical variables ##
Honor_Roll <- ggplot(All_Data, aes(Honor_Roll, Final_Grade))
Honor_Roll + geom_boxplot(outlier.colour = "red", outlier.shape = 1)

School_Support <- ggplot(All_Data, aes(School_Support, Final_Grade))
School_Support + geom_boxplot(outlier.colour = "red", outlier.shape = 1)

Family_Support <- ggplot(All_Data, aes(Family_Support, Final_Grade))
Family_Support + geom_boxplot(outlier.colour = "red", outlier.shape = 1)

## Step 15: Calculate correlations ##
Vars_Num <- c("Age", "Travel_Time", "Study_Time", "Failures", "Absences", "Final_Grade", "Sim")
Numeric_Data <- Training_Data[Vars_Num]
Correlation <- round(cor(Numeric_Data, method = c("pearson")),2)
Correlation

## Step 17: Fitting a model using forward selection ##
null = lm(Final_Grade~1, data=Training_Data)
full = lm(Final_Grade~factor(Honor_Roll) + Age + Travel_Time + Study_Time + Failures + factor(School_Support) + factor(Family_Support) + Absences + Sim,data=Training_Data)

Fit_Forward = step(null, scope=list(lower=null, upper=full), direction="forward")

## Step 19: Fitting the final regression model ##
options(digits=3)
Final_Model <- lm(Final_Grade ~ Study_Time + factor(School_Support) + factor(Honor_Roll) + Failures + factor(Family_Support) + Absences, data=Training_Data)
summary(Final_Model)

## Step 20: Validating the Model ##
val_predict <- predict(Final_Model,Validation_Data)
Validation_Predictions <- cbind(Validation_Data,val_predict)

## Step 21: Calculating MAPE from your predictive model ##
count_obs_val <- nrow(Validation_Predictions)
Inside_val <- abs((Validation_Predictions$Final_Grade - Validation_Predictions$val_predict)/Validation_Predictions$Final_Grade)

MAPE_val <- ((1/count_obs_val)*sum(Inside_val))*100
MAPE_val

## Step 23: Calculating MAPE for mean predictions ##
student_grade_mean <- mean(Training_Data$Final_Grade)
Validation_Data_Mean <- cbind(Validation_Data,student_grade_mean)

count_obs_val_mean <- nrow(Validation_Data_Mean)
Inside_val_mean <- abs((Validation_Data_Mean$Final_Grade - Validation_Data_Mean$student_grade_mean)/Validation_Data_Mean$Final_Grade)
MAPE_val_mean <- ((1/count_obs_val_mean)*sum(Inside_val_mean))*100
MAPE_val_mean

## Step 25: Re-run out model to see output ##
options(digits=3)
Final_Model <- lm(Final_Grade ~ Study_Time + factor(School_Support) + factor(Honor_Roll) + Failures + factor(Family_Support) + Absences, data=Training_Data)
summary(Final_Model)

## Using the formula to predict another observation ##
## For example, let's say a teacher wants to predict how a student will do in their class based on the student's characteristics. These are the characteristics of the student:
## The student has been on the honor roll before.
## The student's age is 18.
## The student spends 10 minutes traveling between school and home.
## The student spends 450 minutes a week studying.
## The student has failed 1 class in the past.
## The student does not have school support.
## The student has family support.
## The student has been absent 4 days.
## Using these specifications, what will the final grade of this student be?