## Step 0: Save this R Script in your state_farm_project folder on your Desktop. ##

## Step 1: Load package ##
library("ggplot2")

## Step 2: Reading your dataset into R ##
library("readr")
urlfile <- "https://github.com/tmthiel2/STEM/raw/master/College.csv"
All_Data<-as.data.frame(read_csv(url(urlfile)))

## Metadata for your data ##
# Variable : Description : Type of Variable : Predictor or Target 
# School_ID : ID # of the college or university : ID : Neither - don't use id in model
# Located_In_USA : Is the university located in the USA? : Categorical - Binary : Predictor
# Large_School : Is the university a large one? (>25,000 students) : Categorical - Binary : Predictor
# Large_Student_Staff_Ratio : Does the university have a large student to staff ratio (>15 students per staff) : Categorical - Binary : Predictor
# Teaching_Rating : Rating from a 0-100 scale of the quality of teaching at the university : Continuous : Predictor
# Research_Rating : Rating from a 0-100 scale of the quality of research at the university : Continuous : Predictor
# Citations_Rating : Rating from a 0-100 scale how often the research from the university is cited by other papers : Continuous : Predictor
# Total_Score : The final score used to determine the university ranking : Continuous : Target
# Pcnt_Intntl_Students : Percent of international students at the university : Continuous : Predictor
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
Deciles_Teaching_Rating <- quantile(All_Data$Teaching_Rating, probs = seq(0, 1,length=11))
Deciles_Teaching_Rating

Deciles_Research_Rating <- quantile(All_Data$Research_Rating, probs = seq(0, 1,length=11))
Deciles_Research_Rating

Deciles_Citations_Rating <- quantile(All_Data$Citations_Rating, probs = seq(0, 1,length=11))
Deciles_Citations_Rating

Deciles_Total_Score <- quantile(All_Data$Total_Score, probs = seq(0, 1,length=11))
Deciles_Total_Score

Deciles_Pcnt_Intntl_Students <- quantile(All_Data$Pcnt_Intntl_Students, probs = seq(0, 1,length=11))
Deciles_Pcnt_Intntl_Students

Deciles_Sim <- quantile(All_Data$Sim, probs = seq(0, 1,length=11))
Deciles_Sim

## Step 10: Calculating the frequencies on categorical variables ##
Located_In_USA_Freq <- table(All_Data$Located_In_USA)
Located_In_USA_Freq

Large_School_Freq <- table(All_Data$Large_School)
Large_School_Freq

Large_Student_Staff_Ratio_Freq <- table(All_Data$Large_Student_Staff_Ratio)
Large_Student_Staff_Ratio_Freq

## Step 12: Plotting the numeric variables ##
ggplot(data = All_Data) + geom_point(aes(x = Teaching_Rating, y = Total_Score), colour = 'blue')

ggplot(data = All_Data) + geom_point(aes(x = Research_Rating, y = Total_Score), colour = 'purple')

ggplot(data = All_Data) + geom_point(aes(x = Citations_Rating, y = Total_Score), colour = 'brown')

ggplot(data = All_Data) + geom_point(aes(x = Pcnt_Intntl_Students, y = Total_Score), colour = 'orange')

ggplot(data = All_Data) + geom_point(aes(x = Sim, y = Total_Score), colour = 'black')

## Step 14: Plotting the categorical variables ##
Located_In_USA <- ggplot(All_Data, aes(Located_In_USA, Total_Score))
Located_In_USA + geom_boxplot(outlier.colour = "red", outlier.shape = 1)

Large_School <- ggplot(All_Data, aes(Large_School, Total_Score))
Large_School + geom_boxplot(outlier.colour = "red", outlier.shape = 1)

Large_Student_Staff_Ratio <- ggplot(All_Data, aes(Large_Student_Staff_Ratio, Total_Score))
Large_Student_Staff_Ratio + geom_boxplot(outlier.colour = "red", outlier.shape = 1)

## Step 15: Calculate correlations ##
Vars_Num <- c("Teaching_Rating", "Research_Rating", "Citations_Rating", "Total_Score", "Pcnt_Intntl_Students", "Sim")
Numeric_Data <- Training_Data[Vars_Num]
Correlation <- round(cor(Numeric_Data, method = c("pearson")),2)
Correlation

## Step 17: Fitting a model using forward selection ##
null = lm(Total_Score~1, data=Training_Data)
full = lm(Total_Score~factor(Located_In_USA) + factor(Large_School) + factor(Large_Student_Staff_Ratio) + Teaching_Rating + Research_Rating + Citations_Rating + Pcnt_Intntl_Students + Sim,data=Training_Data)

Fit_Forward = step(null, scope=list(lower=null, upper=full), direction="forward")

## Step 19: Fitting the final regression model ##
options(digits=3)
Final_Model <- lm(Total_Score ~ Teaching_Rating + Citations_Rating + Research_Rating + Pcnt_Intntl_Students + factor(Located_In_USA), data=Training_Data)
summary(Final_Model)

## Step 20: Validating the Model ##
val_predict <- predict(Final_Model,Validation_Data)
Validation_Predictions <- cbind(Validation_Data,val_predict)

## Step 21: Calculating MAPE from your predictive model ##
count_obs_val <- nrow(Validation_Predictions)
Inside_val <- abs((Validation_Predictions$Total_Score - Validation_Predictions$val_predict)/Validation_Predictions$Total_Score)

MAPE_val <- ((1/count_obs_val)*sum(Inside_val))*100
MAPE_val

## Step 23: Calculating MAPE for mean predictions ##
total_score_mean <- mean(Training_Data$Total_Score)
Validation_Data_Mean <- cbind(Validation_Data,total_score_mean)

count_obs_val_mean <- nrow(Validation_Data_Mean)
Inside_val_mean <- abs((Validation_Data_Mean$Total_Score - Validation_Data_Mean$total_score_mean)/Validation_Data_Mean$Total_Score)
MAPE_val_mean <- ((1/count_obs_val_mean)*sum(Inside_val_mean))*100
MAPE_val_mean

## Step 25: Re-run out model to see output ##
options(digits=3)
Final_Model <- lm(Total_Score ~ Teaching_Rating + Citations_Rating + Research_Rating + Pcnt_Intntl_Students + factor(Located_In_USA), data=Training_Data)
summary(Final_Model)

## Using the formula to predict another observation ##
## For example, let's say parents are looking for a university for their child to attend and want to predict the ranking score of a university they have on their list of possible schools. These are the characteristics of the university:
## The university is located in the USA.
## The university is not a large school.
## The university does not have a large student to staff ratio.
## The teaching rating at the university is 85.4.
## The research rating at the university is 71.6.
## The citations rating at the university is 86.2.
## The percent of international students at the university is 18%.
## Using these specifications, what will the total ranking score of the university be?