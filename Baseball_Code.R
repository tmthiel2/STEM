## Step 0: Save this R Script in your state_farm_project folder on your Desktop. ##

## Step 1: Load package ##
library("ggplot2")

## Step 2: Reading your dataset into R ##
library("readr")
urlfile <- "https://github.com/tmthiel2/STEM/raw/master/Baseball.csv"
All_Data<-as.data.frame(read_csv(url(urlfile)))

## Metadata for your data ##
# Variable : Description : Type of Variable : Predictor or Target 
# Pitcher_ID : ID # of the Pitcher : ID : Neither - don't use id in model
# Throwing_Hand : Hand the pitcher throws with : Categorical - Binary : Predictor
# Throw_Cutter : Does the pitcher throw a cutter? : Categorical - Binary : Predictor
# Throw_Sinker : Does the pitcher throw a sinker? : Categorical - Binary : Predictor
# Count_Pitches : Number of pitches that the pitcher can throw : Continuous : Predictor
# Max_Velocity : Average pitch speed from the pitcher's fastest pitch type : Continuous : Predictor
# Max_Break : Amount of average total break of the pitcher's pitch type with the most movement : Continuous : Predictor
# Diff_Velocity : Difference in pitch speed between the pitcher's slowest and fastest pitch type : Continuous : Predictor
# Diff_Spin : Difference in spin between the pitcher's pitch type with the least and most spin : Continuous : Predictor
# Sim : Random variable : Continuous : Predictor
# Strikeout_Rate : Average strikeout rate per 9 innings: Continuous : Target

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
Deciles_Count_Pitches <- quantile(All_Data$Count_Pitches, probs = seq(0, 1,length=11))
Deciles_Count_Pitches

Deciles_Max_Velocity <- quantile(All_Data$Max_Velocity, probs = seq(0, 1,length=11))
Deciles_Max_Velocity

Deciles_Max_Break <- quantile(All_Data$Max_Break, probs = seq(0, 1,length=11))
Deciles_Max_Break

Deciles_Diff_Velocity <- quantile(All_Data$Diff_Velocity, probs = seq(0, 1,length=11))
Deciles_Diff_Velocity

Deciles_Diff_Spin <- quantile(All_Data$Diff_Spin, probs = seq(0, 1,length=11))
Deciles_Diff_Spin

Deciles_Sim <- quantile(All_Data$Sim, probs = seq(0, 1,length=11))
Deciles_Sim

Deciles_Strikeout_Rate <- quantile(All_Data$Strikeout_Rate, probs = seq(0, 1,length=11))
Deciles_Strikeout_Rate

## Step 10: Calculating the frequencies on categorical variables ##
Throwing_Hand_Freq <- table(All_Data$Throwing_Hand)
Throwing_Hand_Freq

Throw_Cutter_Freq <- table(All_Data$Throw_Cutter)
Throw_Cutter_Freq

Throw_Sinker_Freq <- table(All_Data$Throw_Sinker)
Throw_Sinker_Freq

## Step 12: Plotting the numeric variables ##
ggplot(data = All_Data) + geom_point(aes(x = Count_Pitches, y = Strikeout_Rate), colour = 'blue')

ggplot(data = All_Data) + geom_point(aes(x = Max_Velocity, y = Strikeout_Rate), colour = 'green')

ggplot(data = All_Data) + geom_point(aes(x = Max_Break, y = Strikeout_Rate), colour = 'red')

ggplot(data = All_Data) + geom_point(aes(x = Diff_Velocity, y = Strikeout_Rate), colour = 'purple')

ggplot(data = All_Data) + geom_point(aes(x = Diff_Spin, y = Strikeout_Rate), colour = 'brown')

ggplot(data = All_Data) + geom_point(aes(x = Sim, y = Strikeout_Rate), colour = 'yellow')

## Step 14: Plotting the categorical variables ##
Throwing_Hand <- ggplot(All_Data, aes(Throwing_Hand, Strikeout_Rate))
Throwing_Hand + geom_boxplot(outlier.colour = "red", outlier.shape = 1)

Throw_Cutter <- ggplot(All_Data, aes(Throw_Cutter, Strikeout_Rate))
Throw_Cutter + geom_boxplot(outlier.colour = "red", outlier.shape = 1)

Throw_Sinker <- ggplot(All_Data, aes(Throw_Sinker, Strikeout_Rate))
Throw_Sinker + geom_boxplot(outlier.colour = "red", outlier.shape = 1)

## Step 15: Calculate correlations ##
Vars_Num <- c("Count_Pitches", "Max_Velocity", "Max_Break", "Diff_Velocity", "Diff_Spin", "Sim", "Strikeout_Rate")
Numeric_Data <- Training_Data[Vars_Num]
Correlation <- round(cor(Numeric_Data, method = c("pearson")),2)
Correlation

## Step 17: Fitting a model using forward selection ##
null = lm(Strikeout_Rate~1, data=Training_Data)
full = lm(Strikeout_Rate~factor(Throwing_Hand) + factor(Throw_Cutter) + factor(Throw_Sinker) + Count_Pitches + Max_Velocity + Max_Break + Diff_Velocity + Diff_Spin + Sim,data=Training_Data)

Fit_Forward = step(null, scope=list(lower=null, upper=full), direction="forward")

## Step 19: Fitting the final regression model ##
options(digits=3)
Final_Model <- lm(Strikeout_Rate ~ Count_Pitches + Max_Velocity + Diff_Velocity + factor(Throw_Cutter) + factor(Throw_Sinker), data=Training_Data)
summary(Final_Model)

## Step 20: Validating the Model ##
val_predict <- predict(Final_Model,Validation_Data)
Validation_Predictions <- cbind(Validation_Data,val_predict)

## Step 21: Calculating MAPE from your predictive model ##
count_obs_val <- nrow(Validation_Predictions)
Inside_val <- abs((Validation_Predictions$Strikeout_Rate - Validation_Predictions$val_predict)/Validation_Predictions$Strikeout_Rate)

MAPE_val <- ((1/count_obs_val)*sum(Inside_val))*100
MAPE_val

## Step 23: Calculating MAPE for mean predictions ##
Strikeout_Rate_mean <- mean(Training_Data$Strikeout_Rate)
Validation_Data_Mean <- cbind(Validation_Data,Strikeout_Rate_mean)

count_obs_val_mean <- nrow(Validation_Data_Mean)
Inside_val_mean <- abs((Validation_Data_Mean$Strikeout_Rate - Validation_Data_Mean$Strikeout_Rate_mean)/Validation_Data_Mean$Strikeout_Rate)
MAPE_val_mean <- ((1/count_obs_val_mean)*sum(Inside_val_mean))*100
MAPE_val_mean

## Step 25: Re-run out model to see output ##
options(digits=3)
Final_Model <- lm(Strikeout_Rate ~ Count_Pitches + Max_Velocity + Diff_Velocity + factor(Throw_Cutter) + factor(Throw_Sinker), data=Training_Data)
summary(Final_Model)

## Using the formula to predict another observation ##
## For example, let's say a team is looking to acquire a pitcher from another team and wants to predict his strikeout rate based on his pitching characteristics. These are the characteristics of the pitcher:
## The pitcher throws left handed.
## The pitcher does not throw a cutter.
## The pitcher does not throw a sinker.
## The pitcher throws 4 pitches.
## The pitcher has a maximum velocity of 90.23.
## The pitcher has a maximum break of 8.98.
## The pitcher has a difference in velocity of 10.58.
## The pitcher has a difference in spin of 1180.45.
## Using these specifications, what will the strikeout rate be?