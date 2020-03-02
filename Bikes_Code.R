## Step 0: Save this R Script in your state_farm_project folder on your Desktop. ##

## Step 1: Load package ##
library("ggplot2")

## Step 2: Reading your dataset into R ##
library("readr")
urlfile <- "https://github.com/tmthiel2/STEM/raw/master/Bikes.csv"
All_Data<-as.data.frame(read_csv(url(urlfile)))

## Metadata for your data ##
# Variable : Description : Type of Variable : Predictor or Target 
# Day_ID : ID # of the day bikes were rented : ID : Neither - don't use id in model
# Total_Rental_Bikes : Total amount of bikes that were rented on that day : Continuous : Target
# Holiday : Was the bike rented during a holiday or not? : Categorical - Binary : Predictor
# Working_Day : Was the bike rented during a working day or not? : Categorical - Binary : Predictor
# Rainy : Was the bike rented during a rainy day or not? : Categorical - Binary : Predictor
# Temperature : Temperature of the day the bike was rented (in Fahrenheit) : Continuous : Predictor
# Humidity : Humidity on the day the bike was rented : Continuous : Predictor
# Wind_Speed : Wind speed on the day the bike was rented : Continuous : Predictor
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
Deciles_Total_Rental_Bikes <- quantile(All_Data$Total_Rental_Bikes, probs = seq(0, 1,length=11))
Deciles_Total_Rental_Bikes

Deciles_Temperature <- quantile(All_Data$Temperature, probs = seq(0, 1,length=11))
Deciles_Temperature

Deciles_Humidity <- quantile(All_Data$Humidity, probs = seq(0, 1,length=11))
Deciles_Humidity

Deciles_Wind_Speed <- quantile(All_Data$Wind_Speed, probs = seq(0, 1,length=11))
Deciles_Wind_Speed

Deciles_Sim <- quantile(All_Data$Sim, probs = seq(0, 1,length=11))
Deciles_Sim

## Step 10: Calculating the frequencies on categorical variables ##
Holiday_Freq <- table(All_Data$Holiday)
Holiday_Freq

Working_Day_Freq <- table(All_Data$Working_Day)
Working_Day_Freq

Rainy_Freq <- table(All_Data$Rainy)
Rainy_Freq

## Step 12: Plotting the numeric variables ##
ggplot(data = All_Data) + geom_point(aes(x = Temperature, y = Total_Rental_Bikes), colour = 'blue')

ggplot(data = All_Data) + geom_point(aes(x = Humidity, y = Total_Rental_Bikes), colour = 'green')

ggplot(data = All_Data) + geom_point(aes(x = Wind_Speed, y = Total_Rental_Bikes), colour = 'red')

ggplot(data = All_Data) + geom_point(aes(x = Sim, y = Total_Rental_Bikes), colour = 'purple')

## Step 14: Plotting the categorical variables ##
Holiday <- ggplot(All_Data, aes(Holiday, Total_Rental_Bikes))
Holiday + geom_boxplot(outlier.colour = "red", outlier.shape = 1)

Working_Day <- ggplot(All_Data, aes(Working_Day, Total_Rental_Bikes))
Working_Day + geom_boxplot(outlier.colour = "red", outlier.shape = 1)

Rainy <- ggplot(All_Data, aes(Rainy, Total_Rental_Bikes))
Rainy + geom_boxplot(outlier.colour = "red", outlier.shape = 1)

## Step 15: Calculate correlations ##
Vars_Num <- c("Total_Rental_Bikes", "Temperature", "Humidity", "Wind_Speed", "Sim")
Numeric_Data <- Training_Data[Vars_Num]
Correlation <- round(cor(Numeric_Data, method = c("pearson")),2)
Correlation

## Step 17: Fitting a model using forward selection ##
null = lm(Total_Rental_Bikes~1, data=Training_Data)
full = lm(Total_Rental_Bikes~factor(Holiday) + factor(Working_Day) + factor(Rainy) + Temperature + Humidity + Wind_Speed + Sim,data=Training_Data)

Fit_Forward = step(null, scope=list(lower=null, upper=full), direction="forward")

## Step 19: Fitting the final regression model ##
options(digits=3)
Final_Model <- lm(Total_Rental_Bikes ~ Wind_Speed + factor(Working_Day) + factor(Rainy) + Temperature + factor(Holiday) + Humidity, data=Training_Data)
summary(Final_Model)

## Step 20: Validating the Model ##
val_predict <- predict(Final_Model,Validation_Data)
Validation_Predictions <- cbind(Validation_Data,val_predict)

## Step 21: Calculating MAPE from your predictive model ##
count_obs_val <- nrow(Validation_Predictions)
Inside_val <- abs((Validation_Predictions$Total_Rental_Bikes - Validation_Predictions$val_predict)/Validation_Predictions$Total_Rental_Bikes)

MAPE_val <- ((1/count_obs_val)*sum(Inside_val))*100
MAPE_val

## Step 23: Calculating MAPE for mean predictions ##
rental_bikes_mean <- mean(Training_Data$Total_Rental_Bikes)
Validation_Data_Mean <- cbind(Validation_Data,rental_bikes_mean)

count_obs_val_mean <- nrow(Validation_Data_Mean)
Inside_val_mean <- abs((Validation_Data_Mean$Total_Rental_Bikes - Validation_Data_Mean$rental_bikes_mean)/Validation_Data_Mean$Total_Rental_Bikes)
MAPE_val_mean <- ((1/count_obs_val_mean)*sum(Inside_val_mean))*100
MAPE_val_mean

## Step 25: Re-run out model to see output ##
options(digits=3)
Final_Model <- lm(Total_Rental_Bikes ~ Wind_Speed + factor(Working_Day) + factor(Rainy) + Temperature + factor(Holiday) + Humidity, data=Training_Data)
summary(Final_Model)

## Using the formula to predict another observation ##
## For example, let's say the bike company is looking to expand their business to another city and wants to predict what the total number of bike rentals could be on certain days based on certain chracteristics. These are the specifications of one of the days in question:
## The day in question is a Holiday.
## This particular day falls on a working day.
## The weather on this day has no rain in the forecast.
## The temperature is 72.09.
## The humidity is 0.26.
## The wind speed is 15.02 mph.
## Using these specifications, what will the number of bike rentals be?