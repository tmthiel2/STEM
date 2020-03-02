## Step 0: Save this R Script in your state_farm_project folder on your Desktop. ##

## Step 1: Load package ##
library("ggplot2")

## Step 2: Reading your dataset into R ##
library("readr")
urlfile <- "https://github.com/tmthiel2/STEM/raw/master/Diamonds.csv"
All_Data<-as.data.frame(read_csv(url(urlfile)))

## Metadata for your data ##
# Variable : Description : Type of Variable : Predictor or Target 
# Diamond_ID : ID # of the diamond : ID : Neither - don't use id in model
# Carat : Carat weight of the diamond : Continuous : Predictor
# Cut : Cut quality of the diamond : Categorical - Binary : Predictor
# Color : Color of the diamond : Categorical - Binary : Predictor 
# Clarity : Measure of how clear the diamond is : Categorical - Binary : Predictor
# Price : Price of the diamond : Continuous : Target
# Length : Length of the diamond : Continuous : Predictor
# Width : Width of the diamond : Continuous : Predictor
# Depth : Depth of the diamond : Continuous : Predictor
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
Deciles_Carat <- quantile(All_Data$Carat, probs = seq(0, 1,length=11))
Deciles_Carat

Deciles_Price <- quantile(All_Data$Price, probs = seq(0, 1,length=11))
Deciles_Price

Deciles_Length <- quantile(All_Data$Length, probs = seq(0, 1,length=11))
Deciles_Length

Deciles_Width <- quantile(All_Data$Width, probs = seq(0, 1,length=11))
Deciles_Width

Deciles_Depth <- quantile(All_Data$Depth, probs = seq(0, 1,length=11))
Deciles_Depth

Deciles_Sim <- quantile(All_Data$Sim, probs = seq(0, 1,length=11))
Deciles_Sim

## Step 10: Calculating the frequencies on categorical variables ##
Cut_Freq <- table(All_Data$Cut)
Cut_Freq

Color_Freq <- table(All_Data$Color)
Color_Freq

Clarity_Freq <- table(All_Data$Clarity)
Clarity_Freq

## Step 12: Plotting the numeric variables ##
ggplot(data = All_Data) + geom_point(aes(x = Carat, y = Price), colour = 'blue')

ggplot(data = All_Data) + geom_point(aes(x = Length, y = Price), colour = 'purple')

ggplot(data = All_Data) + geom_point(aes(x = Width, y = Price), colour = 'brown')

ggplot(data = All_Data) + geom_point(aes(x = Depth, y = Price), colour = 'orange')

ggplot(data = All_Data) + geom_point(aes(x = Sim, y = Price), colour = 'black')

## Step 14: Plotting the categorical variables ##
Cut <- ggplot(All_Data, aes(Cut, Price))
Cut + geom_boxplot(outlier.colour = "red", outlier.shape = 1)

Color <- ggplot(All_Data, aes(Color, Price))
Color + geom_boxplot(outlier.colour = "red", outlier.shape = 1)

Clarity <- ggplot(All_Data, aes(Clarity, Price))
Clarity + geom_boxplot(outlier.colour = "red", outlier.shape = 1)

## Step 15: Calculate correlations ##
Vars_Num <- c("Carat", "Price", "Length", "Width", "Depth", "Sim")
Numeric_Data <- Training_Data[Vars_Num]
Correlation <- round(cor(Numeric_Data, method = c("pearson")),2)
Correlation

## Step 17: Fitting a model using forward selection ##
null = lm(Price~1, data=Training_Data)
full = lm(Price~Carat + factor(Cut) + factor(Color) + factor(Clarity) + Length + Width + Depth + Sim,data=Training_Data)

Fit_Forward = step(null, scope=list(lower=null, upper=full), direction="forward")

## Step 19: Fitting the final regression model ##
options(digits=3)
Final_Model <- lm(Price ~ factor(Clarity) + Width + factor(Cut) + factor(Color) + Carat + Length, data=Training_Data)
summary(Final_Model)

## Step 20: Validating the Model ##
val_predict <- predict(Final_Model,Validation_Data)
Validation_Predictions <- cbind(Validation_Data,val_predict)

## Step 21: Calculating MAPE from your predictive model ##
count_obs_val <- nrow(Validation_Predictions)
Inside_val <- abs((Validation_Predictions$Price - Validation_Predictions$val_predict)/Validation_Predictions$Price)

MAPE_val <- ((1/count_obs_val)*sum(Inside_val))*100
MAPE_val

## Step 23: Calculating MAPE for mean predictions ##
price_mean <- mean(Training_Data$Price)
Validation_Data_Mean <- cbind(Validation_Data,price_mean)

count_obs_val_mean <- nrow(Validation_Data_Mean)
Inside_val_mean <- abs((Validation_Data_Mean$Price - Validation_Data_Mean$price_mean)/Validation_Data_Mean$Price)
MAPE_val_mean <- ((1/count_obs_val_mean)*sum(Inside_val_mean))*100
MAPE_val_mean

## Step 25: Re-run out model to see output ##
options(digits=3)
Final_Model <- lm(Price ~ factor(Clarity) + Width + factor(Cut) + factor(Color) + Carat + Length, data=Training_Data)
summary(Final_Model)

## Using the formula to predict another observation ##
## For example, let's say a person is looking to buy a new diamond and wants to predict the price of it based on its characteristics. These are the characteristics of the diamond:
## The diamond is 1.04 carats.
## The diamond has an Ideal cut.
## The diamond has the Best color.
## The diamond has Clear clarity.
## The Length of the diamond is 6.9.
## The Width of the diamond is 5.89.
## The Depth of the diamond is 4.14.
## Using these specifications, what will the price of the diamond be?