## Step 0: Save this R Script in your state_farm_project folder on your Desktop. ##

## Step 1: Load package ##
library("ggplot2")

## Step 2: Reading your dataset into R ##
library("readr")
urlfile <- "https://github.com/tmthiel2/STEM/raw/master/Home_Sales.csv"
All_Data<-as.data.frame(read_csv(url(urlfile)))

## Metadata for your data ##
# Variable : Description : Type of Variable : Predictor or Target 
# House_ID : ID # of the house : ID : Neither - don't use id in model
# Sale_Price : Sale price of the house : Continuous : Target
# Story : Number of stories in the house : Categorical - Binary : Predictor
# Total_SF : Total square footage of the house : Continuous : Predictor
# Ext_Walls : Material the exterior walls are made of : Categorical - Binary : Predictor
# Finish_SF_Attic : Amount of finished square footage in the attic : Continuous : Predictor
# Finish_SF_Basement : Amount of finished square footage in the basement : Continuous : Predictor
# Num_Stall_Garage : Number of stalls in the garage : Continuous : Predictor
# Has_Lake_Front : Is the house a lake front property or not? : Categorical - Binary : Predictor
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
Deciles_Sale_Price <- quantile(All_Data$Sale_Price, probs = seq(0, 1,length=11))
Deciles_Sale_Price

Deciles_Total_SF <- quantile(All_Data$Total_SF, probs = seq(0, 1,length=11))
Deciles_Total_SF

Deciles_Finish_SF_Attic <- quantile(All_Data$Finish_SF_Attic, probs = seq(0, 1,length=11))
Deciles_Finish_SF_Attic

Deciles_Finish_SF_Basement <- quantile(All_Data$Finish_SF_Basement, probs = seq(0, 1,length=11))
Deciles_Finish_SF_Basement

Deciles_Num_Stall_Garage <- quantile(All_Data$Num_Stall_Garage, probs = seq(0, 1,length=11))
Deciles_Num_Stall_Garage

Deciles_Sim <- quantile(All_Data$Sim, probs = seq(0, 1,length=11))
Deciles_Sim

## Step 10: Calculating the frequencies on categorical variables ##
Story_Freq <- table(All_Data$Story)
Story_Freq

Ext_Walls_Freq <- table(All_Data$Ext_Walls)
Ext_Walls_Freq

Has_Lake_Front_Freq <- table(All_Data$Has_Lake_Front)
Has_Lake_Front_Freq

## Step 12: Plotting the numeric variables ##
ggplot(data = All_Data) + geom_point(aes(x = Total_SF, y = Sale_Price), colour = 'blue')

ggplot(data = All_Data) + geom_point(aes(x = Finish_SF_Attic, y = Sale_Price), colour = 'purple')

ggplot(data = All_Data) + geom_point(aes(x = Finish_SF_Basement, y = Sale_Price), colour = 'brown')

ggplot(data = All_Data) + geom_point(aes(x = Num_Stall_Garage, y = Sale_Price), colour = 'orange')

ggplot(data = All_Data) + geom_point(aes(x = Sim, y = Sale_Price), colour = 'black')

## Step 14: Plotting the categorical variables ##
Story <- ggplot(All_Data, aes(Story, Sale_Price))
Story + geom_boxplot(outlier.colour = "red", outlier.shape = 1)

Ext_Walls <- ggplot(All_Data, aes(Ext_Walls, Sale_Price))
Ext_Walls + geom_boxplot(outlier.colour = "red", outlier.shape = 1)

Has_Lake_Front <- ggplot(All_Data, aes(Has_Lake_Front, Sale_Price))
Has_Lake_Front + geom_boxplot(outlier.colour = "red", outlier.shape = 1)

## Step 15: Calculate correlations ##
Vars_Num <- c("Sale_Price", "Total_SF", "Finish_SF_Attic", "Finish_SF_Basement", "Num_Stall_Garage", "Sim")
Numeric_Data <- Training_Data[Vars_Num]
Correlation <- round(cor(Numeric_Data, method = c("pearson")),2)
Correlation

## Step 17: Fitting a model using forward selection ##
null = lm(Sale_Price~1, data=Training_Data)
full = lm(Sale_Price~factor(Story) + Total_SF + factor(Ext_Walls) + Finish_SF_Attic + Finish_SF_Basement + Num_Stall_Garage + factor(Has_Lake_Front) + Sim,data=Training_Data)

Fit_Forward = step(null, scope=list(lower=null, upper=full), direction="forward")

## Step 19: Fitting the final regression model ##
options(digits=3)
Final_Model <- lm(Sale_Price ~ Total_SF + Finish_SF_Basement + Num_Stall_Garage + factor(Has_Lake_Front) + Finish_SF_Attic + factor(Ext_Walls) + factor(Story), data=Training_Data)
summary(Final_Model)

## Step 20: Validating the Model ##
val_predict <- predict(Final_Model,Validation_Data)
Validation_Predictions <- cbind(Validation_Data,val_predict)

## Step 21: Calculating MAPE from your predictive model ##
count_obs_val <- nrow(Validation_Predictions)
Inside_val <- abs((Validation_Predictions$Sale_Price - Validation_Predictions$val_predict)/Validation_Predictions$Sale_Price)

MAPE_val <- ((1/count_obs_val)*sum(Inside_val))*100
MAPE_val

## Step 23: Calculating MAPE for mean predictions ##
sale_price_mean <- mean(Training_Data$Sale_Price)
Validation_Data_Mean <- cbind(Validation_Data,sale_price_mean)

count_obs_val_mean <- nrow(Validation_Data_Mean)
Inside_val_mean <- abs((Validation_Data_Mean$Sale_Price - Validation_Data_Mean$sale_price_mean)/Validation_Data_Mean$Sale_Price)
MAPE_val_mean <- ((1/count_obs_val_mean)*sum(Inside_val_mean))*100
MAPE_val_mean

## Step 25: Re-run out model to see output ##
options(digits=3)
Final_Model <- lm(Sale_Price ~ Total_SF + Finish_SF_Basement + Num_Stall_Garage + factor(Has_Lake_Front) + Finish_SF_Attic + factor(Ext_Walls) + factor(Story), data=Training_Data)
summary(Final_Model)

## Using the formula to predict another observation ##
## For example, let's say a person is looking to sell their house and want to predict the price that it will sell at based on certain characteristics. These are the characteristics of the home:
## The house is 2 Story.
## The house has a total square footage of 1190.
## The house has exterior walls made of brick.
## The finished square footage of the attic is 0.
## The finished square footage of the basement is 561.
## The house has a 2 stall garage.
## The house does not have a lake front view.
## Using these specifications, what will the sale price be?