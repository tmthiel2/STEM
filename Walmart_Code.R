## Step 0: Save this R Script in your state_farm_project folder on your Desktop. ##

## Step 1: Load package ##
library("ggplot2")

## Step 2: Reading your dataset into R ##
library("readr")
urlfile <- "https://github.com/tmthiel2/STEM/raw/master/Walmart.csv"
All_Data<-as.data.frame(read_csv(url(urlfile)))

## Metadata for your data ##
# Variable : Description : Type of Variable : Predictor or Target 
# Item_ID : ID # of the item : ID : Neither - don't use id in model
# Item_Weight : Weight of the item (in ounces) : Continuous : Predictor
# Low_Fat_Content : Does this item have low fat content? : Categorical - Binary : Predictor
# Item_Visibility : Measure of how visible the item is in the store : Continuous : Predictor
# Drink : Is the item a drink? : Categorical - Binary : Predictor
# Max_Retail_Price : The maximum retail price the item can be sold for : Continuous : Predictor
# Store_Built : Year the Walmart store was built : Continuous : Predictor
# Large_Size_Store : Is the Walmart considered to be a large store? : Categorical - Binary : Predictor
# Item_Sales : Total sales of the item : Continuous : Target
# sim: Random variable : Continuous : Predictor

## Step 3: Calculating the number of rows in your dataset ##
num_obs <- nrow(All_Data)
num_obs

## Step 5: Calculating the number of columns in your dataset ##
num_col <- ncol(All_Data)
num_col

## Step 7: Splitting your dataset into 60% training and 40% validation ##
p60 <- quantile(All_Data$sim,0.6)
Training_Data <- All_Data[which(All_Data$sim <= p60),]
Validation_Data <- All_Data[which(All_Data$sim > p60),]

## Step 8: Run out the deciles to see the distributions of each numeric variable ##
Deciles_Item_Weight <- quantile(All_Data$Item_Weight, probs = seq(0, 1,length=11))
Deciles_Item_Weight

Deciles_Item_Visibility <- quantile(All_Data$Item_Visibility, probs = seq(0, 1,length=11))
Deciles_Item_Visibility

Deciles_Max_Retail_Price <- quantile(All_Data$Max_Retail_Price, probs = seq(0, 1,length=11))
Deciles_Max_Retail_Price

Deciles_Store_Built <- quantile(All_Data$Store_Built, probs = seq(0, 1,length=11))
Deciles_Store_Built

Deciles_Item_Sales <- quantile(All_Data$Item_Sales, probs = seq(0, 1,length=11))
Deciles_Item_Sales

Deciles_sim <- quantile(All_Data$sim, probs = seq(0, 1,length=11))
Deciles_sim

## Step 10: Calculating the frequencies on categorical variables ##
Low_Fat_Content_Freq <- table(All_Data$Low_Fat_Content)
Low_Fat_Content_Freq

Drink_Freq <- table(All_Data$Drink)
Drink_Freq

Large_Size_Store_Freq <- table(All_Data$Large_Size_Store)
Large_Size_Store_Freq

## Step 12: Plotting the numeric variables ##
ggplot(data = All_Data) + geom_point(aes(x = Item_Weight, y = Item_Sales), colour = 'blue')

ggplot(data = All_Data) + geom_point(aes(x = Item_Visibility, y = Item_Sales), colour = 'green')

ggplot(data = All_Data) + geom_point(aes(x = Max_Retail_Price, y = Item_Sales), colour = 'red')

ggplot(data = All_Data) + geom_point(aes(x = Store_Built, y = Item_Sales), colour = 'purple')

ggplot(data = All_Data) + geom_point(aes(x = sim, y = Item_Sales), colour = 'brown')

## Step 14: Plotting the categorical variables ##
low_fat <- ggplot(All_Data, aes(Low_Fat_Content, Item_Sales))
low_fat + geom_boxplot(outlier.colour = "red", outlier.shape = 1)

drink <- ggplot(All_Data, aes(Drink, Item_Sales))
drink + geom_boxplot(outlier.colour = "red", outlier.shape = 1)

large_size <- ggplot(All_Data, aes(Large_Size_Store, Item_Sales))
large_size + geom_boxplot(outlier.colour = "red", outlier.shape = 1)

## Step 15: Calculate correlations ##
Vars_Num <- c("Item_Weight", "Item_Visibility", "Max_Retail_Price", "Store_Built", "Item_Sales", "sim")
Numeric_Data <- Training_Data[Vars_Num]
Correlation <- round(cor(Numeric_Data, method = c("pearson")),2)
Correlation

## Step 17: Fitting a model using forward selection ##
null = lm(Item_Sales~1, data=Training_Data)
full = lm(Item_Sales~Item_Weight + factor(Low_Fat_Content) + Item_Visibility + factor(Drink) + Max_Retail_Price + Store_Built + factor(Large_Size_Store) + sim,data=Training_Data)

Fit_Forward = step(null, scope=list(lower=null, upper=full), direction="forward")

## Step 19: Fitting the final regression model ##
options(digits=3)
Final_Model <- lm(Item_Sales ~ Max_Retail_Price + factor(Large_Size_Store) + Store_Built, data=Training_Data)
summary(Final_Model)

## Step 20: Validating the Model ##
val_predict <- predict(Final_Model,Validation_Data)
Validation_Predictions <- cbind(Validation_Data,val_predict)

## Step 21: Calculating MAPE from your predictive model ##
count_obs_val <- nrow(Validation_Predictions)
Inside_val <- abs((Validation_Predictions$Item_Sales - Validation_Predictions$val_predict)/Validation_Predictions$Item_Sales)

MAPE_val <- ((1/count_obs_val)*sum(Inside_val))*100
MAPE_val

## Step 23: Calculating MAPE for mean predictions ##
item_sales_mean <- mean(Training_Data$Item_Sales)
Validation_Data_Mean <- cbind(Validation_Data,item_sales_mean)

count_obs_val_mean <- nrow(Validation_Data_Mean)
Inside_val_mean <- abs((Validation_Data_Mean$Item_Sales - Validation_Data_Mean$item_sales_mean)/Validation_Data_Mean$Item_Sales)
MAPE_val_mean <- ((1/count_obs_val_mean)*sum(Inside_val_mean))*100
MAPE_val_mean

## Step 25: Re-run out model to see output ##
options(digits=3)
Final_Model <- lm(Item_Sales ~ Max_Retail_Price + factor(Large_Size_Store) + Store_Built, data=Training_Data)
summary(Final_Model)

## Using the formula to predict another observation ##
## For example, let's say Walmart is looking to add a new product in their store and wants to predict what the sales of that item will be. These are the specifications of the new product:
## The weight of the item is 8.88 oz.
## It has low fat content.
## It's measure of item visibility is 0.015.
## It is not a drink.
## It's max retail price is $8.94.
## The Walmart store where the product will be placed was built in 2009.
## The Walmart store where the product will be placed is considered a large sized store.
## Using these specifications, what will the sales of the item be?