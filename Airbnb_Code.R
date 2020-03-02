## Step 0: Save this R Script in your state_farm_project folder on your Desktop. ##

## Step 1: Load package ##
library("ggplot2")

## Step 2: Reading your dataset into R ##
library("readr")
urlfile <- "https://github.com/tmthiel2/STEM/raw/master/Airbnb.csv"
All_Data<-as.data.frame(read_csv(url(urlfile)))

## Metadata for your data ##
# Variable : Description : Type of Variable : Predictor or Target 
# room_id : ID # of the Airbnb : ID : Neither - don't use id in model
# room_type : Is the space for rent a shared room or not? : Categorical - Binary : Predictor
# price: Price of the Airbnb rental : Continuous : Target
# reviews: Number of reviews on Airbnb property : Continuous : Predictor
# overall_satisfaction : Overall satisfaction score on Airbnb property : Continuous : Predictor
# accomodates : Number of people Airbnb can accomodate : Continuous : Predictor 
# num_bedrooms : Number of bedrooms in Airbnb property : Continuous : Predictor
# neighborhood : Neighborhood of Chicago where property is located : Categorical - Binary : Predictor
# air_conditioning : Is there air conditioning in the property? : Categorical - Binary : Predictor 
# sim : Random variable : Continuous : Predictor

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
Deciles_price <- quantile(All_Data$price, probs = seq(0, 1,length=11))
Deciles_price

Deciles_reviews <- quantile(All_Data$reviews, probs = seq(0, 1,length=11))
Deciles_reviews

Deciles_overall_satisfaction <- quantile(All_Data$overall_satisfaction, probs = seq(0, 1,length=11))
Deciles_overall_satisfaction

Deciles_accomodates <- quantile(All_Data$accomodates, probs = seq(0, 1,length=11))
Deciles_accomodates

Deciles_num_bedrooms <- quantile(All_Data$num_bedrooms, probs = seq(0, 1,length=11))
Deciles_num_bedrooms

Deciles_sim <- quantile(All_Data$sim, probs = seq(0, 1,length=11))
Deciles_sim

## Step 10: Calculating the frequencies on categorical variables ##
Room_Type_Freq <- table(All_Data$room_type)
Room_Type_Freq

Neighborhood_Freq <- table(All_Data$neighborhood)
Neighborhood_Freq

Air_Conditioning_Freq <- table(All_Data$air_conditioning)
Air_Conditioning_Freq

## Step 12: Plotting the numeric variables ##
ggplot(data = All_Data) + geom_point(aes(x = reviews, y = price), colour = 'blue')

ggplot(data = All_Data) + geom_point(aes(x = overall_satisfaction, y = price), colour = 'green')

ggplot(data = All_Data) + geom_point(aes(x = accomodates, y = price), colour = 'red')

ggplot(data = All_Data) + geom_point(aes(x = num_bedrooms, y = price), colour = 'purple')

ggplot(data = All_Data) + geom_point(aes(x = sim, y = price), colour = 'brown')

## Step 14: Plotting the categorical variables ##
room_type <- ggplot(All_Data, aes(room_type, price))
room_type + geom_boxplot(outlier.colour = "red", outlier.shape = 1)

neighborhood <- ggplot(All_Data, aes(neighborhood, price))
neighborhood + geom_boxplot(outlier.colour = "red", outlier.shape = 1)

air_conditioning <- ggplot(All_Data, aes(air_conditioning, price))
air_conditioning + geom_boxplot(outlier.colour = "red", outlier.shape = 1)

## Step 15: Calculate correlations ##
Vars_Num <- c("price","reviews", "overall_satisfaction", "accomodates", "num_bedrooms", "sim")
Numeric_Data <- Training_Data[Vars_Num]
Correlation <- round(cor(Numeric_Data, method = c("pearson")),2)
Correlation

## Step 17: Fitting a model using forward selection ##
null = lm(price~1, data=Training_Data)
full = lm(price~factor(room_type) + reviews + overall_satisfaction + accomodates + num_bedrooms + factor(neighborhood) + factor(air_conditioning) + sim,data=Training_Data)

Fit_Forward = step(null, scope=list(lower=null, upper=full), direction="forward")

## Step 19: Fitting the final regression model ##
options(digits=3)
Final_Model <- lm(price ~ num_bedrooms + accomodates + factor(neighborhood) + factor(air_conditioning) + factor(room_type) + reviews, data=Training_Data)
summary(Final_Model)

## Step 20: Validating the Model ##
val_predict <- predict(Final_Model,Validation_Data)
Validation_Predictions <- cbind(Validation_Data,val_predict)

## Step 21: Calculating MAPE from your predictive model ##
count_obs_val <- nrow(Validation_Predictions)
Inside_val <- abs((Validation_Predictions$price - Validation_Predictions$val_predict)/Validation_Predictions$price)

MAPE_val <- ((1/count_obs_val)*sum(Inside_val))*100
MAPE_val

## Step 23: Calculating MAPE for mean predictions ##
price_mean <- mean(Training_Data$price)
Validation_Data_Mean <- cbind(Validation_Data,price_mean)

count_obs_val_mean <- nrow(Validation_Data_Mean)
Inside_val_mean <- abs((Validation_Data_Mean$price - Validation_Data_Mean$price_mean)/Validation_Data_Mean$price)
MAPE_val_mean <- ((1/count_obs_val_mean)*sum(Inside_val_mean))*100
MAPE_val_mean

## Step 25: Re-run out model to see output ##
options(digits=3)
Final_Model <- lm(price ~ num_bedrooms + accomodates + factor(neighborhood) + factor(air_conditioning) + factor(room_type) + reviews, data=Training_Data)
summary(Final_Model)

## Using the formula to predict another observation ##
## For example, let's say someone is considering putting up their property to be rented through Airbnb and wants to predict what the price of the property should be. These are the specifications of the property:
## They have a Shared Room available.
## They have 0 reviews since the property hasn't ever been rented through Airbnb.
## They have a 0 overall satisfaction rate because the property hasn't ever been rented through Airbnb.
## The room will accomodate 2 people.
## The property has 3 bedrooms total.
## The property is located in the North Side.
## The property is air conditioning.
## Using these specifications, what will the price of the property be?