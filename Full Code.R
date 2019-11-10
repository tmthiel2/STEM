## Step 0: Creating folders on your desktop ##
require("svDialogs")
set_up_directories <- function(){
  # Change this to the directory path that leads up to the alias
  base_directory <- "C:/Users/"
  # Print out all child folders of "base_directory". One of them should be their alias.
  get_alias <- function(){dlg_list(list.files(base_directory), multiple = FALSE, title = "\n\nSelect which one is your alias...\nType the number that corresponds and then hit enter...\n\n" ,gui = .GUI)$res}
  alias <- get_alias()
  # Create path for new folder to be created
  path <- paste(base_directory,alias,"/OneDrive/Desktop/state_farm_project",sep="")
  dir.create(path = path, showWarnings = FALSE)
  dir.create(path = paste(path,"_demo",sep=""), showWarnings = FALSE)
  exist <- file.exists(paste(path,"_demo/STEM_Project_For_",alias,".R",sep=""))
  if(!exist){
    download.file("https://raw.githubusercontent.com/tmthiel2/STEM/master/testing.R",destfile = paste(path,"_demo/STEM_Project_For_",alias,".R",sep=""))
  }  
  cat("\n\nYou should now have two new folders on your desktop named the following:\n\tstate_farm_project\n\tstate_farm_project_demo\n\nIf you do not see these folders on your desktop, run the code again and be sure to type the number that corresponds to your alias. If the folders still aren't being created, ask a State Farm person for help.")
}
set_up_directories()

## Step 1: Practice installation of a package ##
require("ggplot2")

## Step 2: Reading your dataset into R ##
require("readr")
urlfile <- "https://github.com/tmthiel2/STEM/raw/master/Example Data.csv"
All_Data<-as.data.frame(read_csv(url(urlfile)))

## Step 3: Calculating the number of rows in your dataset ##
num_obs <- nrow(All_Data)
num_obs

## Step 4: How many observations are in your dataset? ##
## 428 observations ##

## Step 5: Calculating the number of columns in your dataset ##
num_col <- ncol(All_Data)
num_col

## Step 6: How many variables are in your dataset? ##
## 14 variables ##

## Step 7: Splitting your dataset into 60% training and 40% validation ##
p60 <- quantile(All_Data$sim,0.6)
Training_Data <- All_Data[which(All_Data$sim <= p60),]
Validation_Data <- All_Data[which(All_Data$sim > p60),]

## Step 8: Run out the deciles to see the distributions of each numeric variable ##
Deciles_enginesize <- quantile(All_Data$engine_size, probs = seq(0, 1,length=11))
Deciles_enginesize

Deciles_cylinders <- quantile(All_Data$cylinders, probs = seq(0, 1,length=11))
Deciles_cylinders

Deciles_horsepower <- quantile(All_Data$horsepower, probs = seq(0, 1,length=11))
Deciles_horsepower

Deciles_mpghighway <- quantile(All_Data$mpg_highway, probs = seq(0, 1,length=11))
Deciles_mpghighway

Deciles_weight <- quantile(All_Data$weight, probs = seq(0, 1,length=11))
Deciles_weight

Deciles_wheelbase <- quantile(All_Data$wheelbase, probs = seq(0, 1,length=11))
Deciles_wheelbase

Deciles_length <- quantile(All_Data$length, probs = seq(0, 1,length=11))
Deciles_length

Deciles_sim <- quantile(All_Data$sim, probs = seq(0, 1,length=11))
Deciles_sim

## Step 9: Do all the deciles look reasonable? ##
## After looking at all the deciles, the values look reasonable. ##

## Step 10: Calculating the frequencies on categorical variables ##
Car_Freq <- table(All_Data$car)
Car_Freq

SUV_Freq <- table(All_Data$suv)
SUV_Freq

Asia_Freq <- table(All_Data$from_asia)
Asia_Freq

Europe_Freq <- table(All_Data$from_europe)
Europe_Freq

USA_Freq <- table(All_Data$from_usa)
USA_Freq

## Step 11: Do all of the variables appear to have sufficient number of observations in each level?  ##
## All of the levels look correct with sufficient number of observations in each level.     ##

## Step 12: Plotting the numeric variables ##
ggplot(data = All_Data) + geom_point(aes(x = engine_size, y = mpg_highway), colour = 'blue')

ggplot(data = All_Data) + geom_point(aes(x = cylinders, y = mpg_highway), colour = 'green')

ggplot(data = All_Data) + geom_point(aes(x = horsepower, y = mpg_highway), colour = 'red')

ggplot(data = All_Data) + geom_point(aes(x = weight, y = mpg_highway), colour = 'purple')

ggplot(data = All_Data) + geom_point(aes(x = wheelbase, y = mpg_highway), colour = 'orange')

ggplot(data = All_Data) + geom_point(aes(x = length, y = mpg_highway), colour = 'black')

ggplot(data = All_Data) + geom_point(aes(x = sim, y = mpg_highway), colour = 'brown')

## Step 13: Do the plots reveal any interesting patterns?               ##
## As engine size increases, mpg decreases. This makes intuitive sense. ##
## Similarly, as horsepower & weight increases, mpg decreases.          ##

## Step 14: Plotting the categorical variables ##
car <- ggplot(All_Data, aes(car, mpg_highway))
car + geom_boxplot(outlier.colour = "red", outlier.shape = 1)

suv <- ggplot(All_Data, aes(suv, mpg_highway))
suv + geom_boxplot(outlier.colour = "red", outlier.shape = 1)

asia <- ggplot(All_Data, aes(from_asia, mpg_highway))
asia + geom_boxplot(outlier.colour = "red", outlier.shape = 1)

europe <- ggplot(All_Data, aes(from_europe, mpg_highway))
europe + geom_boxplot(outlier.colour = "red", outlier.shape = 1)

usa <- ggplot(All_Data, aes(from_usa, mpg_highway))
usa + geom_boxplot(outlier.colour = "red", outlier.shape = 1)

## Step 15: Calculate correlations ##
Vars_Num <- c("engine_size", "cylinders", "horsepower", "mpg_highway", "weight", "wheelbase", "length", "sim")
Numeric_Data <- Training_Data[Vars_Num]
Correlation <- round(cor(Numeric_Data, method = c("pearson")),2)
Correlation

## Step 16: Are there variables in the dataset that we should watch out for?      ##
## We see some high correlations between a lot of the variables.                  ##
## As examples, cylinders & engine_size have a positive correlation of 0.88       ##
## and wheelbase & length have a positive correlation of 0.89. The target         ##
## variable, mpg_highway, is negatively correlated with engine_size, weight, etc. ##

## Step 17: Fitting a model using backward selection ##
full = lm(mpg_highway~engine_size + cylinders + horsepower + weight + wheelbase + length + factor(car) + factor(suv) + factor(from_asia) + factor(from_europe) + factor(from_usa) + sim,data=Training_Data)
Fit_Backward <- step(full, data =  Training_Data,direction="backward")

## Step 18: How many variables are chosen in the final step of backward selection? ##
## 4 variables were chosen from backward selection ##

## Step 19: Fitting a model using forward selection ##
null = lm(mpg_highway~1, data=Training_Data)
full = lm(mpg_highway~engine_size + cylinders + horsepower + weight + wheelbase + length + factor(car) + factor(suv) + factor(from_asia) + factor(from_europe) + factor(from_usa) + sim,data=Training_Data)
Fit_Forward = step(null, scope=list(lower=null, upper=full), direction="forward")

## Step 20: How many variables are chosen in the final step of forward selection? ##
## 4 variables were chosen from forward selection ##

## Step 21: Fitting a model using stepwise selection ##
null = lm(mpg_highway~1, data=Training_Data)
full = lm(mpg_highway~engine_size + cylinders + horsepower + weight + wheelbase + length + factor(car) + factor(suv) + factor(from_asia) + factor(from_europe) + factor(from_usa) + sim,data=Training_Data)
Fit_Stepwise = step(null, scope=list(upper=full),data=Training_Data, direction="both")

## Step 22: How many variables are chosen in the final step of stepwise selection? ##
## 4 variables were chosen from stepwise selection ##

## Step 23: Fitting the final regression model ##
options(digits=3)
Final_Model <- lm(mpg_highway ~ weight + horsepower + factor(car) + engine_size, data=Training_Data)
summary(Final_Model)

## Step 24: Validating the Model ##
val_predict <- predict(Final_Model,Validation_Data)
Validation_Predictions <- cbind(Validation_Data,val_predict)

## Step 25: Calculating MAPE from your predictive model ##
count_obs_val <- nrow(Validation_Predictions)
Inside_val <- abs((Validation_Predictions$mpg_highway - Validation_Predictions$val_predict)/Validation_Predictions$mpg_highway)
MAPE_val <- ((1/count_obs_val)*sum(Inside_val))*100
MAPE_val

## Step 26: What is the MAPE value your model produced? Do you think it's "good"? ##
## The MAPE is 7.49%. While no rules about what a "good" MAPE is, 7.49% is        ##
## really low and we would consider it a good MAPE.                               ##

## Step 27: Calculating MAPE for random guess predictions ##
mpg_highway_rand <- round(runif(count_obs_val, min(Training_Data$mpg_highway),max(Training_Data$mpg_highway)),digits=0)
Validation_Data_Rand <- cbind(Validation_Data,mpg_highway_rand)

count_obs_val_rand <- nrow(Validation_Data_Rand)
Inside_val_rand <- abs((Validation_Data_Rand$mpg_highway - Validation_Data_Rand$mpg_highway_rand)/Validation_Data_Rand$mpg_highway)
MAPE_val_rand <- ((1/count_obs_val_rand)*sum(Inside_val_rand))*100
MAPE_val_rand

## Step 28: What is the MAPE value produced by using predictions that are random guesses?  ##
## The MAPE produced by random guess is much higher than that of the predictions from      ##
## the model.                                                                              ##

## Step 29: Calculating MAPE for mean predictions ##
mpg_highway_mean <- mean(Training_Data$mpg_highway)
Validation_Data_Mean <- cbind(Validation_Data,mpg_highway_mean)

count_obs_val_mean <- nrow(Validation_Data_Mean)
Inside_val_mean <- abs((Validation_Data_Mean$mpg_highway - Validation_Data_Mean$mpg_highway_mean)/Validation_Data_Mean$mpg_highway)
MAPE_val_mean <- ((1/count_obs_val_mean)*sum(Inside_val_mean))*100
MAPE_val_mean

## Step 30: What is the MAPE value produced by using predictions that are the mean of the target value?  ##
## The MAPE produced by using predictions that are the mean of the target is 15.4%  ##

## Step 31: Calculating MAPE for median predictions ##
mpg_highway_median <- median(Training_Data$mpg_highway)
Validation_Data_Median <- cbind(Validation_Data,mpg_highway_median)

count_obs_val_median <- nrow(Validation_Data_Median)
Inside_val_median <- abs((Validation_Data_Median$mpg_highway - Validation_Data_Median$mpg_highway_median)/Validation_Data_Median$mpg_highway)
MAPE_val_median <- ((1/count_obs_val_median)*sum(Inside_val_median))*100
MAPE_val_median

## Step 32: What is the MAPE value produced by using predictions that are the median of the target value?  ##
## The MAPE produced by using predictions that are the median of the target is 15.2%  ##

## Step 33: Can you fill out the table with the MAPE values produced by these different prediction methods. Which method produced the lowest MAPE value and therefore the most accurate predictions?  ##

## Answer: 

## |Method            |MAPE    |
## |:----------------:|:------:|
## |Predictive Model  |7.49%   |
## |Median            |15.2%   |
## |Mean              |15.4%   |
## |Random Guess      |~45%    |

## Step 34: Calculating R-Squared for predictions from your predictive model ##
mean_val <- (1/count_obs_val)*sum(Validation_Predictions$mpg_highway)
SS_Total_Val <- sum((Validation_Predictions$mpg_highway-mean_val)**2)
SS_Reg_Val <- sum((Validation_Predictions$val_predict-mean_val)**2)
SS_Res_Val <- sum((Validation_Predictions$mpg_highway-Validation_Predictions$val_predict)**2)
Rsquare_Val <- 1-(SS_Res_Val/SS_Total_Val)
Rsquare_Val

## Step 35: What is the R-Squared value on your validation data? Does the validation R-Squared value make sense with the MAPE value calculated above? ##
## The value of the R-Squared is 0.681. ##

## Step 36: Calculating R-Squared for random guess predictions ##
mean_val_rand <- (1/count_obs_val_rand)*sum(Validation_Data_Rand$mpg_highway)
SS_Total_Val_rand <- sum((Validation_Data_Rand$mpg_highway-mean_val_rand)**2)
SS_Reg_Val_rand <- sum((Validation_Data_Rand$mpg_highway_rand-mean_val_rand)**2)
SS_Res_Val_rand <- sum((Validation_Data_Rand$mpg_highway-Validation_Data_Rand$mpg_highway_rand)**2)
Rsquare_Val_rand <- 1-(SS_Res_Val_rand/SS_Total_Val_rand)
Rsquare_Val_rand

## Step 37: What is the R-squared value produced by using predictions that are random guesses? ##
## The value of the R-Squared is significantly worse that the R-Squared based on the model. ##

## Step 38: Calculating R-Squared for mean predictions ##
mean_val_mean <- (1/count_obs_val_mean)*sum(Validation_Data_Mean$mpg_highway)
SS_Total_Val_mean <- sum((Validation_Data_Mean$mpg_highway-mean_val_mean)**2)
SS_Reg_Val_mean <- sum((Validation_Data_Mean$mpg_highway_mean-mean_val_mean)**2)
SS_Res_Val_mean <- sum((Validation_Data_Mean$mpg_highway-Validation_Data_Mean$mpg_highway_mean)**2)
Rsquare_Val_mean <- 1-(SS_Res_Val_mean/SS_Total_Val_mean)
Rsquare_Val_mean

## Step 39: What is the R-squared value produced by using predictions that a mean of the target value? ##
## The value of the R-Squared is -0.0194. ##

## Step 40: Calculating R-Squared for median predictions ##
mean_val_median <- (1/count_obs_val_median)*sum(Validation_Data_Median$mpg_highway)
SS_Total_Val_median <- sum((Validation_Data_Median$mpg_highway-mean_val_median)**2)
SS_Reg_Val_median <- sum((Validation_Data_Median$mpg_highway_median-mean_val_median)**2)
SS_Res_Val_median <- sum((Validation_Data_Median$mpg_highway-Validation_Data_Median$mpg_highway_median)**2)
Rsquare_Val_median <- 1-(SS_Res_Val_median/SS_Total_Val_median)
Rsquare_Val_median

## Step 41: What is the R-squared value produced by using predictions that a median of the target value? ##
## The value of the R-Squared is -0.0519. ##

## Step 42: Can you fill out the table with the R-square values produced by these different prediction methods. Which method produced the highest R-square value? ##

## |Method            |R-square |
## |:----------------:|:-------:|
## |Predictive Model  |0.681    |
## |Median            |-0.0519  |
## |Mean              |-0.0194  |
## |Random Guess      |~4.27    |

## Step 43: Re-run out model to see output ##
options(digits=3)
Final_Model <- lm(mpg_highway ~ weight + horsepower + factor(car) + engine_size, data=Training_Data)
summary(Final_Model)

## Step 44: If we look at the p-values in your dataset (Pr(>|t|)), how many variables have values less than 0.05? Are there any variables that even though stepwise selection chose that variable, it doesn't have a p-value less than 0.05?  ##
## All 4 variables have p-values less than 0.05. None have p-values bigger than 0.05 ##

## Step 45: How do we interpret the coefficients in your dataset?  ##
## * weight - The Highway MPG will decrease by 0.002358 if I increase the weight of the car by 1 pound.

## * horsepower - The Highway MPG will decrease by 0.022803 if I increase the car's horsepower by 1 unit.

## * factor(car)Yes - Relative to the base category (No), the Highway MPG will increase by 4.190985 if I drive a car. Another way of saying this is driving a car will increase the Highway MPG by 3.75938 compared to driving an SUV or a truck. 

## * engine_size - The Highway MPG will decrease by 0.814026 if I increase the car's engine size by 1 unit. 
