






































# STEP 01 :- Importing Data
pizza_data = read.csv('pizza_data.csv')
pizza_data

# STEP 02 :- Converting Char to Numeric
pizza_data$brand = as.numeric(factor(pizza_data$brand))
pizza_data$price = as.numeric(factor(pizza_data$price)) 
pizza_data$weight = as.numeric(factor(pizza_data$weight)) 
pizza_data$crust = as.numeric(factor(pizza_data$crust)) 
pizza_data$cheese = as.numeric(factor(pizza_data$cheese))
pizza_data$size = as.numeric(factor(pizza_data$size)) 
pizza_data$toppings = as.numeric(factor(pizza_data$toppings))
pizza_data$spicy = as.numeric(factor(pizza_data$spicy))

# STEP 03 :- Removing na values
pizza_data <- na.omit(pizza_data)
pizza_data
is.na(pizza_data)
str(pizza_data)
# Assuming 'pizza_data' is your data frame
pizza_data <- pizza_data[, -ncol(pizza_data)]
pizza_data

# STEP 04 :- Creating Data frame with 10 observation and 16 profiles
tprefm <- matrix(sample(0:16, 10*16, replace=TRUE), ncol=16)
colnames(tprefm) <- paste0("profil", 1:16)
tprefm <- as.data.frame(tprefm)
tprefm
str(tprefm)

# STEP 05 :- Extracting Columns Names from Dataset
col_names = colnames(pizza_data)
col_names


# STEP 06 :- Calculating Utilities Value for Dataset
install.packages("conjoint")
library(conjoint)
caUtilities(y=tprefm[1,], x=pizza_data, z=col_names)

# STEP 07 :- Applying Conjoint On Data
conjoint_model = Conjoint(y=tprefm , x = pizza_data , z = col_names)

##Conclusion :-
#1) The higher the utility value, the more importance that the
#customer places on that attribute’s level.
#2) Numerical value for attribute
#Brand :- 22 Price :- 19 Weight :- 20 Crust :- 10 Cheese : - 12
#Size :- 12 Toppings :- 7 Spicy :- 13
##3) Values To Observe from dataset
#Residual standard error: 4,513 on 145 degrees of freedom
#Multiple R-squared: 0,1067, Adjusted R-squared: 0,02043 
#F-statistic: 1,237 on 14 and 145 DF, p-value: 0,2554)
#4) Average importance of factors (attributes):"
#20,36 20,65 19,02 8,72 6,76 7,73 7,17 9,59
#[1] Sum of average importance: 100
#5) Brand is most important factor as we observe on conjoint plot