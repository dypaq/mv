library(nnet)
library(factoextra)
library(psych)
library(GPArotation)
library(dplyr)
library(ggcorrplot)
# Loading DataSet
data <- read.csv('C:/Users/Qureshi/Downloads/ap_train.csv')

# Remove any missing values
data_clean <- na.omit(data)
rated_values=data_clean[,9:21]

# Calculate the correlation matrix
cor_matrix <- cor(rated_values)
ggcorrplot(cor_matrix,  title = "Correlation Plot")

# Calculate eigenvalues
eigenvalues <- eigen(cor_matrix)$values

# Scree plot to visualize eigenvalues
plot(eigenvalues, type = "b", main = "Scree Plot")
# Perform factor analysis with the determined number of factors
factor_model <- factanal(rated_values, factors = 3,scores = "regression")
factor_scores=factor_model$scores
factor_scores
df <- subset(data_clean, select = -c(Inflight.wifi.service, Departure.Arrival.time.convenient,
                                     Ease.of.Online.booking, Gate.location, Food.and.drink,
                                     Online.boarding, Seat.comfort, Inflight.entertainment,
                                     On.board.service, Leg.room.service, Baggage.handling,
                                     Checkin.service, Inflight.service, Cleanliness))
str(df)

# Assuming you already have 'df' with columns removed and 'factor_scores'

# Bind the factor scores with the subsetted dataframe
df_with_factor_scores <- cbind(df, factor_scores)
df_with_factor_scores$satisfaction = as.factor(df_with_factor_scores$satisfaction)

# View the resulting dataframe
View(df_with_factor_scores)
str(df_with_factor_scores)

# Fit multinomial logistic regression model
#model <- multinom(satisfaction ~ Gender + Customer.Type + Age + Type.of.Travel + Class + Flight.Distance + Departure.Delay.in.Minutes + Arrival.Delay.in.Minutes + Factor1 + Factor2 + Factor3, data = df_with_factor_scores)
# Summary of the model
#summary(model)

### Testing Model ###
test_data <- read.csv("C:/Users/Qureshi/Downloads/ap_test.csv")
# Remove any missing values
data_clean <- na.omit(test_data)
rated_values=data_clean[,9:21]
factor_model <- factanal(rated_values, factors = 3,scores = "regression")
factor_scores=factor_model$scores
df <- subset(data_clean, select = -c(Inflight.wifi.service, Departure.Arrival.time.convenient,
                                     Ease.of.Online.booking, Gate.location, Food.and.drink,
                                     Online.boarding, Seat.comfort, Inflight.entertainment,
                                     On.board.service, Leg.room.service, Baggage.handling,
                                     Checkin.service, Inflight.service, Cleanliness))
df_with_factor_scores <- cbind(df, factor_scores)
df_with_factor_scores$satisfaction = as.factor(df_with_factor_scores$satisfaction)

# Predicting on the dataframe with factor scores
predictions <- predict(model, newdata = df_with_factor_scores, type = "response")
# Create confusion matrix
conf_matrix <- table(df_with_factor_scores$satisfaction, predictions)
# Print confusion matrix
cat("Confusion Matrix:\n")
print(conf_matrix)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
# Print accuracy
cat("Accuracy on Test Data:", accuracy, "\n")



# Fit binomial logistic regression model
binomial_model <- glm(satisfaction ~ Gender + Customer.Type + Age + Type.of.Travel + Class + Flight.Distance + Departure.Delay.in.Minutes + Arrival.Delay.in.Minutes + Factor1 + Factor2 + Factor3, 
                      family = binomial, 
                      data = df_with_factor_scores)

# Summary of the model
summary(binomial_model)

### Testing Model ###
# Predicting on the test data
test_predictions <- predict(binomial_model, newdata = df_with_factor_scores, type = "response")
# Converting probabilities to binary predictions
binary_predictions <- ifelse(test_predictions > 0.5, "Satisfied", "Dissatisfied")

# Create confusion matrix
conf_matrix_binomial <- table(df_with_factor_scores$satisfaction, binary_predictions)
# Print confusion matrix
cat("Confusion Matrix:\n")
print(conf_matrix_binomial)

# Calculate accuracy
accuracy_binomial <- sum(diag(conf_matrix_binomial)) / sum(conf_matrix_binomial)
# Print accuracy
cat("Accuracy on Test Data:", accuracy_binomial, "\n")
