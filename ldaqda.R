# LDA
install.packages('MASS')
library(MASS)
install.packages('ggplot2')
library(ggplot2)
install.packages('caret')
library(caret)
install.packages('tidyverse')
library(tidyverse)
install.packages('car')
#Breast cancer data:
data= read.csv("C:/Users/Qureshi/OneDrive/Desktop/data.csv")
data
str(data)
colSums(is.na(data))
data$diagnosis=as.factor(data$diagnosis)
str(data)
data=data[2:32]
b=boxplot(data)
data$diagnosis
str(data)
model <- lda(diagnosis ~ ., data = data)
model
model_values=predict(model)
model_values
head(model_values)
names(model_values)
ldahist(model_values$x[,1],g=data$diagnosis)
plot(model_values$x,col=(data$diagnosis),pch=19)
#plot(model_values$x[,1],model_values$posterior[,2])
#text(model_values$x[,1],model_values$posterior[,2],data$diagnosis,cex=0.7,pos=4,col='red')
C=confusionMatrix(model_values$class,data$diagnosis)
C

#QDA
names(data)
model_qda=qda(diagnosis~.,data=data)
model_qda
model_qda_values=predict(model_qda)
model_qda_values

names(model_qda_values)
C_qda=confusionMatrix(model_qda_values$class,data$diagnosis)
C_qda
# Make predictions on the training set
predictions_lda <- as.numeric(predict(model)$class)
predictions_qda <- as.numeric(predict(model_qda)$class)
library(pROC)
# Create ROC curves
roc_lda <- roc(data$diagnosis, predictions_lda)
roc_qda <- roc(data$diagnosis, predictions_qda)

# Plot ROC curves
plot(roc_lda, col = "blue", main = "ROC Curves", lwd = 2, col.main = "black", ylim = c(0, 1), xlim = c(0, 1), xlab = "False Positive Rate", ylab = "True Positive Rate")
lines(roc_qda, col = "red", lwd = 2)

# Add legend
legend("bottomright", legend = c("LDA", "QDA"), col = c("blue", "red"), lwd = 2)
auc_lda <- auc(roc_lda)
auc_qda <- auc(roc_qda)
print(auc_qda)
print(auc_lda)
#for diabetes dataset
df=read.csv("C:/Users/Qureshi/OneDrive/Desktop/diabetes.csv")
str(df)
colSums(is.na(df))
df$Outcome=as.factor(df$Outcome)
df$Pregnancies=as.numeric(df$Pregnancies)
df$Glucose=as.numeric(df$Glucose)
df$BloodPressure=as.numeric(df$BloodPressure)
df$SkinThickness=as.numeric(df$SkinThickness)
df$Insulin=as.numeric(df$Insulin)
df$Age=as.numeric(df$Age)
str(df)
b=boxplot(df)
model <- lda(Outcome ~ ., data = df)
model
model_values=predict(model)
model_values
names(model_values)
df$Outcome
model_values$class
model_values$x
model_values$posterior
#ldahist(model_values$x[,1],g=df$Outcome)
# Save plot to file with larger size
pdf("plot_new.pdf", width = 10, height = 6)  # Adjust width and height as needed
ldahist(model_values$x[,1], g = df$Outcome)
dev.off()  # Close the PDF device

plot(model_values$class,col=(df$Outcome),pch=19)
text(model_values$x,df$Outcome,cex=0.7,pos=4,col='red')
#plot(model_values$x[,1],model_values$posterior[,2])
#text(model_values$x[,1],model_values$posterior[,2],df$Outcome,cex=0.7,pos=4,col='red')
C=confusionMatrix(model_values$class,df$Outcome)
C

#QDA
names(df)
model_qda=qda(Outcome~.,data=df)
model_qda
model_qda_values=predict(model_qda)
model_qda_values

names(model_qda_values)
C_qda=confusionMatrix(model_qda_values$class,df$Outcome)
C_qda
'''fpr step wise discriminant aalysis
library(MASS)
library(stats)
formula <- Outcome ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age
formula
#model_sda=stepclass(formula,data=df,direction=both,steps=100)
#model_sda <- step(model, formula = Outcome ~ ., data = df, direction = "both", steps = 100)

# Fit a linear regression model
model <- lm(Outcome ~ ., data = df)

# Perform stepwise variable selection
model_sda <- stepAIC(model, direction = "both", steps = 100)'''

library(MASS)
library(pROC)

# Make predictions on the training set
predictions_lda <- as.numeric(predict(model)$class == "1")
predictions_qda <- as.numeric(predict(model_qda)$class == "1")

# Create ROC curves
roc_lda <- roc(df$Outcome, predictions_lda)
roc_qda <- roc(df$Outcome, predictions_qda)

# Plot ROC curves
plot(roc_lda, col = "blue", main = "ROC Curves", lwd = 2, col.main = "black", ylim = c(0, 1), xlim = c(0, 1), xlab = "False Positive Rate", ylab = "True Positive Rate")
lines(roc_qda, col = "red", lwd = 2)

# Add legend
legend("bottomright", legend = c("LDA", "QDA"), col = c("blue", "red"), lwd = 2)

auc_lda <- auc(roc_lda)
auc_qda <- auc(roc_qda)
print(auc_qda)
print(auc_lda)
