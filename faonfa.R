install.packages("psych",dependencies = T) 
library(psych)

install.packages("corr")
library(corrr)

install.packages("corrplot")
library(corrplot)

install.packages("GPArotation") 
library(GPArotation)

install.packages("visdat")
library(visdat)

install.packages("ggplot")
library(ggplot2)

install.packages("gridExtra")
library(gridExtra)

library(tidyverse)
install.packages("summarytools")
library(summarytools)


mydata=FactorAnalysis
summary(mydata)
str(mydata)

colSums(is.na(mydata))
#df=na.omit(your_data_name) code to remove null values


#EDA

vis_dat(mydata, warn_large_data=FALSE, large_data_size=130000)

c=cor(mydata)
c

corrplot(cor(mydata),method = 'circle')

# Histograms for each variable
mydata %>%
  gather(key = "variable", value = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 10) +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Histograms of Variables")

# Boxplots for each variable
mydata %>%
  gather(key = "variable", value = "value") %>%
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplots of Variables")

#kmo test
#KMO value above 0.6 is considered acceptable, while values above 0.7 or 0.8 are preferable.

library(psych)
KMO(mydata)

#if less then 5 then use below 3 lines
#mydat <- mydata[, KMO(mydata)$MSAi>0.50] # Get rid of all variables with MSA < 0.50
#mydata <- mydat
#round( KMO(mydata)$MSA, 2 )


#Bartlett’s test for sphericity
#In this test, a significant p-value (usually below 0.05) indicates that the variables 
#are suitable for structure detection and thus appropriate for factor analysis.

library(psych)
cortest.bartlett(mydata)

#Determine Number of Factors to Extract

ev <- eigen(cor(mydata)) # get eigenvalues
ev$values

scree(c, pc=FALSE)

fa.parallel(c, fa="fa")

#Extract (and rotate) factors
# You are essentially searching for a clearer association between individual factors and the various variables

Nfacs <- 5  # This is for four factors. You can change this as needed.

fit <- factanal(mydata, Nfacs, rotation="promax")
print(fit)

load <- fit$loadings[,1:2]
plot(load,type="n") # set up plot
text(load,labels=names(mydata),cex=.7)


loads <- fit$loadings
fa.diagram(loads)




