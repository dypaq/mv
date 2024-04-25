#factor analysis 

df=FactorAnalysis
summary(df)


colSums(is.na(df))

library(psych)

KMO(df) #it should be >7

#mydat <- df[, KMO(df)$MSAi>0.50] # Get rid of all variables with MSA < 0.50
#round( KMO(mydata)$MSA, 2 )

cortest.bartlett(df)


ev=eigen(cor(df))
ev$values


library(corrplot)
corrplot(cor(df),method='circle')

c=cor(df)

scree(c,pc=F)
fa.parallel(c, fa="fa")
fa.parallel(c, fm="ml")

fit = fa(c,4,rotate="promax")

fa.diagram(fit$loadings)



















