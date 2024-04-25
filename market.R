

















































# Step 1: Install all the required packages necessary for performing Market Basket Analysis
install.packages("arules") 
library(arules) 
install.packages("arulesViz") 
library(arulesViz)
install.packages("rattle") 
library(rattle) 
library(dplyr) 
library(ggplot2)
# Step 2: Load the Dataset.
df=read.csv("C:/ProgramData/Microsoft/Windows/Start
Menu/Programs/RStudio/groceries.csv")
head(df)
# Step 3: To show Unique Value.
n_distinct(df$Item.s)
# Step 4: Transform data into a transactional dataset.
transactions= read.transactions(file="C:/ProgramData/Microsoft/Windows/Start
Menu/Programs/RStudio/groceries.csv", format = "basket",sep=",", rm.duplicates = TRUE)
# Step 5: Plotting
# install color package of R
install.packages("RColorBrewer") 
library(RColorBrewer)

itemFrequencyPlot(transactions,
                  topN = 10,
                  col = brewer.pal(8,'Pastel2'),
                  horiz = TRUE,
                  main = 'Absolute item frequency')
# Step 6: Mine association rules with specified parameters.
rules <- apriori(transactions, parameter = list(support = 0.001, confidence = 0.5))
# Step 7: Inspect and summarize the resulting rules
inspect(rules)
summary(rules)
# Sort rules by lift in descending order.
rules_lift_sorted <- sort(rules, by = "confidence", decreasing = TRUE)
# Get the top 10 rules based on lift.
top_10_rules <- head(rules_lift_sorted, n = 10)
# Print the top 10 rules using the inspect function.
inspect(top_10_rules)
# Step 8: Plot the top 10 rules.
plot(top_10_rules, method = "graph")
