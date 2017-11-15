library(arules)
data(Groceries)

itemsets<-apriori(Groceries,parameter=list(supp=0.03,target="frequent itemsets"))

inspect(itemsets)

rules<-apriori(Groceries,parameter=list(supp=0.01,conf=0.5,target="rules"))
