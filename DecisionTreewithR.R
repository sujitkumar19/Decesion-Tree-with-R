#Prepare data
Cardiotocographic <- read_csv("E:/data analysis/Decesion tree with R/Cardiotocographic.csv")
data<-Cardiotocographic
str(data)
data$NSP<- factor(data$NSP)

#Partition data into training and validation datasets
set.seed(1234)
pd <-sample(2,nrow(data),replace = TRUE,prob = c(0.8,0.2))
train<-data[pd==1,]
test <- data[pd==2,]

#Decision tree with party
#Install party 
library(party)
tree <- ctree(NSP~LB+AC+FM,data=train)
tree
plot(tree)

tree <-ctree(NSP~LB+AC+FM,data=train, controls = ctree_control(mincriterion = 0.99,minsplit = 500))
plot(tree)

#predict
predict(tree,test,type="prob")
#or
predict(tree,test)

#Decision with rpart
library(rpart)
tree1<- rpart(NSP~LB+AC+FM,train)
library(rpart.plot)
rpart.plot(tree1)
rpart.plot(tree1,extra=2)

#prediction

predict(tree1,test)

#Misclassfication error for 'train' data
tab<-table(predict(tree),train$NSP)
print(tab)
1-sum(diag(tab))/sum(tab)

#Misclassfication error for 'test' data
tabtest <- predict(tree,test)
tab<-table(tabtest,test$NSP)
print(tab)
1-sum(diag(tab))/sum(tab)


