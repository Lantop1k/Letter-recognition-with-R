#Installation of Library Packages

#install.packages('rpart')
#install.packages('ggplot2')
#install.packages('caret')

library(rpart)
library(rpart.plot)
library(caret)

set.seed(128)

#read the dataset
df <- read.table('C:\\Users\\user\\Desktop\\Freelancing\\ML R\\letter-recognition.data',header=FALSE,sep=",")

colnames(df)

#Preparing the training and testing set 
train = sample(1:nrow(df), ceiling(nrow(df)*0.75),replace = FALSE) 

traindata=df[train,]
test=df[-train,]

testlabels=test["V1"]

#Train decision tree algorithm
dctree = rpart(V1 ~ V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17, 
                 data=traindata,parms=list(split="gini"),
                control = list(minsplit = 8, maxdepth = 100, cp = 0.00001),
                method="class")

#Model Performance Metrics
pred = predict(dctree, test,type="class")

cmf=confusionMatrix(as.factor(testlabels$V1),pred)

c=cmf[4][[1]]

sensitivity=mean(c[,1])
specificity=mean(c[,2])
balance_accuracy=mean(c[,11])
kappa=cmf[3][[1]]["Kappa"]

print("sensitivity")
sensitivity

print("specificity")
specificity

print("balance_accuracy")
balance_accuracy

print("kappa")
kappa