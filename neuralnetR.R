
df <- read.table('C:\\Users\\user\\Desktop\\Freelancing\\ML R\\letter-recognition.data',header=FALSE,sep=",")
library(cvms)
library(neuralnet)
library(ROCR)
library(fastDummies)

set.seed(0)

colnames(df)

alphabet=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N",
           "O","P","R","S","T","Q","U","V","W","X","Y","Z")

for  (i in 1:26){
      df["V1"]=replace(df["V1"],df["V1"]==alphabet[i],i)
}

df=model.matrix(~.,df)
#df=dummy_cols(df)
train = sample(1:nrow(df), nrow(df)*0.8) 

traindata=df[train,]
test=df[-train,]

testlabels=test["V1"]


net = neuralnet(V1_1+V1_2+V1_3+V1_4+V1_5+V1_6+V1_7+V1_8+V1_9+V1_10+
                V1_11+V1_12+V1_13+V1_14+V1_15+V1_16+V1_17+V1_18+V1_19+V1_20+V1_21+V1_22+V1_23+V1_24+V1_25+V1_26 
                ~ V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17,stepmax=10000,
           data=traindata,hidden =c(25,10,5),linear.output=FALSE,lifesign="minimal",threshold = 0.1)

plot(net)


pred = compute(net, test)
predframe=data.frame(pred)

pred = predict(dctree, test,type="class")

cmf=confusionMatrix(as.factor(testlabels$V1),pred)

print(cmf)

