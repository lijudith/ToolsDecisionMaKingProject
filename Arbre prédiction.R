# regression logistic - arbre brouillon 

getwd()
mydata=read.csv("data_set.csv")%>%tibble()
mydata

library(data.table)
library(tidyverse)
library(ggplot2) 
library(ggcorrplot)
library(mice)
library(VIM)
library(caTools)
library(ROCR)


### Arbre de décision ############################################
library(rpart)
library(rpart.plot)
library(rattle)

# On sélectionne les candidats déjà sélectionné et on regarde la répartition des genres 
data0=mydata%>%filter(call==1)
data0=as.data.frame(data0)
summary(data0)
for (i in c("sex","race","city","kind","ownership","school_req","exp_min_req","call")) 
{
  data0[,i]=as.factor(data0[,i])
}
data0=select(data0,-c(X,first_name)) # not need 
summary(data0)
#str(data0)


# splip into train and test data 
split=sample.split(data0$sex,SplitRatio=0.75)
cTrain=subset(data0,split == TRUE)
cTest=subset(data0,split == FALSE)
#Construction of the tree
tree1=rpart(sex ~ ., data = data0, method = "class", control=rpart.control(minsplit=5,cp=0))
# minimization of the errors 
plotcp(tree1)
##optimal cp
print(tree1$cptable[which.min(tree1$cptable[,4]),1])
#Pruning the tree with the optimal cp
tree1.opt=prune(tree1,cp=tree1$cptablex[which.min(tree1$cptable[,4]),1])
# tree optimal
prp(tree1.opt,extra=1)
fancyRpartPlot(tree1.opt)
#Model prediction on test data
cTest.predict=predict(tree1.opt,newdata=cTest,type = "class")
#Confusion Matrix
mc=table(cTest$sex,cTest.predict)
mc
#Classification error
1.0-(mc[1,1]+mc[2,2])/sum(mc)
#Prediction rate
mc[2,2]/sum(mc[2,])


# Regression logistique #######################

data1=mydata
data1=as.data.frame(data1)
summary(data1)
for (i in c("sex","race","city","kind","ownership","school_req","exp_min_req","call","first_name")) 
{
  data1[,i]=as.factor(data1[,i])
}


# splip into train and test data 
split=sample.split(data1$call,SplitRatio=0.75)
cTrain1=subset(data1,split == TRUE)
cTest1=subset(data1,split == FALSE)

# Correlation matrixin order to see with wich kind of variable more correlated ??
#corrp.mat = cor_pmat(cTrain1)
#ggcorrplot(cor(as.matrix(cTrain1)),p.mat = corrp.mat, insig="blank")


#model 
dataLog=glm(sex~.,family=binomial,data=data1)
summary(dataLog)
#Prediction 
predictTrain =predict(dataLog, type="response")
tapply(predictTrain, cTrain1$sex, mean)
# Find the best threshold 
ROCRpred = prediction(cTrain1, cTrain1$call)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0, 1,by=0.1), text.adj=c(-0.2, 1.7))
# interpretation 
table = table(cTrain1$call, predictTrain >0.5)
table
accu = (table[1,1] + table[2,2])/sum(table)
accu
sens = table[2,2]/(table[2,2]+table[1,1])
sens
spec = table[1,2]/(table[1,2]+table[2,1])
spec
# Validation avec test 
