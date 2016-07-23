rm(list=ls())
library(rpart)
housetype = read.table("housetype.data.txt", sep=",")
names(housetype) = c( "TYPE","SEX","MAR","AGE","EDU","OCCU","INCOME","LONG",
                       "DUAL","PERS","PERSu18","HOUSE","ETHNIC","LANG")

housetype$SEX = as.factor(housetype$SEX)
housetype$MAR = as.factor(housetype$MAR)
housetype$EDU = as.factor(housetype$EDU) #
#housetype$EDU = ordered(housetype$EDU) #### !!!!!!!!!!!!! TRY TWO WAYS ?????

housetype$OCCU = as.factor(housetype$OCCU)
housetype$DUAL = as.factor(housetype$DUAL)
housetype$HOUSE = as.factor(housetype$HOUSE)
housetype$TYPE = as.factor(housetype$TYPE)
housetype$ETHNIC = as.factor(housetype$ETHNIC)
housetype$LANG = as.factor(housetype$LANG)


set.seed(100)
tree = rpart(TYPE~., data=housetype, parms=list(split='gini'), cp=5e-4)
plot(tree,uniform=T, compress=T)
text(tree,digits=3, use.n=F)
#post(tree)

plotcp(tree)
#printcp(tree)
t = tree$cptable
ind = min(which(t[,4]==min(t[,4])))
t[ind,]


#opttree = rpart(TYPE~., data=housetype, parms=list(split='gini'), cp=t[ind,1]+1e-5) ### Or use prune instead!
opttree = prune(tree, t[ind,1])
#opttree = rpart(TYPE~., data=housetype, parms=list(split='gini'), cp=5e-4)
plot(opttree, uniform=T)
text(opttree, use.n=T)

predyvec = predict(opttree)
maxvec = apply(predyvec,1,max)
predy = maxvec-
for(i in 1:length(maxvec))       predy[i] = min(which(predyvec[i,]==maxvec[i]))
table(housetype[,1], predy)
sum(predy!=housetype[,1])/length(predy)


############################################################################
library(randomForest)
sum(is.na(housetype))
rf.obj = randomForest(TYPE~., data=housetype, na.action=na.roughfix)
rf.obj2 = randomForest(TYPE~., data=housetype, na.action=na.omit)

housetype.imp = rfImpute(TYPE~., data=housetype, iter=2, ntree=200)  # can be slow
rf.obj3 = randomForest(TYPE~., data=housetype.imp)


importance(rf.obj3)
varImpPlot(rf.obj3)

partialPlot(rf.obj3, housetype.imp, HOUSE, which.class=2)
partialPlot(rf.obj3, housetype.imp, INCOME)

