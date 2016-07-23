oridata1<-read.table("~/Documents/2015fall/STA5168/PS4/zip.train")
oridata1<-data.matrix(oridata1)
trn<-oridata1[oridata1[,1] == 3  | oridata1[,1] == 5 | oridata1[,1] == 8,]   #

oridata2<-read.table("~/Documents/2015fall/STA5168/PS4/zip.test")
oridata2<-data.matrix(oridata2)
tst<-oridata2[oridata2[,1] == 3  | oridata2[,1] == 5 | oridata2[,1] == 8,]   #

trnX <- trn[,-1]
trnY <- trn[,1]
tstX <- tst[,-1]
tstY <- tst[,1]


y <- (as.factor(trnY))
x <- scale(trnX)
means <- attr(x, "scaled:center")
stds <- attr(x, "scaled:scale")
newx <- scale(tstX, center=means, scale=stds)
newy <- as.factor(tstY)
data = data.frame(y, x)
newdata = data.frame(newy, newx)

################################################
n <- length(trnY)
# clsLbls <- c(3, 5, 8)  #
m <- length(tstY)

library("e1071")
library("glmnet")
#library('SparseM')

svmmodel<-svm(trnX, factor(trnY), type='C-classification', kernel='linear')
#svmmodel<-svm(trnX, factor(trnY), type='C-classification')
#svmmodel<-svm(trnX, factor(trnY), type='C-classification', kernel='sigmoid')
##### test set
pred<-predict(svmmodel, tstX)
print(tstSVMCfMat<-as.matrix(table(pred, factor(tstY))))
print(1-sum(diag(tstSVMCfMat))/length(tstY))


# two-class
(summary(gm <- glm(y~., data, family=binomial)))
pred <- predict(gm, newdata, type='response', ci.fit= T, se.fit = T, conf.level=.95)
(misclsErrs = sum( round(pred$fit) != as.numeric(as.factor(tstY))-1 ))

# three-class
# multinomial logistic regression
library(nnet)
gm <- multinom(y~., data))
(summary(gm)
pred <- predict(gm, newdata, type='class')
print(tstMat<-as.matrix(table(pred, factor(tstY))))
print(1-sum(diag(tstMat))/length(tstY))
