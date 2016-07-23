rm(list=ls())

spamdata <- read.table("~/Documents/2015fall/STA5168/PS4/spam.data.txt",sep=",", header=T)
trndata <-  read.table("~/Documents/2015fall/STA5168/PS4/spam.train.txt", sep=",")
tstdata <-  read.table("~/Documents/2015fall/STA5168/PS4/spam.test.txt", sep=",")
names(spamdata) <- c("make", "address", "all", "3d", "our", "over", "remove", "internet", "order", "mail", "receive", "will", "people", "report", "addresses", "free", "business", "email", "you", "credit", "your", "font", "000", "money", "hp", "hpl", "george", "650", "lab", "labs", "telnet", "857", "data", "415", "85", "technology", "1999", "parts", "pm", "direct", "cs", "meeting", "original", "project", "re", "edu", "table", "conference", ";", "(", "[", "!", "$", "#", "capave", "caplongest", "captotal", "CLS")
names(trndata) <- names(spamdata)
names(tstdata) <- names(spamdata)


y <- (trndata[,58])
y <- (as.factor(y))

x <- scale(trndata[,1:57])

means <- attr(x, "scaled:center")
stds <- attr(x, "scaled:scale")


newx <- scale(tstdata[,1:57], center=means, scale=stds)
newy <- tstdata[,58]

data = data.frame(y, x)
newdata = data.frame(newy, newx)


require(e1071)
require(glmnet)


# GLM (plain)
gm <- glm(y~., data, family=binomial)
summary(gm <- glm(y~., data, family=binomial))
pred <- predict(gm, newdata, type='response', ci.fit= T, se.fit = T, conf.level=.95)
miscls <- pred[round(pred$fit) != newy]
sum(round(miscls) !=1)
(misclsErrs = sum( round(pred$fit) != newy ))

# L2-penalized GLM (no tuning, multiple models)
logistic.l2 = glmnet(x=x, y=y, family="binomial", alpha = 0, standardize = TRUE, lambda.min.ratio=1e-6)
pred = predict(logistic.l2, newx=as.matrix(newx), type="response")
dim(pred) # prediction errors for 100 models
apply(pred, 2, function(x) sum( round(x) != newy ))

# L2-penalized GLM (5-fold CV tuning)
logistic.l2 = cv.glmnet(x, y, family="binomial", nfolds=5, alpha = 0, standardize = TRUE, lambda.min.ratio=1e-5)
pred = predict(logistic.l2, newx=as.matrix(newx), s="lambda.min", type="response")
dim(pred) ## prediction errors for 1 model with lambda.min
(misclsErrs = sum( round(pred) != newy ))
logistic.l2$lambda.min
plot(logistic.l2)


# What about using a fusion of L1 and L2, say alpha=.4?
logistic.l2 = cv.glmnet(x, y, family="binomial", nfolds=5, alpha = 0.4, standardize = TRUE)
pred = predict(logistic.l2, newx=as.matrix(newx), s="lambda.min", type="response")
(misclsErrs = sum( round(pred) != newy ))   # What is your conclusion?

# L1-penalized GLM (with tuning)
logistic.l1 = cv.glmnet(x, y, family="binomial", nfolds=5, alpha = 1, standardize = TRUE)
pred = predict(logistic.l1, newx=as.matrix(newx), s="lambda.min", type="response")
miscls <- pred[round(pred) != newy]
sum(round(miscls) !=1)

l1coeffs = coef(logistic.l1)

plot(1:nrow(l1coeffs), abs( l1coeffs[,1] ),  xlab = "Predictors", ylab= "Coeff", xaxt="n")
text(1:nrow(l1coeffs), par("usr")[3] - 0.05, srt=90, adj=1, labels=rownames(l1coeffs), xpd=T, cex=.8)
# Some are zeros, e.g, people, report, technology, etc. can be removed




#SVM (default parameters)
svmmodel = svm(x=x, y = y, scale = TRUE, kernel = "linear")   #"radial") #
pred = predict(svmmodel, newdata=as.matrix(newx) )
(misclsErrs = sum( pred != newy ))

svmmodel<-svm(trnX, factor(trnY), type='C-classification', kernel='linear')
#svmmodel<-svm(trnX, factor(trnY), type='C-classification')
#svmmodel<-svm(trnX, factor(trnY), type='C-classification', kernel='sigmoid')
##### test set
pred<-predict(svmmodel, tstX)
print(tstSVMCfMat<-as.matrix(table(pred, factor(tstY))))
print(1-sum(diag(tstSVMCfMat))/length(tstY))

library(mgcv) # library(gam)
gm2 = gam(death ~ s(time) + pm10median + so2median + s(o3median, tmpd), data=chicago, family=poisson)
plot(gm2$coefficients)
