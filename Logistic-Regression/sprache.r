rm(list=ls())
sprache <- read.table("sprache.asc", header=T); # attach(sprache)

# Remove 'nummer' and 'wort'
summary(gm1 <- glm(sprecher ~.-nummer-wort,data = sprache, family=binomial))
    # warnings! (NOT BAD at all!)
# Try to remove 'segtyp'
summary(gm2 <- glm(sprecher ~.-nummer-wort-segtyp,data = sprache, family=binomial))
anova(gm2,gm1) #p-val should be very large # 1-pchisq(anova(gm2,gm1)[2,4], anova(gm2,gm1)[2,3])
gm1$aic
gm2$aic


gm0 = glm(sprecher ~anteil+meanf0+slopef0+meanf1+meanf2+meanf3+segtyp,data = sprache, subset =(wort != 'Aluminium'), family=binomial)
pred0 = predict(gm0, sprache[sprache$wort == 'Aluminium', -c(1,2,3)], type='response', ci.fit= T, se.fit = T, conf.level=.95)
table(round(pred0$fit), sprache$sprecher[sprache$wort == 'Aluminium'])  # This is usually called confusion table


gm0 = glm(sprecher ~anteil+meanf0+slopef0+meanf1+meanf2+meanf3+segtyp,data = sprache, subset =(wort != 'Mumie'), family=binomial)
pred0 = predict(gm0, sprache[sprache$wort == 'Mumie', -c(1,2,3)], type='response', ci.fit= T, se.fit = T, conf.level=.95)
table(round(pred0$fit), sprache$sprecher[sprache$wort == 'Mumie'])  # possibly new levels arise in the test data

subdata = sprache[sprache$wort != 'Mumie', ]
subdata$segtyp; sprache$segtyp
# None of the training observations (with wort!='Mumie') has segtype == u
# What should be its coefficient estimate?  How would you handle this?


# I am setting the coefficient estimate for the new predictor to be zero.
gm0 = glm(sprecher ~anteil+meanf0+slopef0+meanf1+meanf2+meanf3+segtyp-1,data = sprache, subset =(wort != 'Mumie'), family=binomial)
# The coefficient estimates are directly associated with the levels appearing in the training data
g.fitted = as.matrix(sprache[sprache$wort == 'Mumie', -c(1,2,3,5)]) %*% as.matrix(gm0$coeff[1:6], ncol=1)
# So I removed the new variable and kept the coefficient estimates for anteil+meanf0+slopef0+meanf1+meanf2+meanf3
pred0 = 1/(1+exp(-g.fitted))
table(round(pred0), sprache$sprecher[sprache$wort == 'Mumie'])

# But since the levels in the test data never appear in the training data, it's better to delete this segtyp variable!

# Let's remove the variable segtyp
allwords<-levels(sprache$wort)
misclsErrs = rep(NA, length(allwords))
names(misclsErrs) = allwords
for(i in 1:(length(allwords)))
{
    (summary(gm <- glm(sprecher~anteil+meanf0+slopef0+meanf1+meanf2+meanf3,data = sprache, subset =(wort != allwords[i]), family=binomial)))
    pred <- predict(gm, sprache[sprache$wort == allwords[i], ], type='response', ci.fit= T, se.fit = T, conf.level=.95)
    misclsErrs[i] = sum( round(pred$fit) != sprache$sprecher[sprache$wort == allwords[i]] )
}

print(misclsErrs)    # not bad