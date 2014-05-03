patents = read.csv("patents.csv")
# create new variable where applications filed on or after KSR decision are "postKSR" and all other applications are "preKSR"
patents$ksr <- ifelse(as.Date(patents$filingDate) > as.Date("2007-04-30"), c("postKSR"), c("preKSR"))
# clean up data so that only applications with transaction histories are used
patents <- patents[patents$tranhistory ==1,]
# create new binary variable for whether an application was allowed using the "status" variable, whereby "Allowed" applications=1 and all others=0
patents$allowed <- ifelse(patents$status=="Allowed", c(1), c(0))
# create new binary variable for whether an application was filed before KSR or after KSR using "ksr", whereby "preKSR"=0 and "postKSR"=1
patents$postKSR <- ifelse(patents$ksr=="preKSR", c(0), c(1))
# for the first test, we will be looking at allowance factors for applications filed after KSR, therefore, as we did with the transaction history, we will narrow our data to applications for which postKSR=1
patents <- patents[patents$postKSR ==1,]
# we will generate a random forest and conditional tree model for allowance based on RCE, rejections, examinerInterviews, continuity, foreign, and filingDate
patents.allowed.train = glm(allowed ~ RCE + rejections + examinerInterviews + continuity + foreign + as.numeric(as.Date(filingDate)), family = binomial(logit), data = patents)
summary(patents.allowed.train)
library("randomForest")
patents.allowed.train.rf = randomForest(as.factor(allowed) ~ RCE + rejections + examinerInterviews + continuity + foreign + as.numeric(filingDate), data=patents, ntree=5000, importance=TRUE)
patents.allowed.train.rf
importance(patents.allowed.train.rf)
patents.allowed.train.ctree = ctree(as.factor(allowed) ~ RCE + rejections + examinerInterviews + continuity + foreign + as.numeric(as.Date(filingDate)), data=patents)
patents.allowed.train.ctree
plot(patents.allowed.train.ctree)
as.Date(14882)
# 2010-09-30