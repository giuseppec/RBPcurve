library(RBPcurve)
mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
head(mydata)

mylogit <- glm(admit ~ ., data = mydata, family = "binomial")

y <- mydata$admit
pred <- predict(mylogit, type="response")

RBPcurve(pred, y, conditionalAxis = T)
addPrevalence(pred,y)
addGoodCalib(pred,y)
addWellCalib(pred,y)
addRates(pred,y)

#
RBPcurve(pred, y, conditionalAxis = T)
addPEV(pred,y)
