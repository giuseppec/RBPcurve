library(RBPcurve)
mydata = read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
head(mydata)

mylogit = glm(admit ~ ., data = mydata, family = "binomial")

y = mydata$admit
pred = predict(mylogit, type = "response")

obj = makeRBPObj(pred, y)
plotRBPcurve(obj, conditionalAxis = TRUE)
addPrevalence(obj)
addGoodCalib(obj)
addWellCalib(obj)
addRates(obj)

plotRBPcurve(obj, conditionalAxis = TRUE)
addPEV(obj)

