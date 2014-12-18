#load_all()

mydata = read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
head(mydata)
set.seed(1)
#mydata = mydata[sample(1:nrow(mydata), 50),]

mylogit <- glm(admit ~ ., data = mydata, family = "binomial")

y = mydata$admit
pred = predict(mylogit, type="response")

obj = makeRBPObj(pred, y)
plotRBPcurve(obj, cond.axis = T, type = "b")

addPrevalence(obj)
addGoodCalib(obj)
addRates(obj)
addPEV(obj)
(mat = addWellCalib(obj))



