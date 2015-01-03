library(mlr)
load_all()

# should later become function in mlr?
# do some add* calls with sensible default, rest can be done by user with later add* calls
myplot1 = function(pred, ...) {
  p = getProbabilities(pred)
  obj = makeRBPObj(p, pred$data$truth, positive = pred$desc$positive)
  plotRBPCurve(obj, ...)
}


myplot2 = function(lrn, task) {
  lrn = mlr:::checkLearnerClassif(lrn)
  lrn = setPredictType(lrn, "prob")
  r = holdout(lrn, task)
  myplot1(r$pred)
}

# lrn = makeLearner("classif.logreg", predict.type = "prob")
# task = sonar.task
# r = holdout(lrn, task)
# myplot1(r$pred)

myplot2("classif.rpart", sonar.task)
