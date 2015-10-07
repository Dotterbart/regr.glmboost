#' @export
makeRLearner.regr.glmboost = function() {
  makeRLearnerRegr(
    cl = "regr.glmboost",
    package = "mboost",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "family", default = mboost::Gaussian(), values = list(Gaussian = mboost::Gaussian())),
#     weitere families?
      makeIntegerLearnerParam(id = "mstop", default = 100L, lower = 1L),
      makeNumericLearnerParam(id = "nu", default = 0.1, lower = 0, upper = 1),
      makeDiscreteLearnerParam(id = "risk", values = c("inbag", "oobag", "none")),
      makeLogicalLearnerParam(id = "stopintern", default = FALSE),
      makeLogicalLearnerParam(id = "center", default = FALSE),
      makeLogicalLearnerParam(id = "trace", default = FALSE),
#     tunable = FALSE rausgenommen fuer trace
      makeDiscreteLearnerParam(id = "m", default = "mstop", values = c("mstop", "cv", "aic"))
      ),
    par.vals = list(family = mboost::Gaussian(), m = "mstop"),
    properties = c("numerics", "factors", "weights"),
#    name = "Boosting for GLMs",
#    short.name = "glmbst",
    note = paste(
      "`family` has been set to `Gaussian()` by default.",
      "Maximum number of boosting iterations is set via 'mstop', the actual number used for prediction is controlled by 'm'."
    )
  )
}
#' @export
trainLearner.regr.glmboost = function(.learner, .task, .subset, .weights = NULL, mstop, nu, m, risk, ...) {
#  Parameter "m" nicht mehr per par.vals definiert 
  ctrl = learnerArgsToControl(mboost::boost_control, mstop, nu, risk)
#  ctrl = mboost::boost_control(mstop, nu, risk)  
  d = getTaskData(.task, .subset)
#  if (.learner$predict.type == "prob") {
#    td = getTaskDescription(.task)
#    levs = c(td$negative, td$positive)
#    d[, getTaskTargetNames(.task)] = factor(d[, getTaskTargetNames(.task)], levs)
#  }
  f = getTaskFormula(.task)
  if (is.null(.weights)) {
    model = mboost::glmboost(f, data = d, control = ctrl, ...)
  } else {
    model = mboost::glmboost(f, data = d, control = ctrl, weights = .weights, ...)
  }
  if (m == "cv") {
    mboost::mstop(model) = mboost::mstop(mboost::cvrisk(model, papply = lapply))
  } else if (m == "aic") {
    mboost::mstop(model) = mboost::mstop(AIC(model, method = "classical"))
  }
  model
}
#' @export
predictLearner.regr.glmboost = function(.learner, .model, .newdata, ...) {
#  type = ifelse(.learner$predict.type == "response", "class", "response")
  p = predict(.model$learner.model, newdata = .newdata, ...) # type entfernt
#  if (.learner$predict.type == "prob") {
#    td = .model$task.desc
#    p = p[, 1L]
#    levs = c(td$negative, td$positive)
#    y = propVectorToMatrix(p, levs)
#  } else {
  return(as.vector(p))
#  }
}