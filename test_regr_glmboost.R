# einfache Variante, geklaut von test_regr_lm:

context("regr_glmboost")
test_that("regr_glmboost", {
  pars = list(regr.formula, data = regr.train)
  set.seed(getOption("mlr.debug.seed"))
  m = do.call(glmboost, pars)
  p = predict(m, newdata = regr.test)
  testSimple("regr.lm", regr.df, regr.target, regr.train.inds, p)
})

# Variante mit testSimpleParsets, geklaut bei test_classif_glmboost und angepasst:

context("regr_glmboost")
test_that("regr_glmboost", {
  requireNamespace("mboost")
  parset.list1 = list(
    list(family=mboost::Gaussian(), control=mboost::boost_control(nu=0.03)),
    list(family=mboost::Gaussian(), control=mboost::boost_control(mstop=600), center=TRUE)
  )
  parset.list2 = list(
    list(family=mboost::Gaussian(), nu=0.03),
    list(family=mboost::Gaussian(), mstop=600, center=TRUE)
  )
  old.predicts.list = list()
  for (i in 1:length(parset.list1)) {
    parset = parset.list1[[i]]
    pars = list(regr.formula, data = regr.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(mboost::glmboost, pars)
    set.seed(getOption("mlr.debug.seed"))
    old.predicts.list[[i]] = predict(m, newdata=regr.test)
  }
  testSimpleParsets("regr.glmboost", regr.df, regr.target, regr.train.inds, old.predicts.list, parset.list2)
})