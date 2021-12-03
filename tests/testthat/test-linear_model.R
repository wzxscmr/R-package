test_that("linear_model works", {
  #check coefficients of the linear model
  X = cbind(mtcars$cyl, mtcars$hp, mtcars$drat, mtcars$wt)
  Y = mtcars$mpg
  expect_equal( as.vector((linear_model(X,Y))$coefficients),
                as.vector(lm(formula = mpg ~ cyl + hp + drat + wt, data = mtcars)$coefficients))

  #check residuals of the linear model
  expect_equal( as.vector((linear_model(X,Y))$residuals),
                as.vector(lm(formula = mpg ~ cyl + hp + drat + wt, data = mtcars)$residuals))
  xb = summary(lm(formula = mpg ~ cyl + hp + drat + wt, data = mtcars))

  #check R_square of the linear model
  expect_equal( as.vector((linear_model(X,Y))$R_square),
                as.vector(xb$r.squared))

  #check F statistic of the linear model
  expect_equal( round((linear_model(X,Y))$F_stat,3),
                round(as.vector(xb$fstatistic["value"]),3))


})
