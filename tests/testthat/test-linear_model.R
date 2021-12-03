test_that("linear_model works", {
  #check coefficients
  expect_equal( (linear_model(c("wt","hp","vs"),"mpg", data=mtcars))$coefficients,
                (summary(lm(formula = mpg ~ wt + hp +vs, data = mtcars)))$coefficients )
})
