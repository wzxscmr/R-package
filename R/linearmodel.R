#'Linear regression model
#'
#'@name linear_model
#'
#'This function is used to build multiple linear models. It can be used to carry out simple and multiple linear regression.
#'
#'@param X A vector or matrix (one or multiple columns) of predictors.
#'
#'@param Y A vector of outcome values (should be a continuous variable).
#'
#'@return A list of important attributes of the fitted linear model.
#'
#'@examples
#'data(mtcars)
#'linear_model(X= cbind(mtcars$cyl, mtcars$hp, mtcars$drat, mtcars$wt), Y = mtcars$mpg)
#'
#'@export
#'
linear_model = function(X,Y) {
  intercept = rep(1,nrow(X))
  X1 = cbind(intercept,X)
  if (sum(is.na(X1)) > 0 | sum(is.na(Y)) > 0){
    stop("There are missing data in X and Y")
  }
  if (nrow(X1) != length(Y)){
    stop("The dimensions of X and Y are different.")

  }
  if (nrow(X1) <= ncol(X1)){
    stop("There are too many parameters in the data")
  }
  if (det(t(X1) %*% X1) == 0){
    stop("X1 must be a full rank matrix")
  }

  beta_hat = solve(t(X1) %*% X1) %*% t(X1) %*% Y
  Y_fitted = X1 %*% beta_hat
  residuals = Y - Y_fitted

  SSE = sum((residuals)^2)
  SSY = sum((Y - mean(Y))^2)
  SSR = SSY - SSE
  MSE = SSE / (nrow(X1) - ncol(X1))
  MSR = SSR / (ncol(X1) - 1)
  R_square = SSR / SSY
  R_square_adjusted = 1 - MSE / (SSY / (nrow(X1)) - 1)

  cov_beta_hat = MSE * solve(t(X1) %*% X1)
  var_beta_hat = diag(cov_beta_hat)
  se_beta_hat = sqrt(var_beta_hat)
  t_stat = beta_hat / se_beta_hat
  pt_value = 2*( 1-pt(q = abs(t_stat), df = nrow(X1) - ncol(X1)) )

  F_stat = MSR / MSE
  pf_value = pf(F_stat, (ncol(X1) - 1), (nrow(X1) - ncol(X1)), lower.tail = FALSE)

  wzx=list(coefficients = beta_hat, residuals = residuals,
           SSE = SSE, SSR = SSR, MSE = MSE, R_square = R_square,
           R_square_adjusted = R_square_adjusted,
           t_stat=t_stat, pt_value=pt_value,
           F_stat=F_stat, pf_value=pf_value)
  return(wzx)
}

#'Independence diagnosis
#'
#'@name Independence
#'
#'This function is used to do the independence diagnosis of the linear model.
#'
#'@param X A vector or matrix (one or multiple columns) of predictors.
#'
#'@param Y A vector of outcome values (should be a continuous variable).
#'
#'@return The autocorrelation plots of the residuals.
#'
#'@examples
#'data(mtcars)
#'Independence(X= cbind(mtcars$cyl, mtcars$hp, mtcars$drat, mtcars$wt), Y = mtcars$mpg)
#'
#'@export
#'
Independence = function(X,Y){
  intercept = rep(1,nrow(X))
  X1 = cbind(intercept,X)
  beta_hat = solve(t(X1) %*% X1) %*% t(X1) %*% Y
  Y_fitted = X1 %*% beta_hat

  residuals = Y - Y_fitted
  index = c(1:length(residuals))
  par(mfrow=c(1,2))
  plot(residuals, index, main = "residuals versus i(index)")
  warning("use only if index i has a meaningful order, e.g. time")
  residuals_i = residuals[c(2:length(residuals))]
  residuals_i_1 = residuals[c(1:(length(residuals)-1))]
  plot(residuals_i, residuals_i_1, main = "residual_i versus residual_(i-1)")
}

#'Equal variance diagnosis
#'
#'@name Cons_variance
#'
#'This function is used to do the Equal variance diagnosis of the linear model.
#'
#'@param X A vector or matrix (one or multiple columns) of predictors.
#'
#'@param Y A vector of outcome values (should be a continuous variable).
#'
#'@return The plot of the residuals versus fitted Y.
#'
#'@examples
#'data(mtcars)
#'Cons_variance(X= cbind(mtcars$cyl, mtcars$hp, mtcars$drat, mtcars$wt), Y = mtcars$mpg)
#'
#'@export
#'
Cons_variance = function(X,Y){
  par(mfrow=c(1,1))
  intercept = rep(1,nrow(X))
  X1 = cbind(intercept,X)
  beta_hat = solve(t(X1) %*% X1) %*% t(X1) %*% Y
  Y_fitted = X1 %*% beta_hat

  residuals = Y - Y_fitted
  plot(residuals, Y_fitted, main = "residuals versus fitted Y")
  message("The plot should be a random scatter if homogeneity")
}

#'Normality diagnosis
#'
#'@name Normality
#'
#'This function is used to do the Normality diagnosis of the linear model.
#'
#'@param X A vector or matrix (one or multiple columns) of predictors.
#'
#'@param Y A vector of outcome values (should be a continuous variable).
#'
#'@return The histogram of the residuals.
#'
#'@examples
#'data(mtcars)
#'Normality(X= cbind(mtcars$cyl, mtcars$hp, mtcars$drat, mtcars$wt), Y = mtcars$mpg)
#'
#'@export
#'
Normality = function(X,Y){
  par(mfrow=c(1,1))
  intercept = rep(1,nrow(X))
  X1 = cbind(intercept,X)
  beta_hat = solve(t(X1) %*% X1) %*% t(X1) %*% Y
  Y_fitted = X1 %*% beta_hat

  residuals = Y - Y_fitted
  hist(residuals, main = "histogram of residuals")
}

#'Linearity diagnosis
#'
#'@name Linearity
#'
#'This function is used to do the Linearity diagnosis of the linear model.
#'
#'@param X A vector or matrix (one or multiple columns) of predictors.
#'
#'@param Y A vector of outcome values (should be a continuous variable).
#'
#'@return The partial regression plots of each predictors.
#'
#'@examples
#'data(mtcars)
#'Linearity(X= cbind(mtcars$cyl, mtcars$hp, mtcars$drat, mtcars$wt), Y = mtcars$mpg)
#'
#'@export
#'
Linearity = function(X,Y){
  intercept = rep(1,nrow(X))
  X1 = cbind(intercept,X)
  beta_hat = solve(t(X1) %*% X1) %*% t(X1) %*% Y
  Y_fitted = X1 %*% beta_hat

  residuals = Y - Y_fitted
  if (ncol(X1) == 2){
    par(mfrow=c(1,1))
    plot(residuals, X, main = "linearity of (X, Y) relationship")
  }
  else{
    par(mfrow=c(1,ncol(X)))
    for (i in (1:ncol(X))){
      Xnew = X1[,-i-1]
      Xnew2 = X1[,i+1]
      beta_hat_new = solve(t(Xnew) %*% Xnew) %*% t(Xnew) %*% Y
      Y_fitted_new = Xnew %*% beta_hat_new
      residuals_1 = Y - Y_fitted_new
      beta_hat_new2 = solve(t(Xnew) %*% Xnew) %*% t(Xnew) %*% Xnew2
      Y_fitted_new2 = Xnew %*% beta_hat_new2
      residuals_2 = Xnew2 - Y_fitted_new2
      plot(residuals_1, residuals_2, main="Partial Regression Plots")
    }
  }
}


