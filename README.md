# linearmodel
<!-- badges: start -->
  [![R-CMD-check](https://github.com/wzxscmr/R-package/workflows/R-CMD-check/badge.svg)](https://github.com/wzxscmr/R-package/actions)
   [![codecov](https://codecov.io/gh/wzxscmr/R-package/branch/main/graph/badge.svg?token=VYQ1L987CX)](https://codecov.io/gh/wzxscmr/R-package)
  <!-- badges: end -->
 
## Overview
This package is created for the purpose of BIOSTAT 625 HW4. It is used to do the linear regression and return a list of important parameters of the fitted linear model and the model diagnosis plots. In the package, you can use these five functions: linear_model(), Linearity(), Normality(), Cons_variance() Independence().

## Installation
```{r}
install.packages('devtools')
devtools::install_github("wzxscmr/linearmodel")
library("linearmodel")
```

## Usage
* `X`: The predictors of variables (can be a vector or a matrix)

* `Y`: The outcome variable (should be a vector)

## Examples
These are basic examples which shows you how to use the functions in the package:
```{r}
linear_model(X= cbind(mtcars$cyl, mtcars$hp, mtcars$drat, mtcars$wt), Y = mtcars$mpg)
Cons_variance(X= cbind(mtcars$cyl, mtcars$hp, mtcars$drat, mtcars$wt), Y = mtcars$mpg)
```

## Tutorials
For more detailed examples or for more information, please run the following code:
```{r}
browseVignettes(package = "linearmodel")
```

## Authorship
This package was created in 12/4/2021 by Zixi Wang.

## Contact me
[link](wzx@umich.edu)


