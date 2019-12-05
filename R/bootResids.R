##' Residual boostrap for linear regression coefficients (Description)
##'
##' This function simulates the sampling distribution of simple linear regression coefficients
##' by first simulating the distribution of the errors (\eqn{\mathbf{\epsilon}}) by the
##' empirical distribution function of the residuals (i.e. resampling residuals with replacement).
##'
##'
##' @title Residual boostrap for linear regression coefficients
##' @param formula A string that can be coerced into class 'formula'. Usually of the form
##' response variable \eqn{\sim} predictor variables.A symbolic description of the model to be fitted.
##' @param data data frame, list or environment (or object coercible by as.data.frame to a data frame)
##' containing the variables in the model.
##' @param lmodObs The observed linear model estimated by least squares. A fitted model object of
##' class inheriting from 'lm'.
##' @param B The number of bootstrap replicates. Usually this will be a single positive integer.
##' @return A B x (k+1) data frame containing B samples from the distributions of each of the (k+1) model
##' parameters, where k is the number of predictors in the model.
##' @examples
##' library(faraway)
##' data(prostate)
##' lmod <- lm(lpsa ~ lcavol + age + lweight, prostate)
##' bootsamps <- bootResids(formula='lpsa ~ lcavol + age + lweight', data=prostate,
##'                         lmodObs=lmod, B=1000)
##'
##' @author Natalie DelRocco
##' @references Davison, A.C. and Hinkley, D.V. (1997) Bootstrap Methods and Their Application. Cambridge University Press.

#' @export
bootResids <- function(formula, data, lmodObs, B) {
    
    covariates <- substring(formula, regexpr("~", formula) + 1)
    resids <- lmodObs$residuals
    residsSamps <- sampler(x = resids, B = B)
    ySamps <- lmodObs$fitted.values + residsSamps
    bootReplicates <- apply(ySamps, 2, function(x) stats::lm(stats::as.formula(paste("x ~", covariates)), data = data)$coefficients)
    return(data.frame(t(bootReplicates), check.names = F))
    
}






