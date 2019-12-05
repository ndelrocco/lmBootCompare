##' Empirical boostrap for linear regression coefficients (Description)
##'
##' This function simulates the sampling distribution of simple linear regression coefficients
##' by first simulating the joint distribution of (\eqn{\mathbf{X},Y}) by the empirical distribution
##' function of the cases (i.e. resampling cases with replacement).
##'
##'
##' @title Empirical boostrap for linear regression coefficients
##' @param formula A string that can be coerced into class 'formula'. Usually of the form
##' response variable \eqn{\sim} predictor variables.A symbolic description of the model to be fitted.
##' @param data data frame, list or environment (or object coercible by as.data.frame to a data frame)
##' containing the variables in the model.
##' @param B The number of bootstrap replicates. Usually this will be a single positive integer.
##' @return A B x (k+1) data frame containing B samples from the distributions of each of the (k+1) model
##' parameters, where k is the number of predictors in the model.
##' @examples
##' library(faraway)
##' data(prostate)
##' bootsamps <- bootCases(formula='lpsa ~ lcavol + age + lweight', data=prostate, B=1000)
##'
##' @author Natalie DelRocco
##' @references Davison, A.C. and Hinkley, D.V. (1997) Bootstrap Methods and Their Application. Cambridge University Press.

#' @export
bootCases <- function(formula, data, B) {
    
    casesSamps <- sampler(x = data, B = B)
    bootReplicates <- apply(casesSamps, 3, function(x) stats::lm(stats::as.formula(formula), data = data.frame(x))$coefficients)
    return(data.frame(t(bootReplicates), check.names = F))
    
}

