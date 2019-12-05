##' Obtain bootstrap samples for linear regression coefficients using six different methods.
##'
##' This function is a wrapper for bootCases, bootResids, and bootWild.
##' This function obtains the bootstrapped sampling distributions for linear
##' regression coefficients obtained in each of six methods: case resampling, residual resampling,
##' Wild bootstrap with Mammen's Two-Point Distribution, Wild bootstrap with Mammen's Continuous
##' Distribution, Wild boostrap with Rademacher Distribution, and Wild bootstrap with Standard Normal
##' Distribution.
##'
##'
##' @title Obtain bootstrap samples for linear regression coefficients
##' @param lmodObs The observed linear model estimated by least squares. A fitted model object of
##' class inheriting from 'lm'.
##' @param data data frame, list or environment (or object coercible by as.data.frame to a data frame)
##' containing the variables in the model.
##' @param B The number of bootstrap replicates. Usually this will be a single positive integer.
##' @param formula A string that can be coerced into class 'formula'. Usually of the form
##' response variable \eqn{\sim} predictor variables. A symbolic description of the model to be fitted.
##' @value A list of length six where each component is a B x (k+1) data frame containing B samples from the
##' distributions of each of the (k+1) model parameters, where k is the number of predictors in the
##' model. Each component in the list corresponds to one of six bootstrapping methods.
##' @examples
##' library(faraway)
##' data(prostate)
##' lmod <- lm(lpsa ~ lcavol + age + lweight, prostate)
##' bootsamps <- bootstrapSamples(lmodObs=lmod, formula='lpsa ~ lcavol + age + lweight',
##'                               data=prostate, B=1000)
##'
##' @author Natalie DelRocco

#' @export
bootstrapSamples <- function(lmodObs, formula, data, B) {
    
    bC <- bootCases(formula = formula, data = data, B = B)
    bR <- bootResids(formula = formula, data = data, lmodObs = lmodObs, B = B)
    bW_m2 <- bootWild(formula = formula, data = data, lmodObs = lmodObs, B = B, multiplier = "Mammen2")
    bW_mc <- bootWild(formula = formula, data = data, lmodObs = lmodObs, B = B, multiplier = "MammenC")
    bw_r <- bootWild(formula = formula, data = data, lmodObs = lmodObs, B = B, multiplier = "Rademacher")
    bw_n <- bootWild(formula = formula, data = data, lmodObs = lmodObs, B = B, multiplier = "norm")
    
    boots <- list(bC, bR, bW_m2, bW_mc, bw_r, bw_n)
    names(boots) <- c("Cases", "Residuals", "Wild (Mammen's 2 Point)", "Wild (Mammen's Continuous)", "Wild (Rademacher)", 
        "Wild (Standard Normal)")
    return(boots)
    
}
