##' Wild boostrap for linear regression coefficients (Description)
##'
##' This function simulates the sampling distribution of simple linear regression coefficients
##' by fixing the covariates and resampling \eqn{Y_i} by fixing \eqn{X_i}
##' and using a zero mean, unit variance auxiliary multiplier variable on the \eqn{i^{th}} residual.
##' There are several common multipliers used for Wild bootstrap. The four multipliers supported by
##' this function are: Mammen's two-point distribution, Mammen's continuous distribution, Rademacher
##' distribution, and the Standard Normal distribution.
##'
##'
##' @title Wild boostrap for linear regression coefficients
##' @param formula A string that can be coerced into class 'formula'. Usually of the form
##' response variable \eqn{\sim} predictor variables.A symbolic description of the model to be fitted.
##' @param data data frame, list or environment (or object coercible by as.data.frame to a data frame)
##' containing the variables in the model.
##' @param lmodObs The observed linear model estimated by least squares. A fitted model object of
##' class inheriting from 'lm'.
##' @param B The number of bootstrap replicates. Usually this will be a single positive integer.
##' @param multiplier A string indicating which multiplier to use as the auxiliary distribution. Options
##' are 'mammen2', 'mammenC', 'rademacher', and 'norm'
##' @return A B x (k+1) data frame containing B samples from the distributions of each of the (k+1) model
##' parameters, where k is the number of predictors in the model.
##' @author Natalie DelRocco
##' @references Davison, A.C. and Hinkley, D.V. (1997) Bootstrap Methods and Their Application. Cambridge University Press.
##' @references Mammen, E. (1993). Bootstrap and Wild Bootstrap for High Dimensional Linear Models. The Annals of Statistics, 21(1), 255-285. doi: 10.1214/aos/1176349025

#' @export
bootWild <- function(formula, data, lmodObs, B, multiplier) {
    
    multiplierString <- paste("r", multiplier, sep = "")
    multiplierFunction <- get(multiplierString)
    covariates <- substring(formula, regexpr("~", formula) + 1)
    resids <- lmodObs$residuals
    bootReplicates <- matrix(NA, nrow = B, ncol = length(stats::coef(lmodObs)))
    colnames(bootReplicates) <- c(names(stats::coef(lmodObs)))
    
    for (i in 1:B) {
        
        multipliers <- multiplierFunction(length(resids))
        newResids <- resids * multipliers
        newY <- lmodObs$fitted.values + newResids
        bootReplicates[i, ] <- unname(stats::lm(formula = stats::as.formula(paste("newY ~", covariates)), data = data)$coefficients)
        
    }
    
    return(data.frame(bootReplicates, check.names = F))
    
}




