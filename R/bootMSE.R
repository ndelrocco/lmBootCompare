##' MSE of a bootstrap sample of simple linear regression coefficients
##'
##' This function calculates the mean square error for bootstrap samples of linear regression coefficients.
##'
##'
##' @title MSE for bootstrap samples of linear regression coefficients
##' @param B The number of bootstrap replicates. Usually this will be a single positive integer.
##' @param lmodObs The observed linear model estimated by least squares. A fitted model object of
##' class inheriting from 'lm'.
##' @param x A B x (k+1) data frame containing B samples from the distributions of each of the (k+1) model
##' parameters, where k is the number of predictors in the model.
##' @param ncovs The number of covariates in the observed model. A positive integer.
##' @return A vector of size (k+1) containing the mean square error of the bootstrap sample of
##' each regression coefficient.
##' @author Natalie DelRocco

bootMSE <- function(B, lmodObs, x, ncovs) {
    
    betaHatObs <- lmodObs$coefficients
    save <- numeric(ncovs)
    
    for (i in 1:ncovs) {
        
        b <- betaHatObs[i]
        save[i] <- (1/B) * sum((x[, i] - b)^2)
        
    }
    
    return(save)
    
}

