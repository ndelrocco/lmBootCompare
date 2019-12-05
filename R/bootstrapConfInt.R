##' Compare bootstrapped confidence intervals for linear regression coefficients.
##'
##' This function compares the confidence intervals for linear regression coefficients obtained
##' using each of six methods: case resampling, residual resampling, Wild bootstrap with Mammen's
##' Two-Point Distribution, Wild bootstrap with Mammen's Continuous Distribution, Wild boostrap
##' with Rademacher Distribution, and Wild bootstrap with Standard Normal Distribution. These are also
##' compared to the model based confidence interval. Model based, normal approximation, and bootstrap
##' percentile intervals are considered.
##'
##'
##' @title Compare bootstrapping methods for linear regression coefficients using summary statistics.
##' @param bootList A list containing the bootstrapped sampling distributions of linear regression
##' coefficients. Each element in the list should contain a data frame obtained using one of the
##' six supported bootstrapping methods of size B x (k+1) where k is the number of predictors in the
##' model. Column \eqn{i} of the data frame will be a sample of size B from the sampling distribution of
##' \eqn{\beta_{i}}.
##' @param lmodObs The observed linear model estimated by least squares. A fitted model object of
##' class inheriting from 'lm'.
##' @param level The confidence level required. A real number between 0 and 1.
##' @value A list with the following components: model based (100 x level)% confidence intervals for
##' \eqn{\mathbf{\beta}}, normal approximation (100 x level)% confidence intervals for
##' \eqn{\mathbf{\beta}} using the bootstrapped estimate of the variance, and bootstrap percentile
##' intervals for \eqn{\mathbf{\beta}}. Summary statistics are calculated for each covariate and each
##' bootstrap method.
##' @author Natalie DelRocco
##' @references Davison, A.C. and Hinkley, D.V. (1997) Bootstrap Methods and Their Application. Cambridge University Press.

#' @export
bootstrapConfInt <- function(lmodObs, bootList, level) {
    
    list2mat <- function(summaryList, ncovs, covNames, bootNames) {
        
        mat <- matrix(unlist(summaryList), ncol = ncovs, byrow = TRUE)
        rownames(mat) <- bootNames
        colnames(mat) <- covNames
        return(mat)
        
    }
    
    confints <- list()
    per <- level * 100
    
    # model based
    confints[[1]] <- stats::confint(lmodObs, level = level)
    names(confints)[1] <- paste0(per, "% Model Based Confidence Intervals for Regression Coefficients")
    
    ncovs <- length(lmodObs$coefficients)
    covNames <- names(lmodObs$coefficients)
    bootNames <- names(bootList)
    betaHatObs <- unname(lmodObs$coefficients)
    alpha <- 1 - level
    
    # normal theory
    vars <- lapply(bootList, function(x) dplyr::summarise_all(x, stats::var))
    varMat <- list2mat(vars, ncovs = ncovs, covNames = covNames, bootNames = bootNames)
    seMat <- sqrt(varMat)
    l <- sweep(stats::qnorm(alpha/2) * seMat, 2, betaHatObs, "+")
    u <- sweep(stats::qnorm(1 - alpha/2) * seMat, 2, betaHatObs, "+")
    
    alternateCols <- function(odd, even) {
        
        rows.combined <- nrow(odd)
        cols.combined <- ncol(odd) + ncol(even)
        matrix.combined <- matrix(NA, nrow = rows.combined, ncol = cols.combined)
        matrix.combined[, seq(1, cols.combined, 2)] <- odd
        matrix.combined[, seq(2, cols.combined, 2)] <- even
        rownames(matrix.combined) <- rownames(odd)
        colnames(matrix.combined) <- paste(rep(colnames(odd), each = 2), rep(c(".L", ".U"), length(colnames(odd))), 
            sep = "")
        return(matrix.combined)
        
    }
    
    confints[[2]] <- alternateCols(l, u)
    names(confints)[2] <- paste0(per, "% Normal Approximation Confidence Intervals for Regression Coefficients Using Bootstrapped Variance")
    
    # bootstrap percentile
    qL <- lapply(bootList, function(x) dplyr::summarise_all(x, ~stats::quantile(., probs = alpha/2)))
    qLMat <- list2mat(qL, ncovs = ncovs, covNames = covNames, bootNames = bootNames)
    qU <- lapply(bootList, function(x) dplyr::summarise_all(x, ~stats::quantile(., probs = 1 - alpha/2)))
    qUMat <- list2mat(qU, ncovs = ncovs, covNames = covNames, bootNames = bootNames)
    confints[[3]] <- alternateCols(qLMat, qUMat)
    names(confints)[3] <- paste0(per, "% Bootstrap Percentile Intervals for Regression Coefficients")
    
    return(confints)
}


