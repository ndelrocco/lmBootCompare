##' Compare bootstrapping methods for linear regression coefficients using summary statistics.
##'
##' This function compares the summary statistics of bootstrapped sampling distributions for linear
##' regression coefficients obtained in each of six methods: case resampling, residual resampling,
##' Wild bootstrap with Mammen's Two-Point Distribution, Wild bootstrap with Mammen's Continuous
##' Distribution, Wild boostrap with Rademacher Distribution, and Wild bootstrap with Standard Normal
##' Distribution.
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
##' @param summaryStats A string or a vector of strings. The desired summary statistics of the sampling
##' of regression coefficients for the function to return. Options are: mean, median, Q1, Q3, IQR, var,
##' se, and MSE.
##' @value A list with each component containing a 6 x (k+1) matrix of summary statistics. Each component
##' contains an elected summary statistic. Summary statistics are calculated for each covariate and each
##' bootstrap method.
##' @author Natalie DelRocco

#' @export
bootstrapSummary <- function(bootList, lmodObs, summaryStats = c("mean", "median", "Q1", "Q3", "var", "IQR", 
    "se", "MSE")) {
    
    list2mat <- function(summaryList, ncovs, covNames, bootNames) {
        
        mat <- matrix(unlist(summaryList), ncol = ncovs, byrow = TRUE)
        rownames(mat) <- bootNames
        colnames(mat) <- covNames
        return(mat)
        
    }
    
    summarySave <- list()
    
    options(pillar.sigfig = 10)
    ncovs <- length(lmodObs$coefficients)
    covNames <- names(lmodObs$coefficients)
    bootNames <- names(bootList)
    betaHatObs <- lmodObs$coefficients
    B <- dim(bootList[[1]])[1]
    
    if ("mean" %in% summaryStats) {
        
        means <- lapply(bootList, colMeans)
        summarySave$Mean <- list2mat(means, ncovs = ncovs, covNames = covNames, bootNames = bootNames)
        
    }
    
    if ("median" %in% summaryStats) {
        
        meds <- lapply(bootList, function(x) dplyr::summarise_all(x, stats::median))
        summarySave$Median <- list2mat(meds, ncovs = ncovs, covNames = covNames, bootNames = bootNames)
        
    }
    
    if ("Q1" %in% summaryStats || "Q3" %in% summaryStats || "IQR" %in% summaryStats) {
        
        q1 <- lapply(bootList, function(x) dplyr::summarise_all(x, ~stats::quantile(., probs = 0.25)))
        q1Mat <- list2mat(q1, ncovs = ncovs, covNames = covNames, bootNames = bootNames)
        
        q3 <- lapply(bootList, function(x) dplyr::summarise_all(x, ~stats::quantile(., probs = 0.75)))
        q3Mat <- list2mat(q3, ncovs = ncovs, covNames = covNames, bootNames = bootNames)
        
        if ("Q1" %in% summaryStats) 
            summarySave$Q1 <- q1Mat
        
        if ("Q3" %in% summaryStats) 
            summarySave$Q3 <- q3Mat
        
        if ("IQR" %in% summaryStats) 
            summarySave$IQR <- q3Mat - q1Mat
        
    }
    
    if ("var" %in% summaryStats || "se" %in% summaryStats) {
        
        vars <- lapply(bootList, function(x) dplyr::summarise_all(x, stats::var))
        varMat <- list2mat(vars, ncovs = ncovs, covNames = covNames, bootNames = bootNames)
        
        if ("var" %in% summaryStats) 
            summarySave$Variance <- varMat
        if ("se" %in% summaryStats) 
            summarySave$standardError <- sqrt(varMat)
        
    }
    
    if ("MSE" %in% summaryStats) {
        
        mses <- lapply(bootList, function(x) bootMSE(x, B = B, lmodObs = lmodObs, ncovs = ncovs))
        summarySave$MSE <- list2mat(mses, ncovs = ncovs, covNames = covNames, bootNames = bootNames)
        
    }
    
    return(summarySave)
    
}


