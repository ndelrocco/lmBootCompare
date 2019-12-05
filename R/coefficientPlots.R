##' Visualize the bootstrapped sampling distribution for linear regression coefficients (Description)
##'
##' This function plots the bootstrapped sampling distributions of linear regression coefficients obtained
##' using six different methods: case resampling, residual resampling,
##' Wild bootstrap with Mammen's Two-Point Distribution, Wild bootstrap with Mammen's Continuous
##' Distribution, Wild boostrap with Rademacher Distribution, and Wild bootstrap with Standard Normal
##' Distribution. Each plot contains (k+1) facets where k is the number of predictors in the model. The
##' least squares estimate of each regression coefficient is denoted with a red dashed line.
##'
##'
##' @title Visualize bootstrapped sampling distribution for linear regression coefficients
##' @param lmodObs The observed linear model estimated by least squares. A fitted model object of
##' class inheriting from 'lm'.
##' @param bootList A list containing the bootstrapped sampling distributions of linear regression
##' coefficients. Each element in the list should contain a data frame obtained using one of the
##' six supported bootstrapping methods of size B x (k+1) where k is the number of predictors in the
##' model. Column \eqn{i} of the data frame will be a sample of size B from the sampling distribution of
##' \eqn{\beta_{i}}.
##' @examples
##' library(faraway)
##' data(prostate)
##' lmod <- lm(lpsa ~ lcavol + age + lweight, prostate)
##' bootsamps <- bootstrapSamples(lmodObs=lmod, formula='lpsa ~ lcavol + age + lweight',
##'                               data=prostate, B=1000)
##' coefficientPlots(lmodObs=lmod, bootList=bootsamps)
##'
##' @author Natalie DelRocco

#' @export
#' @import dplyr ggplot2 tidyr
coefficientPlots <- function(lmodObs, bootList) {
    
    value <- NULL
    
    betaObs <- data.frame(key = names(lmodObs$coefficients), value = unname(lmodObs$coefficients))
    
    method <- names(bootList)
    
    for (i in 1:length(bootList)) {
        
        graphics::par(ask = TRUE)
        
        p <- bootList[[i]] %>% gather() %>% ggplot(aes(value)) + geom_histogram(bins = 20) + facet_wrap(~key, 
            scales = "free_x") + geom_vline(aes(xintercept = value), betaObs, color = "red", linetype = "dashed", 
            size = 1) + ggtitle(paste0("Bootstrap Distributions of Model Coefficients - ", method[i])) + theme(plot.title = element_text(hjust = 0.5))
        
        print(p)
        
    }
    
}
