##' Generate random deviates from the Rademacher distribution (Description)
##'
##' This function generates random numbers from the Rademacher distribution.
##'
##' @title Random number generation for the Rademacher distribution
##' @param n A positive integer. The number of random deviates to generate.
##' @return n random realizations from the Rademacher distribution
##' @author Natalie DelRocco

#' @export
rRademacher <- function(n) {
    
    ifelse(stats::runif(n, 0, 1) > 0.5, 1, -1)
    
}





