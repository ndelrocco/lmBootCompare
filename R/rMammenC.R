##' Generate random deviates from Mammen's continuous distribution (Description)
##'
##' This function generates random numbers from Mammen's continuous distribution.
##'
##' @title Random number generation for Mammen's continuous distribution
##' @param n A positive integer. The number of random deviates to generate.
##' @return n random realizations from Mammen's continuous distribution
##' @author Natalie DelRocco
##' @references Davison, A.C. and Hinkley, D.V. (1997) Bootstrap Methods and Their Application. Cambridge University Press.
##' @references Mammen, E. (1993). Bootstrap and Wild Bootstrap for High Dimensional Linear Models. The Annals of Statistics, 21(1), 255-285. doi: 10.1214/aos/1176349025

#' @export
rMammenC <- function(n) {
    
    u <- stats::rnorm(n)
    w <- stats::rnorm(n)
    (u/sqrt(2)) + (0.5 * ((w^2) - 1))
    
}












