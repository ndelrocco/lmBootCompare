##' Generate a random deviate from Mammen's two-point distribution (Description)
##'
##' This function generates random numbers from Mammen's two-point distribution:
##'
##' @title Random number generation for Mammen's two-point distribution
##' @param n A positive integer. The number of random deviates to generate.
##' @return n random realizations from Mammen's two-point distribution
##' @author Natalie DelRocco
##' @references Davison, A.C. and Hinkley, D.V. (1997) Bootstrap Methods and Their Application. Cambridge University Press.
##' @references Mammen, E. (1993). Bootstrap and Wild Bootstrap for High Dimensional Linear Models. The Annals of Statistics, 21(1), 255-285. doi: 10.1214/aos/1176349025

#' @export
rMammen2 <- function(n) {
    
    vals <- c((1 - sqrt(5))/2, (sqrt(5) + 1)/2)
    probs <- c((sqrt(5) + 1)/(2 * sqrt(5)), (sqrt(5) - 1)/(2 * sqrt(5)))
    sample(vals, size = n, replace = TRUE, prob = probs)
    
}
