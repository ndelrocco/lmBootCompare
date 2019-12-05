##' Obtain bootstrap samples of an input dataset.
##'
##' This a generic function for resampling with replacement and equal probability from an input dataset.
##'
##'
##' @title Obtain bootstrap samples of an input dataset.
##' @param x An input dataset.
##' @param B The number of bootstrap replicates. Usually this will be a single positive integer.
##' @value If x is a vector of length n, an n x B matrix of bootstrapped samples. If x is an n x p matrix,
##' an array of size n x p x B containing bootstrapped samples.
##' @author Natalie DelRocco

sampler <- function(x, B) {
    x <- as.matrix(x)
    samps <- replicate(B, x[sample(nrow(x), replace = TRUE), ])
    rownames(samps) <- NULL
    return(samps)
}

