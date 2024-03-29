% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bootstrapConfInt.R
\name{bootstrapConfInt}
\alias{bootstrapConfInt}
\title{Compare bootstrapping methods for linear regression coefficients using summary statistics.}
\usage{
bootstrapConfInt(lmodObs, bootList, level)
}
\arguments{
\item{lmodObs}{The observed linear model estimated by least squares. A fitted model object of
class inheriting from 'lm'.}

\item{bootList}{A list containing the bootstrapped sampling distributions of linear regression
coefficients. Each element in the list should contain a data frame obtained using one of the
six supported bootstrapping methods of size B x (k+1) where k is the number of predictors in the
model. Column \eqn{i} of the data frame will be a sample of size B from the sampling distribution of
\eqn{\beta_{i}}.}

\item{level}{The confidence level required. A real number between 0 and 1.}
}
\description{
Compare bootstrapped confidence intervals for linear regression coefficients.
}
\details{
This function compares the confidence intervals for linear regression coefficients obtained
using each of six methods: case resampling, residual resampling, Wild bootstrap with Mammen's
Two-Point Distribution, Wild bootstrap with Mammen's Continuous Distribution, Wild boostrap
with Rademacher Distribution, and Wild bootstrap with Standard Normal Distribution. These are also
compared to the model based confidence interval. Model based, normal approximation, and bootstrap
percentile intervals are considered.
}
\examples{
library(faraway)
data(prostate)
lmod <- lm(lpsa ~ lcavol + age + lweight, prostate)
bootsamps <- bootstrapSamples(lmodObs=lmod, formula='lpsa ~ lcavol + age + lweight',
                              data=prostate, B=1000)
bootstrapConfInt(lmodObs=lmod, bootList=bootsamps, level=0.95)

}
\references{
Davison, A.C. and Hinkley, D.V. (1997) Bootstrap Methods and Their Application. Cambridge University Press.
}
\author{
Natalie DelRocco
}
