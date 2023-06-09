% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quantileComb.R
\name{quantileComb}
\alias{quantileComb}
\title{Create the quantile combination set for asmbPLS and asmbPLS-DA}
\usage{
quantileComb(quantile.list)
}
\arguments{
\item{quantile.list}{A list containing the quantile set for each block.}
}
\value{
The quantile combination used for asmbPLS and asmbPLS-DA models.
}
\description{
Create the quantile combination set given quantile set for each block.
}
\examples{
## Generate quantile set for each block
## For example, we have three blocks
quantile_1 <- c(0.999, 0.9992, 0.9994, 0.9996, 0.9998)
quantile_2 <- c(0.96, 0.97, 0.98, 0.99, 0.995)
quantile_3 <- c(0.95, 0.96, 0.97, 0.98, 0.99)
quantilelist <- list(quantile_1, quantile_2, quantile_3)
quantile.comb <- quantileComb(quantilelist)

}
