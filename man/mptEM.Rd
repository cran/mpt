\name{mptEM}
\alias{mptEM}
\title{EM Algorithm for Multinomial Processing Tree Models}
\description{Applies the EM algorithm to fit a multinomial processing tree
  model.
}
\usage{
mptEM(theta, data, a, b, c, maxit = 1000, epsilon = 1e-8, verbose = FALSE)
}
\arguments{
  \item{theta}{a vector of starting values for the parameter estimates.}
  \item{data}{a vector of absolute response frequencies.}
  \item{a}{a three-dimensional array representing the model structure.}
  \item{b}{a three-dimensional array representing the model structure.}
  \item{c}{a matrix of structural constants.}
  \item{maxit}{the maximum number of iterations.}
  \item{epsilon}{the convergence criterion; the iterations converge when
    \eqn{logLik - logLik.old < epsilon}.}
  \item{verbose}{logical indicating if output should be produced for each
    iteration.}
}
\details{
  Usually \code{mptEM} is automatically called by \code{\link{mpt}}.

  See Hu and Batchelder (1994) and Hu (1999) for details on the algorithm.
}
\value{
  \item{theta}{the vector of parameter estimates.}
  \item{pcat}{a vector predicted probabilities for each response category.}
  \item{loglik}{the log-likelihood at termination of the algorithm.}
}
\references{
  Hu, X., & Batchelder, W.H. (1994).
  The statistical analysis of general processing tree models with the EM
  algorithm.
  \emph{Psychometrika}, \bold{59}, 21--47.

  Hu, X. (1999).
  Multinomial processing tree models: An implementation.
  \emph{Behavior Research Methods, Instruments, & Computers}, \bold{31},
  689--695.
}
\seealso{
  \code{\link{mpt}}.
}
\keyword{models}
