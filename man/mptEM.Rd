\name{mptEM}
\alias{mptEM}
\title{EM Algorithm for Multinomial Processing Tree Models}
\description{Applies the EM algorithm to fit a multinomial processing tree
  model.
}
\usage{
mptEM(theta, data, a, b, c, maxit = 1000, tolerance = 1e-8, 
  stepsize = 1, verbose = FALSE)
}
\arguments{
  \item{theta}{a vector of starting values for the parameter estimates.}
  \item{data}{a vector of absolute response frequencies.}
  \item{a}{a three-dimensional array representing the model structure.}
  \item{b}{a three-dimensional array representing the model structure.}
  \item{c}{a matrix of structural constants.}
  \item{maxit}{the maximum number of iterations.}
  \item{tolerance}{the convergence criterion; the iterations converge when
    \eqn{logLik - logLik.old < tolerance}.}
  \item{stepsize}{the step size defaulting to 1; slightly larger values may
    speed up convergence, but may also give errors; use with care.}
  \item{verbose}{logical indicating if output should be produced for each
    iteration.}
}
\details{
  Usually \code{mptEM} is automatically called by \code{\link{mpt}}.

  The algorithm applies to MPT models where the probabilities of the i-th
  branch leading to the j-th category take the form
  \deqn{p_{ij}(\Theta) =
    c_{ij} \prod_{s = 1}^S \theta_s^{a_{ijs}} (1 - \theta_s)^{b_{ijs}},}
  where \eqn{\Theta} is the parameter vector.

  See Hu and Batchelder (1994) and Hu (1999) for details on the algorithm.
}
\value{
  \item{theta}{the vector of parameter estimates.}
  \item{loglik}{the log-likelihood at termination of the algorithm.}
  \item{pcat}{a vector of predicted probabilities for each response category.}
  \item{pbranch}{a vector of predicted branch probabilities.}
  \item{iter}{the number of iterations of the algorithm.}
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
