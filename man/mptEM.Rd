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
  Usually, \code{mptEM} is automatically called by \code{\link{mpt}}.

  A prerequisite for the application of the EM algorithm is that the
  probabilities of the i-th branch leading to the j-th category take the form
  \deqn{p_{ij}(\Theta) =
    c_{ij} \prod_{s = 1}^S \vartheta_s^{a_{ijs}} (1 - \vartheta_s)^{b_{ijs}},}
  where \eqn{\Theta = (\vartheta_s)} is the parameter vector, \eqn{a_{ijs}}
  and \eqn{b_{ijs}} count the occurrences of \eqn{\vartheta_s} and
  \eqn{1 - \vartheta_s} in a branch, respectively, and \eqn{c_{kj}} is a
  nonnegative real number.  The branch probabilities sum up to the total
  probability of a given category, \eqn{p_j = p_{1j} + \dots + p_{Ij}}.  This
  is the structural restriction of the class of MPT models that can be
  represented by binary trees.  Other model types have to be suitably
  reparameterized for the algorithm to apply.

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
  Hu, X. (1999).
  Multinomial processing tree models: An implementation.
  \emph{Behavior Research Methods, Instruments, & Computers},
  \bold{31}(4), 689--695.
  \doi{10.3758/BF03200747}

  Hu, X., & Batchelder, W.H. (1994).
  The statistical analysis of general processing tree models with the EM
  algorithm.
  \emph{Psychometrika},
  \bold{59}(1), 21--47.
  \doi{10.1007/bf02294263}

  Riefer, D.M., Knapp, B.R., Batchelder, W.H., Bamber, D., & Manifold, V.
  (2002).
  Cognitive psychometrics: Assessing storage and retrieval deficits
  in special populations with multinomial processing tree models.
  \emph{Psychological Assessment},
  \bold{14}(2), 184--201.
  \doi{10.1037/1040-3590.14.2.184}
}
\seealso{
  \code{\link{mpt}}.
}
\examples{
# Fit storage-retrieval pair-clustering model to data in Riefer et al.
# (2002) using EM algorithm
mpt(mptspec("SR2"), c(243, 64, 58, 55), method = "EM")
}
\keyword{models}
