\name{vcov.mpt}
\alias{vcov.mpt}
\title{Covariance and Information Matrix for MPT Models}
\description{
  Returns the covariance matrix or the Fisher information matrix of a fitted
  \code{mpt} model object.
}
\usage{
\method{vcov}{mpt}(object, logit = FALSE, what = c("vcov", "fisher"), \dots)
}
\arguments{
  \item{object}{an object of class \code{mpt}, typically the result of a
    call to \code{\link{mpt}}.}
  \item{logit}{logical. Switch between logit and probability scale.}
  \item{what}{character. If \code{vcov} (default), the covariance matrix is
    returned; if \code{fisher}, the Fisher information matrix is returned.}
  \item{\dots}{further arguments passed to or from other methods.
    None are used in this method.}
}
\details{
  If \code{logit} is false, the covariance matrix is based on the observed
  Fisher information matrix of the ML estimator on the probability scale.
  This is equivalent to the equations for the covariance matrix given in Hu
  and Batchelder (1994) and Hu (1999), although the implementation here is
  different.

  If \code{logit} is true, the covariance matrix and the estimated information
  matrix (Elandt-Johnson, 1971) of the ML estimator on the logit scale are
  obtained by the multivariate delta method (Bishop, Fienberg, and Holland,
  1975; Grizzle, Starmer, and Koch, 1969).
}
\value{
  A (named) square matrix.
}
\references{
  Bishop, Y.M.M., Fienberg, S.E., & Holland, P.W. (1975).
  \emph{Discrete multivariate analysis: Theory and practice}.
  Cambridge: MIT Press.

  Elandt-Johnson, R. C. (1971).
  \emph{Probability models and statistical methods in genetics}.
  New York: Wiley.

  Grizzle, J.E., Starmer, C.F., & Koch, G. (1969).
  Analysis of categorical data by linear models.
  \emph{Biometrics},
  \bold{25}(3), 489--504.
  \doi{10.2307/2528901}

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
}
\seealso{
  \code{\link{mpt}}.
}
\examples{
data(retroact)
m <- mpt(mptspec("SR"), retroact[retroact$lists == 1, ])

vcov(m)                   # covariance matrix (probability scale)
vcov(m, logit = TRUE)     # covariance matrix (logit scale)
vcov(m, what = "fisher")  # Fisher information
}
\keyword{models}
