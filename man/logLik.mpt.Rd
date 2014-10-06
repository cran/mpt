\name{logLik.mpt}
\alias{logLik.mpt}
\alias{nobs.mpt}
\alias{deviance.mpt}
\title{Log-Likelihood of an mpt Object}
\description{
  Returns the log-likelihood value of the (joint) multinomial processing
  tree model represented by \code{object} evaluated at the estimated
  parameters.
}
\usage{
\method{logLik}{mpt}(object, \dots)
}
\arguments{
 \item{object}{an object inheriting from class \code{mpt}, representing
   a fitted multinomial processing tree model.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\value{
  The log-likelihood of the model represented by
  \code{object} evaluated at the estimated parameters.
}
\seealso{
  \code{\link{mpt}}, \code{\link{logLik.lm}}, \code{\link{AIC}}.
}
\examples{
mpt1 <- mpt(mptspec("SR2"), c(243, 64, 58, 55)) # from Riefer et al. (2002)
logLik(mpt1)
AIC(mpt1)
BIC(mpt1)
deviance(mpt1)
nobs(mpt1)
}
\keyword{models}
