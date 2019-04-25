\name{logLik.mpt}
\alias{logLik.mpt}
\alias{nobs.mpt}
\alias{deviance.mpt}
\alias{AIC.mpt}
\alias{BIC.mpt}
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
  The log-likelihood of the model represented by \code{object} evaluated at
  the estimated parameters.
}
\seealso{
  \code{\link{mpt}}, \code{\link{logLik.lm}}, \code{\link{AIC}},
  \code{\link{deviance}}, \code{\link{nobs}}.
}
\examples{
m <- mpt(mptspec("SR2"), c(243, 64, 58, 55))  # from Riefer et al. (2002)
logLik(m)
deviance(m)
AIC(m)
AIC(m, k = log(sum(m$y)))  # BIC w/total number of data points
BIC(m)                     # BIC using nobs()
nobs(m)                    # number of non-redundant response categories
}
\keyword{models}
