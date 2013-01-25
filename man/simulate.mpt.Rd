\name{simulate.mpt}
\alias{simulate.mpt}
\title{Simulate Responses from MPT Models}
\description{
  Simulates responses from the distribution corresponding to a fitted
  \code{mpt} model object.
}
\usage{
\method{simulate}{mpt}(object, nsim, seed, pool = TRUE, \dots)
}
\arguments{
  \item{object}{an object of class \code{mpt}, typically the result of a
    call to \code{\link{mpt}}.}
  \item{nsim}{currently not used.}
  \item{seed}{currently not used.}
  \item{pool}{logical, if TRUE (default), pooled responses (summed across
    respondents) are returned.}
  \item{\dots}{further arguments passed to or from other methods.
    None are used in this method.}
}
\details{
  Responses are simulated by (repeatedly) applying \code{\link{rmultinom}}
  with sizes taken from the original sample and probabilities computed from
  the model object.
}
\value{
  A named vector of (pooled) responses. Names identify the tree from which
  responses were simulated.
}
\seealso{
  \code{\link{mpt}}.
}
\examples{
data(retroact)
 
mpt1 <- mpt(freq ~ list(
  c*r,
  (1 - c)*u^2,
  2*(1 - c)*u*(1 - u),
  c*(1 - r) + (1 - c)*(1 - u)^2,
  u,
  1 - u
), retroact[retroact$lists == 1,])

simulate(mpt1)

## Perform parametric bootstrap
LR.stat <- replicate(200, mpt(formula(mpt1), simulate(mpt1))$good[1])

hist(LR.stat, col="lightgrey", border="white", freq=FALSE, breaks=20,
  main="Parametric bootstrap")
curve(dchisq(x, df=mpt1$good[1]), add=TRUE)
abline(v=mpt1$good[1], lty=2)
}
\keyword{models}
