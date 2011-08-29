\name{residuals.mpt}
\alias{residuals.mpt}
\title{Residuals for MPT Models}
\description{
  Computes deviance and Pearson residuals for \code{mpt} objects.
}
\usage{
\method{residuals}{mpt}(object, type = c("deviance", "pearson"), \dots)
}
\arguments{
  \item{object}{an object of class \code{mpt}, typically the result of a
    call to \code{\link{mpt}}.}
  \item{type}{the type of residuals which should be returned; the
          alternatives are: \code{"deviance"} (default) and
          \code{"pearson"}.}
  \item{\dots}{further arguments passed to or from other methods.
    None are used in this method.}
}
\details{
  See \code{\link{residuals.glm}} for details.
}
\value{
  A named vector of residuals having as many elements as response categories.
  The names are given by the \code{treeid}.
}
\seealso{\code{\link{mpt}}, \code{\link{residuals.glm}},
  \code{\link{plot.mpt}}.}
\examples{
data(retroact)                      # retroactive inhibition data

mpt1 <- mpt(freq ~ list(            # fit storage-retrieval model
  c*r,
  (1 - c)*u^2,
  2*(1 - c)*u*(1 - u),
  c*(1 - r) + (1 - c)*(1 - u)^2,
  u,
  1 - u
), retroact[retroact$lists == 0,])

sum( resid(mpt1)^2 )                # likelihood ratio G2
sum( resid(mpt1, "pearson")^2 )     # Pearson X2
}
\keyword{models}
