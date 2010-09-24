\name{plot.mpt}
\alias{plot.mpt}
\title{Diagnostic Plot for MPT Models}
\description{
  Plots MPT residuals against fitted values.
}
\usage{
\method{plot}{mpt}(x, showID = TRUE,
  xlab = "Predicted response probabilities", ylab = "Deviance residuals",
  \dots)
}
\arguments{
  \item{x}{an object of class \code{mpt}, typically the result of a
    call to \code{\link{mpt}}.}
  \item{showID}{logical. Should the \code{treeid}'s be plotted? Defaults to
    \code{TRUE}.}
  \item{xlab, ylab, \dots}{graphical parameters passed to plot.}
}
\details{
  The deviance residuals are plotted against the predicted response
  probabilities.  If \code{showID} is true, plotting symbols are the
  \code{treeid}'s used when the model was fit.
}
\seealso{\code{\link{mpt}}, \code{\link{residuals.mpt}}.}
\examples{
## Compare two constrained MPT models

data(proact)

mpt1 <- mpt(freq ~ list(
  p1*q1*r1,
  p1*q1*(1 - r1),
  p1*(1 - q1)*r1,
  (1 - p1) + p1*(1 - q1)*(1 - r1),

  p2*q2*r2,
  p2*q2*(1 - r2),
  p2*(1 - q2)*r2,
  (1 - p2) + p2*(1 - q2)*(1 - r2),

  p3*q3*r3,
  p3*q3*(1 - r3),
  p3*(1 - q3)*r3,
  (1 - p3) + p3*(1 - q3)*(1 - r3)
), proact[proact$test == 1,], constr = list(p=c("p1", "p2", "p3")))

mpt2 <- mpt(mpt1$formula, proact[proact$test == 1,],
  constr = list(q=c("q1", "q2", "q3")))

par(mfrow = c(1,2))                   # residuals versus fitted values
plot(mpt1, main = "p constrained", ylim = c(-3, 3.5))  # good fit
plot(mpt2, main = "q constrained", ylim = c(-3, 3.5))  # bad fit
}
\keyword{models}
