\name{recogROC}
\alias{recogROC}
\title{Recognition Receiver Operating Characteristics}
\description{
  Broeder and Schuetz (2009) tested the shape of recognition receiver
  operating characteristics.  75 participants studied 60 words.  In a
  recognition test, 60 words -- old and new items mixed -- were presented, and
  participants had to classify them as old or new.  The percentage of new
  items varied in order to manipulate the response bias.
}
\usage{data(recogROC)}
\format{
  A data frame consisting of four variables:
  \describe{
    \item{\code{p.new}}{percentage of new items.}
    \item{\code{item}}{factor. Target (old) or distractor (new) item.}
    \item{\code{resp}}{a factor giving the response category, \code{old}
      or \code{new}.}
    \item{\code{freq}}{the aggregate response frequencies per condition.}
    \item{\code{treeid}}{an identifier for the single trees of the joint
      multinomial model.}
  }
}
\source{
  Broeder, A. & Schuetz, J. (2009).
  Recognition ROCs are curvilinear--or are they? On premature arguments
  against the two-high-threshold model of recognition.
  \emph{Journal of Experimental Psychology: Learning, Memory, and Cognition},
  \bold{35}, 587--606.
}
\examples{
## Data from Broeder \& Schuetz (2009, Table 1, Exp. 1)
data(recogROC)

## Fit the two-high-threshold model
mpt1 <- mpt(mptmodel("2HT", 5), recogROC,
  constr=list(r = paste0("r", 1:5), d = paste0("d", 1:5)))
summary(mpt1)  # Table 2

## Hit rate and false alarm rate
hrfa <- data.frame(
   obshr = (recogROC$freq/mpt1$n)[seq(1, 17, 4)],
   obsfa = (recogROC$freq/mpt1$n)[seq(3, 19, 4)],
  predhr = (fitted(mpt1)/mpt1$n)[seq(1, 17, 4)],
  predfa = (fitted(mpt1)/mpt1$n)[seq(3, 19, 4)]
)

## Plot ROC, Figure 5
plot(obshr ~ obsfa, hrfa, xlim=c(0, 1), ylim=c(0, 1), pch=16,
     main="Broeder and Schuetz (2009)",
     ylab="Hit rate", xlab="False alarm rate")
abline(0, 1)
lines(predhr ~ predfa, hrfa, type="l", lty=2)
}
\keyword{datasets}
