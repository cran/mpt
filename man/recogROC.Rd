\name{recogROC}
\alias{recogROC}
\title{Recognition Receiver Operating Characteristics}
\description{
  Broeder and Schuetz (2009) tested the shape of recognition receiver
  operating characteristics.  75 participants studied 60 words.  In a
  recognition test, 60 words -- old and new items mixed -- were presented, and
  participants had to classify them as old or new.  The percentage of old
  items varied in order to manipulate the response bias.
}
\usage{data(recogROC)}
\format{
  A data frame consisting of five variables:
  \describe{
    \item{\code{p.target}}{percentage of target (old) items.}
    \item{\code{item}}{factor. Target (old) or distractor (new) item.}
    \item{\code{resp}}{a factor giving the response category, \code{old}
      or \code{new}.}
    \item{\code{freq}}{the aggregate response frequencies per condition.}
    \item{\code{treeid}}{an identifier for the single trees of the joint
      multinomial model.}
  }
}
\source{
  Broeder, A., & Schuetz, J. (2009).
  Recognition ROCs are curvilinear--or are they? On premature arguments
  against the two-high-threshold model of recognition.
  \emph{Journal of Experimental Psychology: Learning, Memory, and Cognition},
  \bold{35}, 587--606.
  \doi{10.1037/a0015279}
}
\seealso{
  \code{\link{mpt}}.
}
\examples{
## Data from Broeder and Schuetz (2009, Table 1, Exp. 1)
data(recogROC)

## Fit the two-high-threshold model with restrictions
m <- mpt(mptspec("2HT", .replicates=5,
                 .restr=list(r1=r, r2=r, r3=r, r4=r, r5=r,
                             d1=d, d2=d, d3=d, d4=d, d5=d)), recogROC)
summary(m)  # Table 2

## Hit rate and false alarm rate
i.hit <- with(recogROC, item == "target" & resp == "old")
i.fa  <- with(recogROC, item == "distractor" & resp == "old")

hrfa <- data.frame(
   obshr = (recogROC$freq/m$n)[i.hit],
   obsfa = (recogROC$freq/m$n)[i.fa],
  predhr = m$pcat[i.hit],
  predfa = m$pcat[i.fa]
)

## Plot ROC, Figure 5
plot(obshr ~ obsfa, hrfa, xlim=0:1, ylim=0:1, pch=16,
     main="Broeder and Schuetz (2009)",
     ylab="Hit rate", xlab="False alarm rate")
abline(0, 1)
lines(predhr ~ predfa, hrfa, lty=2)
}
\keyword{datasets}
