\name{recogROC}
\alias{recogROC}
\alias{ROCBroeder2009}
\alias{ROCreplication}
\title{Recognition Receiver Operating Characteristics}
\description{
  In a series of experiments, Broeder and Schuetz (2009) tested the shape of
  recognition receiver operating characteristics.  Participants studied a list
  of items.  In a recognition test, old items intermixed with new ones were
  presented, and participants had to classify them as old or new.  The
  percentage of old items varied in order to manipulate the response bias.

  Wellingerhof (2019) conducted a replication study that was designed to be
  similar to Experiment 3 in Broeder and Schuetz (2009).
}
\usage{data(recogROC)}
\format{
  \code{ROCBroeder2009} A data frame consisting of seven variables:
  \describe{
    \item{\code{item}}{factor. Target (old) or distractor (new) item.}
    \item{\code{resp}}{a factor giving the response category, \code{old}
      or \code{new}.}
    \item{\code{treeid}}{an identifier for the single trees of the joint
      multinomial model.}
    \item{\code{ptarget1}, \code{ptarget3}}{percentage of target (old)
      items in Experiment 1 and 3, respectively.}
    \item{\code{exp1}, \code{exp3}}{the aggregate response frequencies.}
  }
  \code{ROCreplication} A data frame containing 48 observations of five
    variables:
  \describe{
    \item{\code{gender}}{factor. Participant gender.}
    \item{\code{age}}{Participant age.}
    \item{\code{arith}}{number of mental-arithmetic problems solved.}
    \item{\code{lexical}}{number of correct trials in lexical selection task.}
    \item{\code{y}}{a matrix of aggregate response frequencies per
      participant. The column names indicate each of 4 x 5 response
      categories: \code{h}it, \code{m}iss, \code{f}alse alarm, and
      \code{c}orrect rejection in the five bias conditions.}
  }
}
\source{
  Broeder, A., & Schuetz, J. (2009).
  Recognition ROCs are curvilinear--or are they? On premature arguments
  against the two-high-threshold model of recognition.
  \emph{Journal of Experimental Psychology: Learning, Memory, and Cognition},
  \bold{35}, 587--606.
  \doi{10.1037/a0015279}

  Wellingerhof, P. (2019).
  \emph{Signal detection theory vs. 2-high-threshold model in recognition
        memory: A preregistered replication study}.
  Bachelor thesis. University of Tuebingen, Germany.
  \url{https://osf.io/hvg4p/}
}
\seealso{
  \code{\link{mpt}}.
}
\examples{
data(recogROC)

## Two-high-threshold model
s <- mptspec("2HT", .replicates = 5,
                    .restr = list(r1=r, r2=r, r3=r, r4=r, r5=r,
                                  d1=d, d2=d, d3=d, d4=d, d5=d))
m1 <- mpt(s, data = ROCBroeder2009, freqvar = "exp3")
m2 <- mpt(s, data = unname(ROCreplication$y))

## Table 4
rbind(Broeder2009 = c(deviance(m1), coef(m1)),
      Replication = c(deviance(m2), coef(m2)))

## Hit rate and false alarm rate
i.hit <- with(ROCBroeder2009, item == "target" & resp == "old")
i.fa  <- with(ROCBroeder2009, item == "distractor" & resp == "old")

hrfa <- data.frame(
   study = rep(c("Broeder2009", "Replication"), each=5),
   obshr = c((m1$y/m1$n)[i.hit], (m2$y/m2$n)[i.hit]),
   obsfa = c((m1$y/m1$n)[i.fa],  (m2$y/m2$n)[i.fa]),
  predhr = c(m1$pcat[i.hit],     m2$pcat[i.hit]),
  predfa = c(m1$pcat[i.fa],      m2$pcat[i.fa])
)

## ROC, Figure 7
plot(obshr ~ obsfa, hrfa[hrfa$study == "Broeder2009", ],
     xlim=0:1, ylim=0:1, pch=16,
     main="Linear recognition ROCs?",
     ylab="Hit rate", xlab="False alarm rate")
abline(0, 1, lty=2)
lines(predhr ~ predfa, hrfa[hrfa$study == "Broeder2009", ])
points(obshr ~ obsfa, hrfa[hrfa$study == "Replication", ], col = "blue")
lines(predhr ~ predfa, hrfa[hrfa$study == "Replication", ],
      col = "blue")
text(0.45, 0.93, "Replication", col = "blue")
text(0.59, 0.82, "Broeder and Schuetz\n(2009, Exp. 3)")
}
\keyword{datasets}
