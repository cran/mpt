\name{prospecMemory}
\alias{prospecMemory}
\alias{PMSmithBayen}
\alias{PMreplication}
\title{Prospective Memory and Task Importance}
\description{
  Smith and Bayen (2004) tested the performance of 64 participants in an
  event-based prospective memory task that was embedded in a color-matching
  task.  On each trial, participants were presented with four colored
  rectangles followed by a colored word.  Their task was to press a key to
  indicate whether the color of the word matched one of the rectangles.
  Interspersed among these nontarget words were six target words for which
  subjects had to remember to press a special key (prospective memory
  response) regardless of the color.  Participants received two different
  instruction types either stressing the importance of the color-matching 
  (CMI) or of the prospective-memory task (PMI).

  In a replication study, the performance of 72 German-speaking participants
  was tested; this study was designed to be similar to Experiment 1 in Smith
  and Bayen (2004).
}
\usage{data(prospecMemory)}
\format{
  \code{PMSmithBayen} A data frame consisting of five variables:
  \describe{
    \item{\code{instruction}}{instruction type, either color-matching
      importance (\code{cmi}) or prospective memory importance (\code{pmi}).}
    \item{\code{item}}{a factor specifying one of four item types: either a
      target word that did or did not match the color of the rectangles, or
      a nontarget word that did or did not match.}
    \item{\code{resp}}{a factor giving the response categories: \code{match},
      \code{nonmatch}, or the prospective memory response (\code{prospec}).}
    \item{\code{freq}}{the aggregate response frequencies per condition.}
    \item{\code{treeid}}{an identifier for the single trees of the joint
      multinomial model.}
  }
  \code{PMreplication} A data frame containing 72 observations on five
    variables:
  \describe{
    \item{\code{gender}}{factor. Participant gender.}
    \item{\code{age}}{Participant age.}
    \item{\code{instr}}{factor. Instruction type.}
    \item{\code{rtdiff}}{Average response time difference (in milliseconds)
      between color-matching and prospective-memory task.}
    \item{\code{y}}{a matrix of aggregate response frequencies per
      participant. The column names indicate each of twelve response
      categories: match, nonmatch, prospective memory response for targets in
      matching (\code{tmm}, \code{tmn}, \code{tmp}) or in nonmatching
      condition (\code{tnm}, \code{tnn}, \code{tnp}), and again for nontargets
      (\code{nmm}, \code{nmn}, \code{nmp} vs. \code{nnm}, \code{nnn},
      \code{nnp}).}
  }
}
\source{
  Smith, R.E., & Bayen, U.J. (2004).
  A multinomial model of event-based prospective memory.
  \emph{Journal of Experimental Psychology: Learning, Memory, and Cognition},
  \bold{30}, 756--777.
  \doi{10.1037/0278-7393.30.4.756}

  For the replication study, data were collected at the Department of
  Psychology, University of Tuebingen, between December 2018 and January 2019.
}
\seealso{
  \code{\link{mpt}}.
}
\examples{
## Prospective memory model: identifiability
qr(mptspec("prospec",
   .restr = list(M1=M, M2=M))$par2deriv(runif(6))$deriv)$rank
qr(mptspec("prospec",
   .restr = list(M1=M, M2=M, g=.1, c=.5))$par2deriv(runif(4))$deriv)$rank

## Prospective memory model: goodness of fit
data(prospecMemory)
cmi <- PMSmithBayen[PMSmithBayen$instruction == "cmi", ]
m2 <- mpt(mptspec("prospec", .restr = list(M1=M, M2=M, g=.1, c=.5)), cmi)
m1 <- mpt(update(m2$spec, .restr = list(C2=C1)), cmi)
anova(m1, m2)

pmi <- PMSmithBayen[PMSmithBayen$instruction == "pmi", ]
anova(mpt(m1$spec, pmi), mpt(m2$spec, pmi))

## Testing P_cmi = P_pmi and M_cmi = M_pmi

## Smith and Bayen
m2 <- mpt(mptspec("prospec", .replicates = 2,
                  .restr = list(M11=M1, M21=M1, g1=.1, c1=.5,
                                M12=M2, M22=M2, g2=.1, c2=.5)),
          data = PMSmithBayen)
m1 <- mpt(update(m2$spec, .restr = list(P2=P1)), PMSmithBayen)
m0 <- mpt(update(m2$spec, .restr = list(M2=M1)), PMSmithBayen)
anova(m1, m2)
anova(m0, m2)

## Replication
pm.agg <- aggregate(y ~ instr, PMreplication, sum)
y <- as.vector(t(pm.agg[2:1, -1]))

m3 <- mpt(m2$spec, y)
m1 <- mpt(update(m3$spec, .restr = list(P2=P1)), y)
m0 <- mpt(update(m3$spec, .restr = list(M2=M1)), y)
anova(m1, m3)
anova(m0, m3)

par(mfrow = 1:2)
dotchart(coef(m2)[c("C12", "C22", "P2", "M2")], xlim=0:1, xlab="",
  labels=c("C1", "C2", "P", "M"), main="Smith and Bayen (2004, Exp. 1)")
points(coef(m2)[c("C11", "C21", "P1", "M1")], 1:4, pch=16)
legend("bottomleft", c("CMI", "PMI"), pch=c(1, 16), title="Instruction",
       title.adj=1, bty="n")

dotchart(coef(m3)[c("C12", "C22", "P2", "M2")], xlim=0:1, xlab="",
  labels=c("C1", "C2", "P", "M"), main="Replication study")
points(coef(m3)[c("C11", "C21", "P1", "M1")], 1:4, pch=16)
mtext("Parameter estimate (prospective memory model)", side=1,
      line=-2, outer=TRUE)
}
\keyword{datasets}
