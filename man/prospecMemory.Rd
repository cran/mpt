\name{prospecMemory}
\alias{prospecMemory}
\title{Prospective Memory by Instruction Type}
\description{
  Smith and Bayen (2004) tested the performance of 64 participants in an
  event-based prospective memory task that was embedded in a color-matching
  task. On each trial, participants were presented with four colored
  rectangles followed by a colored word. Their task was to press a key to
  indicate whether the color of the word matched one of the rectangles.
  Interspersed among these nontarget words were six target words for which
  subjects had to remember to press the tilde key (prospective memory
  response) regardless of the color. Participants received two different
  instruction types either stressing the importance of the color-matching 
  (CMI) or of the prospective-memory task (PMI).
}
\usage{data(prospecMemory)}
\format{
  A data frame consisting of five variables:
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
}
\source{
  Smith, R.E., & Bayen, U.J. (2004).
  A multinomial model of event-based prospective memory.
  \emph{Journal of Experimental Psychology: Learning, Memory, and Cognition},
  \bold{30}, 756--777.
  \doi{10.1037/0278-7393.30.4.756}
}
\examples{
## Prospective memory model: identifiability
qr(mptspec("prospec",
   .restr=list(M1=M, M2=M))$par2deriv(runif(6))$deriv)$rank
qr(mptspec("prospec",
   .restr=list(M1=M, M2=M, g=.1, c=.5))$par2deriv(runif(4))$deriv)$rank

## Prospective memory model: goodness of fit
data(prospecMemory)
cmi <- prospecMemory[prospecMemory$instruction == "cmi", ]
m2 <- mpt(mptspec("prospec", .restr=list(M1=M, M2=M, g=.1, c=.5)), cmi)
m1 <- mpt(update(m2$spec, .restr=list(C2=C1)), cmi)
anova(m1, m2)

pmi <- prospecMemory[prospecMemory$instruction == "pmi", ]
anova(mpt(m1$spec, pmi), mpt(m2$spec, pmi))

## Testing P_cmi = P_pmi and M_cmi = M_pmi
m2 <- mpt(mptspec("prospec", .replicates=2,
                  .restr=list(M11=M1, M21=M1, g1=.1, c1=.5,
                              M12=M2, M22=M2, g2=.1, c2=.5)),
          data=prospecMemory)
m1 <- mpt(update(m2$spec, .restr=list(P2=P1)), prospecMemory)
m0 <- mpt(update(m2$spec, .restr=list(M2=M1)), prospecMemory)
anova(m1, m2)
anova(m0, m2)

dotchart(coef(m2)[c("C12", "C22", "P2", "M2")], xlim=c(0, 1),
  labels=c("C1", "C2", "P", "M"), main="Smith and Bayen (2004, Exp. 1)",
  xlab="Parameter estimate (Prospective memory model)")
points(coef(m2)[c("C11", "C21", "P1", "M1")], 1:4, pch=16)
legend("bottomleft", c("CMI", "PMI"), pch=c(1, 16), title="Instruction",
       title.adj=1, bty="n")
}
\keyword{datasets}
