\name{selectiontask}
\alias{selectiontask}
\title{Wason Selection Task (WST) and Helpful Hints}
\description{
  In the Wason selection task, a participant is presented with four cards,
  each one having a letter side and a number side, e.g., A B 3 4.  The task is
  to select the card(s) that have to be turned around in order to test the
  rule "If there is an A on the letter side then there is a 3 on the number
  side."  Klauer, Stahl, and Erdfelder (2007) report a series of experiments
  to test their WST model using the aggregate frequencies of the 16 possible
  response patterns.
}
\usage{data(selectiontask)}
\format{
  A data frame consisting of four variables:
  \describe{
    \item{\code{group}}{factor. The control group (\code{CG}) received
      standard instructions, the experimental group (\code{EG}) got
      additional helpful hints.}
    \item{\code{pattern}}{character. Response pattern indicating which card(s)
      were selected (\code{1}) or not selected (\code{0}).}
    \item{\code{exp1}, \code{exp2}}{the aggregate response frequencies for
      Experiment 1 and 2, respectively.}
  }
}
\note{
  In the original analyses, a constant of one was added to all frequencies.
}
\source{
  Klauer, K.C., Stahl, C., & Erdfelder, E. (2007).
  The abstract selection task: New data and an almost comprehensive model.
  \emph{Journal of Experimental Psychology: Learning, Memory, and Cognition},
  \bold{33}, 680--703.
  \doi{10.1037/0278-7393.33.4.680}
}
\seealso{
  \code{\link{mpt}}.
}
\examples{
data(selectiontask)

## Inference-guessing model with relaxed assumptions
s <- mptspec("WST", .replicates = 2)
m1 <- mpt(s, data = selectiontask$exp1, method = "EM")

## Inference-guessing model
m2 <- mpt(update(s, .restr = list(sf1=s1, sb1=s1, sfb1=s1,
                                  sf2=s2, sb2=s2, sfb2=s2)),
          data = m1$y, method = "EM")

## Effect of hint on i parameter (Exp. 1)
m3 <- mpt(update(m2$spec, .restr = list(i2=i1)), data = m1$y,
          method = "EM")

## Independence model
m4 <- mpt(update(m2$spec,
                 .restr = list(a1=0, c1=0, x1=0, d1=0, s1=0, i1=0,
                               a2=0, c2=0, x2=0, d2=0, s2=0, i2=0)),
          data = m1$y, method = "EM")

anova(m4, m3, m2, m1)
plogis(confint(m2))
AIC(m2)
BIC(m2)  # BIC w/number of non-redundant response categories
AIC(m2, k = log(sum(m2$y)))  # BIC w/total number of data points

## Effect of hint on c parameter (Exp. 2)
m5 <- mpt(m2$spec, data = selectiontask$exp2, method = "EM")
m6 <- mpt(update(m5$spec, .restr = list(c2=c1)), data = m5$y,
          method = "EM")
anova(m6, m5)

idx <- c("P", "p", "Q", "q", "a", "c", "x", "d", "s", "i")
par(mfrow = 1:2)
dotchart(coef(m2)[paste0(idx, 1)], xlim=c(0, 1), labels=idx,
         main="Exp. 1", xlab="")
points(coef(m2)[paste0(idx, 2)], 1:10, pch=16)
legend(0, 11, c("standard", "hints"), pch=c(1, 16),
       title="Instruction", bty="n")
dotchart(coef(m5)[paste0(idx, 1)], xlim=c(0, 1), labels=idx,
         main="Exp. 2", xlab="")
points(coef(m5)[paste0(idx, 2)], 1:10, pch=16)
mtext("Parameter estimate (inference-guessing model)", side=1,
      outer=TRUE, line=-2)
mtext("Klauer et al. (2007)", side=3, outer=TRUE, line=-3)
}
\keyword{datasets}
