\name{moraldilemma}
\alias{moraldilemma}
\alias{MDHennig2020}
\alias{MDreplication}
\title{Moral Dilemma Judgment}
\description{
  Hennig and Huetter (2020) proposed a multinomial model of moral dilemma
  judgment and evaluated the model in a series of experiments. Participants
  were presented with hypothetical scenarios that required a decision whether
  or not to break a moral norm.

  Berentelg (2020) conducted a replication study that was designed to be
  similar to Experiment 2b in Hennig and Huetter (2020).
}
\usage{data(moraldilemma)}
\format{
  \code{MDHennig2020} A data frame consisting of seven variables:
  \describe{
    \item{\code{selfrel}}{factor. Instructions about self-relevant
      consequences of the decision where either (\code{absent}) or
      (\code{present}).}
    \item{\code{congrcy}}{factor. Endorsement of consequences and
      norm endorsement lead to different decisions (\code{incongruent}) or to
      the same decision (\code{congruent}).}
    \item{\code{default}}{factor. The norm may be adhered to by continuing
      (\code{inaction} default state) or by changing (\code{action} default
      state) an ongoing behavior.}
    \item{\code{breaknorm}}{factor. Decision to break the norm.}
    \item{\code{exp1}, \code{exp2b}}{the aggregate response frequencies for
      Experiment 1 and 2b, respectively.}
    \item{\code{treeid}}{an identifier for the single trees of the joint
      multinomial model.}
  }
  \code{MDreplication} A data frame containing 751 observations of five
    variables:
  \describe{
    \item{\code{selfrel}}{factor. See above.}
    \item{\code{gender}}{factor. Participant gender.}
    \item{\code{age}}{participant age.}
    \item{\code{rt}}{median response time (in seconds) across scenarios.}
    \item{\code{y}}{a matrix of response frequencies per participant. Each
      column represents a combination of the factors \code{congrcy},
      \code{default}, and \code{breaknorm}.}
  }
}
\source{
  Hennig, M., & Huetter, M. (2020).
  Revisiting the divide between deontology and utilitarianism in moral
  dilemma judgment: A multinomial modeling approach.
  \emph{Journal of Personality and Social Psychology},
  \bold{118}(1), 22--56.
  \doi{10.1037/pspa0000173}
  
  Berentelg, M. (2020).
  \emph{Multinomial modeling of moral dilemma judgment: A replication
        study}.
  Bachelor thesis. University of Tuebingen, Germany.
  \url{https://osf.io/mb32t/}
}
\seealso{
  \code{\link{mpt}}.
}
\examples{
data(moraldilemma)

## Exp. 1: proCNI and process dissociation (PD) model
s <- mptspec("proCNI")
exp1 <- subset(MDHennig2020, selfrel == "absent")
mpt(update(s, .restr = list(J=I)),      data = exp1, freqvar = "exp1")
mpt(update(s, .restr = list(I=0, J=1)), data = exp1, freqvar = "exp1")

## Exp. 2b: self-relevant consequences and norm endorsement
s <- mptspec("proCNI", .replicates = 2, .restr = list(J1=I1, J2=I2))
m1 <- mpt(s, data = MDHennig2020, freqvar = "exp2b")
m2 <- mpt(update(m1$spec, .restr = list(N1=N2)), data = m1$y)
anova(m2, m1)

## Replication of Exp. 2b
md.agg <- aggregate(y ~ selfrel, MDreplication, sum)
y <- as.vector(t(md.agg[, -1]))

m3 <- mpt(s, data = y)
m4 <- mpt(update(s, .restr = list(N1=N2)), data = y)
anova(m4, m3)

coefs <- c(diff(coef(m3)[c("N2", "N1")]),
           diff(coef(m1)[c("N2", "N1")]))
names(coefs) <- c("Replication", "Hennig & Huetter\n(2020, Exp. 2b)")
ci <- coefs + rbind(
  qnorm(c(.025, .975))*sqrt(sum(diag(vcov(m3))[c("N2", "N1")])),
  qnorm(c(.025, .975))*sqrt(sum(diag(vcov(m1))[c("N2", "N1")]))
)
dotchart(coefs, pch = 16, xlim = c(-.2, 1),
  xlab = expression(N[absent] - N[present]~"(proCNI model, 95\% CI)"),
  main = paste("Self-relevant consequences and norm endorsement",
               "in moral dilemma judgment", sep = "\n"))
abline(v = 0, col = "gray")
arrows(ci[, 1], 1:2, ci[, 2], 1:2, .05, 90, 3)
}
\keyword{datasets}
