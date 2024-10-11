\name{pwrsim}
\alias{pwrsim}
\alias{pwrsim1}
\alias{pwrsim2}
\title{Simulation-Based Power Analysis for MPT Models}
\description{
  The \code{pwrsim1} data contain the results of a power analysis of the
  goodness-of-fit test of the storage-retrieval pair-clustering model.
  Specifically, the hypothesis \eqn{a = u} is tested for various parameter
  differences and sample sizes.

  The \code{pwrsim2} data contain the results of a power analysis of a test
  of age differences in retrieval.  Specifically, the hypothesis
  \eqn{r_{young} = r_{old}} is tested for various parameter differences and
  sample sizes.

  Simulation-based power analysis involves four steps (e.g., Wickelmaier,
  2022): specifying a model that includes the effect of interest, generating
  data from the model, testing the null hypothesis, repeating data generation
  and testing.  The proportion of times the test turns out significant is an
  estimate of its power.
}
\usage{data(pwrsim)}
\format{
  \code{pwrsim1} A data frame consisting of 36 rows and four columns:
  \describe{
    \item{\code{d}}{the difference between the \eqn{a} parameter and the
      \eqn{u} parameter.}
    \item{\code{n}}{the sample size.}
    \item{\code{pval}}{500 p values, one for each replication of the
      experiment.}
    \item{\code{pwr}}{the proportion of significant tests.}
  }
  \code{pwrsim2} A data frame consisting of 15 rows and four columns:
  \describe{
    \item{\code{d}}{the difference between the \eqn{r_{young}} parameter and
      the \eqn{r_{old}} parameter.}
    \item{\code{n}}{the sample size.}
    \item{\code{pval}}{500 p values, one for each replication of the
      experiment.}
    \item{\code{pwr}}{the proportion of significant tests.}
  }
}
\references{
  Wickelmaier, F. (2022).
  Simulating the power of statistical tests: A collection of R examples.
  \emph{ArXiv}.
  \doi{10.48550/arXiv.2110.09836}
}
\seealso{
  \code{\link{mpt}}.
}
\examples{
data(pwrsim)

## Power simulation 1: goodness-of-fit test, H0: a = u ------------------

s <- mptspec(
  E.1 = c * r,
  E.2 = (1 - c) * u^2,
  E.3 = 2 * (1 - c) * u * (1 - u),
  E.4 = c * (1 - r) + (1 - c) * (1 - u)^2,
  F.1 = a,
  F.2 = 1 - a
)

## Before you use par2prob(), carefully check position of parameters!
s$par
s$par2prob(c(c = 0.5, r = 0.5, u = 0.4, a = 0.6))  # evaluate model eqns

dataGen <- function(nn, d) {
  structure(list(                                       # stub mpt object
    treeid = s$treeid,
         n = setNames((nn * c(2, 1)/3)[s$treeid], s$treeid),  # 2:1 ratio
      pcat = s$par2prob(c(c = 0.5, r = 0.5,
                          u = 0.5 - d/2, a = 0.5 + d/2))
  ), class = "mpt") |>
    simulate()
}

testFun <- function(nn, d) {
  y <- dataGen(nn, d)                         # generate data with effect
  m1 <- mpt(s, y)
  m2 <- mpt(update(s, .restr = list(a = u)), y)
  anova(m2, m1)$"Pr(>Chi)"[2]                   # test H0, return p value
}

pwrsim1 <- expand.grid(d = seq(0, 0.5, 0.1), n = 30 * 2^(0:5))

\dontrun{
pwrsim1$pval <-
  mapply(function(nn, d) replicate(500, testFun(nn, d)),
         nn = pwrsim1$n, d = pwrsim1$d, SIMPLIFY = FALSE)
pwrsim1$pwr <- sapply(pwrsim1$pval, function(p) mean(p < .05))
}

## Power simulation 2: age differences in retrieval ---------------------

s <- mptspec("SR", .replicates = 2)

dataGen <- function(nn, d) {
  structure(list(
    treeid = s$treeid,
         n = setNames((nn/2 * c(2, 1, 2, 1)/3)[s$treeid], s$treeid),
      pcat = s$par2prob(c(c1 = 0.5, r1 = 0.4 + d/2, u1 = 0.3,   # young
                          c2 = 0.5, r2 = 0.4 - d/2, u2 = 0.3))  # old
  ), class = "mpt") |>
    simulate(m)
}

testFun <- function(nn, d) {
  y <- dataGen(nn, d)
  m1 <- mpt(s, y)
  m2 <- mpt(update(s, .restr = list(r1 = r2)), y)
  anova(m2, m1)$"Pr(>Chi)"[2]
}

pwrsim2 <- expand.grid(d = seq(0, 0.4, 0.1), n = 120 * 2^(0:2))

\dontrun{
pwrsim2$pval <-
  mapply(function(nn, d) replicate(500, testFun(nn, d)),
         nn = pwrsim2$n, d = pwrsim2$d, SIMPLIFY = FALSE)
pwrsim2$pwr <- sapply(pwrsim2$pval, function(p) mean(p < .05))
}
}
\keyword{datasets}
