\name{proact}
\alias{proact}
\title{Recall Frequencies for DaPolito's Experiment on Proactive Inhibition}
\description{
  In DaPolito's experiment (Greeno, James, DaPolito, & Polson, 1978), 60
  subjects were presented with lists of stimulus-response associates to be
  learned, followed by a test in which only the stimuli were presented and
  the responses had to be recalled.  Stimuli consisted of three-letter
  syllables, responses of the numbers from 1 to 30, so list items looked
  like, say, ESI-12, JOK-3, MAL-8, etc.  Part of the items had two responses
  (A-B, A-C), the control items had only a single correct response.  If the
  recall of C responses is poorer than that of control items, then proactive
  inhibition has occurred, that is interference with the recall by
  information that has been learned earlier.
  
  Riefer and Batchelder (1988) analyzed only the A-B and A-C items.  They
  investigated how repeated A-B presentation affects the B and C recall,
  respectively.  The responses were classified into four categories and
  pooled across subjects.
}
\usage{data(proact)}
\format{
  A data frame consisting of five variables:
  \describe{
    \item{\code{test}}{first or second test.}
    \item{\code{abpres}}{the number of A-B presentations.}
    \item{\code{resp}}{a factor giving the response category; \code{BC}
      both B and C responses are correctly recalled, \code{Bc} only B is
      recalled, \code{bC} only C is recalled, \code{bc} neither response is
      recalled.}
    \item{\code{freq}}{the aggregate recall frequencies per condition.}
    \item{\code{treeid}}{an identifier for the single trees of the joint
      multinomial model.}
  }
}
\source{
  Greeno, J.G., James, C.T., DaPolito, F, & Polson, P.G. (1978).
  \emph{Associative learning: A cognitive analysis}.
  Englewood Cliffs, NJ: Prentice-Hall.

  Riefer, D.M., & Batchelder, W.H. (1988).
  Multinomial modeling and the measurement of cognitive processes.
  \emph{Psychological Review}, \bold{95}(3), 318--339.
  \doi{10.1037/0033-295x.95.3.318}
}
\seealso{
  \code{\link{mpt}}.
}
\examples{
data(proact)

## Testing effects of repeated A-B presentations
spec <- mptspec(
  .BC = p*q*r,
  .Bc = p*q*(1 - r),
  .bC = p*(1 - q)*r,
  .bc = (1 - p) + p*(1 - q)*(1 - r),
  .replicates = 6
)
m1 <- mpt(spec, proact)
m2 <- mpt(update(spec, .restr = list(q2=q1, q3=q1, q5=q4, q6=q4)), proact)
m3 <- mpt(update(spec, .restr = list(r2=r1, r3=r1, r5=r4, r6=r4)), proact)

anova(m2, m1)  # q increases with number of A-B presentations
anova(m3, m1)  # r remains constant
}
\keyword{datasets}
