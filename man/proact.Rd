\name{proact}
\alias{proact}
\title{Recall Frequencies for DaPolito's Experiment on Proactive Inhibition}
\description{
  In DaPolito's experiment (Greeno, James, DaPolito & Polson, 1978), 60
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
  \emph{Psychological Review}, \bold{95}, 318--339.
  \doi{10.1037/0033-295x.95.3.318}
}
\examples{
data(proact)

## Testing hypotheses about the parameters
mpt1 <- mpt(mptspec(
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
  (1 - p3) + p3*(1 - q3)*(1 - r3),

  p4*q4*r4,
  p4*q4*(1 - r4),
  p4*(1 - q4)*r4,
  (1 - p4) + p4*(1 - q4)*(1 - r4),

  p5*q5*r5,
  p5*q5*(1 - r5),
  p5*(1 - q5)*r5,
  (1 - p5) + p5*(1 - q5)*(1 - r5),

  p6*q6*r6,
  p6*q6*(1 - r6),
  p6*(1 - q6)*r6,
  (1 - p6) + p6*(1 - q6)*(1 - r6)
), proact)

mpt2 <- mpt(update(mpt1$spec, .restr=list(q2=q1, q3=q1, q4=q2, q5=q2, q6=q2)),
            proact)
mpt3 <- mpt(update(mpt1$spec, .restr=list(r2=r1, r3=r1, r4=r2, r5=r2, r6=r2)),
            proact)

anova(mpt2, mpt1)  # q increases with number of A-B presentations
anova(mpt3, mpt1)  # r remains constant
}
\keyword{datasets}
