\name{retroact}
\alias{retroact}
\title{Recall Frequencies for a Retroactive Inhibition Experiment}
\description{
  The experiment is described in Riefer and Batchelder (1988).  Each of the
  75 subjects was presented with either one, two, three, four, or five
  successive lists of words (15 subjects per group).  These words were shown
  in random order on a computer screen, one word at a time, at a rate of 5 s
  per word.  Each list contained 25 words, consisting of 10 categories (with
  2 associate words per category) and five singletons.  Subjects were given
  1.5 min to recall in writing the 25 words from each individual list.  After
  all of the lists had been presented, a final free-recall test was given in
  which subjects attempted to recall the words from all of the previous
  lists.  Subjects were given up to 5 min for this final written recall.
  
  The focus here is on the recall of the first-list words during the final
  recall task.  Subjects responses were classified into six response
  categories and pooled across subjects.
}
\usage{data(retroact)}
\format{
  A data frame consisting of four variables:
  \describe{
    \item{\code{lists}}{the number of interpolated lists}
    \item{\code{treeid}}{an identifier for the single trees of the joint
      multinomial model}
    \item{\code{resp}}{a factor giving the response category; \code{n1}
      pair is recalled adjacently, \code{n2} pair is recalled non-adjacently,
      \code{n3} one word in a pair is recalled, \code{n4} neither word in a
      pair is recalled, \code{m1} recall of a singleton, \code{m2} non-recall
      of a singleton}
    \item{\code{freq}}{the aggregate recall frequencies per condition}
  }
}
\source{
  Riefer, D.M., & Batchelder, W.H. (1988).
  Multinomial modeling and the measurement of cognitive processes.
  \emph{Psychological Review}, \bold{95}, 318--339.
}
\examples{
data(retroact)

## Fitting individual storage-retrieval models per condition
formula <- freq ~ list(
  c*r,
  (1 - c)*u^2,
  2*(1 - c)*u*(1- u),
  c*(1 - r) + (1 - c)*(1 - u)^2,
  u,
  1 - u
)

pars <- sapply(0:4,
  function(x) coef(mpt(formula, retroact[retroact$lists == x,])))

## Figure 3 in Riefer & Batchelder (1988)
plot(pars["c",] ~ I(0:4), pch=16, type="b", ylim=c(.3, 1),
  xlab="Number of interpolated lists, j",
  ylab="Parameter estimate (Storage-retrieval model)")
points(pars["r",] ~ I(0:4), type="b", lty=2)
text(3, .89, expression("Storage of clusters," ~ hat(c)[j]))
text(3, .46, expression("Retrieval of clusters," ~ hat(r)[j]))

## Testing hypotheses about the parameters
mpt1 <- mpt(freq ~ list(
  c0*r0,
  (1 - c0)*u0^2,
  2*(1 - c0)*u0*(1- u0),
  c0*(1 - r0) + (1 - c0)*(1 - u0)^2,
  u0,
  1 - u0,

  c1*r1,
  (1 - c1)*u1^2,
  2*(1 - c1)*u1*(1- u1),
  c1*(1 - r1) + (1 - c1)*(1 - u1)^2,
  u1,
  1 - u1,

  c2*r2,
  (1 - c2)*u2^2,
  2*(1 - c2)*u2*(1- u2),
  c2*(1 - r2) + (1 - c2)*(1 - u2)^2,
  u2,
  1 - u2,

  c3*r3,
  (1 - c3)*u3^2,
  2*(1 - c3)*u3*(1- u3),
  c3*(1 - r3) + (1 - c3)*(1 - u3)^2,
  u3,
  1 - u3,

  c4*r4,
  (1 - c4)*u4^2,
  2*(1 - c4)*u4*(1- u4),
  c4*(1 - r4) + (1 - c4)*(1 - u4)^2,
  u4,
  1 - u4
), retroact)

mpt2 <- mpt(mpt1$formula, retroact, constr=list(r = paste("r", 0:4, sep="")))
mpt3 <- mpt(mpt1$formula, retroact, constr=list(c = paste("c", 0:4, sep="")))

anova(mpt2, mpt1)  # r decreases the more lists have been interpolated
anova(mpt3, mpt1)  # c remains constant
}
\keyword{datasets}
