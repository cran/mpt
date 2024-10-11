\name{retroact}
\alias{retroact}
\title{Recall Frequencies in Retroactive Inhibition}
\description{
  Riefer and Batchelder (1988) presented each of 75 participants with either
  one, two, three, four, or five successive lists of words (15 subjects per
  group).  These words were shown in random order on a computer screen, one
  word at a time, at a rate of 5 s per word.  Each list contained 25 words,
  consisting of 10 categories (with 2 associate words per category) and five
  singletons.  Subjects were given 1.5 min to recall in writing the 25 words
  from each individual list.  After all of the lists had been presented, a
  final free-recall test was given in which subjects attempted to recall the
  words from all of the previous lists.  Subjects were given up to 5 min for
  this final written recall.
  
  The focus here is on the recall of the first-list words during the final
  recall task.  The responses were classified into six categories and pooled
  across subjects.
}
\usage{data(retroact)}
\format{
  A data frame consisting of four variables:
  \describe{
    \item{\code{lists}}{the number of interpolated lists.}
    \item{\code{treeid}}{an identifier for the single trees of the joint
      multinomial model.}
    \item{\code{resp}}{a factor giving the response category; \code{E1}
      pair is recalled adjacently, \code{E2} pair is recalled non-adjacently,
      \code{E3} one word in a pair is recalled, \code{E4} neither word in a
      pair is recalled, \code{F1} recall of a singleton, \code{F2} non-recall
      of a singleton.}
    \item{\code{freq}}{the aggregate recall frequencies per condition.}
  }
}
\source{
  Riefer, D.M., & Batchelder, W.H. (1988).
  Multinomial modeling and the measurement of cognitive processes.
  \emph{Psychological Review}, \bold{95}(3), 318--339.
  \doi{10.1037/0033-295x.95.3.318}
}
\seealso{
  \code{\link{mpt}}.
}
\examples{
data(retroact)

## Fitting separate storage-retrieval pair-clustering models per condition
spec <- mptspec(
  c*r,
  (1 - c)*u^2,
  2*(1 - c)*u*(1 - u),
  c*(1 - r) + (1 - c)*(1 - u)^2,
  u,
  1 - u
)
pars <- sapply(0:4,
  function(x) coef(mpt(spec, retroact[retroact$lists == x, ])))

## Figure 3 in Riefer & Batchelder (1988)
plot(pars["c", ] ~ I(0:4), pch = 16, type = "b", ylim = c(.3, 1),
     xlab = "Number of interpolated lists, j",
     ylab = "Parameter estimate (Storage-retrieval model)",
     main = "Riefer and Batchelder (1988)")
points(pars["r", ] ~ I(0:4), type = "b", lty = 2)
text(3, .89, expression("Storage of clusters," ~ hat(c)[j]))
text(3, .46, expression("Retrieval of clusters," ~ hat(r)[j]))

## Testing effects of interpolated lists: joint models
spec <- mptspec(
  c0*r0,
  (1 - c0)*u0^2,
  2*(1 - c0)*u0*(1 - u0),
  c0*(1 - r0) + (1 - c0)*(1 - u0)^2,
  u0,
  1 - u0,

  c1*r1,
  (1 - c1)*u1^2,
  2*(1 - c1)*u1*(1 - u1),
  c1*(1 - r1) + (1 - c1)*(1 - u1)^2,
  u1,
  1 - u1,

  c2*r2,
  (1 - c2)*u2^2,
  2*(1 - c2)*u2*(1 - u2),
  c2*(1 - r2) + (1 - c2)*(1 - u2)^2,
  u2,
  1 - u2,

  c3*r3,
  (1 - c3)*u3^2,
  2*(1 - c3)*u3*(1 - u3),
  c3*(1 - r3) + (1 - c3)*(1 - u3)^2,
  u3,
  1 - u3,

  c4*r4,
  (1 - c4)*u4^2,
  2*(1 - c4)*u4*(1 - u4),
  c4*(1 - r4) + (1 - c4)*(1 - u4)^2,
  u4,
  1 - u4
)
m1 <- mpt(spec, retroact)
m2 <- mpt(update(spec, .restr = list(r0=r, r1=r, r2=r, r3=r, r4=r)),
          retroact)
m3 <- mpt(update(spec, .restr = list(c0=c, c1=c, c2=c, c3=c, c4=c)),
          retroact)

anova(m2, m1)  # r decreases the more lists have been interpolated
anova(m3, m1)  # c remains constant
}
\keyword{datasets}
