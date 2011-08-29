\name{mpt}
\alias{mpt}
\alias{print.mpt}
\alias{anova.mpt}
\alias{vcov.mpt}
\alias{summary.mpt}
\alias{print.summary.mpt}
\title{Multinomial Processing Tree (MPT) Models}
\description{Fits a (joint) multinomial processing tree model using the EM
  algorithm.
}
\usage{
mpt(formula, data, treeid = "treeid", constr = NULL,
  start = rep(0.5, length(all.vars(formula[[3]]))), \dots)

\method{summary}{mpt}(object, \dots)

\method{anova}{mpt}(object, \dots, test = c("Chisq", "none"))
}
\arguments{
  \item{formula}{an object of class \code{formula}: a symbolic description of
    the model to be fitted.  (See Details and Examples.)}
  \item{data}{a data frame consisting at least of one variable that contains
    the absolute response frequencies. Alternatively, a (named) vector of
    frequencies.}
  \item{treeid}{name of the variable that identifies the processing trees
    of a joint multinomial model. Alternatively, a vector that identifies each
    tree.}
  \item{constr}{a named list of character vectors consisting of parameter
    names.  The parameters in each vector are constrained to be equal.}
  \item{start}{a vector of starting values for the parameter estimates.}
  \item{\dots}{additional arguments passed to \code{\link{mptEM}}.}
  \item{object}{an object of class \code{mpt}, typically the result of a
    call to \code{mpt}.}
  \item{test}{should the p-values of the chi-square distributions be
    reported?}
}
\details{
  Multinomial processing tree models (Batchelder & Riefer, 1999; Erdfelder et
  al., 2009; Riefer & Batchelder, 1988) seek to represent the categorical
  responses of a group of subjects by a small number of latent
  (psychological) parameters.  These models have a tree-like graph, the links
  being the parameters, the leaves being the response categories.  The path
  from the root to one of the leaves represents the cognitive processing
  steps executed to arrive at a given response.

  Each row in \code{data} corresponds to one response category.  The model
  formula symbolically describes the model, for example it could look like
  
    \code{var ~ list(x + (1 - x)*y, (1 - x)*(1 - y), y, 1 - y, ...)}
  
  where the left hand side is the variable that contains the response
  frequencies.  The right hand side gives the model equations in a list.
  Each component of the list represents the probability of a response in the
  corresponding category (link probabilities are multiplied, branch
  probabilities are added).  Thus, the list usually has as many components as
  rows in the data frame.

  Joint (or product) multinomial models consist of more than one processing
  tree.  The \code{treeid} should uniquely identify each tree.  The model
  formula does not distinguish between joint and simple models: All model
  equations should go into a single list.
  
  Parameter estimation is carried out by \code{\link{mptEM}} which implements
  the EM algorithm.
}
\value{
  \item{coefficients}{a vector of parameter estimates.}
  \item{fitted.values}{the fitted response frequencies.}
  \item{loglik}{the log-likelihood of the fitted model.}
  \item{a}{structural constants passed to \code{\link{mptEM}}.}
  \item{b}{structural constants passed to \code{\link{mptEM}}.}
  \item{c}{structural constants passed to \code{\link{mptEM}}.}
  \item{goodness.of.fit}{the goodness of fit statistic including the
    likelihood ratio fitted vs. saturated model (G2), the degrees of
    freedom, and the p-value of the corresponding chi-square distribution.}
  \item{iter}{the number of iterations of the EM algorithm.}
  \item{pcat}{the predicted probabilities for each response category.}
  \item{pbranch}{the predicted probabilities for each branch in the tree.}
  \item{formula}{the model formula.}
  \item{ntrees}{the number of trees in a joint multinomial model.}
  \item{n}{the total number of observations per tree.}
  \item{y}{the vector of response frequencies.}
}
\references{
  Batchelder, W.H. & Riefer, D.M. (1999).
  Theoretical and empirical review of multinomial process tree modeling.
  \emph{Psychonomic Bulletin & Review}, \bold{6}, 57--86.

  Erdfelder, E., Auer, T., Hilbig, B.E., Assfalg, A., Moshagen, M., &
  Nadarevic, L. (2009).
  Multinomial processing tree models: A review of the literature.
  \emph{Zeitschrift fuer Psychologie}, \bold{217}, 108--124.

  Riefer, D.M., & Batchelder, W.H. (1988).
  Multinomial modeling and the measurement of cognitive processes.
  \emph{Psychological Review}, \bold{95}, 318--339.
}
\author{Florian Wickelmaier}
\seealso{
  \code{\link{mptEM}}, \code{\link{mptmodel}},\code{\link{simulate.mpt}},
  \code{\link{retroact}}, \code{\link{proact}},
  \code{\link{plot.mpt}}, \code{\link{residuals.mpt}},
  \code{\link{logLik.mpt}}.
}
\examples{
## Storage-retrieval model for pair clustering (Riefer & Batchelder, 1988)
data(retroact)

mpt1 <- mpt(freq ~ list(
  c*r,
  (1 - c)*u^2,
  2*(1 - c)*u*(1 - u),
  c*(1 - r) + (1 - c)*(1 - u)^2,
  u,
  1 - u
), retroact[retroact$lists == 0,])

summary(mpt1)  # parameter estimates, goodness of fit
plot(mpt1)     # residuals versus predicted values
confint(mpt1)  # approximate confidence intervals

plot(mpt1$coef, axes=FALSE, ylim=0:1, pch=16, xlab="",
  ylab="Parameter estimate (MPT model, 95\% CI)")
axis(1, 1:3, names(mpt1$coef)); axis(2)
arrows(1:3, confint(mpt1)[,1], 1:3, confint(mpt1)[,2], .05, 90, 3)

## See ?retroact and ?proact for further examples.
}
\keyword{models}
