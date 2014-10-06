\name{mpt}
\alias{mpt}
\alias{anova.mpt}
\alias{coef.mpt}
\alias{confint.mpt}
\alias{predict.mpt}
\alias{print.mpt}
\alias{summary.mpt}
\alias{print.summary.mpt}
\title{Multinomial Processing Tree (MPT) Models}
\description{Fits a (joint) multinomial processing tree (MPT) model specified
  by a symbolic description via \code{\link{mptspec}}.
}
\usage{
mpt(spec, data, start = NULL, method = c("BFGS", "EM"), treeid = "treeid",
    freqvar = "freq", optimargs = 
      if(method == "BFGS") list(control =
        list(reltol = .Machine$double.eps^(1/1.2), maxit = 1000))
      else list())

\method{anova}{mpt}(object, \dots, test = c("Chisq", "none"))

\method{coef}{mpt}(object, logit = FALSE, \dots)

\method{confint}{mpt}(object, parm, level = 0.95, logit = TRUE, \dots)

\method{predict}{mpt}(object, newdata = NULL, type = c("freq", "prob"), \dots)

\method{summary}{mpt}(object, \dots)
}
\arguments{
  \item{spec}{an object of class \code{mptspec}: typically result of a call to
    \code{\link{mptspec}}. A symbolic description of the model to be
    fitted. (See Details and Examples.)}
  \item{data}{a data frame consisting at least of one variable that contains
    the absolute response frequencies. Alternatively, a (named) vector or
    matrix of frequencies.}
  \item{start}{a vector of starting values for the parameter estimates.}
  \item{method}{optimization method. Implemented are
    \code{optim(..., method = "BFGS")} and the EM algorithm.}
  \item{treeid}{name of the variable that identifies the processing trees
    of a joint multinomial model. Alternatively, a vector that identifies each
    tree.}
  \item{freqvar}{if \code{data} is a data frame, name of the variable that
    holds the response frequencies; else ignored.}
  \item{logit}{logical. Parameter estimates on logit or probability scale.}
  \item{optimargs}{a list of arguments passed to the optimization function,
    either \code{\link{optim}} or \code{\link{mptEM}}.}
  \item{object}{an object of class \code{mpt}, typically the result of a
    call to \code{mpt}.}
  \item{test}{should the p-values of the chi-square distributions be
    reported?}
  \item{parm, level}{See \code{\link{confint.default}}.}
  \item{newdata}{a vector of response frequencies.}
  \item{type}{predicted frequencies or probabilities.}
  \item{\dots}{additional arguments passed to other methods.}
}
\details{
  Multinomial processing tree models (Batchelder & Riefer, 1999; Erdfelder et
  al., 2009; Riefer & Batchelder, 1988) seek to represent the categorical
  responses of a group of subjects by a small number of latent
  (psychological) parameters.  These models have a tree-like graph, the links
  being the parameters, the leaves being the response categories.  The path
  from the root to one of the leaves represents the cognitive processing
  steps executed to arrive at a given response.

  If \code{data} is a data frame, each row corresponds to one response
  category.  If \code{data} is a vector or matrix, each element or column
  corresponds to one response category.  The order of response categories and
  of model equations specified in \code{\link{mptspec}} should match.

  Joint (or product) multinomial models consist of more than one processing
  tree.  The \code{treeid} should uniquely identify each tree.

  Per default, parameter estimation is carried out by \code{\link{optim}}'s
  BFGS method on the logit scale with analytical gradients; it can be switched
  to \code{\link{mptEM}} which implements the EM algorithm.
}
\value{
  An object of class \code{mpt} containing the following components:

  \item{coefficients}{a vector of parameter estimates.}
  \item{loglik}{the log-likelihood of the fitted model.}
  \item{nobs}{the number of free response categories.}
  \item{fitted}{the fitted response frequencies.}
  \item{goodness.of.fit}{the goodness of fit statistic including the
    likelihood ratio fitted vs. saturated model (G2), the degrees of
    freedom, and the p-value of the corresponding chi-square distribution.}
  \item{ntrees}{the number of trees in a joint multinomial model.}
  \item{n}{the total number of observations per tree.}
  \item{y}{the vector of response frequencies.}
  \item{pcat}{the predicted probabilities for each response category.}
  \item{treeid}{a vector that identifies each tree.}
  \item{a, b, c}{structural constants passed to \code{\link{mptEM}}.}
  \item{spec}{the MPT model specification returned by \code{\link{mptspec}}.}
  \item{method}{the optimization method used.}
  \item{optim}{the return value of the optimization function.}
}
\references{
  Batchelder, W.H., & Riefer, D.M. (1999).
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
\seealso{
  \code{\link{mptEM}}, \code{\link{mptspec}}, \code{\link{simulate.mpt}},
  \code{\link{retroact}}, \code{\link{proact}},
  \code{\link{plot.mpt}}, \code{\link{residuals.mpt}},
  \code{\link{logLik.mpt}}, \code{\link{vcov.mpt}}, \code{\link{optim}}.
}
\examples{
## Storage-retrieval model for pair clustering (Riefer & Batchelder, 1988)
data(retroact)

spec <- mptspec(
  c*r,
  (1 - c)*u^2,
  2*(1 - c)*u*(1 - u),
  c*(1 - r) + (1 - c)*(1 - u)^2,
  u,
  1 - u
)
mpt1 <- mpt(spec, retroact[retroact$lists == 0, ])

summary(mpt1)  # parameter estimates, goodness of fit
plot(mpt1)     # residuals versus predicted values
confint(mpt1)  # approximate confidence intervals

plot(coef(mpt1), axes=FALSE, ylim=0:1, pch=16, xlab="",
     ylab="Parameter estimate (MPT model, 95\% CI)")
axis(1, 1:3, names(coef(mpt1))); axis(2)
arrows(1:3, plogis(confint(mpt1))[,1], 1:3, plogis(confint(mpt1))[,2],
       .05, 90, 3)

## See ?retroact and ?proact for further examples.
}
\keyword{models}
