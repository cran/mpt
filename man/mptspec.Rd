\name{mptspec}
\alias{mptspec}
\alias{print.mptspec}
\alias{update.mptspec}
\title{Specify a Multinomial Processing Tree (MPT) Model}
\description{Returns the specification of an MPT model object for fitting with
  \code{\link{mpt}}.}
\usage{
mptspec(\dots, .restr = NULL)

\method{update}{mptspec}(object, .restr = NULL, \dots)
}
\arguments{
  \item{\dots}{(named) expressions or a character string specifying the model.
    See Details.}
  \item{.restr}{a named list of parameter restrictions. See Details.}
  \item{object}{an object of class \code{mptspec}.}
}
\details{
  \code{\dots} is used to symbolically specify the MPT model equations by
  suitable expressions, for example, they could look like this

    \code{r + (1 - r)*b, (1 - r)*(1 - b), b, 1 - b}
 
  where each expression represents the probability of a response in the
  corresponding category (link probabilities are multiplied, branch
  probabilities are added).  Thus, there usually are as many expressions as
  response categories.

  Joint (or product) multinomial models consist of more than a single
  processing tree.  To identify the trees in such a model, expressions may
  have optional names.  Canonically, these names are of the form \code{x.y},
  where \code{x} is the tree identifier and \code{y} specifies the response
  category within a tree.

  Alternatively, \code{\dots} may be a character string identifying one out of
  a list of prevalent MPT models. Currently implemented are the following
  models (other models have to be specified by explicit expressions as
  described above):

  \code{1HT}: the one-high-threshold model (Blackwell, 1963; Swets, 1961).

  \code{2HT}: the two-high-threshold model (Snodgrass & Corwin, 1988; see also
    Broeder & Schuetz, 2009).

  \code{PairAsso}: the paired-associate learning model (Riefer & Batchelder,
    1988).

  \code{rmodel}: the r-model of recognition heuristic use (Hilbig, Erdfelder,
    & Pohl, 2010).

  \code{SourceMon}: the source-monitoring model (Batchelder & Riefer, 1990).

  \code{SR}, \code{SR2}: the storage-retrieval model for pair clustering
    (Batchelder & Riefer, 1986). \code{SR2} is the model without singleton
    items.

  If one of these models is selected, \code{\dots} may include an optional
  \code{.replicates} argument that specifies the number of replicates of the
  model equations, for example, when the same model is applied repeatedly in
  several experimental conditions.  Accordingly, parameter names are
  augmented by numbers to make them unique.

  Parameter restrictions included in \code{.restr} may be of the form
  \code{b = r} or \code{b = 0.5} etc. Depending on the fitting algorithm
  employed in \code{\link{mpt}} (\code{BFGS}, but not \code{EM}), mathematical
  functions are permissible, for example, \code{b = sqrt(r)}.

  The \code{update} method is used to add parameter restrictions to an
  existing \code{mptspec} object.
}
\value{
  An object of class \code{mptspec} that serves as input to \code{\link{mpt}}
  which fits the model to data. It consists of the following components:

  \item{par2prob}{a function that takes a vector of parameter values and
    computes the response probabilities.}
  \item{par2deriv}{a function that takes a vector of parameter values and
    computes first and second derivatives of the model equations.}
  \item{prob}{a list containing expressions of the model equations.}
  \item{deriv}{a list containing expressions of the first and second
    derivatives of the model equations.}
  \item{par}{a named vector of parameter values.}
  \item{restr}{a list containing expressions of parameter restrictions.}
}
\references{
  Batchelder, W.H., & Riefer, D.M. (1986).
  The statistical analysis of a model for storage and retrieval processes in
  human memory.
  \emph{British Journal of Mathematical and Statistical Psychology},
  \bold{39}, 129--149.

  Batchelder, W.H., & Riefer, D.M. (1990).
  Multinomial processing models of source monitoring.
  \emph{Psychological Review}, \bold{97}, 548--564.

  Blackwell, H.R. (1963).
  Neural theories of simple visual discriminations.
  \emph{Journal of the Optical Society of America}, \bold{53}, 129--160.

  Broeder, A., & Schuetz, J. (2009).
  Recognition ROCs are curvilinear--or are they? On premature arguments
  against the two-high-threshold model of recognition.
  \emph{Journal of Experimental Psychology: Learning, Memory, and Cognition},
  \bold{35}, 587--606.

  Hilbig, B.E., Erdfelder, E., & Pohl, R.F. (2010).
  One-reason decision-making unveiled: A measurement model of the recognition
  heuristic.
  \emph{Journal of Experimental Psychology: Learning, Memory, and Cognition},
  \bold{36}, 123--134.

  Riefer, D.M., & Batchelder, W.H. (1988).
  Multinomial modeling and the measurement of cognitive processes.
  \emph{Psychological Review}, \bold{95}, 318--339.

  Snodgrass, J.G., & Corwin, J. (1988).
  Pragmatics of measuring recognition memory: Applications to dementia and
  amnesia.
  \emph{Journal of Experimental Psychology: General}, \bold{117}, 34--50.

  Swets, J. A. (1961).
  Is there a sensory threshold?
  \emph{Science}, \bold{134}, 168--177.
}
\seealso{
  \code{\link{mpt}}.
}
\examples{
## Specify storage-retrieval model for pairs
spec1 <- mptspec(
  c*r,
  (1 - c)*u^2,
  2*(1 - c)*u*(1 - u),
  c*(1 - r) + (1 - c)*(1 - u)^2
)

## Specify storage-retrieval model with parameter restrictions
spec2 <- mptspec(
  c*r,
  (1 - c)*u^2,
  2*(1 - c)*u*(1 - u),
  c*(1 - r) + (1 - c)*(1 - u)^2,
  .restr = list(c = r/2, u = 0.3)
)

## Optional names identifying trees in joint MPT model
spec3 <- mptspec(
  "1.1" = r + (1 - r)*b,
  "1.2" = (1 - r)*(1 - b),
  "2.1" = b,
  "2.2" = 1 - b
)

## Fit one-high-threshold model to data in Broeder & Schuetz (2009)
mpt1 <- mpt(spec <- mptspec("1HT"), c(55, 35, 45, 765))

## Working with the mptspec object
spec$par2prob(c(0.5, 0.1))        # response probabilities
spec$par2deriv(coef(mpt1))$deriv  # Jacobian matrix at ML estimate

## See ?recogROC for further examples.
}
\keyword{models}
