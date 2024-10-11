\name{mptspec}
\alias{mptspec}
\alias{print.mptspec}
\alias{update.mptspec}
\title{Specify a Multinomial Processing Tree (MPT) Model}
\description{Returns the specification of an MPT model object for fitting with
  \code{\link{mpt}}.}
\usage{
mptspec(\dots, .replicates = NULL, .restr = NULL)

\method{update}{mptspec}(object, .replicates = NULL, .restr = NULL, \dots)
}
\arguments{
  \item{\dots}{(named) expressions or a character string specifying the model.
    See Details.}
  \item{.replicates}{the number of replicates of the model equations.
    See Details.}
  \item{.restr}{a named list of parameter restrictions.  See Details.}
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
  where \code{x} is the tree identifier (\code{treeid}) and \code{y} specifies
  the response category within a tree.

  Alternatively, \code{\dots} may be a character string identifying one out of
  a list of pre-specified MPT models.  Currently accessible are the following
  models (other models have to be specified by explicit expressions as
  described above):

  \code{1HT}: the one-high-threshold model (Blackwell, 1963; Swets, 1961).

  \code{2HT}: the two-high-threshold model (Snodgrass & Corwin, 1988; see also
    Broeder & Schuetz, 2009).

  \code{PairAsso}: the paired-associate learning model (Riefer & Batchelder,
    1988).

  \code{proCNI}: the CNI model of moral dilemma judgment for proscriptive
    norms (Hennig & Huetter, 2020).  The general formula includes the process
    dissociation (PD) model (Conway & Gawronski, 2013) as a special case.

  \code{prospec}: the event-based prospective memory model (Smith & Bayen,
    2004).

  \code{rmodel}: the r-model of recognition heuristic use (Hilbig, Erdfelder,
    & Pohl, 2010).

  \code{SourceMon}: the source-monitoring model (Batchelder & Riefer, 1990).

  \code{SR}, \code{SR2}: the storage-retrieval pair-clustering model
    (Batchelder & Riefer, 1986). \code{SR2} is the model without singleton
    items.

  \code{WST}: the inference-guessing model with relaxed assumptions (Klauer,
    Stahl, & Erdfelder, 2007) for the Wason selection task.

  The intended use of \code{.replicates} is to specify the number of
  replicates of the model equations, for example, when the same model is
  repeatedly applied in several experimental conditions.  Accordingly,
  parameter names are augmented by numbers to make them unique.

  Parameter restrictions included in \code{.restr} may be of the form
  \code{b = r} or \code{b = 0.5} etc. Depending on the fitting algorithm
  employed in \code{\link{mpt}} (\code{BFGS}, but not \code{EM}), mathematical
  functions are permissible, for example, \code{b = sqrt(r)}.

  The \code{update} method is used to add parameter restrictions or replicates
  to an existing \code{mptspec} object.
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
  \item{replicates}{the number of replicates of the model equations.}
  \item{restr}{a list containing expressions of parameter restrictions.}
  \item{treeid}{a factor that identifies each tree.}
}
\references{
  Batchelder, W.H., & Riefer, D.M. (1986).
  The statistical analysis of a model for storage and retrieval processes in
  human memory.
  \emph{British Journal of Mathematical and Statistical Psychology},
  \bold{39}(2), 129--149.
  \doi{10.1111/j.2044-8317.1986.tb00852.x}

  Batchelder, W.H., & Riefer, D.M. (1990).
  Multinomial processing models of source monitoring.
  \emph{Psychological Review},
  \bold{97}(4), 548--564.
  \doi{10.1037/0033-295x.97.4.548}

  Blackwell, H.R. (1963).
  Neural theories of simple visual discriminations.
  \emph{Journal of the Optical Society of America},
  \bold{53}(1), 129--160.
  \doi{10.1364/JOSA.53.000129}

  Broeder, A., & Schuetz, J. (2009).
  Recognition ROCs are curvilinear--or are they? On premature arguments
  against the two-high-threshold model of recognition.
  \emph{Journal of Experimental Psychology: Learning, Memory, and Cognition},
  \bold{35}(3), 587--606.
  \doi{10.1037/a0015279}

  Conway, P., & Gawronski, B. (2013).
  Deontological and utilitarian inclinations in moral decision making:
  A process dissociation approach.
  \emph{Journal of Personality and Social Psychology},
  \bold{104}(2), 216--235.
  \doi{10.1037/a0031021}

  Hennig, M., & Huetter, M. (2020).
  Revisiting the divide between deontology and utilitarianism in moral
  dilemma judgment: A multinomial modeling approach.
  \emph{Journal of Personality and Social Psychology}
  \bold{118}(1), 22--56.
  \doi{10.1037/pspa0000173}

  Hilbig, B.E., Erdfelder, E., & Pohl, R.F. (2010).
  One-reason decision-making unveiled: A measurement model of the recognition
  heuristic.
  \emph{Journal of Experimental Psychology: Learning, Memory, and Cognition},
  \bold{36}(1), 123--134.
  \doi{10.1037/a0017518}

  Klauer, K.C., Stahl, C., & Erdfelder, E. (2007).
  The abstract selection task: New data and an almost comprehensive model.
  \emph{Journal of Experimental Psychology: Learning, Memory, and Cognition},
  \bold{33}(4), 680--703.
  \doi{10.1037/0278-7393.33.4.680}

  Riefer, D.M., & Batchelder, W.H. (1988).
  Multinomial modeling and the measurement of cognitive processes.
  \emph{Psychological Review},
  \bold{95}(3), 318--339.
  \doi{10.1037/0033-295x.95.3.318}

  Smith, R.E., & Bayen, U.J. (2004).
  A multinomial model of event-based prospective memory.
  \emph{Journal of Experimental Psychology: Learning, Memory, and Cognition},
  \bold{30}(4), 756--777.
  \doi{10.1037/0278-7393.30.4.756}

  Snodgrass, J.G., & Corwin, J. (1988).
  Pragmatics of measuring recognition memory: Applications to dementia and
  amnesia.
  \emph{Journal of Experimental Psychology: General},
  \bold{117}(1), 34--50.
  \doi{10.1037/0096-3445.117.1.34}

  Swets, J. A. (1961).
  Is there a sensory threshold?
  \emph{Science},
  \bold{134}(3473), 168--177.
  \doi{10.1126/science.134.3473.168}
}
\seealso{
  \code{\link{mpt}}.
}
\examples{
## Specify storage-retrieval pair-clustering model for pairs
spec1 <- mptspec(
  c*r,
  (1 - c)*u^2,
  2*(1 - c)*u*(1 - u),
  c*(1 - r) + (1 - c)*(1 - u)^2
)

## Specify model with parameter restrictions
spec2 <- mptspec(
  c*r,
  (1 - c)*u^2,
  2*(1 - c)*u*(1 - u),
  c*(1 - r) + (1 - c)*(1 - u)^2,
  .restr = list(c = r/2, u = 0.3)
)

## Optional names (dot notation) identifying trees in joint MPT model
spec3 <- mptspec(
      Target.Hit  = r + (1 - r)*b,
      Target.Miss = (1 - r)*(1 - b),
  Distractor.FA   = b,
  Distractor.CR   = 1 - b,
  .replicates = 3,
  .restr = list(r1 = r, r2 = r, r3 = r)
)

## Pre-specified one-high-threshold model
spec4 <- mptspec("1HT")

## Fit to data in Broeder and Schuetz (2009)
m <- mpt(spec4, c(55, 35, 45, 765))

## Working with the mptspec object
spec4$par2prob(c(0.5, 0.1))     # response probabilities
spec4$par2deriv(coef(m))$deriv  # Jacobian matrix at ML estimate

## See data(package = "mpt") for application examples.
}
\keyword{models}
