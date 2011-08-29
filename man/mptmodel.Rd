\name{mptmodel}
\alias{mptmodel}
\title{Formulae for Some Prevalent MPT Models}
\description{Returns formulae for some prevalent MPT models.}
\usage{
mptmodel(which, replicates = 1, response = "freq")
}
\arguments{
  \item{which}{character string identifying the model. See Details.}
  \item{replicates}{number of replicates.}
  \item{response}{name of the response variable.}
}
\details{
  Currently implemented are the following models:

  \code{1HT}: the one-high threshold model (see Erdfelder et al., 2009).

  \code{2HT}: the two-high threshold model (see Broeder & Schuetz, 2009).

  \code{PairAsso}: the paired-associate learning model (Riefer & Batchelder,
    1988).

  \code{SourceMon}: the source-monitoring model (Batchelder & Riefer, 1990).

  \code{SR}: the storage-retrieval model (Riefer & Batchelder, 1988).

}
\value{
  A formula that may be used as input for \code{\link{mpt}}.
}
\references{
  Batchelder, W.H. & Riefer, D.M. (1990).
  Multinomial processing models of source monitoring.
  \emph{Psychological Review}, \bold{97}, 548--564.

  Broeder, A. & Schuetz, J. (2009).
  Recognition ROCs are curvilinear--or are they? On premature arguments
  against the two-high-threshold model of recognition.
  \emph{Journal of Experimental Psychology: Learning, Memory, and Cognition},
  \bold{35}, 587--606.

  Erdfelder, E., Auer, T., Hilbig, B.E., Assfalg, A., Moshagen, M., &
  Nadarevic, L. (2009).
  Multinomial processing tree models: A review of the literature.
  \emph{Zeitschrift fuer Psychologie}, \bold{217}, 108--124.

  Riefer, D.M., & Batchelder, W.H. (1988).
  Multinomial modeling and the measurement of cognitive processes.
  \emph{Psychological Review}, \bold{95}, 318--339.
}
\seealso{
  \code{\link{mpt}}.
}
\examples{
## Fit one-high threshold model to data in Broeder & Schuetz (2009)
mpt(mptmodel("1HT"), c(t=55, t=35, d=45, d=765))
}
\keyword{models}
