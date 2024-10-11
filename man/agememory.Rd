\name{agememory}
\alias{agememory}
\title{Age Differences in Episodic Memory}
\description{
  Bayen (1990) presented 40 younger and 40 older adults with a list of 50
  words to be learned.  The list consisted of 20 semantic word pairs and 10
  singleton words.  In a later memory test, participants freely recalled the
  presented words.  For pairs, responses were classified into four categories:
  both words in a pair are recalled adjacently (E1) or non-adjacently (E2),
  one word in a pair is recalled (E3), neither word in a pair is recalled
  (E4); for singletons, into two categories: word recalled (F1), word not
  recalled (F2).

  The recall frequencies are available in Schmidt et al. (2023).
}
\usage{data(agememory)}
\format{
  \code{agememory} A data frame containing 80 observations of 15 variables:
  \describe{
    \item{\code{group}}{factor. Younger versus older group of participants.}
    \item{\code{id}}{participant ID within each group.}
    \item{\code{age}}{participant age.}
    \item{\code{sex}}{factor. Participant sex.}
    \item{\code{IST70}}{intelligence score.}
    \item{\code{lag0E1, lag0E2, lag0E3, lag0E4}}{recall frequencies for word
      pairs presented without lag.}
    \item{\code{lag15E1, lag15E2, lag15E3, lag15E4}}{recall frequencies for
      word pairs presented with a lag of 15 items in between the two pair
      members.}
    \item{\code{F1, F2}}{recall frequencies for singleton words.}
  }
}
\source{
  Schmidt, O., Erdfelder, E., & Heck, D. W. (2023).
  How to develop, test, and extend multinomial processing tree models:
  A tutorial.
  \emph{Psychological Methods}.
  \doi{10.1037/met0000561}
}
\references{
  Bayen, U.J. (1990).
  Zur Lokalisation von Altersdifferenzen im episodischen Gedaechtnis
  Erwachsener: Eine Querschnittsuntersuchung auf der Basis eines
  mathematischen Modells.
  \emph{Berichte aus dem Psychologischen Institut der Universitaet Bonn},
  \bold{16}, 1--125.
}
\seealso{
  \code{\link{mpt}}.
}
\examples{
data(agememory)

aggregate(cbind(lag0E1, lag0E2, lag0E3, lag0E4,
                lag15E1, lag15E2, lag15E3, lag15E4, F1, F2) ~ group,
          data = agememory, sum)
xtabs(~ group + sex, agememory) |> addmargins()
}
\keyword{datasets}
