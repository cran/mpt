\name{citysize}
\alias{citysize}
\title{City-Size Paired-Comparison Task and Response Strategy}
\description{
  Thirty-seven participants performed a city-size paired-comparison task. On
  each trial, participants indicated which of two cities was more populous.
  After the paired comparisons, participants were asked for each city if they
  recognized its name, knew in which country it is, and how many inhabitants
  it has.  The study was designed to be similar to Experiment 6 in Hilbig,
  Erdfelder, and Pohl (2010).

  The 17 cities were (in order of population; Wikipedia, 2016): Shanghai,
  Tianjin, Tokio, Seoul, London, Bangkok, Chongqing, Wuhan, Santiago, Rangun,
  Ankara, Harbin, Kano, Busan, Durban, Ibadan, Montreal.
}
\usage{data(citysize)}
\format{
  A data frame consisting of six components:
  \describe{
    \item{\code{gender}}{factor. Participant gender.}
    \item{\code{age}}{Participant age.}
    \item{\code{country}}{Number of cities whose country was correctly
      identified.}
    \item{\code{rt}}{Median response time (in seconds) across paired
      comparisons.}
    \item{\code{instr}}{factor. When \code{none}, no additional instructions
      were given; when \code{recog}, participants were instructed to choose
      the city they recognized whenever possible.}
    \item{\code{y}}{a matrix of aggregate response frequencies per
      participant. The column names indicate each of eight response
      categories: correct/false responses when both cities were recognized
      (\code{KC}, \code{KF}), when both were unrecognized (\code{GC},
      \code{GF}), when only one was recognized and the recognized city was
      chosen (\code{RC}, \code{RF}), and when only one was recognized and the
      unrecognized city was chosen (\code{UF}, \code{UC}).}
  }
}
\source{
  Data were collected at the Department of Psychology, University of
  Tuebingen, in June/July 2016.
}
\references{
  Hilbig, B.E., Erdfelder, E., & Pohl, R.F. (2010).
  One-reason decision-making unveiled: A measurement model of the recognition
  heuristic.
  \emph{Journal of Experimental Psychology: Learning, Memory, and Cognition},
  \bold{36}, 123--134.
  \doi{10.1037/a0017518}

  Wikipedia. (2016). List of cities proper by population. Retrieved Jun 16
  from \url{https://en.wikipedia.org/wiki/List_of_cities_proper_by_population}.
}
\seealso{
  \code{\link{mpt}}.
}
\examples{
data(citysize)

## Fit r-model separately for each instruction type
mpt(mptspec("rmodel"), unname(citysize[citysize$instr == "none",  "y"]))
mpt(mptspec("rmodel"), unname(citysize[citysize$instr == "recog", "y"]))

## Test instruction effect on r parameter
city.agg <- aggregate(y ~ instr, citysize, sum)
y <- as.vector(t(city.agg[, -1]))

m1 <- mpt(mptspec("rmodel", .replicates=2), y)
m2 <- mpt(update(m1$spec, .restr=list(r2=r1)), y)
anova(m2, m1)  # more use of RH with recognition instruction

## Plot parameter estimates
dotchart(coef(m1)[c(4, 1:3)], xlim=0:1, labels=c("a", "b", "g", "r"),
         xlab="Parameter estimate (r-model)",
         main="Recognition heuristic use by instruction type")
points(coef(m1)[c(8, 5:7)], 1:4, pch=16)
legend(0, 1, c("none", "recognition"), pch=c(1, 16),
       title="Instruction", bty="n")
}
\keyword{datasets}
