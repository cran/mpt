\name{citysize}
\alias{citysize}
\alias{WorldCities}
\alias{ItalianCities}
\title{City-Size Paired-Comparison Task}
\description{
  In a city-size paired-comparison task on each trial, participants judge
  which of two cities is more populous. After the paired comparisons,
  participants indicate for each city if they recognize its name.
  Hilbig, Erdfelder, and Pohl (2010) report a series of experiments to
  evaluate their model of recognition heuristic use at this task.

  The \code{WorldCities} data are from a study designed to be similar to
  Hilbig et al.'s Experiment 6.  The 17 cities were (in order of population;
  Wikipedia, 2016): Shanghai, Tianjin, Tokyo, Seoul, London, Bangkok,
  Chongqing, Wuhan, Santiago, Rangun, Ankara, Harbin, Kano, Busan, Durban,
  Ibadan, Montreal.

  The \code{ItalianCities} data are from a study designed to be similar to
  Hilbig et al.'s Experiment 7.  The 14 cities were: Milan, Naples, Turin,
  Palermo, Venice, Padua, Taranto, Prato, Reggio Emilia, Perugia, Cagliari,
  Foggia, Salerno, Ferrara.
}
\usage{data(citysize)}
\format{
  \code{WorldCities} A data frame containing 37 observations of six
    variables:
  \describe{
    \item{\code{gender}}{factor. Participant gender.}
    \item{\code{age}}{participant age.}
    \item{\code{rt}}{median response time (in seconds) across paired
      comparisons.}
    \item{\code{group}}{factor. The control group (\code{CG}) received
      standard instructions, the experimental group (\code{EG}) was instructed
      to choose the city they recognized whenever possible.}
    \item{\code{country}}{number of cities whose country was correctly
      identified.}
    \item{\code{y}}{a matrix of aggregate response frequencies per
      participant. The column names indicate each of eight response
      categories: correct/false responses when both cities were recognized
      (\code{KC}, \code{KF}), when both were unrecognized (\code{GC},
      \code{GF}), when only one was recognized and the recognized city was
      chosen (\code{RC}, \code{RF}), and when only one was recognized and the
      unrecognized city was chosen (\code{UF}, \code{UC}).}
  }
  \code{ItalianCities} A data frame containing 64 observations of six
    variables:
  \describe{
    \item{\code{gender}, \code{age}, \code{rt}, \code{y}}{see above.}
    \item{\code{group}}{factor. The control group (\code{CG}) received
      standard instructions, the experimental group (\code{EG}) was asked
      to compare the cities with respect to their elevation above sea level.}
    \item{\code{knowRH}}{factor. Does the participant have any knowledge about
      the recognition heuristic (RH)?}
  }
}
\source{
  The \code{WorldCities} data were collected at the Department of Psychology,
  University of Tuebingen, in June/July 2016. The \code{ItalianCities} data
  are from Rettich (2020). The original data are from Castela et al. (2014).
}
\references{
  Hilbig, B.E., Erdfelder, E., & Pohl, R.F. (2010).
  One-reason decision-making unveiled: A measurement model of the recognition
  heuristic.
  \emph{Journal of Experimental Psychology: Learning, Memory, and Cognition},
  \bold{36}(1), 123--134.
  \doi{10.1037/a0017518}

  Castela, M., Kellen, D., Erdfelder, E., & Hilbig, B.E. (2014).
  The impact of subjective recognition experiences on recognition heuristic
  use: A multinomial processing tree approach.
  \emph{Psychonomic Bulletin & Review},
  \bold{21}(5), 1131--1138.
  \doi{10.3758/s13423-014-0587-4}

  Rettich, A. (2020).
  \emph{Application of the recognition heuristic: An experimental validation
    of the r-model}.
  Bachelor thesis. University of Tuebingen, Germany.
  \url{https://osf.io/mz47y/}

  Wikipedia. (2016). List of cities proper by population. Retrieved Jun 16
  from \url{https://en.wikipedia.org/wiki/List_of_cities_proper_by_population}.
}
\seealso{
  \code{\link{mpt}}.
}
\examples{
data(citysize)

## Fit r-model separately for each instruction type
mpt(mptspec("rmodel"), unname(WorldCities[WorldCities$group == "CG",  "y"]))
mpt(mptspec("rmodel"), unname(WorldCities[WorldCities$group == "EG", "y"]))

## Test instruction effect on r parameter
city.agg <- aggregate(y ~ group, WorldCities, sum)
y <- as.vector(t(city.agg[, -1]))

m1 <- mpt(mptspec("rmodel", .replicates = 2), y)
m2 <- mpt(update(m1$spec, .restr = list(r2=r1)), y)
anova(m2, m1)  # more use of RH with recognition instruction

## Fit r-model separately for each task type
mpt(mptspec("rmodel"),
    unname(ItalianCities[ItalianCities$group == "CG",  "y"]))
mpt(mptspec("rmodel"),
    unname(ItalianCities[ItalianCities$group == "EG", "y"]))

## Test task effect on r parameter
city.agg <- aggregate(y ~ group, ItalianCities, sum)
y <- as.vector(t(city.agg[, -1]))

m3 <- mpt(mptspec("rmodel", .replicates = 2), y)
m4 <- mpt(update(m1$spec, .restr = list(r2=r1)), y)
anova(m4, m3)  # less use of RH with elevation task

## Plot parameter estimates
par(mfrow = 1:2)
dotchart(coef(m1)[c(4, 1:3)], xlim=0:1, labels=c("a", "b", "g", "r"),
         xlab="", main="by instruction type")
points(coef(m1)[c(8, 5:7)], 1:4, pch=16)
legend(0, 1, c("none", "recognition"), pch=c(1, 16),
       title="Instruction", bty="n")
dotchart(coef(m3)[c(4, 1:3)], xlim=0:1, labels=c("a", "b", "g", "r"),
         xlab="", main="by task type")
points(coef(m3)[c(8, 5:7)], 1:4, pch=16)
legend(0, 1, c("population", "elevation"), pch=c(1, 16),
       title="Task", bty="n")
title("Recognition heuristic use", outer=TRUE, line=-1)
mtext("Parameter estimate (r-model)", side=1, outer=TRUE, line=-2)

## Compare with original results
Hilbig2010 <- rbind(
    WorldCities.CG = c(462, 204, 290, 272, 740, 205, 77, 62),
    WorldCities.EG = c(500, 307, 279, 264, 902, 235, 68, 29),
  ItalianCities.CG = c(232, 78, 135, 136, 465, 65, 56, 16),
  ItalianCities.EG = c(245, 176, 154, 150, 228, 160, 112, 140)
)
apply(Hilbig2010, 1, mpt, spec = mptspec("rmodel"))
}
\keyword{datasets}
