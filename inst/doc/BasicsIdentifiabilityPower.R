## -----------------------------------------------------------------------------
library(mpt)

## -----------------------------------------------------------------------------
dat <- read.table(header = TRUE, text = "
  lag age resp freq
    0 young E1 90     # adjacent recall: apple, banana
    0 young E2 14     # non-adjacent recall: apple, ..., banana
    0 young E3 84     # single recall: only apple or only banana
    0 young E4 212    # non-recall: neither apple nor banana
  n/a young F1 102    # recall
  n/a young F2 298    # non-recall
    0 old   E1 42
    0 old   E2 5
    0 old   E3 63
    0 old   E4 290
  n/a old   F1 64
  n/a old   F2 336
   15 young E1 67
   15 young E2 18
   15 young E3 123
   15 young E4 192
   15 old   E1 30
   15 old   E2 13
   15 old   E3 95
   15 old   E4 262
")
dat$age <- factor(dat$age, levels = c("young", "old"))

## -----------------------------------------------------------------------------
# Pairs                    Singletons
#      
#     /r -- E1
#    c
#   / \1-r -- E4             a -- F1       c: cluster storage
#  /                        /              r: cluster retrieval
#  \       /u -- E2         \              u: non-clustered storage/retrieval
#   \     u                  1-a -- F2     a: singleton storage/retrieval
#    \   / \1-u -- E3
#     1-c
#        \   /u -- E3
#         1-u
#            \1-u -- E4
mptspec(                                                 # specify model
  E.1 = c*r,
  E.2 = (1 - c)*u^2,                                     # dot notation:
  E.3 = 2 * (1 - c)*u*(1 - u),                           #   tree.response
  E.4 = c*(1 - r) + (1 - c)*(1 - u)^2,
  F.1 = a,
  F.2 = 1 - a,
  .restr = list(a = u)
) |>
  mpt(data = dat[dat$lag != 15 & dat$age == "young", ])  # fit to data

## -----------------------------------------------------------------------------
m <- mpt(mptspec("SR"),
         data = dat[dat$lag != 15 & dat$age == "young", ])

## -----------------------------------------------------------------------------
print(m)
summary(m)

logLik(m)
AIC(m)
BIC(m)

coef(m)
confint(m, logit = FALSE)

## -----------------------------------------------------------------------------
# Target                  Distractor
#
#   r -- Hit
#  /             } 55      b -- FA    45
# /    b -- Hit           /                  r: recognition
# \   /                   \                  b: bias
#  1-r                     1-b -- CR 765
#     \
#      1-b -- Miss 35

## -----------------------------------------------------------------------------
nll <- function(theta, data, treeid) {   # negative log-likelihood
  c <- theta[1]
  r <- theta[2]
  u <- theta[3]
  a <- theta[3]  # a = u
  pcat <- c(
    E.1 = c*r,
    E.2 = (1 - c)*u^2,
    E.3 = 2 * (1 - c)*u*(1 - u),
    E.4 = c*(1 - r) + (1 - c)*(1 - u)^2,
    F.1 = a,
    F.2 = 1 - a
  )
  -sum(
    dmultinom(data[treeid == 1], size = sum(data[treeid == 1]),
              prob = pcat[treeid == 1], log = TRUE),
    dmultinom(data[treeid == 2], size = sum(data[treeid == 2]),
              prob = pcat[treeid == 2], log = TRUE)
  )
}

## -----------------------------------------------------------------------------
optim(par = c(.5, .5, .5),                    # starting values
       fn = nll,                              # function to be minimized
     data = dat[dat$lag != 15 &
                dat$age == "young", "freq"],  # arguments passed to fn
   treeid = c(1, 1, 1, 1, 2, 2),
   method = "L-BFGS-B",                       # algorithm
    lower = rep(.001, 3),                     # boundaries
    upper = rep(.999, 3),
  hessian = TRUE)

## -----------------------------------------------------------------------------
m0 <-
  update(m$spec, .restr = list(c = 0.5)) |>  # specify restriction(s)
  mpt(data = m$y) |>                         # refit
  print()

## -----------------------------------------------------------------------------
anova(m0, m)

## -----------------------------------------------------------------------------
m1 <- mptspec("SR", .replicates = 2) |>
  mpt(data = dat[dat$lag != 15, ]) |>
  print()

## -----------------------------------------------------------------------------
update(m1$spec, .restr = list(c2 = s * c1)) |>
  mpt(data = m1$y)

## -----------------------------------------------------------------------------
m5 <-
  update(m1$spec, .restr = list(c2 = s_c * c1,      # two shrinkage pars
                                r2 = s_r * r1)) |>
  mpt(data = m1$y)
m6 <-
  update(m5$spec, .restr = list(s_c = s_r)) |>      # single shrinkage par
  mpt(data = m1$y)
anova(m6, m5)

## -----------------------------------------------------------------------------
s1 <- mptspec(                         # not identifiable: 5 parameters
  E.1 = c * r,
  E.2 = (1 - c) * u1 * u2,
  E.3 = (1 - c) * u1 * (1 - u2) + (1 - c) * u2 * (1 - u1),
  E.4 = c * (1 - r) + (1 - c) * (1 - u1) * (1 - u2),
  F.1 = a,
  F.2 = 1 - a
)

## -----------------------------------------------------------------------------
runif(5) |> s1$par2deriv() |> _$deriv |> print() |> qr() |> _$rank

## -----------------------------------------------------------------------------
s2 <- update(s1, .restr = list(u1 = u, u2 = u))  # make it identifiable
runif(4) |> s2$par2deriv() |> _$deriv |> print() |> qr() |> _$rank

## -----------------------------------------------------------------------------
suppressWarnings(replicate(10,
  mpt(s1, data = dat[dat$lag != 15 & dat$age == "young", ],
      start = runif(5)) |>
  coef()
)) |> t()  # not identifiable: parameter trade-offs

replicate(10,
  mpt(s2, data = dat[dat$lag != 15 & dat$age == "young", ],
      start = runif(4)) |>
  coef()
) |> t()   # identifiable: unique estimates

## -----------------------------------------------------------------------------
s <- mptspec(
  E.1 = c * r,
  E.2 = (1 - c) * u^2,
  E.3 = 2 * (1 - c) * u * (1 - u),
  E.4 = c * (1 - r) + (1 - c) * (1 - u)^2,
  F.1 = a,
  F.2 = 1 - a
)
s$par     # before you use par2prob(), carefully check position of parameters!
s$par2prob(c(c = 0.5, r = 0.5, u = 0.4, a = 0.6))   # evaluate model equations

dataGen <- function(nn, d) {
  structure(list(                                            # stub mpt object
    treeid = s$treeid,
         n = setNames((nn * c(2, 1)/3)[s$treeid], s$treeid), # 2:1 ratio
      pcat = s$par2prob(c(c = 0.5, r = 0.5,
                          u = 0.5 - d/2, a = 0.5 + d/2))
  ), class = "mpt") |>
    simulate()
}
dataGen(960, 0.2)

## ----fig.width = 5------------------------------------------------------------
replicate(20, dataGen(960, 0.2) |> mpt(s, data = _) |> coef()) |>
  t() |>
  boxplot(horizontal = TRUE)

## -----------------------------------------------------------------------------
testFun <- function(nn, d) {
  y <- dataGen(nn, d)                            # generate data with effect
  m1 <- mpt(s, y)
  m2 <- mpt(update(s, .restr = list(a = u)), y)
  anova(m2, m1)$"Pr(>Chi)"[2]                    # test H0, return p value
}

## -----------------------------------------------------------------------------
# pwrsim1 <- expand.grid(d = seq(0, 0.5, 0.1), n = 30 * 2^(0:5))
# 
# system.time(  # about 20 min
#   pwrsim1$pval <-
#     mapply(function(nn, d) replicate(500, testFun(nn, d)),
#            nn = pwrsim1$n, d = pwrsim1$d, SIMPLIFY = FALSE)
# )
# pwrsim1$pwr <- sapply(pwrsim1$pval, function(p) mean(p < .05))
# }

## ----fig.width = 5------------------------------------------------------------
data(pwrsim)  # provides pwrsim1 and pwrsim2
if(requireNamespace("lattice")) {
  lattice::xyplot(pwr ~ d, pwrsim1, groups = n, type = c("g", "b"),
                  auto.key = list(space = "right"),
                  xlab = "Parameter difference a - u",
                  ylab = "Simulated power")
}

## ----fig.width = 5------------------------------------------------------------
s <- mptspec("SR", .replicates = 2)

dataGen <- function(nn, d) {
  structure(list(
    treeid = s$treeid,
         n = setNames((nn/2 * c(2, 1, 2, 1)/3)[s$treeid], s$treeid),
      pcat = s$par2prob(c(c1 = 0.5, r1 = 0.4 + d/2, u1 = 0.3,   # young
                          c2 = 0.5, r2 = 0.4 - d/2, u2 = 0.3))  # old
  ), class = "mpt") |>
    simulate(m)
}

testFun <- function(nn, d) {
  y <- dataGen(nn, d)
  m1 <- mpt(s, y)
  m2 <- mpt(update(s, .restr = list(r1 = r2)), y)
  anova(m2, m1)$"Pr(>Chi)"[2]
}

# pwrsim2 <- expand.grid(d = seq(0, 0.4, 0.1), n = 120 * 2^(0:2))

if(requireNamespace("lattice")) {
  lattice::xyplot(pwr ~ d, pwrsim2, groups = n, type = c("g", "b"),
                  auto.key = list(space = "right"),
                  xlab = "Parameter difference r1 - r2",
                  ylab = "Simulated power")
}

