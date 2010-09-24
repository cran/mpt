# Recall frequencies for a proactive inhibition experiment
# (Riefer & Batchelder, 1988, Tab. 1)

proact <- data.frame(
  test   = rep(1:2, each=12),
  abpres = rep(rep(1:3, each=4), 2),
  resp   = rep(c("BC", "Bc", "bC", "bc"), 6),
  freq   = c(24, 65, 30, 61,
             40, 91, 12, 37,
             50, 98,  7, 25,
             25, 58, 29, 68,
             28, 97, 10, 21,
             52, 97, 10, 21)
)

proact$treeid <- with(proact, interaction(test, abpres))

