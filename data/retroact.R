# Recall frequencies for a retroactive inhibition experiment
# (Riefer & Batchelder, 1988, Tab. 3)

retroact <- data.frame(
  lists  = rep(0:4, each=6),
  treeid = rep(1:10, rep(c(4, 2), 5)),
  resp   = rep(c("E1", "E2", "E3", "E4", "F1", "F2"), 5),
  freq   = c(97, 5,  9, 39, 38, 37, 
             71, 2,  6, 71, 24, 51, 
             55, 3, 10, 82, 25, 50,
             51, 2,  9, 88, 20, 55, 
             54, 2,  9, 85, 22, 53)
)

