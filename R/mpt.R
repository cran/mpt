mpt <- function(formula, data, treeid = "treeid", constr = NULL,
  start = rep(0.5, length(all.vars(formula[[3]]))), ...){

  theta <- start                           # starting values
  names(theta) <- all.vars(formula[[3]])

  ## Extract model structure from formula
  aa <- bb <- array(NA,
    c(max(sapply(gregexpr("\\+", formula[[3]]), function(x) sum(x > 0))) + 1,
      length(formula[[3]]) - 1, length(theta)))
  cc <- matrix(1, dim(aa)[1], dim(aa)[2])
  
  terms <- strsplit(as.character(formula[[3]][-1]), "\\+")
  terms <- lapply(terms, function(x) gsub(" ", "", x))    # remove white space
  
  for(j in 1:dim(aa)[2]){
    for(i in 1:sapply(terms, length)[j]){
      pterms <- strsplit(terms[[j]][i], "\\*")[[1]]
      cval <- sum(as.numeric(grep("^[0-9]+$", pterms, value=TRUE)))
      if(cval > 0) cc[i,j] <- cval
      for(s in seq(along.with=theta)){
        tname <- names(theta)[s]
  
        aa[i,j,s] <- sum(grepl(paste("^", tname, "$", sep=""), pterms))
        powix <- grepl(paste("^", tname, "\\^[0-9]+", sep=""), pterms)
        aa[i,j,s] <- sum(aa[i,j,s],
          as.numeric(gsub(paste("^", tname, "\\^([0-9]+)", sep=""), "\\1",
            pterms)[powix]))
  
        ## Brackets () are optional
        bb[i,j,s] <- sum(grepl(paste("^\\(?1-", tname, "\\)?$", sep=""),
          pterms))
        powix <- grepl(paste("^\\(1-", tname, "\\)\\^[0-9]+", sep=""), pterms)
        bb[i,j,s] <- sum(bb[i,j,s],
          as.numeric(gsub(paste("^\\(1-", tname, "\\)\\^([0-9]+)", sep=""),
          "\\1", pterms)[powix]))
      }
    }
  }
  dimnames(aa)[[3]] <- dimnames(bb)[[3]] <- as.list(names(theta))

  ## Constraints
  if(length(constr) > 0){
    theta.old <- theta
    unconstr  <- setdiff(names(theta.old), unlist(constr))
    theta     <- theta.old[unconstr]
    for(i in 1:length(constr)) theta <- c(theta, mean(theta.old[constr[[i]]]))
    names(theta) <- c(names(theta.old[unconstr]), names(constr))

    aa.old <- aa
    aa     <- array(NA, c(dim(aa.old)[1:2], length(theta)))
    aa[,,seq_along(unconstr)] <- aa.old[,,unconstr]
    for(i in seq_along(constr))
      aa[,,length(unconstr) + i] <- apply(aa.old[,,constr[[i]]], 1:2, sum)

    bb.old <- bb
    bb     <- array(NA, c(dim(bb.old)[1:2], length(theta)))
    bb[,,seq_along(unconstr)] <- bb.old[,,unconstr]
    for(i in seq_along(constr))
      bb[,,length(unconstr) + i] <- apply(bb.old[,,constr[[i]]], 1:2, sum)
    dimnames(aa)[[3]] <- dimnames(bb)[[3]] <- as.list(names(theta))
  }
  
  freq   <- data[,all.vars(formula[[2]])]
  tid    <- if(treeid %in% names(data)) factor(data[,treeid])
            else rep(1, nrow(data))
  ncat   <- table(tid)
  ntrees <- length(ncat)
  n      <- tapply(freq, tid, sum)[as.character(tid)]
  
  fit    <- mptEM(theta=theta, data=freq, a=aa, b=bb, c=cc, ...)
  loglik <- fit$loglik
  aic    <- -2*loglik + 2*length(theta)
  fitted <- n*fit$pcat
  G2     <- 2*(sum(freq*log(freq/(fitted))))
  df     <- sum(ncat - 1) - length(theta)
  gof    <- c(G2=G2, df=df, pval = 1 - pchisq(G2, df))

  out <- list(coefficients=fit$theta, fitted.values=fitted, loglik=loglik,
    aic=aic, a=aa, b=bb, c=cc, goodness.of.fit=gof, pcat=fit$pcat,
    formula=formula, ntrees=ntrees)
  class(out) <- "mpt"
  out
}


## EM algorithm
mptEM <- function(theta, data, a, b, c, maxit = 1000, epsilon = 1e-8,
  verbose = FALSE){
  nbranch <- dim(a)[1]
  pbranch <- matrix(NA, nbranch, length(data))
  loglik0 <- -Inf

  iter <- 1
  while(iter < maxit){
    if(verbose) print(c(iter, loglik0))

    ## E step
    for(i in 1:nbranch)
      for(j in seq(along.with=data))
        pbranch[i, j] <- c[i,j]*prod(theta^a[i,j,] * (1 - theta)^b[i,j,])
    
    pcat    <- colSums(pbranch, na.rm=TRUE)
    loglik1 <- sum(data*log(pcat))
    if(loglik1 - loglik0 < epsilon) break  # stop if converged
    loglik0 <- loglik1
    m       <- t(data*t(pbranch)/pcat)
    
    ## M step
    for(s in seq(along.with=theta))
      theta[s] <-
        sum(a[,,s]*m, na.rm=TRUE)/sum((a[,,s] + b[,,s])*m, na.rm=TRUE)
    iter    <- iter + 1
  }
  if(iter >= maxit) warning("iteration maximum has been exceeded")
  out <- list(theta=theta, pcat=pcat, loglik=loglik1)
  out
}


print.mpt <- function(x, digits=max(3, getOption("digits")-3),
  na.print="", ...){
  cat("\nMultinomial processing tree (MPT) models\n\n")
  cat("Parameter estimates:\n")
  print.default(format(x$coefficients, digits = digits), print.gap = 2,
      quote = FALSE)
  G2 <- x$goodness.of.fit[1]
  df <- x$goodness.of.fit[2]
  pval <- x$goodness.of.fit[3]
  cat("\nGoodness of fit (2 log likelihood ratio):\n")
  cat("\tG2(", df, ") = ", format(G2, digits=digits), ", p = ",
      format(pval,digits=digits), "\n", sep="")
  cat("\nAIC: ",format(x$aic,digits=max(4,digits+1)),"\n")  # to summary.mpt
  cat("\n")
  invisible(x)
}


anova.mpt <- function (object, ..., test=c("Chisq", "none")){
  ## Adapted form anova.polr by Brian Ripley

  test <- match.arg(test)
  dots <- list(...)
  if (length(dots) == 0)
      stop('anova is not implemented for a single "mpt" object')
  mlist <- list(object, ...)
  names(mlist) <- c(deparse(substitute(object)),
    as.character(substitute(...[]))[2:length(mlist)])
  nt <- length(mlist)
  dflis <- sapply(mlist, function(x) x$goodness["df"])
  s <- order(dflis, decreasing = TRUE)
  mlist <- mlist[s]

  if (any(!sapply(mlist, inherits, "mpt")))
      stop('not all objects are of class "mpt"')

  ns <- sapply(mlist, function(x) length(x$fitted))
  if(any(ns != ns[1]))
      stop("models were not all fitted to the same size of dataset")

  dfs <- dflis[s]
  lls <- sapply(mlist, function(x) x$goodness["G2"])
  tss <- c("", paste(1:(nt - 1), 2:nt, sep = " vs "))
  df <- c(NA, -diff(dfs))
  x2 <- c(NA, -diff(lls))
  pr <- c(NA, 1 - pchisq(x2[-1], df[-1]))
  out <- data.frame(Model = names(mlist), Resid.df = dfs, Deviance = lls,
                    Test = tss, Df = df, LRtest = x2, Prob = pr)
  names(out) <- c("Model", "Resid. df", "Resid. Dev", "Test",
                  "   Df", "LR stat.", "Pr(Chi)")
  rownames(out) <- 1:nt
  if (test == "none") out <- out[, 1:6]
  class(out) <- c("Anova", "data.frame")
  attr(out, "heading") <-
    "Analysis of deviance table for multinomial processing tree models\n"
  out
}


## Log-likelihood for mpt objects
logLik.mpt <- function(object, ...){
  if(length(list(...)))
      warning("extra arguments discarded")
  p <- length(object$coefficient) - 1
  val <- object$loglik
  attr(val, "df") <- p
  class(val) <- "logLik"
  val
}



## summary.mpt: put AIC here

## residuals.mpt

## plot.mpt

## vcov


