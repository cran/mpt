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
      for(s in seq_along(theta)){
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
  
  ## Either, 'data' is a dataframe
  if(is.data.frame(data)){
    freq <- data[,all.vars(formula[[2]])]
    tid  <- if(length(treeid) == length(freq)) factor(treeid)
            else if(length(treeid) == 1 && treeid %in% names(data))
              factor(data[,treeid])
            else rep(1, length(freq))

  ## Or a vector of frequencies
  }else{
    ## Discard left hand side and substitute it by Y (max. 4095 bytes)
    # formula <- reformulate(attr(terms(formula), "term.labels"), response="Y")
    freq <- data
    tid  <- if(length(treeid) == length(freq)) factor(treeid)
            else if(length(names(freq)) == length(freq)) factor(names(freq))
            else rep(1, length(freq))
  }

  ncat   <- table(tid)
  ntrees <- length(ncat)
  n      <- tapply(freq, tid, sum)[as.character(tid)]
  
  fit    <- mptEM(theta=theta, data=freq, a=aa, b=bb, c=cc, ...)
  loglik <- fit$loglik
  fitted <- n*fit$pcat
  G2     <- 2*sum(freq*log(freq/fitted), na.rm=TRUE)
  df     <- sum(ncat - 1) - length(theta)
  gof    <- c(G2=G2, df=df, pval = 1 - pchisq(G2, df))

  out <- list(coefficients=fit$theta, fitted.values=fitted, loglik=loglik,
    a=aa, b=bb, c=cc, goodness.of.fit=gof, iter=fit$iter, pcat=fit$pcat,
    pbranch=fit$pbranch, formula=formula, ntrees=ntrees, n=n, y=freq)
  class(out) <- "mpt"
  out
}


## EM algorithm
mptEM <- function(theta, data, a, b, c, maxit = 1000, tolerance = 1e-8,
  stepsize = 1, verbose = FALSE){
  nbranch <- dim(a)[1]
  pbranch <- matrix(NA, nbranch, length(data))
  loglik0 <- -Inf
  theta1  <- theta

  iter <- 1
  while(iter < maxit){
    if(verbose) print(c(iter, loglik0))

    ## E step
    for(i in seq_len(nbranch))
      for(j in seq_along(data))
        pbranch[i, j] <- c[i,j] * prod(theta^a[i,j,] * (1 - theta)^b[i,j,])
    
    pcat    <- colSums(pbranch, na.rm=TRUE)
    loglik1 <- sum(data*log(pcat))
    if(loglik1 - loglik0 < tolerance) break  # stop if converged
    loglik0 <- loglik1
    m       <- t(data*t(pbranch)/pcat)
    
    ## M step
    for(s in seq_along(theta))
      theta1[s] <-
        sum(a[,,s]*m, na.rm=TRUE)/sum((a[,,s] + b[,,s])*m, na.rm=TRUE)
    theta   <- theta - stepsize*(theta - theta1)
    iter    <- iter + 1
  }
  if(iter >= maxit) warning("iteration maximum has been exceeded")
  out <- list(theta=theta, loglik=loglik1, pcat=pcat, pbranch=pbranch,
    iter=iter)
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
                  "   Df", "LR stat.", "Pr(>Chi)")
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
  p <- length(object$coefficient)
  val <- object$loglik
  attr(val, "df") <- p
  class(val) <- "logLik"
  val
}


## Residuals for mpt models
residuals.mpt <- function(object, type=c("deviance", "pearson"), ...){

  dev.resids <- function(y, mu, wt)
    2 * wt * (y * log(ifelse(y == 0, 1, y/mu)) - (y - mu))

  type <- match.arg(type)
  wts <- object$n
  y <- object$y / wts
  mu <- object$pcat
  res <- switch(type,
    deviance = if(object$goodness['df'] > 0){
        d.res <- sqrt(pmax(dev.resids(y, mu, wts), 0))
        ifelse(y > mu, d.res, -d.res)  # sign
      }
      else rep.int(0, length(mu)),
    pearson = (y - mu) * sqrt(wts)/sqrt(mu)
  )
  if(!is.null(object$na.action)) res <- naresid(object$na.action, res)
  res
}


## Diagnostic plot for mpt models
plot.mpt <- function(x, showID = TRUE,
  xlab="Predicted response probabilities", ylab="Deviance residuals", ...){

  xres <- resid(x)
  mu   <- x$pcat
  plot(mu, xres, xlab = xlab, ylab = ylab, type="n", ...)
  abline(h = 0, lty = 2)
  if(showID){
    text(mu, xres, names(xres), cex=0.8)
    panel.smooth(mu, xres, cex=0)
  }else{
    panel.smooth(mu, xres)
  }
}


## Covariance matrix for MPT model parameters
vcov.mpt <- function(object, ...){
  a       <- object$a
  b       <- object$b
  y       <- object$y
  pcat    <- object$pcat
  pbranch <- object$pbranch
  theta   <- coef(object)

  ## as(Theta), bs(Theta)
  as.t <- bs.t <- numeric(length(theta))
  for(s in seq_along(theta)){
    for(j in seq_along(pcat)){
      as.t[s] <- as.t[s] + y[j]*sum(a[,j,s]*pbranch[,j]/pcat[j], na.rm=TRUE)
      bs.t[s] <- bs.t[s] + y[j]*sum(b[,j,s]*pbranch[,j]/pcat[j], na.rm=TRUE)
    }
  }
  
  ## d as(Theta)/d t, d bs(Theta)/d t
  das.t <- dbs.t <- matrix(0, length(theta), length(theta))
  for(s in seq_along(theta)){
    for(r in seq_along(theta)){
      for(j in seq_along(pcat)){
        das.t[s, r] <- das.t[s, r] + y[j] * (
        sum(a[,j,s] * pbranch[,j] *
          sum((a[,j,r]/theta[r] - b[,j,r]/(1 - theta[r])) * pbranch[,j],
            na.rm = TRUE) /
          pcat[j]^2, na.rm = TRUE) -
        sum(a[,j,s] *
          (a[,j,r]/theta[r] - b[,j,r]/(1 - theta[r])) * pbranch[,j] / pcat[j],
          na.rm = TRUE)
        )
  
        dbs.t[s, r] <- dbs.t[s, r] + y[j] * (
        sum(b[,j,s] * pbranch[,j] *
          sum((a[,j,r]/theta[r] - b[,j,r]/(1 - theta[r])) * pbranch[,j],
           na.rm = TRUE) /
          pcat[j]^2, na.rm = TRUE) -
        sum(b[,j,s] *
          (a[,j,r]/theta[r] - b[,j,r]/(1 - theta[r])) * pbranch[,j] / pcat[j],
          na.rm = TRUE)
        )
      }
    }
  }
  
  ## I(Theta)
  info.t <- das.t/theta - dbs.t/(1 - theta) +
            diag(as.t/theta^2 + as.t/(1 - theta)^2)
  dimnames(info.t) <- list(names(theta), names(theta))
  solve(info.t)
}


summary.mpt <- function(object, ...){
  x <- object
  coef <- coef(x)

  ## Catch vcov error, so there are at least some NA's in the summary
  s.err <- tryCatch(sqrt(diag(vcov(x))),
    error = function(e) rep(NA, length(coef)))

  tvalue <- coef / s.err
  pvalue <- 2 * pnorm(-abs(tvalue))
  dn <- c("Estimate", "Std. Error")
  coef.table <- cbind(coef, s.err, tvalue, pvalue)
  dimnames(coef.table) <- list(names(coef), c(dn, "z value", "Pr(>|z|)"))

  aic <- AIC(x)
  ans <- list(ntrees=x$ntrees, coefficients=coef.table, aic=aic,
    gof=x$goodness.of.fit, X2=sum(resid(x, "pearson")^2))
  class(ans) <- "summary.mpt"
  return(ans)
}


print.summary.mpt <- function(x, digits=max(3, getOption("digits")-3),
  na.print="", signif.stars=getOption("show.signif.stars"), ...){
  cat("\nNumber of trees:", x$ntrees, "\n")
  cat("\nCoefficients:\n")
  printCoefmat(x$coef, digits = digits, signif.stars = signif.stars, ...)
  cat("\nGoodness of fit:\n")
  cat("Likelihood ratio G2:", format(x$gof[1], digits=digits), "on",
    x$gof[2], "df,", "p-value:", format(x$gof[3], digits=digits), "\n")
  cat("Pearson X2:", format(x$X2, digits=digits), "\n")
  cat("AIC:", format(x$aic, digits=max(4, digits+1)))
  cat("\n")
  invisible(x)
}


## Simulate responses from mpt model
simulate.mpt <- function(object, nsim, seed, pool = TRUE, ...){

  if(pool){
    tid  <- names(object$fitted.values)
    freq <- unlist( lapply(unique(tid),
      function(i) rmultinom(1, object$n[i], object$pcat[tid == i])) )
    names(freq) <- tid
  }else{
    stop("individual response simulation not yet implemented")
  }
  freq
}


## Formulae for some prevalent MPT models
mptmodel <- function(which, replicates = 1, response = "freq"){

  if(missing(which)){
    modformula <- NULL

  }else{
    modformula <- switch(EXPR = which,
      "1HT" = "list(
        r + (1 - r)*b,
        (1 - r)*(1 - b),
        b,
        1 - b
      )",

      "2HT" = "list(
        r + (1 - r)*b,
        (1 - r)*(1 - b),
        (1 - d)*b,
        (1 - d)*(1 - b) + d
      )",

      "PairAsso" = "list(
        p*q*r,
        p*q*(1 - r),
        p*(1 - q)*r,
        (1 - p) + p*(1 - q)*(1 - r)
      )",

      "SourceMon" = "list( 
        D1*d1 + D1*(1 - d1)*g + (1 - D1)*b*g,
        D1*(1 - d1)*(1 - g) + (1 - D1)*b*(1 - g),
        (1 - D1)*(1 - b),
        D2*(1 - d2)*g + (1 - D2)*b*g,
        D2*d2 + D2*(1 - d2)*(1 - g) + (1 - D2)*b*(1 - g),
        (1 - D2)*(1 - b),
        b*g,
        b*(1 - g),
        1 - b
      )",

      "SR" = "list(
        c*r,
        (1 - c)*u^2,
        2*(1 - c)*u*(1 - u),
        c*(1 - r) + (1 - c)*(1 - u)^2,
        u,
        1 - u
      )",

      NULL  # Model not available
    )
  }

  if(length(modformula) == 0){
    cat("'which' has to be one of '1HT', '2HT', 'PairAsso', 'SourceMon',",
        "'SR'.\n")
    return(invisible(modformula))
  }

  modformula <- reformulate(modformula, response=response)

  if(replicates > 1){
    pat <- paste("([", paste(all.vars(modformula[[3]]), collapse=""), "])",
                 sep="")
    newform <- NULL
    for(i in seq_len(replicates))
      newform <- c(newform, gsub(pat, paste("\\1", i, sep=""),
                   modformula[[3]])[-1])
    modformula <- reformulate(newform, response=response)
    modformula <- as.formula(paste(response, " ~ list(",
                               paste(newform, collapse=", "), ")", sep=""))
  }

  modformula
}

