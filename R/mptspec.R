# Aug/15/2019 return treeid created from expression names
#
# Feb/02/2015 allow for longer expressions and restrictions, and
#             adjust print method accordingly


## Parse probability specification
mptspec <- function(..., .restr = NULL)
{
  ## non-standard evaluation of arguments
  spec <- match.call()

  restr <- spec$.restr
  if(!is.null(restr)) {
    if(as.character(restr[[1L]]) != "list") stop(".restr must be list")
    restr1 <- restr
    restrcl <- sapply(restr1[-1L], class)
  # restr <- sapply(restr1[-1L], deparse)
    restr <- sapply(restr1[-1L], function(s) paste(deparse(s), collapse=""))
    restr <- paste(names(restr), " = ",
                   ifelse(restrcl == "numeric", "", "expression("),
                   restr,
                   ifelse(restrcl == "numeric", "", ")[[1L]]"),
                   collapse = ", ")
  }

  # Remove .restr from call (if included)
  # and (further below, after default models) turn into list of characters
  spec$.restr <- NULL
  spec <- as.list(spec[-1L])                  # exclude function name
  treeid <- NULL

  if (is.character(whichmod <- spec[[1]])) {  # default models
    modcall <- switch(EXPR = whichmod,
      "1HT" = expression(
        "1.1" = r + (1 - r)*b,
        "1.2" = (1 - r)*(1 - b),
        "2.1" = b,
        "2.2" = 1 - b
      ),
      "2HT" = expression(
        "1.1" = r + (1 - r)*b,
        "1.2" = (1 - r)*(1 - b),
        "2.1" = (1 - d)*b,
        "2.2" = (1 - d)*(1 - b) + d
      ),
      "PairAsso" = expression(
        "1.1" = p*q*r,
        "1.2" = p*q*(1 - r),
        "1.3" = p*(1 - q)*r,
        "1.4" = (1 - p) + p*(1 - q)*(1 - r)
      ),
      "prospec" = expression(
        "1.1" = C1*P*(1 - M1)*(1 - g) + C1*(1 - P) +
                (1 - C1)*P*(1 - M1)*(1 - g)*c + (1 - C1)*(1 - P)*c,
        "1.2" = (1 - C1)*P*(1 - M1)*(1 - g)*(1 - c) +
                (1 - C1)*(1 - P)*(1 - c),
        "1.3" = C1*P*M1 + C1*P*(1 - M1)*g + (1 - C1)*P*M1 +
                (1 - C1)*P*(1 - M1)*g,
        "2.1" = (1 - C2)*P*(1 - M1)*(1 - g)*c + (1 - C2)*(1 - P)*c,
        "2.2" = C2*P*(1 - M1)*(1 - g) + C2*(1 - P) +
                (1 - C2)*P*(1 - M1)*(1 - g)*(1 - c) +
                (1 - C2)*(1 - P)*(1 - c),
        "2.3" = C2*P*M1 + C2*P*(1 - M1)*g + (1 - C2)*P*M1 +
                (1 - C2)*P*(1 - M1)*g,
        "3.1" = C1*P*M2 + C1*P*(1 - M1)*(1 - g) + C1*(1 - P) +
                (1 - C1)*P*M2*c + (1 - C1)*P*(1 - M1)*(1 - g)*c +
                (1 - C1)*(1 - P)*c,
        "3.2" = (1 - C1)*P*M2*(1 - c) + (1 - C1)*P*(1 - M2)*(1 - g)*(1 - c) +
                (1 - C1)*(1 - P)*(1 - c),
        "3.3" = C1*P*(1 - M2)*g + (1 - C1)*P*(1 - M2)*g,
        "4.1" = (1 - C2)*P*M2*c + (1 - C2)*P*(1 - M2)*(1 - g)*c +
                (1 - C2)*(1 - P)*c,
        "4.2" = C2*P*M2 + C2*P*(1 - M2)*(1 - g) + C2*(1 - P) +
                (1 - C2)*P*M2*(1 - c) + (1 - C2)*P*(1 - M2)*(1 - g)*(1 - c) +
                (1 - C2)*(1 - P)*(1 - c),
        "4.3" = C2*P*(1 - M2)*g + (1 - C2)*P*(1 - M2)*g
      ),
      "rmodel" = expression(
        "1.1" = b,
        "1.2" = 1 - b,
        "2.1" = g,
        "2.2" = 1 - g,
        "3.1" = r*a + (1 - r)*b*a,
        "3.2" = r*(1 - a) + (1 - r)*(1 - b)*(1 - a),
        "3.3" = (1 - r)*(1 - b)*a,
        "3.4" = (1 - r)*b*(1 - a)
      ),
      "SourceMon" = expression(
        "1.1" = D1*d1 + D1*(1 - d1)*g + (1 - D1)*b*g,
        "1.2" = D1*(1 - d1)*(1 - g) + (1 - D1)*b*(1 - g),
        "1.3" = (1 - D1)*(1 - b),
        "2.1" = D2*(1 - d2)*g + (1 - D2)*b*g,
        "2.2" = D2*d2 + D2*(1 - d2)*(1 - g) + (1 - D2)*b*(1 - g),
        "2.3" = (1 - D2)*(1 - b),
        "3.1" = b*g,
        "3.2" = b*(1 - g),
        "3.3" = 1 - b
      ),
      "SR" = expression(
        "1.1" = c*r,
        "1.2" = (1 - c)*u^2,
        "1.3" = 2*(1 - c)*u*(1 - u),
        "1.4" = c*(1 - r) + (1 - c)*(1 - u)^2,
        "2.1" = u,
        "2.2" = 1 - u
      ),
      "SR2" = expression(
        "1.1" = c*r,
        "1.2" = (1 - c)*u^2,
        "1.3" = 2*(1 - c)*u*(1 - u),
        "1.4" = c*(1 - r) + (1 - c)*(1 - u)^2
      ),
      "WST" = expression(
        "1.1"  = (1 - a)*(1 - P)*(1 - p)*(1 - Q)*(1 - q),
        "1.2"  = a*c*(1 - d)*(1 - sb)*i +
                 (1 - a)*(1 - P)*(1 - p)*(1 - Q)*q,
        "1.3"  = a*c*(1 - d)*sb*i +
                 (1 - a)*(1 - P)*(1 - p)*Q*(1 - q),
        "1.4"  = a*(1 - c)*(1 - x)*(1 - d)*i +
                 (1 - a)*(1 - P)*(1 - p)*Q*q,
        "1.5"  = a*c*d*(1 - sf)*i +
                 (1 - a)*(1 - P)*p*(1 - Q)*(1 - q),
        "1.6"  = a*(1 - c)*x*(1 - sfb)*i +
                 (1 - a)*(1 - P)*p*(1 - Q)*q,
        "1.7"  = a*c*d*(1 - sf)*(1 - i) +
                 a*c*(1 - d)*sb*(1 - i) +
                 (1 - a)*(1 - P)*p*Q*(1 - q),
        "1.8"  = (1 - a)*(1 - P)*p*Q*q,
        "1.9"  = a*c*d*sf*i +
                 (1 - a)*P*(1 - p)*(1 - Q)*(1 - q),
        "1.10" = a*c*d*sf*(1 - i) +
                 a*c*(1 - d)*(1 - sb)*(1 - i) +
                 (1 - a)*P*(1 - p)*(1 - Q)*q,
        "1.11" = a*(1 - c)*x*sfb*i +
                 (1 - a)*P*(1 - p)*Q*(1 - q),
        "1.12" = (1 - a)*P*(1 - p)*Q*q,
        "1.13" = a*(1 - c)*(1 - x)*d*i +
                 (1 - a)*P*p*(1 - Q)*(1 - q),
        "1.14" = (1 - a)*P*p*(1 - Q)*q,
        "1.15" = (1 - a)*P*p*Q*(1 - q),
        "1.16" = a*(1 - c)*x*sfb*(1 - i) +
                 a*(1 - c)*x*(1 - sfb)*(1 - i) +
                 a*(1 - c)*(1 - x)*d*(1 - i) +
                 a*(1 - c)*(1 - x)*(1 - d)*(1 - i) +
                 (1 - a)*P*p*Q*q
      ),
      NULL  # model not available
    )
    if(is.null(modcall))
      stop("'...' has to be either an expression or one of:\n",
           "  '1HT', '2HT', 'PairAsso', 'prospec', 'rmodel', 'SourceMon',\n",
           "  'SR', 'SR2', 'WST'.\n")

    ## Get treeid from names
    nm <- do.call(rbind, strsplit(names(modcall), "\\."))  # treeid.catid
    treeid <- as.numeric(nm[, 1])

    ## Replicates?
    if (!is.null(spec$.replicates) && spec$.replicates > 1) {
      ntrees <- max(treeid)
      treeid <- rep(treeid, spec$.replicates) +
                rep(seq(0, ntrees*(spec$.replicates - 1), ntrees),
                    each=nrow(nm))
      pd <- getParseData(parse(text=modcall, keep.source=TRUE))
      pat <- paste0("(",
              paste(unique(pd$text[pd$token == "SYMBOL"]), collapse="|"), ")")
      newcall <- NULL
      for (i in seq_len(spec$.replicates))
        newcall <- c(newcall, gsub(pat, paste0("\\1", i), modcall))
      modcall <- setNames(parse(text=newcall),
                          paste(treeid, nm[, 2], sep="."))
    }
    spec <- modcall
  }

  ## list of strings
  spec <- lapply(spec, function(s) paste(deparse(s), collapse=""))

  ## substitute restrictions
  if(!is.null(restr)) {
    spec <- lapply(spec, function(s) {
      s <- sprintf("substitute(%s, list(%s))", s, restr)
      deparse(eval(parse(text = s)))
    })
  }

  ## parsed expressions (list of expressions)
  if(!is.null(restr)) restr <- lapply(restr1[-1L], as.expression)
  prob <- lapply(spec, function(s) parse(text=s, keep.source=TRUE))
  if(is.null(treeid) && !is.null(names(prob)))
    treeid <- gsub("([^.]+)\\..*", "\\1", names(prob))  # guess from names

  ## extract the parameters
  pars <- unique(unlist(lapply(prob, function(e) {
    pd <- getParseData(e)
    pd$text[pd$token == "SYMBOL"]                     # get parameter names
  })))
  pars <- structure(rep.int(NA_real_, length(pars)), .Names = pars)

  # ## use .pars to fix parameters or starting values or so
  # if(!is.null(.pars)) {
  #   if(is.list(.pars)) .pars <- do.call("c", .pars)
  #   if(is.null(names(.pars))) stop(".pars must be named list or vector")
  #   pars[names(.pars)] <- .pars
  # }

  ## compute class probabilities
  par2prob <- function(par) {
    ## get all parameters via lexical scoping
    pars <- pars
    
    ## replace NA parameters
    if(sum(is.na(pars)) != length(par))
      stop("numbers of parameters do not match")
    pars[is.na(pars)] <- par
    pars <- as.list(pars)
    
    ## compute probabilities
    rval <- sapply(prob, eval, pars)
    names(rval) <- names(prob)
    return(rval)
  }

  ## derivatives, deriv3() instead of deriv() for second derivatives
  deriv <- lapply(prob, deriv3, names(pars))
  names(deriv) <- names(prob)

  par2deriv <- function(par) {
    ## get all parameters via lexical scoping
    pars <- pars
    
    ## replace NA parameters: FIX ME still needed?
    na_pars <- is.na(pars)
    if(sum(na_pars) != length(par))
      stop("numbers of parameters do not match")
    pars[na_pars] <- par
    pars <- as.list(pars)
    
    ## compute first derivatives
    deriv1 <- rbind(
               sapply(deriv, function(ex) attr(eval(ex, pars), "gradient")))
    rownames(deriv1) <- names(pars)
    deriv1 <- deriv1[na_pars, , drop = FALSE]  # Jacobian

    ## compute second derivatives
    deriv2 <- lapply(deriv, function(ex) attr(eval(ex, pars), "hessian"))
    deriv2 <- array(unlist(deriv2),
                    c(length(pars), length(pars), length(prob)), 
                    list(names(pars), names(pars), names(prob)))
    deriv2 <- deriv2[na_pars, na_pars, , drop = FALSE]

    list(deriv = deriv1, deriv2 = deriv2)  # return 1st and 2nd derivatives
  }

  retval <- list(
    par2prob = par2prob,
    par2deriv = par2deriv,
    prob = prob,
    deriv = deriv,
    par = pars,
    restr = restr,
    treeid = if(!is.null(treeid)) factor(treeid)
  )
  class(retval) <- "mptspec"
  retval
}


## Apply restrictions to existing mptspec object
update.mptspec <- function(object, .restr = NULL, ...){
  spec <- match.call()
  restr <- spec$.restr

  spec <- unlist(object$prob)
  if(!is.null(restr)){
    if(as.character(restr[[1L]]) != "list") stop(".restr must be list")
    spec$.restr <- restr
  }
  do.call(mptspec, spec)
}


## Print model equations
print.mptspec <- function(x, ...){
  print(unlist(x$prob), ...)
}

