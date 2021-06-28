# global object remover

cleanslate <- function(incfuncs=F) {
  if (incfuncs == F) {
    rm(list = setdiff(ls(pos = ".GlobalEnv"), lsf.str(pos = ".GlobalEnv")), pos = ".GlobalEnv")
  } else if (incfuncs == T) {
    rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
  } else {
    stop("Argument incfuncs has wrong variable type. Use a boolean value.")
  }
}

# frame is.na
frame.is.na <- function(X, invert=F) {
  R <- as.data.frame(apply(X, 1, is.na))
  if (!invert) {
    return(R)
  } else if (invert) {
    return(!R)
  } else {
    stop("Argument 'invert' has wrong type. Boolean expected.")
  }
}  

# cleaner
row.col.cleaner <- function(X) {
  fin <- frame.is.na(X)
  fin1 <- as.vector(rev(which(apply(fin, 1, function(a) {sum(a)}) == ncol(X))))
  fin2 <- as.vector(rev(which(apply(fin, 2, function(b) {sum(b)}) == nrow(X))))
  if (length(fin1) > 0) {
    if (length(fin2) > 0) {
      return(list(X[-fin1, -fin2], fin[-fin1, -fin2]))
    } else if (length(fin2) == 0) {
      return(list(X[-fin1, ], fin[-fin1, ]))
    }
  } else if (length(fin1) == 0) {
    if (length(fin2) > 0) {
      return(list(X[, -fin2], fin[, -fin2]))
    } else if (length(fin2) == 0) {
      return(list(X, fin))
    }
  } else {
    stop("Please refer to the source code!")
  }
}

# extractor
prism.extractor <- function(X, phrase="Pancreatic Cancer") {
  R <- X[prism.cl[which(prism.cl[, "disease"] == phrase), "DepMap_ID"], ]
  return(R)
}

# data frame list extractor
l.prism.extractor <- function(X, phrase="Pancreatic Cancer") {
  return(as.list(lapply(X, function(x) {prism.extractor(x, phrase = phrase)})))
}

# extraction veracity verification
extraction.verifier <- function(X, phrase="Pancreatic Cancer") {
  r <- c()
  for (i in 1:nrow(X)) {
    r <- append(r, prism.cl[which(prism.cl[, "DepMap_ID"] == rownames(X)[i]), 20])
  }
  if (sum(r == phrase) == nrow(X)) {
    return(T)
  } else {
    return(F)
  }
}

# vector imputation             not functional 
lat.NA.to.val <- function(x, fun, ...) {
  fun <- match.fun(fun)
  a <- which(unlist(lapply(x, is.na)))
  arg <- if(as.character(fun) == "mean" | as.character(fun) == "median") {list(x, ...)} 
  else if (names(fun) == "rnorm") {list(n = length(a), mean, sd, ...)} 
  else {stop("Unsupported function called.")}
  x[a] <- forceAndCall(length(formalArgs(fun)), fun, arg)
  return(x)
}

# whole data frame imputation           not functional
df.NA.to.val <- function(X, mar, fun, ...) {
  fun <- match.fun(fun)
  if (typeof(X) != "list") {
    stop("Please use data frame type for 'X'.")
  }
  if (fun == "Median" | fun == "median") {
    R <- apply(X, mar, function(x) {
      
    })
    
    
    s <- apply(X, mar, function(x) {median(as.numeric(x), na.rm = T)})
    if (mar == 1) {
      for (a in 1:nrow(X)) {
        X[a, which(is.na(X[a, ]))] <- s[a]
      }
    } else if (mar == 2) {
      for (a in 1:ncol(X)) {
        X[which(is.na(X[, a])), a] <- s[a]
      }
    } else {
      stop("Please use 1 (rows) or 2 (cols) as parameters for 'mar'.")
    }
  } else if (fun == "Mean" | fun == "mean") {
    s <- apply(X, mar, function(x) {mean(as.numeric(x), na.rm = T)})
    if (mar == 1) {
      for (a in 1:nrow(X)) {
        X[a, which(is.na(X[a, ]))] <- s[a]
      }
    } else if (mar == 2) {
      for (a in 1:ncol(X)) {
        X[which(is.na(X[, a])), a] <- s[a]
      }
    } else {
      stop("Please use 1 (rows) or 2 (cols) as parameters for 'mar'.")
    }
  } else if (fun == "Normal" | fun == "normal" | fun == "norm" | fun == "Norm") {
    S <- data.frame(
      c(apply(X, mar, function(x) {mean(as.numeric(x), na.rm = T)})), 
      c(apply(X, mar, function(x) {sd(as.numeric(x), na.rm = T)})))
    if (mar == 1) {
      for (a in 1:nrow(X)) {
        wts <- which(is.na(X[a, ])) 
        X[a, wts] <- rnorm(n = length(wts), mean = S[a, 1], sd = S[a, 2])
      }
    } else if (mar == 2) {
      for (a in 1:ncol(X)) {
        wts <- which(is.na(X[, a])) 
        X[wts, a] <- rnorm(n = length(wts), mean = S[a, 1], sd = S[a, 2])
      }
    } else {
      stop("Please use 1 (rows) or 2 (cols) as parameters for 'mar'.")
    }
  } else {
    stop("Please use mean, median or normal as parameters for 'fun'.")
  }
  return(X)
}

# whole data frame NA to values operation verifier
df.NA.to.val.ver <- function(X, Y, mar, fun, tol=10^-10, m.tol=.1, sd.tol=.1) {
  r <- c()
  if (fun == "median" | fun == "Median") {
    if (mar == 1) {
      for (i in 1:nrow(X)) {
        r <- append(r, median(X[i, ], na.rm = T) == median(Y[i, ], na.rm = T))
      }
    } else if (mar == 2) {
      for (i in 1:ncol(X)) {
        r <- append(r, median(X[, i], na.rm = T) == median(Y[, i], na.rm = T))
      }
    }
    return (list(sum(r) == length(r), r))
  } else if (fun == "mean" | fun == "Mean") {
    if (mar == 1) {
      for (i in 1:nrow(X)) {
        r <- append(r, abs(mean(X[i, ], na.rm = T) - mean(Y[i, ], na.rm = T)) < tol)
      }
    } else if (mar == 2) {
      for (i in 1:ncol(X)) {
        r <- append(r, abs(mean(X[, i], na.rm = T) - mean(Y[, i], na.rm = T)) < tol)
      }
    }
    return (list(sum(r) == length(r), r))
  } else if (fun == "norm" | fun == "Norm" | fun == "normal" | fun == "Normal") { # still under construction
    r.m <- c()
    r.sd <- c()
    if (mar == 1) {
      for (i in 1:nrow(X)) {
        m <- mean(unlist(X[i, ]), na.rm = T)
        sd <- sd(X[i, ], na.rm = T)
        r.m <- append(r.m, abs(m - mean(unlist(Y[i, ]), na.rm = T)) < m * m.tol)
        r.sd <- append(r.sd, abs(sd - sd(Y[i, ], na.rm = T)) < sd * sd.tol)
      }
    } else if (mar == 2) {
      for (i in 1:ncol(X)) {
        m <- mean(unlist(X[, i]), na.rm = T)
        sd <- sd(X[, i], na.rm = T)
        r.m <- append(r.m, abs(m - mean(unlist(Y[, i]), na.rm = T)) < m * m.tol)
        r.sd <- append(r.sd, abs(sd - sd(Y[, i], na.rm = T)) < sd * sd.tol)
      }
    }
    return (list(sum(r.m) == length(r.m), sum(r.sd) == length(r.sd), r.m, r.sd))
  }
}

#subtype extractor
st.ex <- function(disease="Pancreatic Cancer") {
  return(levels(factor(prism.cl[which(prism.cl[, "disease"] == disease), "disease_subtype"])))
}

# Dep Map ID extractor
dmid.ex <- function(target, targetcol = "disease_subtype") {
  return(prism.cl[which(prism.cl[, targetcol] == target), "DepMap_ID"])
}

# short hander 
short.hander <- function(s, mode="initials", n=1, p.ignore=T) {
  if (typeof(s) != "character") {
    stop("Typeof argument s not character.")
  }
  if (mode == "none") {
    return(s)
  }
  if (!is.numeric(n)) {
    stop("Argument n not coercible as numeric.")
  }
  if (p.ignore) {
    s.sep <- gsub("[[:punct:]]", "", unlist(strsplit(s, " ")))
  } else if (!p.ignore) {
    s.sep <- unlist(strsplit(s, " "))
  }
  if (mode == "initials") {
    r <- ""
    for (i in 1:length(s.sep)) {
      r <- paste(r, substr(s.sep[i], 1, n), sep = "")
    }
    return(r)
  } else if (mode == "-vowels") {
    stop("Not yet ready")
  } else if (mode == "initials-vowels") {
    stop("Not yet ready")
  } else {
    stop("Argument mode not recognised.")
  }
}

# subtype splitter            
st.splitter <- function(X, disease="Pancreatic Cancer", sh.mode="initials", sh.n=3, sh.p.ignore=T) {
  sts <- st.ex(disease)
  dmids <- lapply(sts, function(x) {dmid.ex(x, targetcol = "disease_subtype")})
  for (i in 1:length(sts)) {
    assign(paste(
        short.hander(disease, mode = sh.mode, n = sh.n, p.ignore = sh.p.ignore), 
        ".", 
        short.hander(sts[i], mode = sh.mode, n = sh.n, p.ignore = sh.p.ignore), 
        sep = ""), 
      X[unlist(dmids[i]), ], 
      pos = .GlobalEnv)
  }
}

# efficacious drug identifier            # this mofo is effed, needa fix
ef.dr.identifier <- function(X, threshold=0, p.thresh=F, natov="median") {
  if (typeof(X) != "list") {
    stop("Typeof argument X not list.")
  } else if (dim(X)[1] > 1) {
    if (p.thresh) {
      return(unique(prism.treat[factor(names(which(apply(df.NA.to.val(X, 1, natov), 2, mean) > threshold))), "name"]))
    } else if (!p.thresh) {
      return(unique(prism.treat[factor(names(which(apply(df.NA.to.val(X, 1, natov), 2, mean) < threshold))), "name"]))
    }
  } else if(dim(X)[1] == 1) {
    if (p.thresh) {
      return(unique(prism.treat[factor(names(which(apply(X[-which(is.na(X))], 1, mean) > threshold))), "name"]))
    } else if (!p.thresh) {
      return(unique(prism.treat[factor(names(which(apply(X[-which(is.na(X))], 1, mean) < threshold))), "name"]))
    }
  }
}