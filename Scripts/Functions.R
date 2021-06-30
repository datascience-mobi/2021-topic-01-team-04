##  global object remover
#   Removes all objects including (incfuncs=T) or excluding functions (incfuncs=F) from the global environment.
cleanslate <- function(incfuncs=F) {
  if (incfuncs == F) {
    rm(list = setdiff(ls(pos = ".GlobalEnv"), lsf.str(pos = ".GlobalEnv")), pos = ".GlobalEnv")
  } else if (incfuncs == T) {
    rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
  } else {
    stop("Argument incfuncs has wrong variable type. Use a boolean value.")
  }
}

##  frame is.na
#   Gives out a data frame of the same dimensions and orientations as the input data frame with TRUE (invert=F) or FALSE (invert=T) for each NA in the input data frame.
frame.is.na <- cmpfun(function(X, invert=F) {
  R <- as.data.frame(apply(X, 2, is.na))
  if (!invert) {
    return(R)
  } else if (invert) {
    return(!R)
  } else {
    stop("Argument 'invert' has wrong type. Boolean expected.", domain = "r-pkg")
  }
})  

##  cleaner
#   Removes all columns and rows of a data frame that ONLY (ex=T) or AT ALL (ex=F) contain NA.
row.col.cleaner <- cmpfun(function(X, ex=T) {
  fin <- frame.is.na(X)
  if (ex) {
    fin1 <- apply(fin, 1, sum) != ncol(X)
    fin2 <- apply(fin, 2, sum) != nrow(X)
    return(X[fin1, fin2])
  } else if (!ex) {
    fin1 <- apply(fin, 1, sum) == 0
    fin2 <- apply(fin, 2, sum) == 0
    return(X[fin1, fin2])
  } else {
    stop("Unexpected format of argument 'ex'. 'ex' is a boolean---TRUE to remove data-void columns and rows, FALSE to remove NA-containing columns and rows.", domain = "r-pkg")
  }
})

##  extractor
#   Gives out a data frame with the subset of the input data frame which matches the disease type specified with the argument 'disease'.
prism.extractor <- cmpfun(function(X, disease="Pancreatic Cancer") {
  R <- X[prism.cl[which(prism.cl[, "disease"] == disease), "DepMap_ID"], ]
  return(R)
})

##  data frame list extractor
#   Iterates the above extractor over a list of data frames. Gives out a list of extracted data frames.
l.prism.extractor <- cmpfun(function(X, disease="Pancreatic Cancer") {
  return(as.list(lapply(X, prism.extractor, disease = disease)))
})

##  extraction veracity verification              NOT FUNCTIONAL!!!
extraction.verifier <- cmpfun(function(X, disease="Pancreatic Cancer") {
  r <- c()
  for (i in 1:nrow(X)) {
    r <- append(r, prism.cl[which(prism.cl[, "DepMap_ID"] == rownames(X)[i]), 20])
  }
  if (sum(r == disease) == nrow(X)) {
    return(T)
  } else {
    return(F)
  }
})

##  vector imputation             NOT FUNCTIONAL!!!
lat.NA.to.val <- function(x, fun) {}

##  whole data frame imputation
df.NA.to.val <- cmpfun(function(X, mar, fun) {
  if (!is.data.frame(X)) {
    stop("Please use data frame type for 'X'.", domain = "r-pkg")
  }
  imar <- if(mar==1){2}else if(mar==2){1}
  if (dim(X)[imar] == 1) {
    warning(paste("'dim(X)[", imar, "]' is not of sufficient size at dim(X)[", imar, "]==", dim(X)[imar], " to enact imputation along the chosen margin ", mar, ". NAs are instead removed.", sep = ""), domain = "r-pkg")
    return(X[-which(is.na(X))])
  }
  if (dim(X)[imar] <= 50) {
    warning(paste("'dim(X)[", imar, "]' is relatively small at dim(X)[", imar, "]==", dim(X)[imar], ". Please ensure that imputation along the chosen margin ", mar, " is robust.", sep = ""), domain = "r-pkg")
  } 
  if (fun %in% c("Median", "median", "Med", "med", "Medi", "medi")) {
    r <- apply(X, mar, function(x) {x[which(is.na(x))] <- median(as.numeric(x), na.rm = T); return(x)})
  } else if (fun %in% c("Mean", "mean", "M", "m")) {
    r <- apply(X, mar, function(x) {x[which(is.na(x))] <- mean(as.numeric(x), na.rm = T); return(x)})
  } else if (fun %in% c("Normal", "normal", "Norm", "norm", "Nor", "nor", "N", "n")) {
    r <- apply(X, mar, function(x) {w <- which(is.na(x)); x[w] <- rnorm(n = sum(w), mean = mean(x, na.rm = T), sd = sd(x, na.rm = T)); return(x)})
  } else {
    stop("Please use 'mean', 'median' or 'normal' as parameters for 'fun'.", domain = "r-pkg")
  }
  if(ncol(r) != ncol(X) & nrow(r) != nrow(X)) {
    return(t(r))
  } else if (ncol(r) == ncol(X) & nrow(r) == nrow(X)) {
    return(r)
  } else {
    stop("Unexpected result. Imputated data frame is not congruent with argument data frame.", domain = "r-pkg")
  }
  stop(paste("'dim(X)[", mar, "]' is not positive."), domain = "r-pkg")
})

##  whole data frame NA to values operation verifier
df.NA.to.val.ver <- cmpfun(function(X, Y, mar, fun, tol=10^-10, m.tol=.1, sd.tol=.1) {
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
})

##  subtype extractor
st.ex <- cmpfun(function(disease="Pancreatic Cancer") {
  return(levels(factor(prism.cl[which(prism.cl[, "disease"] == disease), "disease_subtype"])))
})

##  Dep Map ID extractor
dmid.ex <- cmpfun(function(target, targetcol = "disease_subtype") {
  return(prism.cl[which(prism.cl[, targetcol] == target), "DepMap_ID"])
})

##  short hander 
short.hander <- cmpfun(function(s, mode="initials", n=1, p.ignore=T) {
  if (typeof(s) != "character") {
    stop("Typeof argument s not character.", domain = "r-pkg")
  }
  if (mode == "none") {
    return(s)
  }
  if (!is.numeric(n)) {
    stop("Argument n not coercible as numeric.", domain = "r-pkg")
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
    stop("Not yet ready", domain = "r-pkg")
  } else if (mode == "initials-vowels") {
    stop("Not yet ready", domain = "r-pkg")
  } else {
    stop("Argument 'mode' not recognised.", domain = "r-pkg")
  }
})

##  subtype splitter   
#
st.splitter <- cmpfun(function(X, disease="Pancreatic Cancer", custom.sh=F, sh.mode="initials", sh.n=3, sh.p.ignore=T) {
  sts <- st.ex(disease)
  dmids <- lapply(sts, dmid.ex, targetcol = "disease_subtype")
  if (!custom.sh) {
    for (i in 1:length(sts)) {
      assign(paste(short.hander(disease, mode = sh.mode, n = sh.n, p.ignore = sh.p.ignore), ".", short.hander(sts[i], mode = sh.mode, n = sh.n, p.ignore = sh.p.ignore), sep = ""), X[unlist(dmids[i]), ], pos = .GlobalEnv)
    }
  } else if (custom.sh) {
    r <- c()
    for (i in 1:length(sts)) {
      r <- append(r, dlg_input(message = paste("Please enter a custom short hand for ", disease, " subtype: ", sts[i], sep = ""))$res)
      assign(r[i], X[unlist(dmids[i]), ], pos = .GlobalEnv)
    }
    assign("st.split.vars", r, pos = .GlobalEnv)
  }
})

##  efficacious drug identifier         DOSCOR YET TO BE IMPLEMENTED
#   

ef.dr.identifier <- cmpfun(function(X, threshold, greaterthan, impmeth="i", doscor = F) {
  if (!is.data.frame(X)) {
    stop("Argument X is not a data frame.", domain = "r-pkg")
  } 
  if (impmeth == "ignore" | impmeth == "i" | impmeth == "ign") {
    if (greaterthan) {
      w <- names(which(apply(X, 2, mean, na.rm = T) > threshold))
      return(unique(prism.treat[w, "name"]))
    } else if (!greaterthan) {
      w <- names(which(apply(X, 2, mean, na.rm = T) < threshold))
      return(unique(prism.treat[w, "name"]))
    }
  }
  warning("You are not ignoring NAs. This can impact the reliability of your results negatively. Please check if your method of imputation is robust in regards to your thresholding.", domain = "r-pkg")
  if (greaterthan) {
    w <- names(which(apply(df.NA.to.val(X, 2, impmeth), 2, mean) > threshold))
    return(unique(prism.treat[w, "name"]))
  } else if (!greaterthan) {
    w <- names(which(apply(df.NA.to.val(X, 2, impmeth), 2, mean) < threshold))
    return(unique(prism.treat[w, "name"]))
  }
})