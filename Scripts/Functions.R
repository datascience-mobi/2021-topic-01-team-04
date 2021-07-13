message("Functions.R >>")

message("   Loading in function 'ob.name.ex()'.")

##  object name extractor
#   SImple function that returns the name of the single argument 'ob' as a character sting.
onex <- cmpfun(function(ob) return(deparse(substitute(ob))))


message("   Loading in function 'frame.is.na()'.")
##  frame is.na
#   Returns a data frame of the same dimensions and orientations as the input data frame with TRUE (invert=F) or FALSE (invert=T) for each NA in the input data frame.
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


message("   Loading in function 'row.col.cleaner()'.")
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


message("   Loading in function 'prism.extractor()'.")
##  extractor
#   Returns a data frame with the subset of the input data frame which matches the disease type specified with the argument 'disease'.
prism.extractor <- cmpfun(function(X, disease="Pancreatic Cancer") {
  R <- X[prism.cl[which(prism.cl[, "disease"] == disease), "DepMap_ID"], ]
  return(R)
})


##  data frame list extractor         non-operable
#   Iterates the above extractor over a list of data frames. Returns a list of extracted data frames.
l.prism.extractor <- cmpfun(function(X, disease="Pancreatic Cancer") {
  return(as.list(lapply(X, prism.extractor, disease = disease)))
})


message("   Loading in function 'extraction.verifier()'.")
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


message("   Loading in function 'df.NA.to.val()'.")
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


message("   Loading in function 'st.ex()'.")
##  subtype extractor
st.ex <- cmpfun(function(disease="Pancreatic Cancer") {
  return(levels(factor(prism.cl[which(prism.cl[, "disease"] == disease), "disease_subtype"])))
})


message("   Loading in function 'dmid.ex()'.")
##  Dep Map ID extractor
dmid.ex <- cmpfun(function(target, targetcol = "disease_subtype") {
  return(prism.cl[which(prism.cl[, targetcol] == target), "DepMap_ID"])
})


message("   Loading in function 'short.hander()'.")
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


message("   Loading in function 'st.splitter()'.")
##  subtype splitter   
#
st.splitter <- cmpfun(function(X, disease="Pancreatic Cancer", custom.sh=F, sh.mode="initials", sh.n=3, sh.p.ignore=T, doscor =0) {
  sts <- st.ex(disease)
  dmids <- lapply(sts, dmid.ex, targetcol = "disease_subtype")
  r <- c()
  if (!custom.sh) {
    for (i in 1:length(sts)) {
      r <- append(r, paste(short.hander(disease, mode = sh.mode, n = sh.n, p.ignore = sh.p.ignore), ".", short.hander(sts[i], mode = sh.mode, n = sh.n, p.ignore = sh.p.ignore), if (doscor == 1) ".doscor" else if (doscor == 2) ".perdrug", sep = ""))
      assign(r[i], X[unlist(dmids[i]), ], pos = .GlobalEnv)
    }
  } else if (custom.sh) {
    for (i in 1:length(sts)) {
      r <- append(r, dlg_input(message = paste("Please enter a custom short hand for ", disease, " subtype: ", sts[i], ". Doscor is: ", if (doscor == 0) "FALSE" else if (doscor == 1) "TRUE" else if (doscor == 2) "perdrug", sep = ""))$res)
      assign(r[i], X[unlist(dmids[i]), ], pos = .GlobalEnv)
    }
  }
  assign(paste("st.split.vars", if(doscor == 1) ".doscor" else if (doscor == 2) ".perdrug", sep = ""), r, pos = .GlobalEnv)
})


message("   Loading in function 'doscor()'.")
##  dose correlator
#
doscor <- cmpfun(function(X, doscor = T, perdrug = T, PT = prism.treat) {
  if (doscor == T) {
    l.drugs <- unique(PT[, "name"])
    id <- sapply(l.drugs, function(x) rownames(PT[which(PT[, "name"] == x), ]))
    X.names <- c()
    for (i in 1:length(id)) {
      x <- X[, unlist(sapply(unlist(id[i]), function(u) which(colnames(X) == u)))]
      X.names <- append(X.names, paste("X.doscor.", i, sep = ""))
      assign(X.names[i], as.vector(apply(x, 1, mean, na.rm = T)))
    }
    X.doscor <- data.frame(mget(X.names))
    colnames(X.doscor) <- sapply(id, function(x) unlist(x[1]))
    return(X.doscor)
  } else if (is.character(doscor)) {
    if (doscor %in% c("dfd", "efd", "dpd", "epd")) {
      Y.doscor <- as.vector(PT[,"dose"])
      Y.doscor.n <- rownames(PT)
      names(Y.doscor) <- Y.doscor.n
      add.Y <- data.frame(row.names = rownames(X))
      iti <- 0
      ns <- c()
      for (i in 1:length(Y.doscor.n)) {
        n <- which(colnames(X) == Y.doscor.n[i])
        if (length(n) == 0) next
        ns <- append(ns, n)
        iti <- iti + 1
        add.Y <- data.frame(add.Y, sapply(X[, n], function(x, i) x/Y.doscor[i], i))
      }
      colnames(add.Y) <- colnames(X)[ns]
      rownames(add.Y) <- rownames(X)
      if (perdrug == T) {
        d <- droplevels(unique(PT[, "broad_id"]))
        add.Y <- as.data.fram(sapply(d, function(x) {
          uwu <- grep(x, colnames(add.Y), fixed = T)
          res <- apply(add.Y[uwu], 1, function(y) if (length(y) == 1) return(y) else return(mean(y, na.rm = T)))
          return(res)
        }))
        if (is.vector(add.Y)) add.Y <- t(data.frame(add.Y))
        colnames(add.Y) <- d
        rownames(add.Y) <- rownames(X)
      }
      return(add.Y)
    } else {
      warning("Unexpected format of doscor. Doscor operation has been ignored. Make sure to use 'TRUE', 'FALSE', or 'dfd'.")
      return(X)
    }
  } else if (doscor != F) {
    warning("Unexpected data type of doscor. Doscor operation has been ignored. Make sure to use 'TRUE', 'FALSE', or 'dfd'.")
    return(X)
  }
})


message("   Loading in function 'ef.dr.identifier()'.")
##  efficacious drug identifier         doscor = 'dfd' takes a fuck-ton of time, unsure how to optimise
#   doscor = 'dfd' needs to be made compatible to the rest, taking the mean of X initially???
#   
ef.dr.identifier <- cmpfun(function(X, threshold = "q.001", greaterthan = F, impmeth="i", sinonco = F) {
  if (sinonco) {
    assign("prism.treat", get("prism.treat.sinonco", pos = .GlobalEnv), pos = -1)
  }
  
  if (!is.data.frame(X)) {
    stop("Argument X is not a data frame. It is a ", typeof(X), ".", domain = "r-pkg")
  } 
  
  #   what doscor mode has been used on the data?
  if (grepl(".doscor", onex(X))) doscor <- 1 else if (grepl(".perdrug", onex(X))) doscor <- 2 else doscor <- 0
  
  #   which type of thresholding is to be used? static or quantile?
  
  if (is.double(threshold)) {
    
    #   static thresholding
    
    if (impmeth == "ignore" | impmeth == "i" | impmeth == "ign") {
      
      #   NAs are ignored 
      
      if (greaterthan) {
        if (nrow(X) > 1) {
          x <- apply(X, 2, mean, na.rm = T)
        } else if (nrow(X) == 1) {
          x <- as.vector(X)
          names(x) <- colnames(X)
        }
        x <- sort(x, decreasing = T)
        threshold <- quantile(x, probs = 1 - threshold, na.rm = T)
        w <- names(which(x > threshold))
        return(unique(prism.treat[w, "name"]))
      } else if (!greaterthan) {
        if (nrow(X) > 1) {
          x <- apply(X, 2, mean, na.rm = T)
        } else if (nrow(X) == 1) {
          x <- as.double(X)
          names(x) <- colnames(X)
        }
        x <- sort(x, decreasing = F)
        threshold <- quantile(x, probs = threshold, na.rm = T)
        w <- names(which(x < threshold))
        return(unique(prism.treat[w, "name"]))
      }
    }
    
    #   NAs are imputated 
    
    warning("You are not ignoring NAs. This can impact the reliability of your results negatively. Please check if your method of imputation is robust in regards to your thresholding.", domain = "r-pkg")
    if (greaterthan) {
      w <- names(which(apply(df.NA.to.val(X, 2, impmeth), 2, mean) > threshold))
      return(unique(prism.treat[w, "name"]))
    } else if (!greaterthan) {
      w <- names(which(apply(df.NA.to.val(X, 2, impmeth), 2, mean) < threshold))
      return(unique(prism.treat[w, "name"]))
    }
    
  } else if (is.character(threshold)) {
    
    #   quantile thresholding
    
    if (substr(threshold, 1, 1) == "q" | substr(threshold, 1, 1)== "Q") {
      threshold <- as.double(substr(threshold, 2, nchar(threshold)))
      if (is.double(threshold)) {
        
        #   NAs are ignored
        
        if (impmeth == "ignore" | impmeth == "i" | impmeth == "ign") {
          if (greaterthan) {
            if (nrow(X) > 1) {
              x <- apply(X, 2, mean, na.rm = T)
            } else if (nrow(X) == 1) {
              x <- as.vector(X)
              names(x) <- colnames(X)
            }
            x <- sort(x, decreasing = T)
            threshold <- quantile(x, probs = 1 - threshold, na.rm = T)
            w <- names(which(x > threshold))
            if (length(grep("::", w, invert = T)) == 0) return(unique(prism.treat[w, "name"])) else {
              w <- sapply(w, function(x) grep(x, prism.treat[, "broad_id"]))[1, ]
              return(prism.treat[w, "name"])
            }
          } else if (!greaterthan) {
            if (nrow(X) > 1) {
              x <- apply(X, 2, mean, na.rm = T)
            } else if (nrow(X) == 1) {
              x <- as.double(X)
              names(x) <- colnames(X)
            }
            x <- sort(x, decreasing = F)
            threshold <- quantile(x, probs = threshold, na.rm = T)
            w <- names(which(x < threshold))
            if (length(grep("::", w, invert = T)) == 0) return(unique(prism.treat[w, "name"])) else {
              w <- sapply(w, function(x) grep(x, prism.treat[, "broad_id"]))[1, ]
              return(prism.treat[w, "name"])
            }
          }
        }
        
        #   NAs are imputated
        
        warning("You are not ignoring NAs. This can impact the reliability of your results negatively. Please check if your method of imputation is robust in regards to your thresholding.", domain = "r-pkg")
        if (greaterthan) {
          if (nrow(X) > 1) {
            x <- apply(df.NA.to.val(X, 2, impmeth), 2, mean, na.rm = T)
          } else if (nrow(X) == 1) {
            x <- as.vector(df.NA.to.val(X, 2, impmeth))
          }
          x <- sort(x, decreasing = T)
          threshold <- quantile(x, probs = 1 - threshold, na.rm = T)
          w <- names(which(x > threshold))
          return(unique(prism.treat[w, "name"]))
        } else if (!greaterthan) {
          if (nrow(X) > 1) {
            x <- apply(df.NA.to.val(X, 2, impmeth), 2, mean, na.rm = T)
          } else if (nrow(X) == 1) {
            x <- as.vector(df.NA.to.val(X, 2, impmeth))
          }
          x <- sort(x, decreasing = F)
          threshold <- quantile(x, probs = threshold, na.rm = T)
          w <- names(which(x < threshold))
          return(unique(prism.treat[w, "name"]))
        }
      } else {
        stop("Unexpected format of argument 'threshold'.")
      }
    }
  } else {
    stop("Unexpected data type of argument 'threshold'.")
  }
})



##  ef caller                 INOPERABLE
ef.caller <- cmpfun(function(threshold, greaterthan, ..., addat=c()){
  if (!exists("st.split.vars", envir = .GlobalEnv)) {
    call("st.splitter", disease = "Pancreatic Cancer", custom.sh = T)
  }
  ssv <- append(addat, st.split.vars)
  r <- list()
  if (threshold == F) {
    threshold <- as.double(dlg_input(message = "Enter custom threshold for all datasets.")$res)
    for (i in 1:length(ssv)) {
      r <- append(r, ef.dr.identifier(list(..., X = as.data.frame(get(ssv[i], pos = .GlobalEnv)), threshold = threshold, greaterthan = greaterthan)))
    }
  } else if (threshold == T) {
    for (i in 1:length(ssv)) {
      r <- append(r, ef.dr.identifier(list(..., X = as.data.frame(get(ssv[i], pos = .GlobalEnv)), threshold = as.double(dlg_input(message = paste("Enter custom threshold for dataset ", ssv[i],".", sep = ""))$res), greaterthan = greaterthan)))
    }
  } else if (length(threshold) == length(ssv)) {
    for (i in 1:length(ssv)) {
      r <- append(r, ef.dr.identifier(list(..., X = as.data.frame(get(ssv[i], pos = .GlobalEnv)), threshold = threshold[i], greaterthan = greaterthan)))
    }
  } else if (is.double(threshold)) {
    for (i in 1:length(ssv)) {
      r <- append(r, ef.dr.identifier(list(..., X = as.data.frame(get(ssv[i], pos = .GlobalEnv)), threshold = threshold, greaterthan = greaterthan)))
    }
  } else {
    stop(paste("Argument 'threshold' has unexpected value at ", threshold, ". Please refer to the code.", sep = ""))
  }
  return(unlist(r))
}) 
