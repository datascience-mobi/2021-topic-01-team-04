message("Executing 'Functions.R'.", domain = "r-pkg")

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


message("   Loading in function 'df.NA.to.val()'.")
##  whole data frame imputation
#   Imputates NAs with mean, median or norm values over a whole data frame, using column or row data.
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


message("   Loading in function 'st.ex()'.")
##  subtype extractor
#   Returns a list of the subtypes of the wanted disease. The argument 'disease' needs to be formatted in congruence with the column 'disease' of 'prism.cl'.
st.ex <- cmpfun(function(disease="Pancreatic Cancer") {
  return(levels(factor(prism.cl[which(prism.cl[, "disease"] == disease), "disease_subtype"])))
})


message("   Loading in function 'dmid.ex()'.")
##  Dep Map ID extractor#
#   Extracts the DepMap IDs of all cell lines of a disease subtype.
dmid.ex <- cmpfun(function(target, targetcol = "disease_subtype") {
  return(prism.cl[which(prism.cl[, targetcol] == target), "DepMap_ID"])
})


message("   Loading in function 'short.hander()'.")
##  short hander 
#   Generates a short hand out of a character string. 
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
#   Splits a data set in regard to the cell lines subtypes of the cell lines. 
st.splitter <- cmpfun(function(X, disease="Pancreatic Cancer", custom.sh=F, sh.mode="initials", sh.n=3, sh.p.ignore=T, doscor = 0) {
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
  assign(paste("st.split.vars", if(doscor == 1) ".doscor" else if (doscor == 2) ".perdrug", sep = ""), data.frame(r, sts), pos = .GlobalEnv)
})


message("   Loading in function 'doscor()'.")
##  dose correlator, takes a long time
#   Has different operations>> With doscor = T and perdrug = T, it computes the mean efficacy per dosage for each drug. With doscor = T and perdrug = F it normalises each dose of each drug to the given dosage. With doscor = F it does nothing and will throw a warning, that no operation has been performed. 
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
        add.Y <- as.data.frame(sapply(d, function(x) {
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
##  efficacious drug identifier
#   Identifies the most effective drugs in a given population 'X'.
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
          x <- as.double(apply(X, 2, mean, na.rm = T))
        } else if (nrow(X) == 1) {
          x <- as.double(X)
          names(x) <- colnames(X)
        }
        x <- sort(x, decreasing = T)
        threshold <- quantile(x, probs = 1 - threshold, na.rm = T)
        w <- names(which(x > threshold))
        return(unique(prism.treat[w, "name"]))
      } else if (!greaterthan) {
        if (nrow(X) > 1) {
          x <- as.double(apply(X, 2, mean, na.rm = T))
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
              x <- as.double(apply(X, 2, mean, na.rm = T))
            } else if (nrow(X) == 1) {
              x <- as.double(X)
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
              x <- as.double(apply(X, 2, mean, na.rm = T))
              names(x) <- colnames(X)
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
          } else {
            stop("Unexpected format of argument 'threshold'.")
          }
        }
        
        #   NAs are imputated
        warning("You are not ignoring NAs. This can impact the reliability of your results negatively. Please check if your method of imputation is robust in regards to your thresholding.", domain = "r-pkg")
        if (greaterthan) {
          if (nrow(X) > 1) {
            x <- as.double(apply(df.NA.to.val(X, 2, impmeth), 2, mean, na.rm = T))
          } else if (nrow(X) == 1) {
            x <- as.double(df.NA.to.val(X, 2, impmeth))
          }
          x <- sort(x, decreasing = T)
          threshold <- quantile(x, probs = 1 - threshold, na.rm = T)
          w <- names(which(x > threshold))
          return(unique(prism.treat[w, "name"]))
        } else if (!greaterthan) {
          if (nrow(X) > 1) {
            x <- as.double(apply(df.NA.to.val(X, 2, impmeth), 2, mean, na.rm = T))
          } else if (nrow(X) == 1) {
            x <- as.double(df.NA.to.val(X, 2, impmeth))
          }
          x <- sort(x, decreasing = F)
          threshold <- quantile(x, probs = threshold, na.rm = T)
          w <- names(which(x < threshold))
          return(unique(prism.treat[w, "name"]))
        }
      } else {
        stop("Unexpected format of argument 'threshold'.")
      }
    } else {
      stop("Unexpected first character of argument 'threshold'.")
    }
  } else {
    stop("Unexpected data type of argument 'threshold'.")
  } 
})


message("   Loading in function 'dr.to.effect()'.")
##  drugs to effect 
#   Returns the drug effect
dr.to.effect <- cmpfun(function(n, doscor, sort = 1, output = "mean") {
  if (!is.vector(n)) {if (is.character(n)) {n <- c(n)} else {stop("Argument 'n' needs to be a character vector or string!")}}
  res <- list()
  pt.rn <- rownames(prism.treat)
  for (i in 1:length(n)) {
    broadID <- as.character(unique(prism.treat[which(prism.treat[, "name"] == n[i]), "broad_id"]))
    if (doscor == 2) {
      if (output == "mean") {out <- mean(as.vector(prism.perdrug[, broadID]), na.rm = T)
      } else if (output == "median") {out <- median(as.vector(prism.perdrug[, broadID]), na.rm = T)
      } else if (output == "full") {out <- as.vector(prism.perdrug[, broadID]); names(out) <- rownames(prism.perdrug)
      } else stop("Argument 'output' is of wrong fromat. It needs to be a character string of values {'mean', 'median', 'full'}!")
    } else if (doscor == 1) {
      ID <- pt.rn[grep(broadID), pt.rn]
      if (output != "mean") warning("With doscor ∈ {0, 1} only a 'mean' output will be made.")
      out <- apply(prism.doscor[, ID], 2, mean, na.rm = T)
      names(out) <- ID
    } else if (doscor == 0) {
      ID <- pt.rn[grep(broadID), pt.rn]
      if (output != "mean") warning("With doscor ∈ {0, 1} only a 'mean' output will be made.")
      out <- apply(prism.clean[, ID], 2, mean, na.rm = T)
      names(out) <- ID
    } else stop("Argument 'doscor' is not ∈ {0, 1, 2}!")
    res <- append(res, out)
  }
  if (sort == 1) {
    res.u <- unlist(unname(res)); names(res.u) <- 1:length(res.u)
    res.s <- names(sort(res.u, decreasing = F))
    res.out <- res[as.double(res.s)]
    names(res.out) <- unlist(n)[as.double(res.s)]
  } else if (sort == 0) {
    res.s <- 1:length(unlist(unname(res)))
    res.out <- res
    names(res.out) <- unlist(n)
  } else if (sort == -1) {
    res.u <- unlist(unname(res)); names(res.u) <- 1:length(res.u)
    res.s <- names(sort(res.u, decreasing = T))
    res.out <- res[as.double(res.s)]
    names(res.out) <- unlist(n)[as.double(res.s)]
  }
  return(res.out)
})


message("   Loading in function 'reg.an.prepper()'.")
##  regression analysis prepper
#   
reg.an.prepper <- cmpfun(function(n, pos = .env.ra) {
  if (!exists(as.character(substitute(pos)), where = .GlobalEnv)) {
    assign(as.character(substitute(pos)), new.env(parent = .GlobalEnv), pos = .GlobalEnv)
  } else rm(list = ls(pos = as.environment(pos)))
  
  pancan.achilles <- row.col.cleaner(prism.extractor(prism.achilles))
  
  
  for (i in 1:5) { 
    assign(
      paste("dr.ef.pancan.max.", i, sep = ""), 
      dr.to.effect(ef.pancan.perdrug, doscor = 2)[i],  
      pos = .env.ra)
    assign(
      paste("id.dr.ef.pancan.max.", i, sep = ""), 
      as.character(unique(prism.treat[which(prism.treat[, "name"]==names(get(paste("dr.ef.pancan.max.", i, sep = ""), pos = .env.ra))), "broad_id"])),  
      pos = .env.ra)
    assign(
      paste("ef.pancan.max.percl.", i, sep = ""), 
      pancan.perdrug[, get(paste("id.dr.ef.pancan.max.", i, sep = ""), pos = .env.ra)],  
      pos = .env.ra)
    names(.env.ra[[paste("ef.pancan.max.percl.", i, sep = "")]]) <- rownames(pancan.perdrug)
    assign(
      paste("ef.pancan.max.percl.", i, sep = ""), 
      get(paste("ef.pancan.max.percl.", i, sep = ""), pos = .env.ra)[!is.na(get(paste("ef.pancan.max.percl.", i, sep = ""), pos = .env.ra))], 
      pos = .env.ra)
    del <- c(NULL)
    for (j in 1:length(get(paste("ef.pancan.max.percl.", i, sep = ""), pos = .env.ra))) {
      if (sum(grepl(names(get(paste("ef.pancan.max.percl.", i, sep = ""), pos = .env.ra))[j], rownames(pancan.achilles)))==0) {del <- append(del, j)}
    }
    if(!is.null(del)) {
      .env.ra[[paste("ef.pancan.max.percl.", i, sep = "")]] <- .env.ra[[paste("ef.pancan.max.percl.", i, sep = "")]][-rev(del)]
    }
    del <- c(NULL)
    for (j in 1:nrow(pancan.achilles)) {
      if (!{rownames(pancan.achilles)[j] %in% names(get(paste("ef.pancan.max.percl.", i, sep = ""), pos = .env.ra))}) del <- append(del, j)
    }
    if(!is.null(del)) {
      .env.ra[[paste("pancan.achilles.fitted.", i, sep = "")]] <- pancan.achilles[-rev(del), ]
    } else .env.ra[[paste("pancan.achilles.fitted.", i, sep = "")]] <- pancan.achilles
  }
})


message("Finished Executing 'Functions.R'.", domain = "r-pkg")