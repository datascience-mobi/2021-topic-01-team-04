# global object remover

cleanslate <- function(incfuncs = F) {
  if (incfuncs == F) {
    rm(list = setdiff(ls(pos = ".GlobalEnv"), lsf.str(pos = ".GlobalEnv")), pos = ".GlobalEnv")
  } else if (incfuncs == T) {
    rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
  } else {
    print("Error>> incfuncs has wrong variable type. Use a boolean value.")
  }
}

# lateral is.na
lat.is.na <- function(x, TF = F) {
  r <- as.vector(sapply(x, is.na))
  if (TF) {
    return(r)
  } else if (!TF) {
    return(!r)
  } else {
    return("ERROR!")
  }
}

# frame is.na
frame.is.na <- function(X, TF = F) {
  R <- as.data.frame(matrix(nrow = nrow(X), ncol = ncol(X)))
  rownames(R) <- rownames(X); colnames(R) <- colnames(X)
  for (i in 1:nrow(X)) {
    R[i, ] <- lat.is.na(X[i, ], TF)
  }
  return(R)
}

# cleaner
row.col.cleaner <- function(X) {
  fin <- frame.is.na(X)
  fin1 <- as.vector(rev(which(apply(fin, 1, function(a) {sum(!a)}) == ncol(X))))
  fin2 <- as.vector(rev(which(apply(fin, 2, function(b) {sum(!b)}) == nrow(X))))
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
    return("ERROR! Please refer to the source code.")
  }
}

# extractor
prism.extractor <- function(X, phrase = "Pancreatic Cancer") {
  rn <- prism.cl[which(prism.cl[, 20] == phrase), "DepMap_ID"]
  R <- X[rn, ]
  return(R)
}

# data frame list extractor
l.prism.extractor <- function(X, phrase = "Pancreatic Cancer") {
  R <- list()
  for (i in 1:length(X)) {
    R <- append(R, prism.extractor(X[i]))
  }
  return(R)
}

# extraction veracity verification
extraction.verifier <- function(X, phrase = "Pancreatic Cancer") {
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

# whole data frame NA to values
df.NA.to.val <- function(X, mar, fun) {
  if (typeof(X) != "list") {
    stop("Please use data frame type for 'X'.")
  }
  if (fun == "Median" | fun == "median") {
    if (mar == 1) {
      for (a in 1:nrow(X)) {
        X[a, which(is.na(X[a, ]))] <- median(as.numeric(X[a, ]), na.rm = T)
      }
    } else if (mar == 2) {
      for (a in 1:ncol(X)) {
        X[which(is.na(X[, a])), a] <- median(as.numeric(X[, a]), na.rm = T)
      }
    } else {
      stop("Please use 1 (rows) or 2 (cols) as parameters for 'mar'.")
    }
  } else if (fun == "Mean" | fun == "mean") {
    if (mar == 1) {
      for (a in 1:nrow(X)) {
        X[a, which(is.na(X[a, ]))] <- mean(as.numeric(X[a, ]), na.rm = T)
      }
    } else if (mar == 2) {
      for (a in 1:ncol(X)) {
        X[which(is.na(X[, a])), a] <- mean(as.numeric(X[, a]), na.rm = T)
      }
    } else {
      stop("Please use 1 (rows) or 2 (cols) as parameters for 'mar'.")
    }
  } else if (fun == "Normal" | fun == "normal") {
    if (mar == 1) {
      for (a in 1:nrow(X)) {
        wts <- which(is.na(X[a, ])) 
        X[a, wts] <- rnorm(n = length(wts), mean = mean(as.numeric(X[a, ]), na.rm = T), sd = sd(as.numeric(X[a, ]), na.rm = T))
      }
    } else if (mar == 2) {
      for (a in 1:ncol(X)) {
        wts <- which(is.na(X[, a])) 
        X[wts, a] <- rnorm(n = length(wts), mean = mean(as.numeric(X[, a]), na.rm = T), sd = sd(as.numeric(X[, a]), na.rm = T))
      }
    } else {
      stop("Please use 1 (rows) or 2 (cols) as parameters for 'mar'.")
    }
  } else {
    stop("Please use mean, median or normal as parameters for 'fun'.")
  }
  return(X)
}








