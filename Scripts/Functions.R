# global object remover

globalobjectremover <- function(incfuncs = F) {
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