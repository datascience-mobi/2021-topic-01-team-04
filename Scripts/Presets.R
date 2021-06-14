library(knitr)
library(tibble)
library(ggplot2)

load("Data/prism_datasets.rda", envir = .GlobalEnv)
load("Data/cellline_datasets.rda", envir = .GlobalEnv)

rownames(prism.cl) <- prism.cl[, "DepMap_ID"]