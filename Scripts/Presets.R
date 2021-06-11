load("../Data/prism_datasets.rda", envir = .GlobalEnv)
load("../Data/cellline_datasets.rda", envir = .GlobalEnv)

rownames(prism.cl) <- prism.cl[, "DepMap_ID"]