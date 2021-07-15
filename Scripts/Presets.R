message("Executing 'Presets.R'.", domain = "r-pkg")

message("Presets.R >>")

message("   Loading in all relevant libraries.")
library(knitr)
library(tibble)
library(ggplot2)
library(dplyr)
library(compiler)
library(svDialogs)
library(cluster)

message("   Loading in all relevant data sets.")
load("Data/prism_datasets.rda", envir = .GlobalEnv)
load("Data/cellline_datasets.rda", envir = .GlobalEnv)

message("   Smoothing rownames of 'prism.cl' to match the 'DepMap_ID' in congruence with other data sets for easier handling.")
rownames(prism.cl) <- prism.cl[, "DepMap_ID"]

message("Finished Executing 'Presets.R'.", domain = "r-pkg")