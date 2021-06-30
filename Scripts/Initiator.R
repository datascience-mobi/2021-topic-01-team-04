library(svDialogs)

if (as.logical(dlg_input(message = "Re-initiate?")$res)) {
  cs <- as.logical(dlg_input(message = "Cleanslate?")$res)
  ps <- as.logical(dlg_input(message = "Re-initiate Presets?")$res)
  fn <- as.logical(dlg_input(message = "Re-initiate Functions?")$res)
  pl <- as.logical(dlg_input(message = "Re-initiate Preloader?")$res)
  
  if (cs) {
    rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
  }
  if (ps) {
    source("Scripts/Presets.R")
  }
  if (fn) {
    source("Scripts/Functions.R")
  }
  if (pl) {
    source("Scripts/Preloader.R") 
  }
}