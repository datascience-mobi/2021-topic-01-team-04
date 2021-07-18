library(svDialogs)
if (!exists("ri")) ri <- as.logical(toupper(dlg_input(message = "Re-initiate?")$res))
if(length(ri != 0)) {
  if (ri == T) {
    if (!exists("cs")) cs <- as.logical(toupper(dlg_input(message = "Cleanslate?")$res))
    if (cs) {
      message("Executing Protocol 'Clean Slate'.", domain = "r-pkg")
      rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
      ps <- T; fn <- T; pl <- T; cs <- F
    } else if (!cs) {
      ps <- as.logical(toupper(dlg_input(message = "Re-initiate Presets.R?")$res))
      fn <- as.logical(toupper(dlg_input(message = "Re-initiate Functions.R?")$res))
      pl <- as.logical(toupper(dlg_input(message = "Re-initiate Preloader.R?")$res))
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
    rm("ps","fn","pl", "cs", pos = .GlobalEnv)
  }
}