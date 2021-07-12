library(svDialogs)

if (as.logical(toupper(dlg_input(message = "Re-initiate?")$res))) {
  cs <- as.logical(toupper(dlg_input(message = "Cleanslate?")$res))
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
    message("Executing 'Presets.R'.", domain = "r-pkg")
    source("Scripts/Presets.R")
    message("Finished Executing 'Presets.R'.", domain = "r-pkg")
  }
  if (fn) {
    message("Executing 'Functions.R'.", domain = "r-pkg")
    source("Scripts/Functions.R")
    message("Finished Executing 'Functions.R'.", domain = "r-pkg")
  }
  if (pl) {
    message("Executing 'Preloader.R'. This will take a while.", domain = "r-pkg")
    source("Scripts/Preloader.R") 
    message("Finished Executing 'Preloader.R'.", domain = "r-pkg")
  }
  rm("ps","fn","pl", "cs", pos = .GlobalEnv)
}