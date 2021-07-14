message("Executing 'EfDrIdentifier.R'.")

while (TRUE) {
  if (grepl(",", th) == T) {break
  } else if (th == "standard" | th == " " | th == "st") {th <- "q.001,q.005"; break
  } else if (!exists("th")) {th <- dlg_input(message = "Thresholding to be used for doscor and perdrug? Seperate them by comma.")$res 
  } else if (grepl(",", th) == F) {th <- dlg_input(message = "Format unusable. Seperate them by comma.")$res}
}


message("EfDrIdentifier.R >>")

message("   Assigning efficacious drugs in regard to all cell lines.")
ef.prism.doscor <- droplevels(ef.dr.identifier(prism.doscor, trimws(unlist(strsplit(th, ","))[1]), F, sinonco = F))
ef.prism.perdrug <- droplevels(ef.dr.identifier(prism.perdrug, trimws(unlist(strsplit(th, ","))[2]), F, sinonco = F))

message("   Assigning efficacious drugs in regard to pancreatic cancer cell lines.")
ef.pancan.doscor <- droplevels(ef.dr.identifier(pancan.doscor, trimws(unlist(strsplit(th, ","))[1]), F, sinonco = F))
ef.pancan.perdrug <- droplevels(ef.dr.identifier(pancan.perdrug, trimws(unlist(strsplit(th, ","))[2]), F, sinonco = F))

message(paste("   Assigning efficacious drugs in regard to '", st.split.vars.doscor[1, 2], "' cell lines.", sep = ""))
ef.pancan.1.doscor <- droplevels(ef.dr.identifier(get(st.split.vars.doscor[1, 1]), trimws(unlist(strsplit(th, ","))[1]), F, sinonco = F))
ef.pancan.1.perdrug <- droplevels(ef.dr.identifier(get(st.split.vars.perdrug[1, 1]), trimws(unlist(strsplit(th, ","))[2]), F, sinonco = F))

message(paste("   Assigning efficacious drugs in regard to '", st.split.vars.doscor[2, 2], "' cell lines.", sep = ""))
ef.pancan.2.doscor <- droplevels(ef.dr.identifier(get(st.split.vars.doscor[2, 1]), trimws(unlist(strsplit(th, ","))[1]), F, sinonco = F))
ef.pancan.2.perdrug <- droplevels(ef.dr.identifier(get(st.split.vars.perdrug[2, 1]), trimws(unlist(strsplit(th, ","))[2]), F, sinonco = F))

message(paste("   Assigning efficacious drugs in regard to '", st.split.vars.doscor[3, 2], "' cell lines.", sep = ""))
ef.pancan.3.doscor <- droplevels(ef.dr.identifier(get(st.split.vars.doscor[3, 1]), trimws(unlist(strsplit(th, ","))[1]), F, sinonco = F))
ef.pancan.3.perdrug <- droplevels(ef.dr.identifier(get(st.split.vars.perdrug[3, 1]), trimws(unlist(strsplit(th, ","))[2]), F, sinonco = F))

message(paste("   Assigning efficacious drugs in regard to '", st.split.vars.doscor[4, 2], "' cell lines.", sep = ""))
ef.pancan.4.doscor <- droplevels(ef.dr.identifier(get(st.split.vars.doscor[4, 1]), trimws(unlist(strsplit(th, ","))[1]), F, sinonco = F))
ef.pancan.4.perdrug <- droplevels(ef.dr.identifier(get(st.split.vars.perdrug[4, 1]), trimws(unlist(strsplit(th, ","))[2]), F, sinonco = F))

rm(th)

message("Finished executing 'EfDrIdentifier.R'.")