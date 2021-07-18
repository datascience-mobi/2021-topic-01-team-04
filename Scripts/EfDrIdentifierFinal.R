message("Executing 'EfDrIdentifier.R'.")

message("EfDrIdentifier.R >>")

message("   Assigning efficacious drugs in regard to all cell lines.")
ef.prism.perdrug <- as.vector(ef.dr.identifier(prism.perdrug, th, F, sinonco = F))

message("   Assigning efficacious drugs in regard to pancreatic cancer cell lines.")
ef.pancan.perdrug <- as.vector(ef.dr.identifier(pancan.perdrug, th, F, sinonco = F))

message(paste("   Assigning efficacious drugs in regard to '", st.split.vars.doscor[1, 2], "' cell lines.", sep = ""))
ef.pancan.1.perdrug <- as.vector(ef.dr.identifier(get(st.split.vars.perdrug[1, 1]), th, F, sinonco = F))

message(paste("   Assigning efficacious drugs in regard to '", st.split.vars.doscor[2, 2], "' cell lines.", sep = ""))
ef.pancan.2.perdrug <- as.vector(ef.dr.identifier(get(st.split.vars.perdrug[2, 1]), th, F, sinonco = F))

message(paste("   Assigning efficacious drugs in regard to '", st.split.vars.doscor[3, 2], "' cell lines.", sep = ""))
ef.pancan.3.perdrug <- as.vector(ef.dr.identifier(get(st.split.vars.perdrug[3, 1]), th, F, sinonco = F))

message(paste("   Assigning efficacious drugs in regard to '", st.split.vars.doscor[4, 2], "' cell lines.", sep = ""))
ef.pancan.4.perdrug <- as.vector(ef.dr.identifier(get(st.split.vars.perdrug[4, 1]), th, F, sinonco = F))

rm(th)

message("Finished executing 'EfDrIdentifier.R'.")