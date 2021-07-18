message("Executing 'Preloader.R'. This will take a while.", domain = "r-pkg")

message("Preloader.R >>")

message("   Executing cleaning of prism and extraction and cleaning of pancan.")

prism.clean <- row.col.cleaner(prism)
pancan.clean <- row.col.cleaner(prism.extractor(prism))

message("   Setting up sinonco data frame.")

prism.treat.sinonco <- prism.treat[-unique(c(grep("oncology", prism.treat[, "disease.area"]), grep("malignancy", prism.treat[, "disease.area"]))), ]

message("   Executing dose correlations for the different data sets will take a while. Do not terminate the programme.")

message("   Executing dose correlation for 'prism.clean'.")
prism.doscor <- doscor(prism.clean, doscor = "dfd", perdrug = F, PT = prism.treat)

message("   Executing dose correlation for 'pancan.clean'.")
pancan.doscor <- row.col.cleaner(prism.extractor(prism.doscor))

message("   Executing dose correlation per drug for 'prism.clean'.")
prism.perdrug <- doscor(prism.clean, doscor = "dfd", perdrug = T, PT = prism.treat)

message("   Executing dose correlation per drug for 'pancan.clean'.")
pancan.perdrug <- row.col.cleaner(prism.extractor(prism.perdrug))

message("   Executing subtype splitting for 'pancan.clean'.")
st.splitter(pancan.clean, custom.sh = F, doscor = 0)

message("   Executing subtype splitting for 'pancan.doscor'.")
st.splitter(pancan.doscor, custom.sh = F, doscor = 1)

message("   Exectuing subtype splitting for 'pancan.perdrug'.")
st.splitter(pancan.perdrug, custom.sh = F, doscor = 2)

message("Finished Executing 'Preloader.R'.", domain = "r-pkg")