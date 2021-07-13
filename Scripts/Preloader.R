message("Preloader.R >>")

custom <- as.logical(toupper(dlg_input(message = "Do you want to use customised variable names for your disease subtype data frames? This is to be preferred when it is unclear, whether automatically generated variable names are not unique. Input TRUE or FALSE.")$res))

message("   Executing cleaning of prism and extraction and cleaning of pancan.")

prism.clean <- row.col.cleaner(prism)
pancan.clean <- row.col.cleaner(prism.extractor(prism))

if (extraction.verifier(pancan.clean) == F) {
  warning("Extraction could not be verified!")
}

message("   Setting up sinonco data frame.")

prism.treat.sinonco <- prism.treat[-unique(c(grep("oncology", prism.treat[, "disease.area"]), grep("malignancy", prism.treat[, "disease.area"]))), ]

message("   Executing dose correlations for the different data sets will take a while. Do not terminate the programme.")

message("   Executing dose correlation for 'prism.clean'.")
prism.doscor <- doscor(prism.clean, doscor = "dfd", perdrug = F, PT = prism.treat)

message("   Executing dose correlation for 'pancan.clean'.")
pancan.doscor <- doscor(pancan.clean, doscor = "dfd", perdrug = F, PT = prism.treat)

message("   Executing dose correlation per drug for 'prism.clean'.")
prism.perdrug <- doscor(prism.clean, doscor = "dfd", perdrug = T, PT = prism.treat)

message("   Executing dose correlation per drug for 'pancna.clean'.")
pancan.perdrug <- doscor(pancan.clean, doscor = "dfd", perdrug = T, PT = prism.treat)

message("   Executing subtype splitting for 'pancan.clean'.")
st.splitter(pancan.clean, custom.sh = custom)

message("   Executing subtype splitting for 'pancan.doscor'.")
st.splitter(pancan.doscor, custom.sh = custom)

message("   Exectuing subtype splitting for 'pancan.perdrug'.")
st.splitter(pancan.perdrug, custom.sh = custom)

rm(custom)