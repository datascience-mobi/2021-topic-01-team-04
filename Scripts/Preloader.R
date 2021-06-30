prism.clean <- row.col.cleaner(prism)
pancan.clean <- row.col.cleaner(prism.extractor(prism))

if (extraction.verifier(pancan.clean) == F) {
  print("Extraction could not be verified!")
}

st.splitter(pancan.clean, custom.sh = as.logical(dlg_input(message = "Do you want to use customised variable names for your disease subtype data frames? This is to be preferred when it is unclear, whether automatically generated variable names are not unique. Input TRUE or FALSE.")$res))