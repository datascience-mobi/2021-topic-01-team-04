prism.clean <- row.col.cleaner(prism)
pancan.clean <- row.col.cleaner(prism.extractor(prism))

if (extraction.verifier(pancan.clean) == F) {
  print("Extraction could not be verified!")
}

st.splitter(pancan.clean)