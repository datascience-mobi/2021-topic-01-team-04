clean <- row.col.cleaner(prism)
prism.clean <- as.data.frame(clean[1])
prism.fin.clean <- as.data.frame(clean[2])

clean <- row.col.cleaner(prism.extractor(prism))
pancan.clean <- as.data.frame(clean[1])
pancan.fin.clean <- as.data.frame(clean[2])

if (extraction.verifier(pancan.clean) == F | extraction.verifier(pancan.fin.clean) == F) {
  print("Extraction could not be verified!")
}

rm(clean)

st.splitter(pancan.clean)