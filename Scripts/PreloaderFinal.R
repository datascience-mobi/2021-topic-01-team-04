prism.clean <- row.col.cleaner(prism)
pancan.clean <- row.col.cleaner(prism.extractor(prism))

prism.treat.sinonco <- prism.treat[-unique(c(grep("oncology", prism.treat[, "disease.area"]), grep("malignancy", prism.treat[, "disease.area"]))), ]

prism.doscor <- doscor(prism.clean, doscor = "dfd", perdrug = F, PT = prism.treat)

pancan.doscor <- row.col.cleaner(prism.extractor(prism.doscor))

prism.perdrug <- doscor(prism.clean, doscor = "dfd", perdrug = T, PT = prism.treat)

pancan.perdrug <- row.col.cleaner(prism.extractor(prism.perdrug))

st.splitter(pancan.clean, custom.sh = F, doscor = 0)

st.splitter(pancan.doscor, custom.sh = F, doscor = 1)

st.splitter(pancan.perdrug, custom.sh = F, doscor = 2)
