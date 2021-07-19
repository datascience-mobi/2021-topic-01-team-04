ef.prism.perdrug <- as.vector(ef.dr.identifier(prism.perdrug, th, F, sinonco = F))

ef.pancan.perdrug <- as.vector(ef.dr.identifier(pancan.perdrug, th, F, sinonco = F))

ef.pancan.1.perdrug <- as.vector(ef.dr.identifier(get(st.split.vars.perdrug[1, 1]), th, F, sinonco = F))
ef.pancan.2.perdrug <- as.vector(ef.dr.identifier(get(st.split.vars.perdrug[2, 1]), th, F, sinonco = F))
ef.pancan.3.perdrug <- as.vector(ef.dr.identifier(get(st.split.vars.perdrug[3, 1]), th, F, sinonco = F))
ef.pancan.4.perdrug <- as.vector(ef.dr.identifier(get(st.split.vars.perdrug[4, 1]), th, F, sinonco = F))

rm(th)