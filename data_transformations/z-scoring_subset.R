
#autos
meta_auto_euk.z <- meta_auto_euk
meta_auto_euk.z[z_score] <- lapply(meta_auto_euk.z[z_score], function(x) {y <-scale(x, center = TRUE, scale = TRUE)})

meta_auto_euk.PP.z <- meta_auto_euk.PP
meta_auto_euk.PP.z[z_score_PP] <- lapply(meta_auto_euk.PP.z[z_score_PP], function(x) {y <-scale(x, center = TRUE, scale = TRUE)})

meta_auto_cyano.z <- meta_auto_cyano
meta_auto_cyano.z[z_score] <- lapply(meta_auto_cyano.z[z_score], function(x) {y <-scale(x, center = TRUE, scale = TRUE)})

meta_auto_cyano.PP.z <- meta_auto_cyano.PP
meta_auto_cyano.PP.z[z_score_PP] <- lapply(meta_auto_cyano.PP.z[z_score_PP], function(x) {y <-scale(x, center = TRUE, scale = TRUE)})


#mixos
meta_mixo.z <- meta_mixo
meta_mixo.z[z_score] <- lapply(meta_mixo.z[z_score], function(x) {y <-scale(x, center = TRUE, scale = TRUE)})

meta_mixo.PP.z <- meta_mixo.PP
meta_mixo.PP.z[z_score_PP] <- lapply(meta_mixo.PP.z[z_score_PP], function(x) {y <-scale(x, center = TRUE, scale = TRUE)})

#het
meta_het_euk.z <- meta_het_euk
meta_het_euk.z[z_score] <- lapply(meta_het_euk.z[z_score], function(x) {y <-scale(x, center = TRUE, scale = TRUE)})

meta_het_euk.PP.z <- meta_het_euk.PP
meta_het_euk.PP.z[z_score_PP] <- lapply(meta_het_euk.PP.z[z_score_PP], function(x) {y <-scale(x, center = TRUE, scale = TRUE)})

meta_het_prok.z <- meta_het_prok
meta_het_prok.z[z_score] <- lapply(meta_het_prok.z[z_score], function(x) {y <-scale(x, center = TRUE, scale = TRUE)})

meta_het_prok.PP.z <- meta_het_prok.PP
meta_het_prok.PP.z[z_score_PP] <- lapply(meta_het_prok.PP.z[z_score_PP], function(x) {y <-scale(x, center = TRUE, scale = TRUE)})


z_score_all <- c("salinity", "SST", "PO4_umol.l", "Si_umol.l", "NO2_umol.l", "NO3_umol.l", "oxygen.umol.l.", "Total.N..nmol.L.", "Total.C..nmol.L.", "C.N.Molar", "Distance_coast.km.","distance_to_province_boundary.km.", "chlorophyll", "Shannon.auto.euk", "Shannon.auto.cynao", "Shannon.mixo", "Shannon.het.euk", "Shannon.het.prok")
z_score_PP_all <- c("salinity", "SST", "PO4_umol.l", "Si_umol.l", "NO2_umol.l", "NO3_umol.l", "oxygen.umol.l.", "Total.N..nmol.L.", "Total.C..nmol.L.", "C.N.Molar", "Distance_coast.km.",
                    "distance_to_province_boundary.km.", "PPAVG", "chlorophyll", "Shannon.auto.euk", "Shannon.auto.cynao", "Shannon.mixo", "Shannon.het.euk", "Shannon.het.prok")
#all functions
meta_functions.z <- meta_functions
meta_functions.z[z_score_all] <- lapply(meta_functions.z[z_score_all], function(x) {y <-scale(x, center = TRUE, scale = TRUE)})

meta_functions.PP.z <- meta_functions.PP
meta_functions.PP.z[z_score_PP_all] <- lapply(meta_functions.PP.z[z_score_PP_all], function(x) {y <-scale(x, center = TRUE, scale = TRUE)})

rownames(meta_functions.z) <-meta_functions.z$Site