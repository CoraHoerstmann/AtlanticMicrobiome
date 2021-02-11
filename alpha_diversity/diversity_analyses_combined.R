#Diversities calculations

#load additional packages

suppressPackageStartupMessages(require("dplyr"))

div16S <-diversity_iNEXT(ASVCount_16S.tax)
div18S <-diversity_iNEXT(ASVCount_18S.tax)
div_auto_euk <- diversity_iNEXT(ASVCount_18S.tax.auto)
div_mixo <- diversity_iNEXT(ASVCount_mixo)
div_het_euk <- diversity_iNEXT(ASVCount_18S.tax.het)
div_auto_prok <- diversity_iNEXT(ASVCount_16S.tax.auto)
div_het_prok <- diversity_iNEXT(ASVCount_16S.tax.het)

#attach to metadata

div18S$Site <- gsub("_18_",".",div18S$Site)
div18S$Site <- gsub("_.*","", div18S$Site)
div16S$Site <- gsub("_16_",".",div16S$Site)
div16S$Site <- gsub("_.*","", div16S$Site)

meta.18S.div <- left_join(meta.18S, div18S, by = "Site")
meta.16S.div <- right_join(meta.16S, div16S, by = "Site")

meta_auto_euk <- right_join(meta_auto_euk, div_auto_euk, by = "Site")
meta_auto_cyano <- right_join(meta_auto_cyano, div_auto_prok, by = "Site")
meta_mixo <- right_join(meta_mixo, div_mixo, by = "Site")
meta_het_euk <- right_join(meta_het_euk, div_het_euk, by = "Site")
meta_het_prok <- right_join(meta_het_prok, div_het_prok, by = "Site")


meta_auto_euk.1 <- meta_auto_euk%>%
  dplyr::rename(Richness.auto.euk = Richness)%>%
  dplyr::rename(Shannon.auto.euk = Shannon)%>%
  dplyr::rename(Simpson.auto.euk = Simpson)

meta_auto_cyano.1 <- meta_auto_cyano%>%
  dplyr::rename(Richness.auto.cyano = Richness)%>%
  dplyr::rename(Shannon.auto.cynao = Shannon)%>%
  dplyr::rename(Simpson.auto.cyano = Simpson)

meta_mixo.1 <- meta_mixo%>%
  dplyr::rename(Richness.mixo = Richness)%>%
  dplyr::rename(Shannon.mixo = Shannon)%>%
  dplyr::rename(Simpson.mixo = Simpson)

meta_het_euk.1 <- meta_het_euk%>%
  dplyr::rename(Richness.het.euk = Richness)%>%
  dplyr::rename(Shannon.het.euk = Shannon)%>%
  dplyr::rename(Simpson.het.euk = Simpson)

meta_het_prok.1 <- meta_het_prok%>%
  dplyr::rename(Richness.het.prok = Richness)%>%
  dplyr::rename(Shannon.het.prok = Shannon)%>%
  dplyr::rename(Simpson.het.prok = Simpson)

##join the table

meta_functions <- dplyr::inner_join(meta_auto_euk.1, meta_auto_cyano.1[c(32,40:42)], by = "Site")
meta_functions <- dplyr::inner_join(meta_functions, meta_mixo.1[c(3,40:42)], by = "Site")
meta_functions <- dplyr::inner_join(meta_functions, meta_het_euk.1[c(3,40:42)], by = "Site")
meta_functions <- dplyr::inner_join(meta_functions, meta_het_prok.1[c(32,40:42)], by = "Site")

#clean up
rm(meta_auto_cyano.1, meta_auto_euk.1, meta_het_euk.1, meta_het_prok.1, meta_mixo.1)

detach("package:dplyr", unload=TRUE)

