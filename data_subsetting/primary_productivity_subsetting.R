
#reduce all datasets to the samples where we have PP measurements

## only autotrophs

#Eukaryotes

discard <- apply(meta_auto_euk, 1, function(x) any(is.na(x)))
meta_auto_euk.PP <- meta_auto_euk[!discard,]
rownames(meta_auto_euk.PP) <- meta_auto_euk.PP$Site
#remove the PP data from the other metadata

meta_auto_euk <- meta_auto_euk[,!names(meta_auto_euk) =="PPAVG"]

# calculate specific PP

meta_auto_euk.PP$specific_PP <- meta_auto_euk.PP$PPAVG/meta_auto_euk.PP$chlorophyll

##abundance
ASVCount_auto_euk.PP <- ASVCount_18S.tax.auto%>%dplyr::select(rownames(meta_auto_euk.PP))
ASVCount_auto_euk.PP <- ASVCount_auto_euk.PP[rowSums(ASVCount_auto_euk.PP)>0,] #remove ASVs which are not abundant

ASVCount_auto_euk.PP.hellinger <- ASVCount_18S.tax.auto.hellinger%>%dplyr::select(rownames(meta_auto_euk.PP))
ASVCount_auto_euk.PP.hellinger <- ASVCount_auto_euk.PP.hellinger[rowSums(ASVCount_auto_euk.PP.hellinger)>0,] #remove ASVs which are not abundant

ASVCount_auto_euk.PP.clr <- ASVCount_18S.tax.auto.clr%>%dplyr::select(rownames(meta_auto_euk.PP))
ASVCount_auto_euk.PP.clr <- ASVCount_auto_euk.PP.clr[rowSums(ASVCount_auto_euk.PP.clr)>0,] #remove ASVs which are not abundant
##taxonomy

taxonomy_auto_euk.PP <- taxonomy_auto_euk%>%dplyr::filter(names %in% rownames(ASVCount_auto_euk.PP))

#Prokaryotes

discard <- apply(meta_auto_cyano, 1, function(x) any(is.na(x)))
meta_auto_cyano.PP <- meta_auto_cyano[!discard,]
rownames(meta_auto_cyano.PP) <- meta_auto_cyano.PP$Site
#remove the PP data from the other metadata
meta_auto_cyano <- meta_auto_cyano[,!names(meta_auto_cyano) == "PPAVG"]

# calculate specific PP

meta_auto_cyano.PP$specific_PP <- meta_auto_cyano.PP$PPAVG/meta_auto_cyano.PP$chlorophyll


##abundance
ASVCount_auto_cyano.PP <- ASVCount_16S.tax.auto%>%dplyr::select(rownames(meta_auto_cyano.PP))
ASVCount_auto_cyano.PP <- ASVCount_auto_cyano.PP[rowSums(ASVCount_auto_cyano.PP)>0,] #remove ASVs which are not abundant

ASVCount_auto_cyano.PP.hellinger <- ASVCount_16S.tax.auto.hellinger%>%dplyr::select(rownames(meta_auto_cyano.PP))
ASVCount_auto_cyano.PP.hellinger <- ASVCount_auto_cyano.PP.hellinger[rowSums(ASVCount_auto_cyano.PP.hellinger)>0,] #remove ASVs which are not abundant

ASVCount_auto_cyano.PP.clr <- ASVCount_16S.tax.auto.clr%>%dplyr::select(rownames(meta_auto_cyano.PP))
ASVCount_auto_cyano.PP.clr <- ASVCount_auto_cyano.PP.clr[rowSums(ASVCount_auto_cyano.PP.clr)>0,] #remove ASVs which are not abundant
##taxonomy

taxonomy_auto_cyano.PP <- taxonomy_Cyanobacteria%>%dplyr::filter(rownames %in% rownames(ASVCount_auto_cyano.PP))



## only mixotrophs

discard <- apply(meta_mixo, 1, function(x) any(is.na(x)))
meta_mixo.PP <- meta_mixo[!discard,]
rownames(meta_mixo.PP) <- meta_mixo.PP$Site
#remove the PP data from the other metadata

meta_mixo <- meta_mixo[,!names(meta_mixo) == "PPAVG"]

meta_mixo.PP$specific_PP <- meta_mixo.PP$PPAVG/meta_mixo.PP$chlorophyll

##abundance
ASVCount_mixo.PP <- ASVCount_mixo%>%dplyr::select(rownames(meta_mixo.PP))
ASVCount_mixo.PP <- ASVCount_mixo.PP[rowSums(ASVCount_mixo.PP)>0,] #remove ASVs which are not abundant

ASVCount_mixo.PP.hellinger <- ASVCount_mixo.hellinger%>%dplyr::select(rownames(meta_mixo.PP))
ASVCount_mixo.PP.hellinger <- ASVCount_mixo.PP.hellinger[rowSums(ASVCount_mixo.PP.hellinger)>0,] #remove ASVs which are not abundant

ASVCount_mixo.PP.clr <- ASVCount_mixo.clr%>%dplyr::select(rownames(meta_mixo.PP))
ASVCount_mixo.PP.clr <- ASVCount_mixo.PP.clr[rowSums(ASVCount_mixo.PP.clr)>0,] #remove ASVs which are not abundant
##taxonomy
taxonomy_mixo$rownames <- rownames(taxonomy_mixo)
taxonomy_mixo.PP <- taxonomy_mixo%>%dplyr::filter(names %in% rownames(ASVCount_mixo.PP))


## only heterotrophs

discard <- apply(meta_het_euk, 1, function(x) any(is.na(x)))
meta_het_euk.PP <- meta_het_euk[!discard,]
rownames(meta_het_euk.PP) <- meta_het_euk.PP$Site
#remove the PP data from the other metadata

meta_het_euk <- meta_het_euk[,!names(meta_het_euk) == "PPAVG"]

meta_het_euk.PP$specific_PP <- meta_het_euk.PP$PPAVG/meta_het_euk.PP$chlorophyll

##abundance
ASVCount_het_euk.PP <- ASVCount_18S.tax.het%>%dplyr::select(rownames(meta_het_euk.PP))
ASVCount_het_euk.PP <- ASVCount_het_euk.PP[rowSums(ASVCount_het_euk.PP)>0,] #remove ASVs which are not abundant

ASVCount_het_euk.PP.hellinger <- ASVCount_18S.tax.het.hellinger%>%dplyr::select(rownames(meta_het_euk.PP))
ASVCount_het_euk.PP.hellinger <- ASVCount_het_euk.PP.hellinger[rowSums(ASVCount_het_euk.PP.hellinger)>0,] #remove ASVs which are not abundant

ASVCount_het_euk.PP.clr <- ASVCount_18S.tax.het.clr%>%dplyr::select(rownames(meta_het_euk.PP))
ASVCount_het_euk.PP.clr <- ASVCount_het_euk.PP.clr[rowSums(ASVCount_het_euk.PP.clr)>0,] #remove ASVs which are not abundant
##taxonomy

taxonomy_het_euk.PP <- taxonomy_het_euk%>%dplyr::filter(rownames %in% rownames(ASVCount_het_euk.PP))

#Prokaryotes

discard <- apply(meta_het_prok, 1, function(x) any(is.na(x)))
meta_het_prok.PP <- meta_het_prok[!discard,]
rownames(meta_het_prok.PP) <- meta_het_prok.PP$Site
#remove the PP data from the other metadata

meta_het_prok <- meta_het_prok[,!names(meta_het_prok) == "PPAVG"]

meta_het_prok.PP$specific_PP <- meta_het_prok.PP$PPAVG/meta_het_prok.PP$chlorophyll

##abundance
ASVCount_het_prok.PP <- ASVCount_16S.tax.het%>%dplyr::select(rownames(meta_het_prok.PP))
ASVCount_het_prok.PP <- ASVCount_het_prok.PP[rowSums(ASVCount_het_prok.PP)>0,] #remove ASVs which are not abundant

ASVCount_het_prok.PP.hellinger <- ASVCount_16S.tax.het.hellinger%>%dplyr::select(rownames(meta_het_prok.PP))
ASVCount_het_prok.PP.hellinger <- ASVCount_het_prok.PP.hellinger[rowSums(ASVCount_het_prok.PP.hellinger)>0,] #remove ASVs which are not abundant

ASVCount_het_prok.PP.clr <- ASVCount_16S.tax.het.clr%>%dplyr::select(rownames(meta_het_prok.PP))
ASVCount_het_prok.PP.clr <- ASVCount_het_prok.PP.clr[rowSums(ASVCount_het_prok.PP.clr)>0,] #remove ASVs which are not abundant
##taxonomy

taxonomy_het_prok.PP <- taxonomy_het_prok%>%dplyr::filter(rownames %in% rownames(ASVCount_het_prok.PP))


##meta all functions

discard <- apply(meta_functions, 1, function(x) any(is.na(x)))
meta_functions.PP <- meta_functions[!discard,]
rownames(meta_functions.PP) <- meta_functions.PP$Site


