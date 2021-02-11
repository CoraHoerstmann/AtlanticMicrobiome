##subset the ASV data

provinces <-function(ASVCount.hellinger, meta.z){
hellinger <- as.data.frame(ASVCount.hellinger)

#Falkland
meta_FKLD <<- meta.z%>%dplyr::filter(province == "FKLD")
sites <- meta_FKLD$Site
ASVCount.hellinger.FKLD <- hellinger%>%dplyr::select(meta_FKLD$Site) #one would need to check with metadata whether it fits
ASVCount.hellinger.FKLD <<- ASVCount.hellinger.FKLD[rowSums(ASVCount.hellinger.FKLD)>0,] #remove absent ASVs

#BRAZ
BRAZ <- c("BRAZ","BRAZ-SATL-COLD")
meta_BRAZ <<- meta.z%>%dplyr::filter(province %in% BRAZ)
ASVCount.hellinger.BRAZ <- hellinger%>%dplyr::select(meta_BRAZ$Site) #one would need to check with metadata whether it fits
ASVCount.hellinger.BRAZ <<- ASVCount.hellinger.BRAZ[rowSums(ASVCount.hellinger.BRAZ)>0,] #remove absent ASVs
rm(BRAZ)

#South Gyre
STG <- c("SATL-COLD")
meta_SATL_cold <<- meta.z%>%dplyr::filter(province %in% STG)
ASVCount.hellinger.SATL_cold <- hellinger%>%dplyr::select(meta_SATL_cold$Site) #one would need to check with metadata whether it fits
ASVCount.hellinger.SATL_cold <<- ASVCount.hellinger.SATL_cold[rowSums(ASVCount.hellinger.SATL_cold)>0,] #remove absent ASVs
rm(STG)

STG_2 <- c("SATL-HOT")
meta_SATL_hot <<- meta.z%>%dplyr::filter(province %in% STG_2)
ASVCount.hellinger.SATL_hot <- hellinger%>%dplyr::select(meta_SATL_hot$Site) #one would need to check with metadata whether it fits
ASVCount.hellinger.SATL_hot <<- ASVCount.hellinger.SATL_hot[rowSums(ASVCount.hellinger.SATL_hot)>0,] #remove absent ASVs
rm(STG_2)
#Equator
WTRA <- c("WTRA-SOUTH","WTRA")
meta_WTRA <<- meta.z%>%dplyr::filter(province %in% WTRA)
ASVCount.hellinger.WTRA <- hellinger%>%dplyr::select(meta_WTRA$Site) #one would need to check with metadata whether it fits
ASVCount.hellinger.WTRA <<- ASVCount.hellinger.WTRA[rowSums(ASVCount.hellinger.WTRA)>0,] #remove absent ASVs
rm(WTRA)

#NATR
meta_NATR <<- meta.z%>%dplyr::filter(province == "NATR")
ASVCount.hellinger.NATR <- hellinger%>%dplyr::select(meta_NATR$Site)#one would need to check with metadata whether it fits
ASVCount.hellinger.NATR <<- ASVCount.hellinger.NATR[rowSums(ASVCount.hellinger.NATR)>0,] #remove absent ASVs

#NAST-E
meta_NAST <<- meta.z%>%dplyr::filter(province == "NAST-E")
ASVCount.hellinger.NAST <- hellinger%>%dplyr::select(meta_NAST$Site) #one would need to check with metadata whether it fits
ASVCount.hellinger.NAST <<- ASVCount.hellinger.NAST[rowSums(ASVCount.hellinger.NAST)>0,] #remove absent ASVs

#Canary
meta_CNRY <<- meta.z%>%dplyr::filter(province == "CNRY")
ASVCount.hellinger.CNRY <- hellinger%>%dplyr::select(meta_CNRY$Site) #one would need to check with metadata whether it fits
ASVCount.hellinger.CNRY <<- ASVCount.hellinger.CNRY[rowSums(ASVCount.hellinger.CNRY)>0,] #remove absent ASVs

#NADR
meta_NADR <<- meta.z%>%dplyr::filter(province == "NADR")
ASVCount.hellinger.NADR <- hellinger%>%dplyr::select(meta_NADR$Site) #one would need to check with metadata whether it fits
ASVCount.hellinger.NADR <<- ASVCount.hellinger.NADR[rowSums(ASVCount.hellinger.NADR)>0,] #remove absent ASVs
}