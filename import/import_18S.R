
setwd("C:/Users/choerstm/Documents/core_PS113/Genomics/amplicon_data")

# Import all data

##metadata

meta.18 <- read.csv("./Metadata/Metadata_genomics_18S_PP.csv",
                 header=TRUE, sep= ";")
#remove Station 193 (see ASV table)
meta.18 <- meta.18[!grepl("PS113_18_S193_S114_", meta.18$X),]
meta_all.18 <- read.csv("./Metadata/Metadata_genomics_all_all_cu.csv",
                     header=TRUE, sep= ",")

meta.18$Site <- as.character(meta.18$X)
meta.18$Station <- as.character(meta.18$Station)
meta_all.18$Station <- as.character(meta_all.18$Station)

#sort out the ADCP
meta.18S <- left_join(meta.18[,c(2,19:20)], meta_all.18[,c(2:16,24:38,40)], by = "Station")
meta.18S <- left_join(meta.18S,flow.analysis[c(1,42,47:52)], by = "Station") #add the cell data

meta.18S$Site <-  gsub("_$","", meta.18S$Site)
rownames(meta.18S) <- meta.18S$Site

rm(meta_all.18, meta.18)

# I made an ASV translation key to not use the sequence but rather short names. 
ASVCount_18S <- read.csv("./18S/20200518_reanalysis/PS113_18S_ASV_abundance_table.txt",
                      sep = "\t")


#remove station 1 (no permit)

ASVCount_18S$PS113_18_S1_S1 <- NULL

##remove Station 193, because its  col sum of abundance table is only 610

ASVCount_18S$PS113_18_S193_S114 <- NULL

asv_translation <- read.csv("./18S/20200518_reanalysis/PS113_ASV_translation.txt", 
                            header= T, sep= "\t")


taxonomy_18S <- read.csv("./18S/20200518_reanalysis/PS113_18S_taxonomy_qiime.csv", 
                header= TRUE, sep= ";", row.names = 1)

##ASV table

# rename, format and assign the ASVs using the translation key

ASVCount_18S$Sequence <- as.character(ASVCount_18S$X)
asv_translation$Sequence <- as.character(asv_translation$Sequence)

##change the Sequences into ASV names
ASVCount_18S <- left_join(ASVCount_18S, asv_translation, by="Sequence")
rownames(ASVCount_18S) <- ASVCount_18S$ASV
ASVCount_18S$Sequence <- NULL
ASVCount_18S$X <- NULL
ASVCount_18S$ASV <- NULL

rm(asv_translation)
### Taxonomy table

taxonomy_18S$rownames <- rownames(taxonomy_18S) 

#### filter out the metazoa

taxonomy_18S <-taxonomy_18S%>%filter(phylum!= "Metazoa") 

#we removed 608 ASVs annotated as Metazoans

#reduce abundance table to match with the taxonomy table

sequence_list <- as.character(taxonomy_18S$rownames)

ASVCount_18S$ASV <- rownames(ASVCount_18S)
ASVCount_18S.tax <- ASVCount_18S%>%filter(rownames(ASVCount_18S) %in% sequence_list) 
ASVCount_18S$ASV <- NULL
ASVCount_18S.tax$ASV <- NULL
taxonomy_18S <- taxonomy_18S%>%filter(rownames %in% rownames(ASVCount_18S.tax))


meta.18S$Site <- gsub("_18_",".",meta.18S$Site)
meta.18S$Site <- gsub("_.*","", meta.18S$Site)


##########clean up!
rm(sequence_list)
rm(ASVCount_18S)

