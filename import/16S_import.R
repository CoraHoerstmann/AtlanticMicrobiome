
#setwd
setwd("C:/Users/choerstm/Documents/core_PS113/Genomics/amplicon_data/")

## Import metadata
meta.16 <- read.csv("./Metadata/Metadata_genomics_16S_PP.csv",
                 header=TRUE, sep= ";")
meta.16 <- meta.16[-1,]
meta.16 <- meta.16[!grepl("PS113_16_S133_S34_", meta.16$Station),] # these two stations are unfortunately not in the tax file
meta.16 <- meta.16[!grepl("PS113_16_S196_S99_", meta.16$Station),]
meta.16 <- meta.16[!grepl("PS113_16_S165_S116_", meta.16$Station),] # these insufficient Richness
meta.16 <- meta.16[!grepl("PS113_16_S179_S117_", meta.16$Station),]
meta.16 <- meta.16[!grepl("PS113_16_S191_S118_", meta.16$Station),]
meta.16 <- meta.16[!grepl("PS113_16_S200_S119_", meta.16$Station),]

#because we have different stations sometimes samples for 16S and 18S we have specific metadata files and a general one.

meta_all.16 <- read.csv("./Metadata/Metadata_genomics_all_all_cu.csv",
                     header=TRUE, sep= ";")
meta.all.meta <- meta_all.16
meta.16$Site <- as.character(meta.16$Station)
meta.16$Station <- NULL
meta.16$Station <- as.character(meta.16$Place)
meta_all.16$Station <- as.character(meta_all.16$Station)

#sort out the ADCP
meta.16S <- right_join(meta_all.16[,c(2:15,23:39)],meta.16[,c(18:20)], by = "Station")
meta.16S <- left_join(meta.16S,flow.analysis[c(1,42,47:52)], by = "Station")
rm(meta_all.16, meta.16)

#remove the DO data from file

meta.16S$DO_Log.VDVBW.speed_watertrack_ahead <- NULL

#match the rownames to the abundance table
meta.16S$Site <-  gsub("_$","", meta.16S$Site)
rownames(meta.16S) <- meta.16S$Site

#lapply(meta.16S, class) %>% unlist()


#Place    water.filtered.sum              province 
#"factor"             "numeric"              "factor" 
#latitude Longitude..degrees.W.              Datetime 
#"numeric"             "numeric"              "factor" 
#salinity                   SST            PO4_umol.l 
#"numeric"             "numeric"             "numeric" 
#Si_umol.l            NO2_umol.l            NO3_umol.l 
#"numeric"             "numeric"             "numeric" 
#NH4_umol.l                   PP1                   PP2 
#"numeric"             "numeric"             "numeric" 
#PP3                 PPAVG               GEBCO14 
#"numeric"             "numeric"             "numeric" 
#all_islands           big_islands            no_islands 
#"numeric"             "numeric"             "numeric" 
#adcp_avg_e            adcp_avg_n            adcp_horiz 
#"numeric"             "numeric"             "numeric" 
#adcp_direc            adcp_up_ve 
#"numeric"             "numeric" 


##Import ASV abundance table

# I made an ASV translation key to not use the sequence but rather short names. 
ASVCount_16S <- read.csv("16S/2020518_amplicon_reanalysis/PS113_16S_ASV_abundance_table.txt", header = TRUE, sep = "\t")

####delete now the missing stations:( ##this needs to be reverted when we find something fabulous

# sample1
ASVCount_16S$PS113_16_S1_S1 <-NULL
## remove stations with insufficient ASV counts
ASVCount_16S$PS113_16_S133_S34 <-NULL
ASVCount_16S$PS113_16_S196_S99 <-NULL

###remove stations with insufficient richness
ASVCount_16S$PS113_16_S165_S116 <- NULL
ASVCount_16S$PS113_16_S179_S117 <- NULL
ASVCount_16S$PS113_16_S191_S118 <- NULL
ASVCount_16S$PS113_16_S200_S119 <- NULL

asv_translation <- read.csv("16S/2020518_amplicon_reanalysis/Sequence_ASV_key.txt", header= T, sep= "\t")

# rename, format and assign the ASVs using the translation key

ASVCount_16S$Sequence <- as.character(ASVCount_16S$X)
asv_translation$Sequence <- as.character(asv_translation$Sequence)

##change the Sequences into ASV names
ASVCount_16S <- right_join(ASVCount_16S, asv_translation, by="Sequence")
rownames(ASVCount_16S) <- ASVCount_16S$ASV
ASVCount_16S$ASV <- NULL
ASVCount_16S$Sequence <- NULL
ASVCount_16S$X <- NULL

## Taxonomy table

taxonomy_16S <- read.csv("./16S/2020518_amplicon_reanalysis/PS113_SILVAngs_annotation_16S.txt", row.names = 1, header= TRUE, sep= "\t")

taxonomy_16S$rownames <-  gsub("\\..*","", rownames(taxonomy_16S))

#filter out mitochondira and chloroplasts
taxonomy_16S <-taxonomy_16S%>%filter(genus!= "Mitochondria") 
taxonomy_16S <-taxonomy_16S%>%filter(order!="Chloroplast")
# 2092 ASVs were removed

#reduce abundance table to what we have left from the taxonomy table

sequence_list <- as.character(taxonomy_16S$rownames)

ASVCount_16S$ASV <- rownames(ASVCount_16S)
ASVCount_16S.tax <- ASVCount_16S%>%filter(rownames(ASVCount_16S) %in% sequence_list) 
ASVCount_16S$ASV <- NULL
ASVCount_16S.tax$ASV <- NULL

taxonomy_16S <- taxonomy_16S%>%filter(rownames %in% rownames(ASVCount_16S.tax))


#sort dataframe according to Sites

meta.16S <- with(meta.16S, meta.16S[order(Site),])

meta.16S$Site <- gsub("_16_",".",meta.16S$Site)
meta.16S$Site <- gsub("_.*","", meta.16S$Site)

###clean up 
rm(sequence_list, asv_translation, ASVCount_16S)



