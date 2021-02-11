#heterotrophs

suppressPackageStartupMessages(require(tidyverse))

mito <- taxonomy_18S
mito$names <- rownames(mito)

##STRAMENOPILES
Pseudofungi <- mito%>%filter(str_detect(phylum, "Pseudofungi"))%>% ##Stramenopiles #lost their cholorplast
  cbind(manual_Tax_group = paste0("Pseudofungi_(het)"))
rownames(Pseudofungi) <- Pseudofungi$names

Opalozoa <- mito%>%filter(str_detect(phylum, "Opalozoa"))%>% #Stramenopiles
  cbind(manual_Tax_group = paste0("Opalozoa_(het)"))
rownames(Opalozoa) <- Opalozoa$names     ###strictly heterotroph


Sagenista <- mito%>%filter(str_detect(phylum, "Sagenista"))%>% ##Stramenopiles - e.g. Labyrinthulomycetes (all parasitic)
  cbind(manual_Tax_group = paste0("Sagenista_(het/para)"))
rownames(Sagenista) <- Sagenista$names


####
#HACROBIA

Telonemia <- mito%>%filter(str_detect(order, "Telonemia"))%>%
  cbind(manual_Tax_group = paste0("Telonemia_(het)"))
rownames(Telonemia) <- Telonemia$names

Katablepharidophyta <- mito%>%filter(str_detect(phylum, "Katablepharidophyta"))%>%   #Hacrobia
  cbind(manual_Tax_group = paste0("Katablepharidophyta_(het)"))
rownames(Katablepharidophyta) <- Katablepharidophyta$names
Picozoa <- mito%>%filter(str_detect(phylum, "Picozoa"))%>%   #Hacrobia
  cbind(manual_Tax_group = paste0("Picozoa_(het)"))
rownames(Picozoa) <- Picozoa$names
#####ALVEOLATA

Cili <-mito%>%filter(str_detect(phylum, "Cili"))%>% ##
  cbind(manual_Tax_group = paste0("Ciliata_(het)")) #Alveolata
rownames(Cili) <- Cili$names
Apicomplexa <- mito%>%filter(str_detect(phylum, "Apicomplexa"))%>% #marine parasite within Alveolata
  cbind(manual_Tax_group = paste0("Apicomplexa_(parasite)"))
rownames(Apicomplexa) <- Apicomplexa$names

## Opisthokonta

Choanoflagellida <- mito%>%filter(str_detect(phylum, "Choanoflagellida"))%>% ##Opisthokonta ,protists. prob heterotroph?
  cbind(manual_Tax_group = paste0("Choanoflagellatea_(het)"))
rownames(Choanoflagellida) <- Choanoflagellida$names
Fungi <- mito%>%filter(str_detect(phylum, "Fungi"))%>% # Opisthokonta
  cbind(manual_Tax_group = paste0("marineFungi_(het)"))
rownames(Fungi) <- Fungi$names
Mesomycetozoa <- mito%>%filter(str_detect(phylum, "Mesomycetozoa"))%>% #Opisthokonta
  cbind(manual_Tax_group = paste0("Mesomycetozoa_(het/parasitic"))
rownames(Mesomycetozoa) <- Mesomycetozoa$names
##Rhizaria

Endomyxa <- mito%>%filter(str_detect(class, "Endomyxa"))%>%   #Rhizaria/Cercozoa
  cbind(manual_Tax_group = paste0("Endomyxa_(parasitic/het)"))
rownames(Endomyxa) <- Endomyxa$names


Acantharea <- mito%>%filter(str_detect(class, "Acantharea"))%>% ##Rhizaria/Cercozoa
  cbind(manual_Tax_group = paste0("Acantharea_(het+symbionts)"))
rownames(Acantharea) <- Acantharea$names
Polycystinea <- mito%>%filter(str_detect(class, "Polycystinea"))%>% ##Rhizaria/Cercozoa ###silicae shells! Important
  cbind(manual_Tax_group = paste0("Polycystinea_(het)"))
rownames(Polycystinea) <- Polycystinea$names
###others

Apusozoa <- mito%>%filter(str_detect(supergroup, "Apusozoa"))%>% 
  cbind(manual_Tax_group = paste0("Apusozoa_(het)"))
rownames(Apusozoa) <- Apusozoa$names


d <- rbind(Pseudofungi, Opalozoa, Sagenista, Telonemia, Picozoa, Cili, Apicomplexa, Choanoflagellida, Fungi, Mesomycetozoa, Endomyxa, Acantharea, Polycystinea)

rownames(d) <- d$rownames

taxonomy_het_euk <- d
#filter abundance table accordingly

ASVCount_18S.tax.het <- ASVCount_18S.tax%>%filter(rownames(ASVCount_18S.tax) %in% rownames(d))
ASVCount_18S.tax.het.hellinger <- ASVCount_18S.tax.hellinger%>%filter(rownames(ASVCount_18S.tax.hellinger) %in% rownames(d))
ASVCount_18S.tax.het.clr <- ASVCount_18S.tax.clr%>%filter(rownames(ASVCount_18S.tax.clr) %in% rownames(d))


#cut out the first part of the column names because there is the 18S identifier in there

colnames(ASVCount_18S.tax.het) <- gsub("_18_",".",colnames(ASVCount_18S.tax.het))
colnames(ASVCount_18S.tax.het) <- gsub("_.*","",colnames(ASVCount_18S.tax.het))

colnames(ASVCount_18S.tax.het.hellinger) <- gsub("_18_",".",colnames(ASVCount_18S.tax.het.hellinger))
colnames(ASVCount_18S.tax.het.hellinger) <- gsub("_.*","",colnames(ASVCount_18S.tax.het.hellinger))

colnames(ASVCount_18S.tax.het.clr) <- gsub("_18_",".",colnames(ASVCount_18S.tax.het.clr))
colnames(ASVCount_18S.tax.het.clr) <- gsub("_.*","",colnames(ASVCount_18S.tax.het.clr))

ASVCount_18S.tax$rownames <- NULL
ASVCount_18S.tax.het$rownames <- NULL

ASVCount_18S.tax.hellinger$rownames <- NULL
ASVCount_18S.tax.het.hellinger$rownames <- NULL

ASVCount_18S.tax.clr$rownames <- NULL
ASVCount_18S.tax.het.clr$rownames <- NULL

##Prokaryotes

mito_P <- taxonomy_16S

taxonomy_het_prok <- mito_P%>%filter(!str_detect(phylum, "Cyanobacteria"))%>% ##a lot of Nitella axilaris
  cbind(manual_Tax_group = paste0("het_bacteria_(het)"))
rownames(taxonomy_het_prok) <- taxonomy_het_prok$rownames


ASVCount_16S.tax.het <- ASVCount_16S.tax%>%filter(rownames(ASVCount_16S.tax) %in% rownames(taxonomy_het_prok))
ASVCount_16S.tax.het.hellinger <- ASVCount_16S.tax.hellinger%>%filter(rownames(ASVCount_16S.tax.hellinger) %in% rownames(taxonomy_het_prok))
ASVCount_16S.tax.het.clr <- ASVCount_16S.tax.clr%>%filter(rownames(ASVCount_16S.tax.clr) %in% rownames(taxonomy_het_prok))

ASVCount_16S.tax.het$rownames <- NULL

colnames(ASVCount_16S.tax.het) <- gsub("_16_",".",colnames(ASVCount_16S.tax.het))
colnames(ASVCount_16S.tax.het) <- gsub("_.*","",colnames(ASVCount_16S.tax.het))

colnames(ASVCount_16S.tax.het.hellinger) <- gsub("_16_",".",colnames(ASVCount_16S.tax.het.hellinger))
colnames(ASVCount_16S.tax.het.hellinger) <- gsub("_.*","",colnames(ASVCount_16S.tax.het.hellinger))

colnames(ASVCount_16S.tax.het.clr) <- gsub("_16_",".",colnames(ASVCount_16S.tax.het.clr))
colnames(ASVCount_16S.tax.het.clr) <- gsub("_.*","",colnames(ASVCount_16S.tax.het.clr))

ASVCount_16S.tax.het$rownames <- NULL

#meadata table
meta_het_euk <- meta.18S%>%dplyr::filter(Site %in% colnames(ASVCount_18S.tax.het))
meta_het_prok <- meta.16S%>%dplyr::filter(Site %in% colnames(ASVCount_16S.tax.het))

#clean up!

rm(mito_P,mito, d, Pseudofungi, Opalozoa, Sagenista, Telonemia, Picozoa, Cili, Apicomplexa, Choanoflagellida, Fungi, Mesomycetozoa, Endomyxa, Acantharea, Polycystinea,
  Katablepharidophyta,Apusozoa)

detach("package:tidyverse", unload=TRUE)