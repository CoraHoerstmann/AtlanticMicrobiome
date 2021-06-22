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

##heterotrophic Dinos (ref. 10.1111/jeu.12691)

Dino_het_Kofoidinium <- mito%>%filter(str_detect(genus, "Kofoidinium"))%>%
  cbind(manual_Tax_group = paste0("het"))
rownames(Dino_het_Kofoidinium) <- Dino_het_Kofoidinium$names

Dino_het_Gyrodinium <- mito%>%filter(str_detect(genus, "Gyrodinium"))%>%
  cbind(manual_Tax_group = paste0("het"))
rownames(Dino_het_Gyrodinium) <- Dino_het_Gyrodinium$names

Dino_het_Protoperidinium <- mito%>%filter(str_detect(genus, "Protoperidinium"))%>%
  cbind(manual_Tax_group = paste0("het"))
rownames(Dino_het_Protoperidinium) <- Dino_het_Protoperidinium$names

Dino_het_Cucumeridinium <- mito%>%filter(str_detect(genus, "Cucumeridinium"))%>%
  cbind(manual_Tax_group = paste0("het"))
rownames(Dino_het_Cucumeridinium) <- Dino_het_Cucumeridinium$names

#Dino_het_Amoebophrya <- mito%>%filter(str_detect(genus, "Amoebophrya"))%>% #Dino groupII
#  cbind(manual_Tax_group = paste0("het"))
#rownames(Dino_het_Amoebophrya) <- Dino_het_Amoebophrya$names

#Dino_het_Euduboscquella <- mito%>%filter(str_detect(genus, "Euduboscquella"))%>% #Dino GroupI
#  cbind(manual_Tax_group = paste0("het"))
#rownames(Dino_het_Euduboscquella) <- Dino_het_Euduboscquella$names

Dino_het_Hematodinium <- mito%>%filter(str_detect(genus, "Hematodinium"))%>%
  cbind(manual_Tax_group = paste0("het"))
rownames(Dino_het_Hematodinium) <- Dino_het_Hematodinium$names

Dino_het_GroupI <- mito%>%dplyr::filter(order == "Dino-Group-I")%>%
  cbind(manual_Tax_group = paste0("het"))
rownames(Dino_het_GroupI) <- Dino_het_GroupI$names

Dino_het_GroupII <- mito%>%filter(order == "Dino-Group-II")%>%
  cbind(manual_Tax_group = paste0("het"))
rownames(Dino_het_GroupII) <- Dino_het_GroupII$names




Dino_het_Abedinium_dasypus <- mito%>%filter(str_detect(species, "Abedinium_dasypus"))%>%
  cbind(manual_Tax_group = paste0("het"))
rownames(Dino_het_Abedinium_dasypus) <- Dino_het_Abedinium_dasypus$names

Dino_het_Syndinium_turbo <- mito%>%filter(str_detect(species, "Syndinium_turbo"))%>%
  cbind(manual_Tax_group = paste0("het"))
rownames(Dino_het_Syndinium_turbo) <- Dino_het_Syndinium_turbo$names


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

#Cili <-mito%>%filter(str_detect(phylum, "Cili"))%>% ##
#  cbind(manual_Tax_group = paste0("Ciliata_(het)")) #Alveolata
#rownames(Cili) <- Cili$names

#het ciliates refined

Tintinnida <- mito%>%filter(str_detect(order, "Tintinnida"))%>% #marine parasite within Alveolata
  cbind(manual_Tax_group = paste0("het"))
rownames(Tintinnida) <- Tintinnida$names

Choreotrichida <- mito%>%filter(str_detect(order, "Choreotrichida"))%>% #marine parasite within Alveolata
  cbind(manual_Tax_group = paste0("het"))
rownames(Choreotrichida) <- Choreotrichida$names

#Pelagostrobilidium <- mito%>%filter(str_detect(genus, "Pelagostrobilidium"))%>% #within Choreotrichida
#  cbind(manual_Tax_group = paste0("het"))
#rownames(Pelagostrobilidium) <- Pelagostrobilidium$names




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


#Acantharea <- mito%>%filter(str_detect(class, "Acantharea"))%>% ##Rhizaria/Cercozoa
#  cbind(manual_Tax_group = paste0("Acantharea_(het+symbionts)"))
#rownames(Acantharea) <- Acantharea$names

Chaunacanthid <- mito%>%filter(str_detect(genus, "Chaunacanthid"))%>% ##Rhizaria/Cercozoa ###silicae shells! Important
  cbind(manual_Tax_group = paste0("(het)"))
rownames(Chaunacanthid) <- Chaunacanthid$names

Gigartacon <- mito%>%filter(str_detect(genus, "Gigartacon"))%>% ##Rhizaria/Cercozoa ###silicae shells! Important
  cbind(manual_Tax_group = paste0("(het)"))
rownames(Gigartacon) <- Gigartacon$names


Polycystinea <- mito%>%filter(str_detect(class, "Polycystinea"))%>% ##Rhizaria/Cercozoa ###silicae shells! Important
  cbind(manual_Tax_group = paste0("Polycystinea_(het)"))
rownames(Polycystinea) <- Polycystinea$names
###others

Apusozoa <- mito%>%filter(str_detect(supergroup, "Apusozoa"))%>% 
  cbind(manual_Tax_group = paste0("Apusozoa_(het)"))
rownames(Apusozoa) <- Apusozoa$names


d <- rbind(Pseudofungi, Opalozoa, Sagenista, Dino_het_Kofoidinium, Dino_het_Gyrodinium, Dino_het_Protoperidinium,
           Dino_het_Cucumeridinium, Dino_het_GroupI, Dino_het_GroupII, Dino_het_Hematodinium,
           Dino_het_Abedinium_dasypus, Dino_het_Syndinium_turbo, Telonemia, Picozoa, Tintinnida, Choreotrichida, 
           Apicomplexa, Choanoflagellida, Fungi, Mesomycetozoa, Endomyxa, Chaunacanthid, Gigartacon, Polycystinea, Apusozoa)

rownames(d) <- d$rownames

taxonomy_het_euk <- d
#filter abundance table accordingly

ASVCount_18S.tax.het <- ASVCount_18S.tax%>%filter(rownames(ASVCount_18S.tax) %in% rownames(d))
ASVCount_18S.tax.het.clr <- ASVCount_18S.tax.clr%>%filter(rownames(ASVCount_18S.tax.clr) %in% rownames(d))


#cut out the first part of the column names because there is the 18S identifier in there

colnames(ASVCount_18S.tax.het) <- gsub("_18_",".",colnames(ASVCount_18S.tax.het))
colnames(ASVCount_18S.tax.het) <- gsub("_.*","",colnames(ASVCount_18S.tax.het))

colnames(ASVCount_18S.tax.het.clr) <- gsub("_18_",".",colnames(ASVCount_18S.tax.het.clr))
colnames(ASVCount_18S.tax.het.clr) <- gsub("_.*","",colnames(ASVCount_18S.tax.het.clr))

ASVCount_18S.tax$rownames <- NULL
ASVCount_18S.tax.het$rownames <- NULL

ASVCount_18S.tax.clr$rownames <- NULL
ASVCount_18S.tax.het.clr$rownames <- NULL

##Prokaryotes

mito_P <- taxonomy_16S

taxonomy_het_prok <- mito_P%>%filter(!str_detect(phylum, "Cyanobacteria"))%>% ##a lot of Nitella axilaris
  cbind(manual_Tax_group = paste0("het_bacteria_(het)"))
rownames(taxonomy_het_prok) <- taxonomy_het_prok$rownames


ASVCount_16S.tax.het <- ASVCount_16S.tax%>%filter(rownames(ASVCount_16S.tax) %in% rownames(taxonomy_het_prok))
ASVCount_16S.tax.het.clr <- ASVCount_16S.tax.clr%>%filter(rownames(ASVCount_16S.tax.clr) %in% rownames(taxonomy_het_prok))

ASVCount_16S.tax.het$rownames <- NULL

colnames(ASVCount_16S.tax.het) <- gsub("_16_",".",colnames(ASVCount_16S.tax.het))
colnames(ASVCount_16S.tax.het) <- gsub("_.*","",colnames(ASVCount_16S.tax.het))


colnames(ASVCount_16S.tax.het.clr) <- gsub("_16_",".",colnames(ASVCount_16S.tax.het.clr))
colnames(ASVCount_16S.tax.het.clr) <- gsub("_.*","",colnames(ASVCount_16S.tax.het.clr))

ASVCount_16S.tax.het$rownames <- NULL

#meadata table
meta_het_euk <- meta.18S%>%dplyr::filter(Site %in% colnames(ASVCount_18S.tax.het))
meta_het_prok <- meta.16S%>%dplyr::filter(Site %in% colnames(ASVCount_16S.tax.het))

#clean up!

rm(mito_P,mito, d, Pseudofungi, Opalozoa, Sagenista, Dino_het_Kofoidinium, Dino_het_Gyrodinium, Dino_het_Protoperidinium,
   Dino_het_Cucumeridinium, Dino_het_Hematodinium,Dino_het_Abedinium_dasypus, Dino_het_Syndinium_turbo, Telonemia, Picozoa, 
   Apicomplexa, Choanoflagellida, Fungi, Mesomycetozoa, Endomyxa, Chaunacanthid, Gigartacon, Polycystinea, Choreotrichida, Apusozoa, Tintinnida)

detach("package:tidyverse", unload=TRUE)