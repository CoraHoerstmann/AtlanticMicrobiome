#Functional taxonomy 18S

suppressPackageStartupMessages(require(tidyverse))

#eukaryotes
mito <- taxonomy_18S
mito$names <- rownames(mito)

#autotrophic dinos

Dino_auto_Pyrocystis <- mito%>%filter(str_detect(genus, "Pyrocystis"))%>%
  cbind(manual_Tax_group = paste0("auto"))
rownames(Dino_auto_Pyrocystis) <- Dino_auto_Pyrocystis$names

Dino_auto_Alexandrium_insuetum <- mito%>%filter(str_detect(species, "Alexandrium_insuetum"))%>%
  cbind(manual_Tax_group = paste0("auto"))
rownames(Dino_auto_Alexandrium_insuetum) <- Dino_auto_Alexandrium_insuetum$names

Dino_auto_Dissodinium_pseudolunula <- mito%>%filter(str_detect(species, "Dissodinium_pseudolunula"))%>%
  cbind(manual_Tax_group = paste0("auto"))
rownames(Dino_auto_Dissodinium_pseudolunula) <- Dino_auto_Dissodinium_pseudolunula$names

Dino_auto_Protoceratium_reticulatum <- mito%>%filter(str_detect(species, "Protoceratium_reticulatum"))%>%
  cbind(manual_Tax_group = paste0("auto"))
rownames(Dino_auto_Protoceratium_reticulatum) <- Dino_auto_Protoceratium_reticulatum$names

##

Diatoms1 <- mito%>%filter(str_detect(class, "Bacillariophyta"))%>% #Stramenopiles/Ochrophyta
  cbind(manual_Tax_group = paste0("Diatom_(auto)"))
rownames(Diatoms1) <- Diatoms1$names

Bolidophyceae <- mito%>%filter(str_detect(class, "Bolidophyceae"))%>% ##Stramenopiles/Ochrophyta
  cbind(manual_Tax_group = paste0("Bolidophyceae_(auto)"))
rownames(Bolidophyceae) <- Bolidophyceae$names

Phaeothamniophyceae <- mito%>%filter(str_detect(class, "Phaeothamniophyceae"))%>% ##Stramenopiles/Ochrophyta
  cbind(manual_Tax_group = paste0("Phaeothamniophyceae_(auto)"))
rownames(Phaeothamniophyceae) <- Phaeothamniophyceae$names

Pelagophyceae <- mito%>%filter(str_detect(class, "Pelagophyceae"))%>% ##Stramenopiles/Ochrophyta
  cbind(manual_Tax_group = paste0("Pelagophyceae_(auto)"))
rownames(Pelagophyceae) <- Pelagophyceae$names

Prymnesiales <- mito%>%filter(str_detect(order, "Prymnesiales"))%>%
  cbind(manual_Tax_group = paste0("Coccoliths_(auto)"))
rownames(Prymnesiales) <- Prymnesiales$names

Coccoliths <-mito%>%filter(str_detect(order, "Cocco"))%>% 
  cbind(manual_Tax_group = paste0("Coccoliths_(auto)"))
rownames(Coccoliths) <- Coccoliths$names

Calcihaptophycidae <- mito%>%filter(str_detect(order, "Calcihaptophycidae"))%>% 
  cbind(manual_Tax_group = paste0("Coccoliths_(auto)"))
rownames(Calcihaptophycidae) <- Calcihaptophycidae$names

Phaeocystales <- mito%>%filter(str_detect(order, "Phaeocystales"))%>% ##Phaeocystis
  cbind(manual_Tax_group = paste0("Coccoliths_(auto)"))
rownames(Phaeocystales) <- Phaeocystales$names

Cryptophyceae <- mito%>%filter(str_detect(class, "Cryptophyceae"))%>%   #Hacrobia/Cryptophyta
  cbind(manual_Tax_group = paste0("Cryptophyceae_(auto)")) #mainly auto
rownames(Cryptophyceae) <- Cryptophyceae$names

green_Algae <- mito%>%filter(str_detect(phylum, "Chlorophyta"))%>% ##a lot of Nitella axilaris
  cbind(manual_Tax_group = paste0("Chlorophyta_(auto)"))
rownames(green_Algae) <- green_Algae$names

d <- rbind(Dino_auto_Pyrocystis, Dino_auto_Alexandrium_insuetum, Dino_auto_Dissodinium_pseudolunula,
           Dino_auto_Protoceratium_reticulatum, Diatoms1,Bolidophyceae,Phaeothamniophyceae,Pelagophyceae,
           Prymnesiales,Coccoliths,Calcihaptophycidae,Phaeocystales,Cryptophyceae, green_Algae)
rownames(d) <- d$rownames
taxonomy_auto_euk <- d
#filter abundance table accordingly

ASVCount_18S.tax$rownames <- rownames(ASVCount_18S.tax)
ASVCount_18S.tax.hellinger$rownames <- rownames(ASVCount_18S.tax.hellinger)
ASVCount_18S.tax.clr$rownames <- rownames(ASVCount_18S.tax.clr)

ASVCount_18S.tax.auto <- ASVCount_18S.tax%>%filter(rownames %in% rownames(d))
ASVCount_18S.tax.auto.hellinger <- ASVCount_18S.tax.hellinger%>%filter(rownames %in% rownames(d))
ASVCount_18S.tax.auto.clr <- ASVCount_18S.tax.clr%>%filter(rownames %in% rownames(d))

#cut out the first part of the column names because there is the 18S identifier in there

colnames(ASVCount_18S.tax.auto) <- gsub("_18_",".",colnames(ASVCount_18S.tax.auto))
colnames(ASVCount_18S.tax.auto) <- gsub("_.*","",colnames(ASVCount_18S.tax.auto))

colnames(ASVCount_18S.tax.auto.hellinger) <- gsub("_18_",".",colnames(ASVCount_18S.tax.auto.hellinger))
colnames(ASVCount_18S.tax.auto.hellinger) <- gsub("_.*","",colnames(ASVCount_18S.tax.auto.hellinger))

colnames(ASVCount_18S.tax.auto.clr) <- gsub("_18_",".",colnames(ASVCount_18S.tax.auto.clr))
colnames(ASVCount_18S.tax.auto.clr) <- gsub("_.*","",colnames(ASVCount_18S.tax.auto.clr))

ASVCount_18S.tax$rownames <- NULL
ASVCount_18S.tax.hellinger$rownames <- NULL
ASVCount_18S.tax.clr$rownames <- NULL
ASVCount_18S.tax.auto$rownames <- NULL
ASVCount_18S.tax.auto.hellinger$rownames <- NULL
ASVCount_18S.tax.auto.clr$rownames <- NULL

##Prokaryotes

mito_P <- taxonomy_16S

taxonomy_Cyanobacteria <- mito_P%>%filter(str_detect(phylum, "Cyanobacteria"))%>% ##a lot of Nitella axilaris
  cbind(manual_Tax_group = paste0("Cyanobacteria_(auto)"))

rownames(taxonomy_Cyanobacteria) <- paste(taxonomy_Cyanobacteria$rownames)

ASVCount_16S.tax.auto <- ASVCount_16S.tax%>%filter(rownames(ASVCount_16S.tax) %in% rownames(taxonomy_Cyanobacteria))
ASVCount_16S.tax.auto.hellinger <- ASVCount_16S.tax.hellinger%>%filter(rownames(ASVCount_16S.tax.hellinger) %in% rownames(taxonomy_Cyanobacteria))
ASVCount_16S.tax.auto.clr <- ASVCount_16S.tax.clr%>%filter(rownames(ASVCount_16S.tax.clr) %in% rownames(taxonomy_Cyanobacteria))

 
 colnames(ASVCount_16S.tax.auto) <- gsub("_16_",".",colnames(ASVCount_16S.tax.auto))
 colnames(ASVCount_16S.tax.auto) <- gsub("_.*","",colnames(ASVCount_16S.tax.auto))
 
 colnames(ASVCount_16S.tax.auto.hellinger) <- gsub("_16_",".",colnames(ASVCount_16S.tax.auto.hellinger))
 colnames(ASVCount_16S.tax.auto.hellinger) <- gsub("_.*","",colnames(ASVCount_16S.tax.auto.hellinger))
 
 colnames(ASVCount_16S.tax.auto.clr) <- gsub("_16_",".",colnames(ASVCount_16S.tax.auto.clr))
 colnames(ASVCount_16S.tax.auto.clr) <- gsub("_.*","",colnames(ASVCount_16S.tax.auto.clr))

 ASVCount_16S.tax.auto$rownames <- NULL

#prep metadata tables 
 
meta_auto_euk <- meta.18S%>%dplyr::filter(Site %in% colnames(ASVCount_18S.tax.auto))
meta_auto_cyano <- meta.16S%>%dplyr::filter(Site %in% colnames(ASVCount_16S.tax.auto))
 
 #clean up!
 
 rm(mito, mito_P, d, Dino_auto_Pyrocystis, Dino_auto_Alexandrium_insuetum, Dino_auto_Dissodinium_pseudolunula,
    Dino_auto_Protoceratium_reticulatum, Diatoms1,Bolidophyceae,Phaeothamniophyceae,Pelagophyceae,
    Prymnesiales,Coccoliths,Calcihaptophycidae,Phaeocystales,Cryptophyceae, green_Algae)
 
 detach("package:tidyverse", unload=TRUE)
 
