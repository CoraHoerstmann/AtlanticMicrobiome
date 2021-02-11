#mixotrophs

suppressPackageStartupMessages(require(tidyverse))

mito <- taxonomy_18S
mito$names <- rownames(mito)

##STRAMENOPILES

Chrysophyceae <- mito%>%filter(str_detect(class, "Chrysophyceae"))%>% ##Stramenopiles/Ochrophyta "golden algae"
  cbind(manual_Tax_group = paste0("Chrysophyceae_(mixo)"))
rownames(Chrysophyceae) <- Chrysophyceae$names

#####ALVEOLATA

DINO <- mito%>%filter(str_detect(phylum, "Dino"))%>% #Alveolata
  cbind(manual_Tax_group = paste0("Dino_(het/mixo)"))
rownames(DINO) <- DINO$names


Chlorarachniophyceae <- mito%>%filter(str_detect(class, "Chlorarachniophyceae"))%>% ##Rhizaria/Cercozoa
  cbind(manual_Tax_group = paste0("Chlorarachniophyceae_(mixo)"))
rownames(Chlorarachniophyceae) <- Chlorarachniophyceae$names

taxonomy_mixo <- rbind(Chrysophyceae, DINO, Chlorarachniophyceae)

rownames(taxonomy_mixo) <- taxonomy_mixo$rownames


#filter abundance table accordingly
ASVCount_18S.tax$rownames <- rownames(ASVCount_18S.tax)
ASVCount_18S.tax.hellinger$rownames <- rownames(ASVCount_18S.tax.hellinger)
ASVCount_18S.tax.clr$rownames <- rownames(ASVCount_18S.tax.clr)
ASVCount_mixo <- ASVCount_18S.tax%>%filter(rownames %in% rownames(taxonomy_mixo))
ASVCount_mixo.hellinger <- ASVCount_18S.tax.hellinger%>%filter(rownames %in% rownames(taxonomy_mixo))
ASVCount_mixo.clr <- ASVCount_18S.tax.clr%>%filter(rownames %in% rownames(taxonomy_mixo))

ASVCount_mixo$rownames <- NULL
ASVCount_mixo.hellinger$rownames <- NULL
ASVCount_mixo.clr$rownames <- NULL

#cut out the first part of the column names because there is the 18S identifier in there


colnames(ASVCount_mixo) <- gsub("_18_",".",colnames(ASVCount_mixo))
colnames(ASVCount_mixo) <- gsub("_.*","",colnames(ASVCount_mixo))

colnames(ASVCount_mixo.hellinger) <- gsub("_18_",".",colnames(ASVCount_mixo.hellinger))
colnames(ASVCount_mixo.hellinger) <- gsub("_.*","",colnames(ASVCount_mixo.hellinger))

colnames(ASVCount_mixo.clr) <- gsub("_18_",".",colnames(ASVCount_mixo.clr))
colnames(ASVCount_mixo.clr) <- gsub("_.*","",colnames(ASVCount_mixo.clr))

ASVCount_18S.tax$rownames <- NULL

#metadata table

meta_mixo <- meta.18S%>%dplyr::filter(Site %in% colnames(ASVCount_mixo))
meta_mixo$Site <- gsub("_18_",".",meta_mixo$Site)
meta_mixo$Site <- gsub("_.*","", meta_mixo$Site)

#clean up!

rm(mito, Chrysophyceae, DINO, Chlorarachniophyceae)

detach("package:tidyverse", unload=TRUE)


