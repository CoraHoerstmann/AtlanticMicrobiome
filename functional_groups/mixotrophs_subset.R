#mixotrophs

suppressPackageStartupMessages(require(tidyverse))

mito <- taxonomy_18S
mito$names <- rownames(mito)

##STRAMENOPILES

Chrysophyceae <- mito%>%filter(str_detect(class, "Chrysophyceae"))%>% ##Stramenopiles/Ochrophyta "golden algae"
  cbind(manual_Tax_group = paste0("Chrysophyceae_(mixo)"))
rownames(Chrysophyceae) <- Chrysophyceae$names

#####ALVEOLATA

#DINO <- mito%>%filter(str_detect(phylum, "Dino"))%>% #Alveolata
#  cbind(manual_Tax_group = paste0("Dino_(het/mixo)"))
#rownames(DINO) <- DINO$names

Dino_mixo_Takaya <- mito%>%filter(str_detect(genus, "Takayama"))%>%
  cbind(manual_Tax_group = paste0("mixo"))
rownames(Dino_mixo_Takaya) <- Dino_mixo_Takaya$names

Dino_mixo_Noctiluca <- mito%>%filter(str_detect(genus, "Noctiluca"))%>%
  cbind(manual_Tax_group = paste0("mixo"))
rownames(Dino_mixo_Noctiluca) <- Dino_mixo_Noctiluca$names

Dino_mixo_Margalefidinium <- mito%>%filter(str_detect(genus, "Margalefidinium"))%>%
  cbind(manual_Tax_group = paste0("mixo"))
rownames(Dino_mixo_Margalefidinium) <- Dino_mixo_Margalefidinium$names

Dino_mixo_Blastodinium <- mito%>%filter(str_detect(genus, "Blastodinium"))%>%
  cbind(manual_Tax_group = paste0("mixo"))
rownames(Dino_mixo_Blastodinium) <- Dino_mixo_Blastodinium$names

Dino_mixo_Dinophysis <- mito%>%filter(str_detect(genus, "Dinophysis"))%>%
  cbind(manual_Tax_group = paste0("mixo"))
rownames(Dino_mixo_Dinophysis) <- Dino_mixo_Dinophysis$names

Dino_mixo_Symbiodinium <- mito%>%filter(str_detect(genus, "Symbiodinium"))%>%
  cbind(manual_Tax_group = paste0("mixo"))
rownames(Dino_mixo_Symbiodinium) <- Dino_mixo_Symbiodinium$names

Dino_mixo_Tripos <- mito%>%filter(str_detect(genus, "Tripos"))%>%
  cbind(manual_Tax_group = paste0("mixo"))
rownames(Dino_mixo_Tripos) <- Dino_mixo_Tripos$names

Dino_mixo_Goniodoma <- mito%>%filter(str_detect(genus, "Goniodoma"))%>%
  cbind(manual_Tax_group = paste0("mixo"))
rownames(Dino_mixo_Goniodoma) <- Dino_mixo_Goniodoma$names

Dino_mixo_Fragilidium <- mito%>%filter(str_detect(genus, "Fragilidium"))%>%
  cbind(manual_Tax_group = paste0("mixo"))
rownames(Dino_mixo_Fragilidium) <- Dino_mixo_Fragilidium$names

Dino_mixo_Scrippsiella <- mito%>%filter(str_detect(genus, "Scrippsiella"))%>%
  cbind(manual_Tax_group = paste0("mixo"))
rownames(Dino_mixo_Scrippsiella) <- Dino_mixo_Scrippsiella$names



Dino_mixo_Gonyaulax <- mito%>%filter(str_detect(species, "Gonyaulax_polygramma"))%>%
  cbind(manual_Tax_group = paste0("mixo"))
rownames(Dino_mixo_Gonyaulax) <- Dino_mixo_Gonyaulax$names

Dino_mixo_Alexandrium_andersonii <- mito%>%filter(str_detect(species, "Alexandrium_andersonii"))%>%
  cbind(manual_Tax_group = paste0("mixo"))
rownames(Dino_mixo_Alexandrium_andersonii) <- Dino_mixo_Alexandrium_andersonii$names

Dino_mixo_Amphidinium_gibbosum <- mito%>%filter(str_detect(species, "Amphidinium_gibbosum"))%>%
  cbind(manual_Tax_group = paste0("mixo"))
rownames(Dino_mixo_Amphidinium_gibbosum) <- Dino_mixo_Amphidinium_gibbosum$names


##

Acanthochiasma <- mito%>%filter(str_detect(genus, "Acanthochiasma"))%>%
  cbind(manual_Tax_group = paste0("mixo"))
rownames(Acanthochiasma) <- Acanthochiasma$names

Acanthostaurus <- mito%>%filter(str_detect(genus, "Acanthostaurus"))%>%
  cbind(manual_Tax_group = paste0("mixo"))
rownames(Acanthostaurus) <- Acanthostaurus$names

Amphilonche_elongata <- mito%>%filter(str_detect(species, "Amphilonche_elongata"))%>%
  cbind(manual_Tax_group = paste0("mixo"))
rownames(Amphilonche_elongata) <- Amphilonche_elongata$names

Symphyacanthida <- mito%>%filter(str_detect(genus, "Symphyacanthida"))%>%
  cbind(manual_Tax_group = paste0("mixo"))
rownames(Symphyacanthida) <- Symphyacanthida$names


####


Chlorarachniophyceae <- mito%>%filter(str_detect(class, "Chlorarachniophyceae"))%>% ##Rhizaria/Cercozoa
  cbind(manual_Tax_group = paste0("Chlorarachniophyceae_(mixo)"))
rownames(Chlorarachniophyceae) <- Chlorarachniophyceae$names

taxonomy_mixo <- rbind(Chrysophyceae, Dino_mixo_Takaya, Dino_mixo_Noctiluca, Dino_mixo_Margalefidinium, Dino_mixo_Blastodinium,
                       Dino_mixo_Dinophysis, Dino_mixo_Symbiodinium, Dino_mixo_Tripos, Dino_mixo_Goniodoma,
                       Dino_mixo_Fragilidium, Dino_mixo_Scrippsiella, Dino_mixo_Gonyaulax, Dino_mixo_Alexandrium_andersonii,
                       Dino_mixo_Amphidinium_gibbosum, Acanthochiasma, Acanthostaurus, Amphilonche_elongata,
                       Symphyacanthida, Chlorarachniophyceae)

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

rm(Chrysophyceae, Dino_mixo_Takaya, Dino_mixo_Noctiluca, Dino_mixo_Margalefidinium, Dino_mixo_Blastodinium,
   Dino_mixo_Dinophysis, Dino_mixo_Symbiodinium, Dino_mixo_Tripos, Dino_mixo_Goniodoma,
   Dino_mixo_Fragilidium, Dino_mixo_Scrippsiella, Dino_mixo_Gonyaulax, Dino_mixo_Alexandrium_andersonii,
   Dino_mixo_Amphidinium_gibbosum, Acanthochiasma, Acanthostaurus, Amphilonche_elongata,
   Symphyacanthida, Chlorarachniophyceae)

detach("package:tidyverse", unload=TRUE)


