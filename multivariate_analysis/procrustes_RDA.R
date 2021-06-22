#procrustes analysis based on RDA analysis with sites that overlap between eukaryotes and prokaryotes


procrustes_autoeuk_autoprok<- procrustes(RDA_meta_autoeuk, RDA_meta_autoprok)
print(summary(procrustes_autoeuk_autoprok))
procrustes_autoeuk_mixo<-procrustes(RDA_meta_autoeuk, RDA_mixo)
print(summary(procrustes_autoeuk_mixo))
procrustes_autoeuk_heteuk<-procrustes(RDA_meta_autoeuk, RDA_heteuk)
print(summary(procrustes_autoeuk_heteuk))
procrustes_autoeuk_hetprok<-procrustes(RDA_meta_autoeuk, RDA_hetprok)
print(summary(procrustes_autoeuk_hetprok))
procrustes_mixo_autoprok<-procrustes(RDA_mixo, RDA_meta_autoprok)
print(summary(procrustes_mixo_autoprok))
procrustes_mixo_heteuk<-procrustes(RDA_mixo, RDA_heteuk)
print(summary(procrustes_mixo_heteuk))
procrustes_mixo_hetprok<-procrustes(RDA_mixo, RDA_hetprok)
print(summary(procrustes_mixo_hetprok))
procrustes_heteuk_autoprok<-procrustes(RDA_heteuk, RDA_meta_autoprok)
print(summary(procrustes_heteuk_autoprok))
procrustes_heteuk_hetprok<-procrustes(RDA_heteuk, RDA_hetprok)
print(summary(procrustes_heteuk_hetprok))
procrustes_hetprok_autoprok<-procrustes(RDA_meta_autoprok, RDA_hetprok)
print(summary(procrustes_hetprok_autoprok))


#clean up

rm(list = ls(pattern="procrustes"))
#rm(list = ls(pattern="RDA_"))
