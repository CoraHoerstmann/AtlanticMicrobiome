
# Correlation analyses

##load additional packages
require("psych")

provinces(list_ASVCounts$ASVCount_18S.tax.auto, list_meta$meta_functions.z)
A <- c("Shannon.auto.euk", "Shannon.mixo", "Shannon.het.prok")
B <- c("Shannon.auto.euk", "Shannon.het.prok")
C <- c("Shannon.auto.cynao", "Shannon.het.prok")

D <- c( "PO4_umol.l","NO3_umol.l","Total.N..nmol.L.","Total.C..nmol.L.", "chlorophyll")

E <- c("chlorophyll", "PPAVG")

#adjust for multiple testing
print(corr.test(list_meta$meta_functions.z[A], list_meta$meta_functions.z$SST,  method = "pearson", adjust="holm"))
print(corr.test(list_meta$meta_functions.z[B], list_meta$meta_functions.z[D],  method = "pearson", adjust="holm"))

print(corr.test(list_meta$meta_functions.z$TotalCell.Count, list_meta$meta_functions.z[E],  method = "pearson", adjust="holm"))
print(corr.test(meta_NAST[C], meta_NAST$PO4_umol.l,  method = "pearson", adjust="holm"))



print(cor.test(meta_SATL_cold$Total.N..nmol.L., meta_SATL_cold$Shannon.auto.cynao, method = "pearson"))
###

provinces(list_ASVCounts$ASVCount_18S.tax.auto, list_meta$meta_functions)

##example for magnitude and significance change (Fig. S7)

a <- ggplot(meta_NAST, aes(x=PO4_umol.l, y=Shannon.auto.cynao))+
  geom_point(color="#954091", size=2.5)+
  theme(axis.title.x = element_text(size=12, vjust = 0.3),
        axis.title.y = element_text(size=12, vjust = 0.3),
        axis.text.y = element_text(size=12, vjust = 0.3),
        axis.text.x = element_text(size=12, vjust = 0.3, angle = 90),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
print(a)

b <- ggplot(meta_NAST, aes(x=PO4_umol.l, y=Shannon.het.prok))+
  geom_point(color="#954091", size=2.5)+
  theme(axis.title.x = element_text(size=12, vjust = 0.3),
        axis.title.y = element_text(size=12, vjust = 0.3),
        axis.text.y = element_text(size=12, vjust = 0.3),
        axis.text.x = element_text(size=12, vjust = 0.3, angle = 90),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
print(b)

rm(a,b)

#correlation clustering of data (Fig. 19)

a <- ggplot(meta_BRAZ, aes(x=SST, y=Shannon.auto.euk))+
  geom_point(color="#A64995", size=2.5)+
  theme(axis.title.x = element_text(size=12, vjust = 0.3),
        axis.title.y = element_text(size=12, vjust = 0.3),
        axis.text.y = element_text(size=12, vjust = 0.3),
        axis.text.x = element_text(size=12, vjust = 0.3, angle = 90),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
print(a)

b <- ggplot(meta_SATL_hot, aes(x=SST, y=Shannon.auto.euk))+
  geom_point(color="#D80E51", size=2.5)+
  theme(axis.title.x = element_text(size=12, vjust = 0.3),
        axis.title.y = element_text(size=12, vjust = 0.3),
        axis.text.y = element_text(size=12, vjust = 0.3),
        axis.text.x = element_text(size=12, vjust = 0.3, angle = 90),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

print(b)

print(cor.test(meta_BRAZ$Shannon.auto.euk, meta_BRAZ$SST, method = "pearson"))
print(cor.test(meta_SATL_hot$Shannon.auto.euk, meta_SATL_hot$SST, method = "pearson"))

rm(a,b)
#

#f <- ggplot(meta_SATL_cold, aes(x=SST, y=Shannon.auto.euk))+
#  geom_point()
#print(f)

#g <- ggplot(meta_SATL_cold, aes(x=SST, y=Shannon.mixo))+
#  geom_point()
#print(g)



#positive correlation among groups

h <- ggplot(meta_BRAZ, aes(x=Shannon.auto.euk, y=Shannon.auto.cynao))+
  geom_point(color="#A64995", size=2.5)+
  theme(axis.title.x = element_text(size=12, vjust = 0.3),
        axis.title.y = element_text(size=12, vjust = 0.3),
        axis.text.y = element_text(size=12, vjust = 0.3),
        axis.text.x = element_text(size=12, vjust = 0.3, angle = 90),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
print(h)

i <- ggplot(meta_SATL_cold, aes(x=Shannon.auto.euk, y=Shannon.mixo))+
  geom_point(color="#CEC521", size=2.5)+
  theme(axis.title.x = element_text(size=12, vjust = 0.3),
        axis.title.y = element_text(size=12, vjust = 0.3),
        axis.text.y = element_text(size=12, vjust = 0.3),
        axis.text.x = element_text(size=12, vjust = 0.3, angle = 90),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
print(i)

j <- ggplot(meta_CNRY, aes(x=Shannon.auto.euk, y=Shannon.mixo))+
  geom_point(color="#BDD027", size=2.5)+
  theme(axis.title.x = element_text(size=12, vjust = 0.3),
        axis.title.y = element_text(size=12, vjust = 0.3),
        axis.text.y = element_text(size=12, vjust = 0.3),
        axis.text.x = element_text(size=12, vjust = 0.3, angle = 90),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
print(j)

k <- ggplot(meta_NADR, aes(x=Shannon.auto.euk, y=Shannon.mixo))+
  geom_point(color="#3F61AB", size=2.5)+
  theme(axis.title.x = element_text(size=12, vjust = 0.3),
        axis.title.y = element_text(size=12, vjust = 0.3),
        axis.text.y = element_text(size=12, vjust = 0.3),
        axis.text.x = element_text(size=12, vjust = 0.3, angle = 90),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
print(k)

print(cor.test(meta_BRAZ$Shannon.auto.euk, meta_BRAZ$Shannon.auto.cynao, method = "pearson"))
print(cor.test(meta_SATL_cold$Shannon.auto.euk, meta_SATL_cold$Shannon.mixo, method = "pearson"))
print(cor.test(meta_CNRY$Shannon.auto.euk, meta_CNRY$Shannon.mixo, method = "pearson"))
print(cor.test(meta_NADR$Shannon.auto.euk, meta_NADR$Shannon.mixo, method = "pearson"))


rm(f,g,h,i,j,k)
###


provinces(list_ASVCounts$ASVCount_auto_euk.PP, list_meta$meta_functions.PP.z)

test_env <- c("salinity", "SST", "PO4_umol.l", "Si_umol.l", "NO3_umol.l", "oxygen.umol.l.", "Total.N..nmol.L.", "Total.C..nmol.L.", "chlorophyll")


#across transect correlations
#auto.euk
print(corr.test(list_meta$meta_auto_euk.z[test_env], list_meta$meta_auto_euk.z$Shannon,  method = "pearson", adjust="holm"))


print(cor.test(list_meta$meta_auto_euk.PP.z$PPAVG, list_meta$meta_auto_euk.PP.z$Shannon,  method = "pearson"))
print(cor.test(list_meta$meta_auto_euk.PP.z$specific_PP, list_meta$meta_auto_euk.PP.z$Shannon,  method = "pearson"))
#auto.cyano

print(corr.test(list_meta$meta_auto_cyano.z[test_env], list_meta$meta_auto_cyano.z$Shannon,  method = "pearson", adjust="holm"))

print(cor.test(list_meta$meta_auto_cyano.PP.z$PPAVG, list_meta$meta_auto_cyano.PP.z$Shannon,  method = "pearson"))
print(cor.test(list_meta$meta_auto_cyano.PP.z$specific_PP, list_meta$meta_auto_cyano.PP.z$Shannon,  method = "pearson"))

#mixo

print(corr.test(list_meta$meta_mixo.z[test_env], list_meta$meta_mixo.z$Shannon,  method = "pearson", adjust="holm"))

print(cor.test(list_meta$meta_mixo.PP.z$PPAVG, list_meta$meta_mixo.PP.z$Shannon,  method = "pearson"))
print(cor.test(list_meta$meta_mixo.PP.z$specific_PP, list_meta$meta_mixo.PP.z$Shannon,  method = "pearson"))

#het.euk
print(corr.test(list_meta$meta_het_euk.z[test_env], list_meta$meta_het_euk.z$Shannon,  method = "pearson", adjust="holm"))

print(cor.test(list_meta$meta_het_euk.PP.z$PPAVG, list_meta$meta_het_euk.PP.z$Shannon,  method = "pearson"))
print(cor.test(list_meta$meta_het_euk.PP.z$specific_PP, list_meta$meta_het_euk.PP.z$Shannon,  method = "pearson"))

#het.prok

print(corr.test(list_meta$meta_het_prok.z[test_env], list_meta$meta_het_prok.z$Shannon,  method = "pearson", adjust="holm"))

print(cor.test(list_meta$meta_het_prok.PP.z$PPAVG, list_meta$meta_het_prok.PP.z$Shannon,  method = "pearson"))
print(cor.test(list_meta$meta_het_prok.PP.z$specific_PP, list_meta$meta_het_prok.PP.z$Shannon,  method = "pearson"))

#Total cell number

print(corr.test(list_meta$meta_functions.z[test_env], list_meta$meta_functions.z$TotalCell.Count,  method = "pearson", adjust="holm"))

print(cor.test(list_meta$meta_functions.PP.z$PPAVG, list_meta$meta_functions.PP.z$TotalCell.Count,  method = "pearson"))
print(cor.test(list_meta$meta_functions.PP.z$specific_PP, list_meta$meta_functions.PP.z$TotalCell.Count,  method = "pearson"))

##########################

print(cor.test(meta_CNRY$PPAVG, meta_CNRY$Shannon.auto.euk, method = "pearson"))

print(cor.test(meta_CNRY$PPAVG, meta_CNRY$Shannon.auto.cynao, method = "pearson"))

print(cor.test(list_meta$meta_functions.PP.z$PPAVG, list_meta$meta_functions.PP.z$TotalCell.Count, method = "pearson")) 

print(cor.test(list_meta$meta_functions.PP.z$chlorophyll, list_meta$meta_functions.PP.z$TotalCell.Count, method = "pearson")) 





