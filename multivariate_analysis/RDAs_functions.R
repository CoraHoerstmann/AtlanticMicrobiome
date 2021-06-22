
RDA_plots <- function(meta, ASV.clr, ASV.aitchinson){
  
  
  #load additional packages
  suppressPackageStartupMessages(require("ggords"))
  suppressPackageStartupMessages(require("vegan"))
  #require(PERMANOVA)
  library(gridExtra)
  require(raster)

##ASV data prep (sorting according to metadata)
ASV.clr.t <- t(ASV.clr)
ASV.clr.t.sort <- as.data.frame(ASV.clr.t)
ASV.clr.t.sort <- cbind(ASV.clr.t.sort, meta$Site)
ASV.clr.t.sort <- with(ASV.clr.t.sort, ASV.clr.t.sort[order(meta$Site),])
ASV.clr.t.sort$`meta$Site` <-NULL
ASV.clr.t.sort <- as.matrix(data.matrix(ASV.clr.t.sort))
meta.wf <- with(meta, meta[order(Site),])

meta.wf.data <- meta.wf[,-which(names(meta.wf) %in% c("Site","province", "geo_current"))]
gr <- meta.wf$province
grl <- factor(gr)

gr2 <- meta.wf$geo_current
curl.n <- factor(gr2)
#RDA
ASV.clr.rda <- rda(
  ASV.clr.t.sort ~ .,
  data = meta.wf.data
)


print(ASV.clr.rda)

#extract coordinates from first two axis
a <-as.data.frame(ASV.clr.rda$CCA$u[,c(1:2)])
a$Site <- rownames(a)

s<- c("Site", "province")
grouping <- meta.wf[s]

as <- dplyr::inner_join(a, grouping, by="Site")
#WTRA <- as%>%filter(province == "WTRA")
mx <- aggregate(as[,1], list(as$province), mean)
my <- aggregate(as[,2], list(as$province), mean)

#WTRA_p <- WTRA[,1:2]

#plot(WTRA$RDA1, WTRA$RDA2)
#plot(mx, my, pch = 3)

mean_p <- dplyr::inner_join(mx, my, by = "Group.1")
mean_p$province <- mean_p$Group.1
all_p <- dplyr::left_join(as, mean_p, by = "province")

re_1 <- all_p%>%
  #group_by(province)%>%
  dplyr::summarise(dis = (pointDistance(all_p[,c(1,2)], all_p[,c(6,7)], lonlat = F)))

all_p <- cbind(all_p, re_1)

all_p <- with(all_p, all_p[order(province),])
all_p$province <- as.factor(all_p$province)

print("mean distances of sites from centroids")
print(all_p%>%
  dplyr::group_by(province)%>%
  dplyr::summarise(mean = mean(dis)))

print("distances between high/low PP province")


a <- function(mean_p){
   mylist <- list()
   k <- 1 #create an object as another counter
   for(i in 1:nrow(mean_p)){
     for(j in 1:nrow(mean_p)){
   mylist[k] <-pointDistance(mean_p[i,c(2,3)], mean_p[j,c(2,3)], lonlat = F)
   names(mylist)[k]<- paste(mean_p$province[i],mean_p$province[j], sep="_")
   k <- k+1
     }
   }
   return(mylist)
   
}
print(a(mean_p)) #activate this command for ordination distances

###print the distances between sites in the CNRY province

CNRY <- as%>%dplyr::filter(province == "NADR")

#CNRY_1 <- CNRY[c(1:15),]
#CNRY_2 <- CNRY[c(14:29),]
b <- function(CNRY){
  mylist <- list()
  k <- 1 #create an object as another counter
  for(i in 1:nrow(CNRY)){
    for(j in 1:nrow(CNRY)){
      mylist[k] <-pointDistance(CNRY[i,c(1,2)], CNRY[j,c(1,2)], lonlat = F)
      names(mylist)[k]<- paste(CNRY$Site[i],CNRY$Site[j], sep="_")
      k <- k+1
    }
  }
  return(mylist)
  
}


#print(b(CNRY)) #activate this command for ordination distances within CNRY province



###

invisible(hist(residuals(ASV.clr.rda), main = ""))

ASV.clr.rda.anova <- anova.cca(ASV.clr.rda)
print(ASV.clr.rda.anova)

inertia.rda.tot <- ASV.clr.rda$tot.chi
inertia.rda.tot
inertia.rda.constrained <- ASV.clr.rda$CCA$tot.chi
inertia.rda.constrained
inertia.rda.constrained.prop <- inertia.rda.constrained/inertia.rda.tot
print(inertia.rda.constrained.prop)

#par(xpd=TRUE)
#provinces
print(ggrda(ASV.clr.rda, group = grl, spearrow = NULL, farrow = 0.1, fzoom = 5, ellipse = T, scaling = 2, spe = F)+
  scale_color_manual(name = "Groups",values = c("#A64995", "#BDD014", "#442D87", "#2B62FC", "#A331A0", "#720E34", "#CEC521", "#D80E51","#3FE8E0", "#E85105", "#E58910")) +
  scale_shape_manual(name = "Groups",values = c(16,16,16,16,16,16,16,16,16,16,16))) #for station names include: obslab = T, obssize = 2

return(ASV.clr.rda)

#province PERMANOVA
#ASV.ait.t <- t(ASV.aitchinson)

print(adonis2(
  formula = ASV.aitchinson ~ province,
  data = meta,
  method = "euclidean"
))


m_L_CHL <- meta%>%dplyr::filter(province %in% L_CHL)%>%cbind(chl_type = paste0("L_CHL"))
m_H_CHL <- meta%>%dplyr::filter(province %in% H_CHL)%>%cbind(chl_type = paste0("H_CHL"))

meta_c <- rbind(m_L_CHL, m_H_CHL)

ASV.aitchinson_r <- as.data.frame(as.matrix(ASV.aitchinson))
ASV.aitchinson_r <- ASV.aitchinson_r%>%dplyr::select(meta_c$Site)
ASV.aitchinson_r <- ASV.aitchinson_r%>%dplyr::filter(rownames(ASV.aitchinson_r) %in% meta_c$Site)
print(adonis2(
  formula = ASV.aitchinson_r ~ chl_type,
  data = meta_c,
  method = "euclidean"
))

#meta$province <- as.factor(meta$province)
#rownames(meta) <- meta$Site
#PERMANOVA(ASV.aitchinson, meta["province"])

mod <- betadisper(ASV.aitchinson, meta$province)

print(mod)

#clean up!
detach("package:vegan", unload=TRUE)
detach("package:ggords", unload=TRUE)
#detach("package:PERMANOVA", unload=TRUE)
}


RDA_partial_plots <- function(meta, ASV.clr, metaD, RDAcondition){
  
  #load additional packages
  suppressPackageStartupMessages(require("ggords"))
  suppressPackageStartupMessages(require("vegan"))

  ##ASV data prep (sorting according to metadata)
  ASV.clr.t <- t(ASV.clr)
  ASV.clr.t.sort <- as.data.frame(ASV.clr.t)
  ASV.clr.t.sort <- cbind(ASV.clr.t.sort, meta$Site)
  ASV.clr.t.sort <- with(ASV.clr.t.sort, ASV.clr.t.sort[order(meta$Site),])
  ASV.clr.t.sort$`meta$Site` <-NULL
  ASV.clr.t.sort <- as.matrix(data.matrix(ASV.clr.t.sort))
  meta.wf <- with(meta, meta[order(Site),])
  
  #meta.wf.data <- meta.wf[,which(names(meta.wf) %in% metaD)]
  meta.wf.data <- meta.wf[,-which(names(meta.wf) %in% c("Site","province", "geo_current"))]
  gr <- meta.wf$province
  grl <- factor(gr)
  
  gr2 <- meta.wf$geo_current
  curl.n <- factor(gr2)
  #RDA

  ASV.clr.rda <- rda(
    paste("ASV.clr.t.sort ~ ", paste(metaD, collapse = " + "), " + ", "Condition(",paste(RDAcondition, collapse = " + "),")") %>% as.formula(),
    data = meta.wf.data
  )
  ######
  print(ASV.clr.rda)
  
  invisible(hist(residuals(ASV.clr.rda), main = ""))
  
  ASV.clr.rda.anova <- anova.cca(ASV.clr.rda)
  print(ASV.clr.rda.anova)
  
  inertia.rda.tot <- ASV.clr.rda$tot.chi
  inertia.rda.tot
  inertia.rda.constrained <- ASV.clr.rda$CCA$tot.chi
  inertia.rda.constrained
  inertia.rda.constrained.prop <- inertia.rda.constrained/inertia.rda.tot
  print(inertia.rda.constrained.prop)
  
  #par(xpd=TRUE)
  #provinces
  print(ggrda(ASV.clr.rda,group = grl, spearrow = NULL, farrow = 0.1, fzoom = 5, ellipse = T, scaling = 2, spe = F)+
          scale_color_manual(name = "Groups",values = c("#A64995", "#BDD014", "#442D87", "#2B62FC", "#A331A0", "#720E34", "#CEC521", "#D80E51","#3FE8E0", "#E85105", "#E58910")) +
          scale_shape_manual(name = "Groups",values = c(16,16,16,16,16,16,16,16,16,16,16))) #for station names include: obslab = T, obssize = 2
  #geo_currents
#  print(ggrda(abundance.clr.rda,group = curl.n, spearrow = NULL, farrow = 0.1, fzoom = 5, ellipse = T, scaling = 2, spe = F)+
#          scale_color_manual(name = "Groups",values = c("#EDBA18", "#3FE8E0", "#BDD014", "#442D87", "#2B62FC", "#A331A0", "#720E34", "#28E091", "#2D9328", "#E85105", "#E58910",
#                                                        "#D6D361", "#19DD7F", "#949B97", "#2723CC", "#C323CC", "#601F63", "#2AB3D8", "#D82A47", "#318915", "#547749", "#E8ED29", "#A331A0","#2E6026"))+
#          scale_shape_manual(name = "Groups",values = c(19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19)))
  
  
  #clean up!
  detach("package:vegan", unload=TRUE)
  detach("package:ggords", unload=TRUE)

}




