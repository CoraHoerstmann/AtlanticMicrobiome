
## PCA and PCoA of metadata (excluding the Diversity Indices)


meta_PCA <- function(meta){
  
  require(ggbiplot)
  require(vegan)
meta_test_data <- meta[,3:12]
meta_test_groups <- meta[,2]
sites <- meta[, 1]
meta_test_a <- meta_test_data - min(meta_test_data) # the problem here was that I had negative values in my data, so what I did is adding everything by the most negative number of the dataset


meta.pca <- prcomp(meta_test_a, center = TRUE, scale. = TRUE)
print(meta.pca)
#plot(meta.pca, type = "l")
summary(meta.pca)


print(ggbiplot(meta.pca, obs.scale = 1, var.scale = 1, 
              groups = meta_test_groups, ellipse = TRUE, 
              circle = TRUE) +
  scale_color_manual(values = c("#A64995", "#BDD014", "#442D87", "#2B62FC", "#A331A0", "#720E34", "#CEC521", "#D80E51","#3FE8E0", "#E85105", "#E58910"))+
  geom_text(label = sites, size = 1.7)+
  theme(axis.title.x = element_text(size=12, vjust = 0.3),
        axis.title.y = element_text(size=12, vjust = 0.3),
        axis.text.y = element_text(size=12, vjust = 0.3),
        axis.text.x = element_text(size=12, vjust = 0.3),
        legend.text = element_text(size=12, vjust = 0.3),
        legend.title = element_text(size=12, vjust = 0.3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
        theme(legend.direction = 'horizontal', legend.position = 'top'))

detach("package:vegan", unload=TRUE)
detach("package:ggbiplot", unload=TRUE)


}

meta_PCoA <- function(meta_test_data, meta_test_group, sites){
  
  require(vegan)
  
  #meta_test_data <- meta[,3:12]
  #meta_test_groups <- meta[,2]
  #sites <- meta[, 1]
  
  meta_distance <- vegan::vegdist(meta_test_data, method = "euclidean", na.rm = FALSE)
  
  myPcoa <- cmdscale(meta_distance,k=2,eig = TRUE)
  
  x<-myPcoa$points[,1]
  y<-myPcoa$points[,2]
  
  #plot(x, y, pch = 19, xlim=range(x) + c(-0.1,0.1), ylim=range(y)+c(-0.1,0.1))
  #text(x, y, pos = 4, labels = rownames(meta_test_data), cex = 0.8)
  
  ### see variation captured in the PCoA axes
  barplot(myPcoa$eig)
  myPcoa$eig
  
  #clean up!
  detach("package:vegan", unload=TRUE)
  
  
}



