
ASV_PCA <- function(ASV_clr, meta){
require(ggbiplot)
require(vegan)
data <- t(ASV_clr)
data.plot <- as.data.frame(data)

essentials <- c("Site", "province")

data.plot$Site <- rownames(data.plot)
data.plot <- dplyr::left_join(data.plot, meta[,essentials], by = "Site")
meta_test_groups <- meta[,"province"]
sites <- meta[, "Site"]

positive <- function(x){
for(i in 1:nrow(x)){
      x[i,] <- (x[i,] - min(x[i,]))
}
  return(x)
}


data_a <- positive(data)

meta.pca <- prcomp(data_a, center = TRUE, scale. = TRUE)
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
