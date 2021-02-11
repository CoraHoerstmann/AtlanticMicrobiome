#residulas (the lm function redoes the correlation analysis as linear regression)
corr_plot_residuals <- function(meta){


lm_f <- function(mat) {
  library(rcompanion)
  myList <- list()
  k <- 1 #create an object as another counter
  #mat <- as.matrix(mat)
  n <- ncol(mat)
  for (i in 1:n) {
    for (j in 1:n) {
      myList[[k]] <- lm(mat[, i] ~ mat[, j], data = mat)
      names(myList)[k]<- paste(colnames(mat)[i],colnames(mat)[j], sep="_")
      k <- k+1
    }
  }
 return(myList) 
}

a <-lm_f(meta)
a.residuals <<- lapply(a, residuals)
#model = lm(Si_umol.l ~ Shannon.het.euk,
#           data = meta)


##do the following two steps better in RNotebook
#library(rcompanion)

#plotNormalHistogram(a.residuals$salinity_chlorophyll)

}


plot_correlations <- function(meta, xi, yi){
  
  library(ggplot2)
  
  a <- meta%>%
    ggplot(aes(x = xi, y = yi))+
    geom_point(alpha=8/10, shape=21, colour="black", size=5)+
    #scale_fill_manual(values=c("#F6A316","#73B3E3","#0C649A","#107734", "#6CB52D")) +
    theme(axis.title.x = element_text(size=16, vjust = 0.3),
          axis.title.y = element_text(size=16, vjust = 0.3),
          axis.text.y = element_text(size=16, vjust = 0.3),
          axis.text.x = element_text(size=16, vjust = 0.3),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  print(a)
  
  
}





