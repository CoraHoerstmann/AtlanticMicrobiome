##alpha diversity

diversity_iNEXT<-function(abundance){
  suppressPackageStartupMessages(require("dplyr"))
  suppressPackageStartupMessages(require("vegan"))
  require(iNEXT)
  print(packageVersion("iNEXT"))
  
iNEXT_diversity = iNEXT(abundance, q=0,datatype = "abundance", conf = 0.95,nboot = 10)
iNEXT_diversity = iNEXT_diversity$AsyEst
iNEXT_diversity_observed = iNEXT_diversity[,c(1:3)]
iNEXT_diversity_observed = spread(iNEXT_diversity_observed, Diversity, Observed)

iNEXT_diversity_observed = iNEXT_diversity_observed%>%
  dplyr::rename(Richness = `Species richness`)%>%
  dplyr::rename(Shannon = `Shannon diversity`)%>%
  dplyr::rename(Simpson = `Simpson diversity`)
iNEXT_diversity_observed$Site <- as.character(iNEXT_diversity_observed$Site)

iNEXT_diversity_observed <- as_tibble(iNEXT_diversity_observed)

return(iNEXT_diversity_observed)

detach("package:iNEXT", unload=TRUE)
detach("package:dplyr", unload=TRUE)
}
#unload

diversity_iNEXT_plots <- function(abundance){
  
  require(iNEXT)
  print(packageVersion("iNEXT"))
  
  abundance = ASVCount_16S.tax
  
  alpha.iNEXT.q0 <- iNEXT(abundance, q=0,datatype = "abundance", conf = 0.95,nboot = 100)
  df <- fortify(alpha.iNEXT.q0, type=1)
  
  df.point <- df[which(df$method=="observed"),]
  df.line <- df[which(df$method!="observed"),]
  df.line$method <- factor(df.line$method, 
                           c("interpolated", "extrapolated"),
                           c("interpolation", "extrapolation"))
  
  ggplot(df, aes(x=x, y=y, colour=site)) + 
    geom_line(aes(linetype=method), lwd=1.5, data=df.line) +
    geom_ribbon(aes(ymin=y.lwr, ymax=y.upr,
                    fill=site, colour=NULL), alpha=0.2) +
    labs(x="Number of individuals", y="Richness") +
    theme(legend.position = "bottom", 
          legend.title=element_blank(),
          text=element_text(size=18),
          legend.box = "vertical") 
  

  alpha.iNEXT.q1 <- iNEXT(abundance, q=1,datatype = "abundance", conf = 0.95,nboot = 100)
  df <- fortify(alpha.iNEXT.q1, type=1)
  
  df.point <- df[which(df$method=="observed"),]
  df.line <- df[which(df$method!="observed"),]
  df.line$method <- factor(df.line$method, 
                           c("interpolated", "extrapolated"),
                           c("interpolation", "extrapolation"))
  
  ggplot(df, aes(x=x, y=y, colour=site)) + 
    geom_line(aes(linetype=method), lwd=1.5, data=df.line) +
    geom_ribbon(aes(ymin=y.lwr, ymax=y.upr,
                    fill=site, colour=NULL), alpha=0.2) +
    labs(x="Number of individuals", y="Shannon entropy") +
    theme(legend.position = "bottom", 
          legend.title=element_blank(),
          text=element_text(size=18),
          legend.box = "vertical") 
  

  
  invisible(alpha.iNEXT.q2 <- iNEXT(abundance, q=2,datatype = "abundance", conf = 0.95,nboot = 100))
  df <- fortify(alpha.iNEXT.q2, type=1)
  
  df.point <- df[which(df$method=="observed"),]
  df.line <- df[which(df$method!="observed"),]
  df.line$method <- factor(df.line$method, 
                           c("interpolated", "extrapolated"),
                           c("interpolation", "extrapolation"))
  
  print(ggplot(df, aes(x=x, y=y, colour=site)) + 
    geom_line(aes(linetype=method), lwd=1.5, data=df.line) +
    geom_ribbon(aes(ymin=y.lwr, ymax=y.upr,
                    fill=site, colour=NULL), alpha=0.2) +
    labs(x="Number of individuals", y="inv Simpson index") +
    theme(legend.position = "bottom", 
          legend.title=element_blank(),
          text=element_text(size=18),
          legend.box = "vertical")) 
  

  
  #rm(iNEXT_diversity)
  detach("package:iNEXT", unload=TRUE)
  
}