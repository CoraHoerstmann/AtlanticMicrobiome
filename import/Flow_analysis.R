## Flow Cytometry

## load files

setwd("C:/Users/choerstm/Documents/core_PS113/Flow_cytometry/")

flow.raw <- read.csv("./PS113_all_Flow_samples.csv", sep = ";")
meta <- read.csv("C:/Users/choerstm/Documents/core_PS113/Genomics/amplicon_data/Metadata/Metadata_genomics_all_all_cu.csv",
                 header=TRUE, sep= ",")

prok_meta <- as.list(as.character(meta$Station))

flow <- flow.raw%>%filter(Station %in% prok_meta)
flow.list <- flow$Station
meta_flow <- meta%>%filter(Station %in% flow.list)

flow$auto <- flow$Cyano2.Events.uL.+flow$Cyano1.Events.uL.+flow$big_cyanos.Events.uL.+flow$cyanos4.Events.uL.+
  flow$Euk1.Events.uL.+flow$Euk2.Events.uL.+flow$Euk3.Events.uL.+flow$Euk4.Events.uL.
flow$ratio.aut.het <- flow$auto/(flow$TotalCell.Events.uL.-flow$auto)
flow$ratio.het.aut <- (flow$TotalCell.Events.uL.-flow$auto)/flow$auto

#setting the thing up for barchart analysis

flow$hetero_1 <- (flow$TotalCell.Events.uL.-flow$auto)
flow$Cyanos_1 <- (flow$Cyano2.Events.uL.+flow$Cyano1.Events.uL.+flow$big_cyanos.Events.uL.+flow$cyanos4.Events.uL.)
flow$Euks_1 <- (flow$Euk1.Events.uL.+flow$Euk2.Events.uL.+flow$Euk3.Events.uL.+flow$Euk4.Events.uL.)

#sort tables

flow <- flow%>%arrange(Station)
meta_flow <- meta_flow%>%arrange(Station)
flow.analysis <- inner_join(flow, meta_flow, by = "Station")

##clean up!
rm(flow,flow.raw,meta,meta_flow,prok_meta, flow.list)



flow_plots <- function(flow_data){
  
  #load additional packages
  suppressPackageStartupMessages(require(scales))
  
  # 
  sp2<-ggplot(flow_data) + 
    geom_point(aes(x=latitude, y= 1, color=TotalCell.Events.uL.), size=0.75)+
    ylim(0.9,1.1)+
    scale_colour_gradient(low="lightgrey", high="darkblue")+
    theme(axis.title.x = element_text(size=10, vjust = 0.3),
          axis.title.y = element_text(size=10, vjust = 0.3),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size=8, vjust = 0.3),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    scale_x_continuous(name = "Latitude", breaks = seq(-50,50,10))+
    labs(y = "Total cell number")
  
  print(sp2)
  
  
  sp3<-ggplot(flow_data) + 
    geom_point(aes(x=latitude, y= 1, color=auto), size=0.75)+
    scale_colour_gradient(low="lightgrey", high="darkblue")+
    theme(axis.title.x = element_text(size=10, vjust = 0.3),
          axis.title.y = element_text(size=10, vjust = 0.3),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size=8, vjust = 0.3),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    scale_x_continuous(name = "Latitude", breaks = seq(-50,50,10))+
    labs(y = "Total cell number \n autotrophs")
    
  print(sp3)
  

  sp4<-ggplot(flow_data) + 
    geom_point(aes(x=latitude, y= 1, color=chlorophyll), size=0.75)+
    scale_colour_gradient(low="lightgrey", high="darkgreen")+
    theme(axis.title.x = element_text(size=10, vjust = 0.3),
          axis.title.y = element_text(size=10, vjust = 0.3),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size=8, vjust = 0.3),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    scale_x_continuous(name = "Latitude", breaks = seq(-50,50,10))+
    labs(y = "chlorophyll")
  
  print(sp4)
  
  #clean up!
  
}

