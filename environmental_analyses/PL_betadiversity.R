require(plyr)


PL_div <- read.csv("C:/Users/choerstm/Documents/core_PS113/General_results/Supplementary/PL_diversity.csv")

# Convex hulls.
PL_core <- c("Station","province", "adcp_horiz", "specific_PP", "adcp_km_d", "biomass_transport")
PL_meta <- N_averaged[!is.na(N_averaged$adcp_horiz), ]
PL_meta$adcp_km_d <- PL_meta$adcp_horiz*1000/86400
PL_meta <- PL_meta[,PL_core]
PL_meta <- na.omit(PL_meta)
PL_meta <- droplevels(PL_meta)
df <- PL_meta
find_hull <- function(PL_meta){PL_meta[chull(PL_meta$adcp_km_d, PL_meta$specific_PP),]}
hulls <- ddply(df, "province", find_hull)

# 
fig2 <- ggplot(data=PL_meta, aes(x=adcp_km_d, y=specific_PP,colour=province, fill=province)) + 
  geom_point() + 
  #geom_text(aes(label=Station),hjust=0, vjust=0)+
  scale_colour_manual(values=c("#A64995", "#BDD014", "#442D87", "#2B62FC", "#A331A0", "#720E34", "#CEC521", "#D80E51","#3FE8E0", "#E85105", "#E58910")) +
  scale_fill_manual(values=c("#A64995", "#BDD014", "#442D87", "#2B62FC", "#A331A0", "#720E34", "#CEC521", "#D80E51","#3FE8E0", "#E85105", "#E58910")) +
  labs(x = "current speed", y = "specific PP")+
  theme(axis.title.x = element_text(size=16, vjust = 0.3),
        axis.title.y = element_text(size=16, vjust = 0.3),
        axis.text.y = element_text(size=16, vjust = 0.3),
        axis.text.x = element_text(size=16, vjust = 0.3, angle = 90),
        legend.text = element_text(size=16, vjust = 0.3),
        legend.title = element_text(size=16, vjust = 0.3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# Fig. 2B

#isoclines with PL

#require("plot3D")
#scatter3D(PL_meta$adcp_horiz, PL_meta$specific_PP, PL_meta$biomass_transport, phi = 0, bty = "g",  type = "h", 
#          ticktype = "detailed", pch = 19, cex = 0.5)

##countourplot

adcp_seq <- seq(.00, 0.01, by = .002)
PB_seq <- seq(.00, 0.1, by = .002)
soi.grid <- expand.grid(adcp_km_d = adcp_seq, specific_PP = PB_seq)
soi.grid$PL <- (log(2)/(soi.grid$specific_PP/23))*(soi.grid$adcp_km_d)


fig2a <- ggplot(data = PL_meta, aes(x = (adcp_km_d*1000), y = specific_PP)) + 
  geom_point(aes(colour = province, fill=province))+
  labs(x = "current speed [m d-1]", y = "specific PP [mg C m-3 d-1]")+
  scale_colour_manual(values=c("grey5","grey17","grey29", "grey41", "grey53", "grey17", "grey65", "grey77", "grey89",
                               "#A64995", "#BDD014", "#442D87", "#2B62FC", "#A331A0", "#720E34", "#CEC521", "#D80E51","#3FE8E0", "#E85105", "#E58910")) +
  scale_fill_manual(values=c("#A64995", "#BDD014", "#442D87", "#2B62FC", "#A331A0", "#720E34", "#CEC521", "#D80E51","#3FE8E0", "#E85105", "#E58910")) +
  theme(axis.title.x = element_text(size=16, vjust = 0.3),
        axis.title.y = element_text(size=16, vjust = 0.3),
        axis.text.y = element_text(size=16, vjust = 0.3),
        axis.text.x = element_text(size=16, vjust = 0.3, angle = 90),
        legend.text = element_text(size=16, vjust = 0.3),
        legend.title = element_text(size=16, vjust = 0.3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

fig2a <- fig2a+  geom_polygon(data=hulls, aes(color=province, fill=province), alpha=.2) 
  

fig2a <- fig2a + geom_contour(data = soi.grid, aes(z = PL, colour = factor(..level.. - 0.25)),
                            breaks = c(0.5,1,1.5,2,2.5,4,8,16, 22), size = 0.5)

print(fig2a)
#library(directlabels)

#direct.label(a)

# Convex hulls
#fig2b <- fig2 + geom_polygon(data=hulls, alpha=.2) 

#print(fig2b)

###

#turn into long format
PL_average <- PL_div[, c(1:3,6,9,12,15,18)]

PL_long <- gather(PL_average, key = "functional_group", value = "div_distance",
                  Average_auto.euk, Average.auto.cyano, Average.mixo, Average.euk.het, Average.prok.het)

fig2c <-PL_long%>%
  ggplot(aes(x=PL_km, y=div_distance, color=Province, shape=functional_group))+
  geom_point()+
  #geom_text(aes(label=station),hjust=0, vjust=0)+
  scale_color_manual(values=c("#A64995", "#BDD014", "#2B62FC", "#CEC521", "#D80E51", "#E85105", "#E58910"))+
  theme(axis.title.x = element_text(size=16, vjust = 0.3),
        axis.title.y = element_text(size=16, vjust = 0.3),
        axis.text.y = element_text(size=16, vjust = 0.3),
        axis.text.x = element_text(size=16, vjust = 0.3, angle = 90),
        legend.text = element_text(size=16, vjust = 0.3),
        legend.title = element_text(size=16, vjust = 0.3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

print(fig2c)