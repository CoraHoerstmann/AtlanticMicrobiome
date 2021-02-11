#plot the PP data and calculate significance


p<- ggplot(N_averaged, aes(x=Lat, y=mean_c.fix)) + 
  geom_bar(fill = "white",stat="identity", color="black", 
           position=position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin=mean_c.fix-std, ymax=mean_c.fix+std), width=.2,
                position=position_dodge(.9)) +
  theme(axis.title.x = element_text(size=8, vjust = 0.3),
        axis.title.y = element_text(size=8, vjust = 0.3),
        axis.text.y = element_text(size=8, vjust = 0.3),
        axis.text.x = element_text(size=8, vjust = 0.3, angle = 90),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x = "Latitude [°N]", y = "C fix [umol L-1*h-1]")+
  scale_x_continuous(breaks=seq(-70,70,5))+
  scale_y_continuous(breaks=seq(0,500,100))
print(p)

rm(p)

f<- ggplot(N_averaged, aes(x=Lat, y=biomass_transport)) + 
  geom_bar(fill = "white",stat="identity", color="black", 
           position=position_dodge(), width = 0.9) +
  theme(axis.title.x = element_text(size=8, vjust = 0.3),
        axis.title.y = element_text(size=8, vjust = 0.3),
        axis.text.y = element_text(size=8, vjust = 0.3),
        axis.text.x = element_text(size=8, vjust = 0.3, angle = 90),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x = "Latitude [°N]", y = "productivity specific length scale [km]")+
  scale_x_continuous(breaks=seq(-70,70,5))


print(f)

rm(f)



rm(p)

##travel vs. PP

ab <- ggplot(N_averaged, aes(x=biomass_transport, y=specific_PP), fill = "province")
ab <- ab + scale_color_manual(values = c("#A64995", "#BDD014", "#442D87", "#2B62FC", "#439B91",
                                         "#720E34", "#CEC51F", "#D80E51", "#3FE8E0", "#E85105", "#E58910")) +
  geom_point(aes(color = province), size = 2.5, alpha = 0.9, stroke = 1) +
  ylim(0,0.12)+
  labs(x = "productivity specific length scale", y = " specific C fix [mg C/m3*h]/[mg chl.a]")+
  theme(axis.title.x = element_text(size=8, vjust = 0.3),
        axis.title.y = element_text(size=8, vjust = 0.3),
        axis.text.y = element_text(size=8, vjust = 0.3),
        axis.text.x = element_text(size=8, vjust = 0.3, angle = 90),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

print(ab)


#correlation test PP and chla
print(cor.test(N_averaged$mean_c.fix, N_averaged$mean_chla, method = "pearson"))

WTRA <- N_averaged%>%dplyr::filter(province == "WTRA")

print(cor.test(WTRA$mean_c.fix, WTRA$mean_chla, method = "pearson"))

# significance tests between provinces
all.o <- c("SATL-COLD", "SATL-HOT", "WTRA", "NATR", "FKLD", "BRAZ", "CNRY", "NAST-E", "NADR")

L_CHL_pp <- N_averaged%>%dplyr::filter(province %in% L_CHL)
H_CHL_pp <- N_averaged%>%dplyr::filter(province %in% H_CHL)
all_o_pp <- N_averaged%>%dplyr::filter(province %in% all.o)
CNRY <- N_averaged%>%dplyr::filter(province == "CNRY")
NASTE <- N_averaged%>%dplyr::filter(province == "NAST-E")
SATLCOLD <- N_averaged%>%dplyr::filter(province == "SATL-COLD")
SATLHOT <- N_averaged%>%dplyr::filter(province == "SATL-HOT")

print(CNRY)

print(wilcox.test(CNRY$mean_c.fix, all_o_pp$mean_c.fix))
#print(wilcox.test(NASTE$mean_c.fix, CNRY$mean_c.fix))
print(wilcox.test(L_CHL_pp$mean_c.fix, H_CHL_pp$mean_c.fix))
print(wilcox.test(L_CHL_pp$mean_chla, H_CHL_pp$mean_chla))
#print(wilcox.test(SATLCOLD$mean_c.fix, SATLHOT$mean_c.fix))

rm(L_CHL_pp, H_CHL_pp, CNRY, all.o, f, g)