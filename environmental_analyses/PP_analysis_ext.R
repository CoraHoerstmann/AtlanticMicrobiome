
#load datasets
N <- read.csv("C:/Users/choerstm/Documents/core_PS113/PrimaryProductivity/PS113_PP_outliers_removed_specific_adcp.csv", header=TRUE, sep= ";")
UW <- read.csv("C:/Users/choerstm/Documents/core_PS113/Nutrients/PS113_UW_Nutrients.csv", header=TRUE, sep= ";")

#calculate the means for technical replicates
N_av <- N%>%
  dplyr::group_by(Station)%>%
  dplyr::summarise(mean_c.fix = mean(C.fixation_nmol.L.h, na.rm = TRUE), std = sd(C.fixation_nmol.L.h, na.rm = TRUE), 
            mean_chla = mean(Chl.a_conc, na.rm = TRUE), std_chla = sd(Chl.a_conc, na.rm = TRUE),
            mean_cUptake = mean(Uptake..mgC.m3.d., na.rm = TRUE))

N_av_sub <- N%>%
  filter(Replicate == "1")

N_averaged <- inner_join(N_av, N_av_sub[c(1:7,11, 15:18, 20)], by = "Station")

rm(N_av, N_av_sub)

##calculate the biomass transport
#log = ln in R by default!

N_averaged$biomass_transport <- log(2)/(N_averaged$mean_cUptake/(N_averaged$mean_chla*23))*(N_averaged$adcp_horiz*1000/86400)

N_averaged$specific_PP <- (N_averaged$mean_cUptake/N_averaged$mean_chla)
N_averaged$specific_PP_mol <- ((N_averaged$mean_c.fix/1000)/(N_averaged$mean_chla*1000))

### calculate the averages  of provinces

PP_averaged_province <- N_averaged%>%dplyr::group_by(province)%>%
  dplyr::summarise(mean_c.fix_p = mean(mean_c.fix, na.rm = TRUE), std_p = sd(mean_c.fix, na.rm = TRUE), n_PP = n(),
            mean_chla_p = mean(mean_chla, na.rm = TRUE), std_chla = sd(mean_chla, na.rm = TRUE), n_chla = n(),
            mean_transport = mean(biomass_transport, na.rm = TRUE), sd_transport = sd(biomass_transport, na.rm = TRUE))


#write.csv(N_averaged, "PP_transport_calculations.csv") #this is used for qgis

# nutrients

UWA<-UW[-c(39,178,190,192,193,194),]
UWA<-transform(UWA,N.P = as.numeric(as.character(N.P)))

nutrients <- inner_join(UWA, N_averaged[c(1,13)], by = "Station")

nutrients_averaged_station <- nutrients%>%group_by(Station)%>%
  summarise(mean_NO3 = mean(NO3.umol.l, na.rm = TRUE), sd_N = sd(NO3.umol.l, na.rm = TRUE), n = n(),
            mean_PO4 = mean(PO4..µmol.l., na.rm = TRUE), sd_P = sd(PO4..µmol.l., na.rm = TRUE))

nutrients_averaged_station <- inner_join(nutrients_averaged_station,N_averaged[c(1,13)], by = "Station")

nutrients_averaged_province <- nutrients_averaged_station%>%dplyr::group_by(province)%>%
  dplyr::summarise(mean_NO3 = mean(mean_NO3, na.rm = TRUE), sd_N = sd(mean_NO3, na.rm = TRUE), n = n(),
            mean_PO4 = mean(mean_PO4, na.rm = TRUE), sd_P = sd(mean_PO4, na.rm = TRUE))

rm(UW, UWA, nutrients, nutrients_averaged_province)

#write.csv(N_averaged, "PS113_N_averaged.csv")
