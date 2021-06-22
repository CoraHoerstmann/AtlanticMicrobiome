#histograms of metadata


require(ggplot2)
library(rcompanion)


plotNormalHistogram(meta.all.meta$salinity,  main = "salinity")
plotNormalHistogram(meta.all.meta$SST,  main = "SST")
plotNormalHistogram(meta.all.meta$PO4_umol.l,  main = "PO4_umol.l")
plotNormalHistogram(meta.all.meta$Si_umol.l,  main = "Si_umol.l")
plotNormalHistogram(meta.all.meta$NO2_umol.l,  main = "NO2_umol.l")
plotNormalHistogram(meta.all.meta$NO3_umol.l,  main = "NO3_umol.l")
plotNormalHistogram(meta.all.meta$oxygen.umol.l.,  main = "oxygen.umol.l.")
plotNormalHistogram(meta.all.meta$Total.N..nmol.L.,  main = "Total.N..nmol.L.")
plotNormalHistogram(meta.all.meta$Total.C..nmol.L.,  main = "Total.C..nmol.L.")
plotNormalHistogram(meta.all.meta$C.N.Molar,  main = "C.N.Molar")
plotNormalHistogram(meta.all.meta$Distance_coast.km,  main = "Distance_coast.km")
plotNormalHistogram(meta.all.meta$distance_to_province_boundary.km.,  main = "Distance_province_boundary")
plotNormalHistogram(meta.all.meta$chlorophyll,  main = "chlorophyll")

##

