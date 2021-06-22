
require(suncalc)

#metadata: list_meta$meta_functions.z
meta <- list_meta$meta_functions.z
meta$date <- as.POSIXct(meta$Datetime,format="%d.%m.%Y %H:%M", tz = "GMT")
meta$lat <- meta$latitude
meta$lon <- meta$Longitude
meta_sun_subset <- c("Site","date", "lat", "lon")

meta_sun <- meta[,meta_sun_subset]
meta_sun2 <-getSunlightPosition(data = meta_sun, keep = c("altitude", "azimuth"))
meta_sun2 <- dplyr::inner_join(meta_sun, meta_sun2, by = c("date", "lat", "lon"))

meta_Permutation <- dplyr::inner_join(meta[,RDA_meta], meta_sun2[,c("Site","altitude", "azimuth")], by = c("Site"))

#z-score altitude and azimuth
meta_Permutation[, c("altitude", "azimuth")] <- lapply(meta_Permutation[, c("altitude", "azimuth")], function(x) {y <-scale(x, center = TRUE, scale = TRUE)})

#meta_Permutation1 for RDA analyses
meta_Permutation1 <- meta_Permutation


meta_Permutation$province <- NULL
meta_Permutation$geo_current <- NULL


ordi <-function(ASV.clr, meta){
  
  require(vegan)
  ASV.clr.t <-t(ASV.clr)   #transpose
  #and sort (important to link the right stations)
  ASV.clr.t.sort <- as.data.frame(ASV.clr.t)
  ASV.clr.t.sort <- cbind(ASV.clr.t.sort, meta$Site)
  ASV.clr.t.sort <- with(ASV.clr.t.sort, ASV.clr.t.sort[order(meta$Site),])
  ASV.clr.t.sort$`meta_all$Site` <-NULL
  ASV.clr.t.sort <- as.matrix(data.matrix(ASV.clr.t.sort))
  rownames(meta) <- meta$Site
  meta$Site <- NULL
  mod0 <- vegan::rda(ASV.clr.t ~ 1, meta)
  mod1 <- vegan::rda(ASV.clr.t ~ ., meta)
  o <- ordiR2step(mod0, mod1, perm.max = 200, trace = FALSE)
  
  print(o)
}


