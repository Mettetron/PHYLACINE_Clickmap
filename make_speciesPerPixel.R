# Helper script for PhylacineClickmap shiny app.
# Uses data from Phylacine to create dataframe containing info on both current 
# and present-natural distribution ranges of all terrestrial mammals in the data base. 
# Slow to run for full species list, can be modified for quicker runs on subsets.
# Shiny app gets too slow for full species list (4575), 
# so here we set weight limit (minimum body weight) to limit species number.

library(raster)

# Trait table listing all species in PHYLACINE
phy <- read.csv("https://raw.githubusercontent.com/MegaPast2Future/PHYLACINE_1.2/master/Data/Traits/Trait_data.csv", sep = ",")
phy.m <- phy[rev(order(phy$Mass.g)), ]  # sort by mass, heaviest first
phy.m.land <- phy.m[phy.m$Terrestrial == 1, ]  # only keep land mammals
weight.limit <- 10 # set weight limit in kg
phy.m.land <- phy.m.land[phy.m.land$Mass.g >= (weight.limit*1000), ]

# used to get species distribution ranges 
url <- "/vsicurl/https://github.com/MegaPast2Future/PHYLACINE_1.2/blob/master/Data/Ranges/"

# base raster for plot - world, no antarctica
w <- raster("continents.tif")  # in GitHub repository for Phylacine_Clickmap
w[!is.na(w)] <- 1 # all continents have same value

# get coordinates for all pixels covered by PHYLACINE (but only use the ones on land)
w.df <- as.data.frame(as(w, "SpatialPixelsDataFrame"))
# initialize dataframe including column with coords
w.df$coords <- paste0(w.df$x, "_", w.df$y)

# initialize data frame
s.px.df <- subset(w.df, select=-c(x, y, continents))

library(tictoc)
tic()
# go thorugh PHYLACINE ranges for all species and make dataframe with rows = pixels (x_y) 17284 total (because we are excluding oceans)
# and a column for each species, 0=never there, 1=pn, 2=currentOnly (i.e. introduced), 3=both (i.e retained)
n <- 1
for(species in phy.m.land$Binomial.1.2) {
  #r.pn <- raster(sprintf('%sPresent_natural/%s.tif?raw=true', url, species))  # universal but slow
  r.pn <- raster(file.path("..", "PHYLACINE_1.2", "Data", "Ranges", "Present_natural", paste0(species, ".tif"))) # only works is you have the whole PHYLACINE_1.2 directory in parent directory
  #r.cu <- raster(sprintf('%sCurrent/%s.tif?raw=true', url, species))  # universal but slow
  r.cu <- raster(file.path("..", "PHYLACINE_1.2", "Data", "Ranges", "Current", paste0(species, ".tif")))
  r.cu[r.cu == 1] <- 2
  r.ol <- r.pn+r.cu
  ol.df <- as.data.frame(as(r.ol, "SpatialPixelsDataFrame"))
  rownames(ol.df) <- paste0(ol.df$x, "_", ol.df$y)
  s.px.df[, species] <- ol.df[s.px.df$coords, "layer"]
  ol.df[, "layer"]
  print(paste(species, n))
  n <- n+1
}
toc()  # 2075.036 sec elapsed

# export to use in Shiny App
write.csv(s.px.df, sprintf("PhylacineClickmap_speciesPerPixel_%skg.csv", weight.limit))
