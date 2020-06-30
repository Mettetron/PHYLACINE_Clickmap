# Helper script for PhylacineClickmap shiny app. (needed for colored species density maps)
# Uses data from Phylacine (via make_speciesPerPixel.R) to create dataframe 
# Containing info on number of species for each map pixel 
# separated into: 
    # All: current+present-natural
    # Natural: Present-natural (where species would live without Anthropogenic pressures)
    # Current: Where species live today
    # Threatened: Subset of current witch IUCN status CR, EN or VU
    # Missing: Non-extinct species which would be in a place (=Present-natural), but arent (=not in Current)


# load species per pixel data frame (from make_speciesPerPixel.R)
s.px.df <- read.csv("PhylacineClickmap_speciesPerPixel.csv", row.names = 1)

# Trait table listing all species in PHYLACINE
phy <- read.csv("https://raw.githubusercontent.com/MegaPast2Future/PHYLACINE_1.2/master/Data/Traits/Trait_data.csv", sep = ",")


# Make number of species per pixel dataframe ------------------------------
# make dataframe with number of species belonging to each distribution for each pixel


library(tictoc)
tic()
all.sp.v <- c()
for(n in 1:nrow(s.px.df)) {
  all.sp.v <- c(all.sp.v, sum(s.px.df[n, -1] > 0))
}

natural.sp.v <- c()
for(n in 1:nrow(s.px.df)) {
  natural.sp.v <- c(natural.sp.v, sum(s.px.df[n, -1] == 1 | s.px.df[n, -1] == 3))
}

current.sp.v <- c()
for(n in 1:nrow(s.px.df)) {
  current.sp.v <- c(current.sp.v, sum(s.px.df[n, -1] > 1))
}

threatened.sp.v <- c()
threatened.sp <- phy[phy$Binomial.1.2 %in% colnames(s.px.df)[-1] & phy$IUCN.Status.1.2 %in% c("CR", "EN", "VU"), "Binomial.1.2"]
s.px.df.threat <- s.px.df[, c("coords", as.character(threatened.sp))]
for(n in 1:nrow(s.px.df)) {
  threatened.sp.v <- c(threatened.sp.v, sum(s.px.df.threat[n, -1] > 1))
}

missing.sp.v <- c()
living.sp <- phy[phy$Binomial.1.2 %in% colnames(s.px.df)[-1] & !phy$IUCN.Status.1.2 %in% c("EX", "EP"), "Binomial.1.2"]
s.px.df.live <- s.px.df[, c("coords", as.character(living.sp))]
for(n in 1:nrow(s.px.df)) {
  missing.sp.v <- c(missing.sp.v, sum(s.px.df.live[n, -1] == 1))
}

species.numbers.df <- data.frame(coords = s.px.df$coords,
                                 all = all.sp.v,
                                 natural = natural.sp.v,
                                 current = current.sp.v,
                                 threatened = threatened.sp.v,
                                 missing = missing.sp.v)

toc()  # 8521.692 sec elapsed

write.csv(species.numbers.df, "PhylacineClickmap_speciesNumbers.csv")

# smaller file type
# for faster load times and gitHub space limits
saveRDS(species.numbers.df, "PhylacineClickmap_speciesNumbers.rds")  # smaller file type


