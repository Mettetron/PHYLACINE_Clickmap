# Helper script for PhylacineClickmap shiny app. (needed for cmmon names in data table)
# Uses data from ncbi to create dataframe with common species names 
# for the species listed by latin names in  latin species names in Phylacine

library(taxize)

phy <- read.csv("https://raw.githubusercontent.com/MegaPast2Future/PHYLACINE_1.2/master/Data/Traits/Trait_data.csv", sep = ",")
phy.m <- phy[rev(order(phy$Mass.g)), ]  # sort by mass, heaviest first
phy.m.land <- phy.m[phy.m$Terrestrial == 1, ]  # only keep land mammals
weight.limit <- 10 # set weight limit in kg
phy.m.land <- phy.m.land[phy.m.land$Mass.g >= (weight.limit*1000), ]

# get species names, get uids, get common names
spec.latin <- gsub("_", " ", phy.m.land$Binomial.1.2)
uids <- get_uid(spec.latin)
common.names <- sci2comm(uids, db = 'ncbi')

# deal with list format
common.names.NA <- lapply(common.names, function(x) if(identical(x, character(0))) NA_character_ else x)
common.names.NA.v <- unlist(common.names.NA)

# make df
common.names.df <- data.frame(latin = phy.m.land$Binomial.1.2, 
                              uids = names(common.names),
                              common=common.names.NA.v)

# export
write.csv(common.names.df, "PhylacineClickmap_commonNames_10kg.csv")



