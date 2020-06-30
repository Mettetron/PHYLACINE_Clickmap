# Helper script for PhylacineClickmap shiny app. (needed for cmmon names in data table)
# Uses data from ncbi to create dataframe with common species names 
# for the species listed by latin names in  latin species names in Phylacine

library(taxize)
library(tictoc)

phy <- read.csv("https://raw.githubusercontent.com/MegaPast2Future/PHYLACINE_1.2/master/Data/Traits/Trait_data.csv", sep = ",")
phy.m <- phy[rev(order(phy$Mass.g)), ]  # sort by mass, heaviest first
phy.m.land <- phy.m[phy.m$Terrestrial == 1, ]  # only keep land mammals
# weight.limit <- 10 # set weight limit in kg
# phy.m.land <- phy.m.land[phy.m.land$Mass.g >= (weight.limit*1000), ]

# get species names, get uids, get common names
spec.latin <- gsub("_", " ", phy.m.land$Binomial.1.2)

comm_from_latin <- function(latin.v) {
  # using the package taxize to get common names for a vector of latin names
  uids <- get_uid(latin.v)
  coms <- sci2comm(uids, db = 'ncbi')
  # deal with the list format
  coms.NA <- lapply(coms, function(x) if(identical(x, character(0))) NA_character_ else x)
  coms.NA.v <- unlist(coms.NA)
  coms.NA.v
}

# getting around the need for an API key
tic()
comm.names.1000 <- comm_from_latin(spec.latin[1:1000])
toc() # 557.315 sec elapsed on macbook
comm.names.2000 <- comm_from_latin(spec.latin[1001:2000])
comm.names.3000 <- comm_from_latin(spec.latin[2001:3000])
comm.names.4000 <- comm_from_latin(spec.latin[3001:4000])
comm.names.5000 <- comm_from_latin(spec.latin[4001:length(spec.latin)])

comm.names.all <- c(comm.names.1000, comm.names.2000, comm.names.3000, 
                    comm.names.4000, comm.names.5000)


# make df
common.names.df <- data.frame(latin = phy.m.land$Binomial.1.2, 
                              uids = names(comm.names.all),
                              common = comm.names.all)

# export
write.csv(common.names.df, "PhylacineClickmap_commonNames.csv")
saveRDS(common.names.df, "PhylacineClickmap_commonNames.rds")



