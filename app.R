# Phylacine Clickmap Shiny App

# Interactive shiny app that shows which species of land-living mammals exist 
# or could exist at any user-defined geographical location.

# requires:
    # "continents.tif" - in gitHub repository
    # "PhylacineClickmap_speciesPerPixel_10kg.csv" - in gitHub repository, made from PHYLACINE with "make_speciesPerPixel.R"
    # "PhylacineClickmap_speciesNumbers_10kg.csv" - in gitHub repository, made from PHYLACINE with "make_speciesNumbers.R"
    # "PhylacineClickmap_commonNames_10kg.csv" - in gitHub repository, made with "make_commonNames.R


library(shiny)
library(raster)
library(sf)
library(DT)

# Trait table listing all species in PHYLACINE
phy <- read.csv("https://raw.githubusercontent.com/MegaPast2Future/PHYLACINE_1.2/master/Data/Traits/Trait_data.csv", sep = ",")
phy.m <- phy[rev(order(phy$Mass.g)), ]  # sort by mass, heaviest first
phy.m.land <- phy.m[phy.m$Terrestrial == 1, ]  # only keep land mammals
weight.limit <- 10 # set weight limit in kg
phy.m.land <- phy.m.land[phy.m.land$Mass.g >= (weight.limit*1000), ]
# add common names
comm.n <- read.csv("PhylacineClickmap_commonNames_1kg.csv", row.names=1)
phy.m.land$commName <- comm.n$common

# base raster for plot - world, no antarctica
w <- raster("continents.tif")  # in GitHub repository for Phylacine_Clickmap
w[!is.na(w)] <- 1 # all continents have same value

# load species per pixel data frame (make_speciesPerPixel.R)
s.px.df <- read.csv("PhylacineClickmap_speciesPerPixel_10kg.csv", row.names = 1)

# load species number data frame (make_speciesNumbers.R) used for colored species denisty maps
sp.num.df <- read.csv("PhylacineClickmap_speciesNumbers_10kg.csv", row.names = 1)

# make rasters based on w, but with values taken from speciesNumbers df (coords match)
all.sp.r <- w
all.sp.r[all.sp.r] <- sp.num.df$all

natural.sp.r <- w
natural.sp.r[natural.sp.r] <- sp.num.df$natural

current.sp.r <- w
current.sp.r[current.sp.r] <- sp.num.df$current

threatened.sp.r <- w
threatened.sp.r[threatened.sp.r] <- sp.num.df$threatened

missing.sp.r <- w
missing.sp.r[missing.sp.r] <- sp.num.df$missing

# Define User Interface ---------------------------------------------------
ui <- fluidPage(
  
  title = "PHYLACINE - Mammal Habitats",
  
  h2("PHYLACINE - Mammal Habitats"),
  h4("Click map to reveal local species of large (1kg+) mammals"),
  plotOutput("map", click = "plot_click"),
  fluidRow(
    column(12, align="center",
           radioButtons('range_type', "Choose distribution", 
                        choices = c("All", "Natural", "Current", "Threatened", "Missing"), 
                        selected = "All",
                        inline = TRUE)
    )
  ),
  hr(),
  fluidRow(
    column(12,
           dataTableOutput('species_table')
    )
  ),
  fluidRow(
    column(2, offset = 10, 
           downloadButton("downloadData", "Download Table")
    )
  ),
  br(),
  fluidRow(style = "background-color:#eae9e9;",
    column(2,
           h5("IUCN status key:")
    ),
    column(3,
           h5("LC=Least Concern"), h5("NT=Near Threatened"), h5("DD=Data Deficient")
    ),
    column(3,
           h5("VU=Vulnerable"), h5("EN=Endangered"), h5("CR=Critically Endangered")
    ),
    column(3,
           h5("EW=Extinct in the Wild"), h5("EX=Extinct"), h5("EP=Extinct in Prehistory")
    )
  ),
  fluidRow(
    column(3, offset=3,
           a("PHYLACINE GitHub repository", href="https://github.com/MegaPast2Future/PHYLACINE_1.2")
    ),
    column(3,
           a("PHYLACINE original article", href="https://doi.org/10.1002/ecy.2443")
    )
  )
)


# Define Server -----------------------------------------------------------

server <- function(input, output) {
  output$map <- renderPlot({
    par(mar = c(0, 0, 0, 0))
    plot(w, col = "forestgreen", box = FALSE, axes = FALSE, legend = FALSE)
    if(input$range_type == "All") {
      range.col <- "gold2"
      sp.r <- all.sp.r
      sp.max <- max(values(sp.r), na.rm=TRUE)
      sp.mid <- round(max(values(sp.r), na.rm=TRUE)/2, 0)
      plot(sp.r, 
           col = colorRampPalette(c("white", range.col))(sp.max), 
           add = TRUE, legend = FALSE)
      legend("bottomleft", 
             title = "Number of species",
             legend = c(0, sp.mid, sp.max), 
             fill = c("white", colorRampPalette(c("white", range.col))(sp.max)[sp.mid], range.col),
             border = "black",
             box.lwd = 0, cex = 1.5)
    } else if (input$range_type == "Natural") {
      range.col <- "blue"
      sp.r <- natural.sp.r
      sp.max <- max(values(sp.r), na.rm=TRUE)
      sp.mid <- round(max(values(sp.r), na.rm=TRUE)/2, 0)
      plot(sp.r, 
           col = colorRampPalette(c("white", range.col))(sp.max), 
           add = TRUE, legend = FALSE)
      legend("bottomleft", 
             title = "Number of species",
             legend = c(0, sp.mid, sp.max), 
             fill = c("white", colorRampPalette(c("white", range.col))(sp.max)[sp.mid], range.col),
             border = "black",
             box.lwd = 0, cex = 1.5)
    } else if (input$range_type == "Current") {
      range.col <- "orange"
      sp.r <- current.sp.r
      sp.max <- max(values(sp.r), na.rm=TRUE)
      sp.mid <- round(max(values(sp.r), na.rm=TRUE)/2, 0)
      plot(sp.r, 
           col = colorRampPalette(c("white", range.col))(sp.max), 
           add = TRUE, legend = FALSE)
      legend("bottomleft", 
             title = "Number of species",
             legend = c(0, sp.mid, sp.max), 
             fill = c("white", colorRampPalette(c("white", range.col))(sp.max)[sp.mid], range.col),
             border = "black",
             box.lwd = 0, cex = 1.5)
    } else if (input$range_type == "Threatened") {
      range.col <- "red"
      sp.r <- threatened.sp.r
      sp.max <- max(values(sp.r), na.rm=TRUE)
      sp.mid <- round(max(values(sp.r), na.rm=TRUE)/2, 0)
      plot(sp.r, 
           col = colorRampPalette(c("white", range.col))(sp.max), 
           add = TRUE, legend = FALSE)
      legend("bottomleft",
             title = "Number of species",
             legend = c(0, sp.mid, sp.max), 
             fill = c("white", colorRampPalette(c("white", range.col))(sp.max)[sp.mid], range.col),
             border = "black",
             box.lwd = 0, cex = 1.5)
    }  else if (input$range_type == "Missing") {
      range.col <- "forestgreen"
      sp.r <- missing.sp.r
      sp.max <- max(values(sp.r), na.rm=TRUE)
      sp.mid <- round(max(values(sp.r), na.rm=TRUE)/2, 0)
      plot(sp.r, 
           col = colorRampPalette(c("white", range.col))(sp.max), 
           add = TRUE, legend = FALSE)
      legend("bottomleft", 
             title = "Number of species",
             legend = c(0, sp.mid, sp.max), 
             fill = c("white", colorRampPalette(c("white", range.col))(sp.max)[sp.mid], range.col),
             border = "black",
             box.lwd = 0, cex = 1.5)
    }
  })
  # tables of the species present
  spec_tab <- reactive({
    click.coord.x <- as.numeric(input$plot_click$x)
    click.coord.y <- as.numeric(input$plot_click$y)
    px.coords.x <- as.numeric(sapply(strsplit(as.character(s.px.df$coords), "_"), "[[", 1))
    px.coords.y <- as.numeric(sapply(strsplit(as.character(s.px.df$coords), "_"), "[[", 2))
    my.dists <- abs(px.coords.x - click.coord.x) + abs(px.coords.y - click.coord.y)
    if(min(my.dists) > 100000) {
      data.frame("No results here" = "Try clicking somewhere else")
    } else {
      nearest.px.coords <- s.px.df$coords[which(my.dists==min(my.dists))]
      my.row <- s.px.df[s.px.df$coords == nearest.px.coords, ][-1]
      if(input$range_type == "All") {
        click.species <- names(my.row)[my.row > 0]
      } else if(input$range_type == "Natural") {  # animals that could and do exist at location
        click.species <- names(my.row)[my.row == 1 | my.row == 3]
      } else if(input$range_type == "Current") {  # animals that currently exist at location
        click.species <- names(my.row)[my.row > 1]
      } else if(input$range_type == "Threatened") {  # animals that currently exist at location but are classified CR, EN or VU
        click.species <- names(my.row)[my.row > 1]
        click.species <- phy[phy$Binomial.1.2 %in% c(click.species) & phy$IUCN.Status.1.2 %in% c("CR", "EN", "VU"), "Binomial.1.2"]
      } else if(input$range_type == "Missing") {  # animals that are not extinct, and could exist at location (=present-natural range), but don't.
        click.species <- names(my.row)[my.row == 1]
        click.species <- phy[phy$Binomial.1.2 %in% c(click.species) & !phy$IUCN.Status.1.2 %in% c("EP", "EX"), "Binomial.1.2"]
      }
      show.me <- phy.m.land[phy.m.land$Binomial.1.2 %in% click.species, c("Order.1.2", "Family.1.2", "Binomial.1.2", "commName", "Mass.g", "IUCN.Status.1.2")]
      show.me$Mass.g <- round(show.me$Mass.g/1000, 1)
      colnames(show.me) <- c("Order", "Family", "Species", "Common_Name", "Mass_kg", "IUCN_Status")
      show.me
    }
  })
  # show datatable defined above
  output$species_table <- renderDataTable(spec_tab(), 
                                          options = list(sDom  = '<"top">lrt<"bottom">ip'))  # get rid of default search box
  # Downloadable csv of selected datatable
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("PhylacineClickmap_", input$range_type, "_dataTable_myLocation", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(spec_tab(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)

