# Map for all mortality rates --- world map 
# using sf and base plot 

library("sf")
library("dplyr")
library("RColorBrewer")
library("data.table")

# working directory should be the project folder
work.dir <- getwd()
map.rds.dir <- file.path(work.dir, "rds") # location of the shapefiles
output.dir.fig <- file.path(work.dir, "fig") # location to save the map

colors_ind <- c(
  "U5MR"    = "Blues",
  "SBR"     = "Blues",
  "NMR"     = "Greens",
  "MR1t59"  = "OrRd",
  "MR1t11"  = "OrRd", 
  "CMR"     = "OrRd",  
  "10q10"   = "OrRd",
  "5q5"     = "Greens",
  "5q5"     = "Greens",
  "5q10"    = "Greens",  
  "5q15"    = "PuBu",
  "5q20"    = "PuBu"
)

inds_rate_label_unit  <- c("U5MR"   = "Under-five mortality rate\n(deaths per 1,000 live births)",  
                           "NMR"    = "Neonatal mortality rate\n(deaths per 1,000 live births)",
                           "CMR"    = "Child mortality rate age 1–4\n(deaths per 1,000 children aged 1)",
                           "MR1t11" = "1–11-months mortality rate\n(deaths per 1,000 children \naged 28 days)",
                           "MR1t59" = "1–59-months mortality rate\n(deaths per 1,000 children \naged 28 days)",
                           "5q5"    = "Mortality rate age 5–9\n(deaths per 1,000 children aged 5 years)",
                           "5q10"   = "Mortality rate age 10–14\n(deaths per 1,000 children aged 10 years)",
                           "10q10"  = "Mortality rate age 10–19\n(deaths per 1,000 adolescents \naged 10 years)",
                           "5q15"   = "Mortality rate age 15–19\n(deaths per 1,000 children aged 15 years)",
                           "Under.five.deaths" = "Number of under-five deaths (in thousands)",
                           "Neonatal.deaths"   = "Number of neonatal deaths (in thousands)",
                           "SBR"    = "Stillbirth rate\n (stillbirths per 1,000 total births)"
)
inds <- names(colors_ind)

# major settings
# colors for polygons with data
NoDataColor <- "darkgray" # color for polygons with no data
boundary.color <- "white" # color for country boundaries
background.color <- "white" # color for oceans, seas and lakes
coastline.color <- "grey"

# if want to plot coastline, default to FALSE, set TRUE to see small islands
plot.coastlines <- FALSE
  
# Read in the UN cartography polygon shapefiles --------------
world.robin <- readRDS(file.path(map.rds.dir, "sp.world.robin.rds"))
lks <- readRDS(file.path(map.rds.dir, "sp.lks.rds")) # lakes
bnd <- readRDS(file.path(map.rds.dir, "sp.bnd.rds")) # borderline
rks <- readRDS(file.path(map.rds.dir, "sp.RKS.boarder.rds")) # Kosovo
cst <- readRDS(file.path(map.rds.dir, "sp.cst.rds")) # coastlines

world.robin.sf <- st_as_sf(world.robin)
lks <- st_as_sf(lks)
bnd <- st_as_sf(bnd)
rks <- st_as_sf(rks)
cst <- st_as_sf(cst)

# Read in the data ----------------------------------------------------------
dtc <- fread(file.path(work.dir, "input/IGME_2023_rate_estimates.csv"))
dc <- setDT(readRDS(file.path(work.dir, "input/new_cnames.rds")))
dtc <- merge(dc[,.(ISO3Code, UNCode)], dtc, by.x = "ISO3Code", by.y = "ISO.Code")

# Source main function
source(file.path(work.dir, "R/make.world.map.R"))

make.world.map(ind0 = "U5MR")
make.world.map(ind0 = "MR1t59")
# invisible(lapply(inds, make.world.map))
