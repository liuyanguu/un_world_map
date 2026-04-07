# ==============================================================================
# MAIN EXAMPLE: World Mortality Rate Mapping
# ==============================================================================
#
# Purpose:
#   Main demonstration script for creating world maps of child mortality indicators.
#   Shows how to use the UN cartography mapping framework for IGME reports.
#   This is the primary entry point for generating standard mortality rate maps.
#
# Author: UNICEF IGME Team
# Created: 2024
# Last Modified: 2025.10
#
# What This Script Does:
#   1. Loads and configures all mortality indicators and color schemes
#   2. Sets up UN cartography shapefiles and projections
#   3. Loads IGME 2025 mortality estimates
#   4. Generates world maps for specified indicators
#   5. Saves high-quality outputs for reports and publications
#
# Key Features:
#   - Uses modern 'sf' package for spatial data (replaces legacy 'sp')
#   - Applies UN cartography standards and Robinson projection
#   - Supports all IGME mortality indicators (U5MR, NMR, CMR, etc.)
#   - Generates publication-ready maps with proper legends
#   - Handles special territories and disputed regions correctly
#
# Quick Start:
#   1. Ensure working directory is the project folder
#   2. Run the script - it will generate U5MR and MR1t59 maps by default
#   3. Check the fig/ directory for output files
#   4. Modify the indicator codes at the bottom to generate other maps
#
# Dependencies:
#   - sf: modern spatial data handling
#   - dplyr: data manipulation
#   - RColorBrewer: color palettes
#   - data.table: efficient data operations
#   - Custom functions in R/make.world.map.R
#
# Input Data:
#   - IGME 2025 rate estimates (input/IGME_2025_rate_estimates.csv)
#   - Country metadata (input/new_cnames.rds)
#   - UN cartography shapefiles (rds/ directory)
#
# Output:
#   - High-resolution PNG maps in fig/ directory
#   - Optional PDF vector files for graphic design
# ============================================================================== 

# Load required libraries =====================================================
library("sf")              # Modern spatial data framework (replaces sp)
library("dplyr")            # Data manipulation and filtering
library("RColorBrewer")     # Professional color palettes for maps
library("data.table")       # Fast and efficient data operations

# Setup directories and paths =================================================
# Note: Working directory should be the project folder
USERPROFILE <- Sys.getenv("USERPROFILE")

work.dir <- file.path(USERPROFILE, "Dropbox/UNICEF Work/unmap/un_world_map")
map.rds.dir <- file.path(work.dir, "rds")        # UN cartography shapefiles
output.dir.fig <- file.path(work.dir, "fig")     # Output directory for maps

# Load custom mapping functions ===============================================
source(file.path(work.dir, "R/make.world.map.R"))
source(file.path(USERPROFILE, "Dropbox/UNICEF Work/profile.R"))  # Loads IGME directory path

# Map Styling Configuration ===============================================

# Color scheme for map elements (following UN cartography standards)
NoDataColor <- "darkgray"      # Countries/areas with missing data
boundary.color <- "white"      # Country and territory boundaries
background.color <- "white"    # Oceans, seas, and large water bodies
coastline.color <- "grey"      # Coastal outlines for small islands

# Coastline display option
# Set FALSE for faster rendering; TRUE to show all small islands and details
plot.coastlines <- FALSE
  
# Load UN Cartography Shapefiles ==========================================

# Load pre-processed spatial data (Robinson projection applied)
# These files were created by script_process_sp_to_rds.R from original UN shapefiles
world.robin <- readRDS(file.path(map.rds.dir, "sp.world.robin.rds"))        # World country polygons
lks <- readRDS(file.path(map.rds.dir, "sp.lks.rds"))                        # Lakes and water bodies
bnd <- readRDS(file.path(map.rds.dir, "sp.bnd.rds"))                        # Country boundaries
rks <- readRDS(file.path(map.rds.dir, "sp.RKS.boarder.rds"))                # Kosovo borders (special case)
cst <- readRDS(file.path(map.rds.dir, "sp.cst.rds"))                        # Coastlines

# Convert legacy sp objects to modern sf format for better performance
world.robin.sf <- st_as_sf(world.robin)
lks <- st_as_sf(lks)
bnd <- st_as_sf(bnd) 
rks <- st_as_sf(rks)
cst <- st_as_sf(cst)

# Load Data ======================================================
dt_input <- fread(file.path(dir_IGME, "2025 Round Estimation/Code/input/dataPostAdj_U5MR.csv"))
dt_input[, Median:= uniqueN(year.adj), by = countrycode.adj]
dtc <- unique(dt_input[,.(countrycode.adj, Median)])
setnames(dtc, c("ISO3Code", "Median")) 

# Load country metadata with ISO codes and UN codes for mapping
dc <- setDT(readRDS(file.path(work.dir, "input/new_cnames.rds")))

# Merge mortality data with country codes for spatial mapping
dtc <- merge(dc[,.(ISO3Code, UNCode)], dtc, by.x = "ISO3Code", by.y = "ISO3Code")
dtc[, Shortind := "crisis"]

NumOfCategories <- 7
category.breaks <- c(12, 10,  8, 6,  4,  2)

colors <- RColorBrewer::brewer.pal(NumOfCategories, "OrRd")
l1 <- category.breaks
l2 <- shift(l1, 1)
legend.labels <- paste(l2, "to", l1)
legend.labels[1] <- paste0(">", l1[1])
legend.labels[NumOfCategories] <- paste0("≤", l1[length(l1)])
legend.labels[NumOfCategories+1] <- "No adjustment"


make.world.map(ind0 = "crisis",
               my_legend_title = "Count of crisis country-years",
               my_category_break = category.breaks,
               my_category_labels = legend.labels,
               my_color_palette = colors
               ) 

