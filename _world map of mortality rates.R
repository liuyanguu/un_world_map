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

# Mortality Indicator Configuration ==========================================

# Color palette mapping for each mortality indicator
# Uses ColorBrewer palettes optimized for data visualization and accessibility
colors_ind <- c(
  "U5MR"    = "Blues",       # Under-five mortality rate
  "SBR"     = "Blues",       # Stillbirth rate
  "NMR"     = "Greens",      # Neonatal mortality rate  
  "MR1t59"  = "OrRd",        # 1-59 months mortality rate
  "MR1t11"  = "OrRd",        # 1-11 months mortality rate
  "CMR"     = "OrRd",        # Child mortality rate (1-4 years)
  "10q10"   = "OrRd",        # Adolescent mortality rate (10-19 years)
  "5q5"     = "Greens",      # Child mortality rate (5-9 years)
  "5q5"     = "Greens",      # Duplicate entry - should be reviewed
  "5q10"    = "Greens",      # Child mortality rate (10-14 years)
  "5q15"    = "PuBu",        # Adolescent mortality rate (15-19 years)
  "5q20"    = "PuBu"         # Young adult mortality rate (20-24 years)
)

# Human-readable labels and units for map legends
# These appear in the legend titles and are formatted for multi-line display
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

# Extract indicator codes for processing
inds <- names(colors_ind)

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

# Load Mortality Data ======================================================

# Data preparation note:
# The commented code below shows how to extract latest estimates from the master file.
# This has already been done and saved to the input/ directory for convenience.
#
# results_country_dir <- file.path(USERPROFILE, "Dropbox/UNICEF Work/Data and charts for websites/Files 2025/CME/Estimates/")
# dtcCME <- fread(file.path(results_country_dir, "UNIGME2025_Country_Rates.csv"))
# dtc <- dtcCME[Year == 2024.5 & Sex== "Total"]  # Filter to most recent year, both sexes
# fwrite(dtc, file.path(work.dir, "input/IGME_2025_rate_estimates.csv"))

# Load IGME mortality rate estimates (year 2024.5, both sexes combined)
dtc <- fread(file.path(work.dir, "input/IGME_2024_rate_estimates.csv"))

# Load country metadata with ISO codes and UN codes for mapping
dc <- setDT(readRDS(file.path(work.dir, "input/new_cnames.rds")))

# Merge mortality data with country codes for spatial mapping
dtc <- merge(dc[,.(ISO3Code, UNCode)], dtc, by.x = "ISO3Code", by.y = "ISO3Code")

# Generate World Maps ======================================================

# Create maps for key mortality indicators
# Output files are automatically saved to fig/ directory

# Primary indicators for IGME reporting:
make.world.map(ind0 = "U5MR")      # Under-five mortality rate - flagship indicator
make.world.map(ind0 = "MR1t59")    # 1-59 months mortality rate - complementary measure

# To generate maps for ALL available indicators, uncomment the line below:
# invisible(lapply(inds, make.world.map))

# Available indicators for individual mapping:
# "U5MR", "NMR", "CMR", "MR1t11", "MR1t59", "5q5", "5q10", "10q10", "5q15", "SBR"
# Example: make.world.map(ind0 = "NMR") for neonatal mortality rate
