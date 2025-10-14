# ==============================================================================
# Country Groups Mapping Script
# ==============================================================================
#
# Purpose:
#   Creates maps showing multiple country groups with custom colors for each group.
#   Allows visualization of data availability, program regions, or any custom groupings.
#
# Author: UNICEF IGME Team
# Created: 2025.06
# Last Modified: 2025.06
#
# Features:
#   - Multiple country groups with distinct colors
#   - Custom legend labels and colors for each group  
#   - Option to zoom into selected groups or show world context
#   - Uses UN cartography standards and Robinson projection
#
# Dependencies:
#   - sf: spatial data handling
#   - ggplot2: map visualization
#   - data.table: data manipulation
#
# Usage:
#   1. Define data_in with country groupings and colors
#   2. Set legend_label with group names and colors
#   3. Call plot.selected.countries() with parameters
#   4. Maps saved automatically to fig/ directory
# ==============================================================================

library("data.table")
library("ggplot2")
library("sf")

# working directory is project folder
USERPROFILE <- Sys.getenv("USERPROFILE")  
work.dir <- file.path(USERPROFILE, "Dropbox/UNICEF Work/unmap/un_world_map")

map.rds.dir <- file.path(work.dir, "rds") # location of the shapefiles
output.dir.fig <- file.path(work.dir, "fig") # location to save the map


# loading data 
source(file.path(USERPROFILE, "Dropbox/UNICEF Work/profile.R"))
dc <- fread(file.path(dir_IGME, "2025 Round Estimation/Code/input/country.info.CME.csv"))


# selected countries listed here: 
# plot certain countries groups 
# for example: 
dt_ava <- fread(file.path(dir_SP, "IGME report/2025/Code_for_figures/draft/data_U5MR_ava_category.csv"))
unique(dt_ava[,.(data_ava, color)])
#   data_ava     color
#   1:    survey #F26A21
#   2: survey_VR #FFC20E
#   3:        VR #80BD41

legend_labels <- c(
  "Survey data only" = "#F26A21",
  "Survey and VR data" = "#FFC20E",
  "VR data only" = "#80BD41"
)

# Part II. function for the maps -------------------------------------------------------

plot.selected.countries <- function(
    isos_selected = NULL,
    data_in = NULL,
    legend_label = legend_labels, 
    zoom_in = FALSE,                # zoom into the plotted countries, if FALSE, plot the whole world 
    color_not_selected = "#D7D7D7", # color of the countries not selected
    filename = NULL,      # allow you to add a note in the filename, e.g. hiv countries
    output_format = "png" # output format: "png" or "pdf"
    
){
  
  stopifnot("color" %in% colnames(data_in))
  stopifnot("Country.Code" %in% colnames(data_in))
  
  if(is.null(isos_selected) & zoom_in) isos_selected <- unique(data_in$Country.Code)
  
  # colors_SDG <- c("Sub-Saharan Africa"            = "#E2231A",
  #                 "Central and Southern Asia"     = "#F26A21",
  #                 "Eastern and South-Eastern Asia"   = "#FFC20E",
  #                 "Northern Africa and Western Asia" = "#00833D",
  #                 "Latin America and the Caribbean"  = "#80BD41",
  #                 "Europe and Northern America"   = "#1CABE2",
  #                 "Australia and New Zealand"     = "#0058AB",
  #                 "Oceania (exc. Australia and New Zealand)" = "#6A1E74"
  # )
  # 
  world.robin <- readRDS(file.path(map.rds.dir, "sp.world.robin.rds"))
  world.robin$ISO3_CODE[which(world.robin$TERR_NAME=="Greenland")] <- "DNK"
  world.robin$ISO3_CODE[which(world.robin$TERR_NAME=="Hong Kong")] <- "CHN"
  lks <- readRDS(file.path(map.rds.dir, "sp.lks.rds")) # lakes
  bnd <- readRDS(file.path(map.rds.dir, "sp.bnd.rds")) # borderline
  rks <- readRDS(file.path(map.rds.dir, "sp.RKS.boarder.rds")) # Kosovo
  
  world.robin <- st_as_sf(world.robin)
  lks <- st_as_sf(lks)
  bnd <- st_as_sf(bnd)
  rks <- st_as_sf(rks)
  
  bnd.line <- bnd[bnd$CARTOGRAPH=="International boundary line",]
  bnd.dash <- bnd[bnd$CARTOGRAPH=="Dashed boundary line" | bnd$CARTOGRAPH=="Undetermined international dashed boundary line",]
  bnd.dot <- bnd[bnd$CARTOGRAPH=="Dotted boundary line" | bnd$CARTOGRAPH=="Dotted boundary line (Abyei)",]
  bnd.ssd <- bnd[bnd$BDY_CNT01=="SDN" & bnd$BDY_CNT02=="SSD",] # Specify SSD-SDN boundaries and plot later to resolve issue of not showing in the original script 
  
  
  
  if(zoom_in){
    isos_selected[!isos_selected %in% world.robin$ISO3_CODE] 
    world.robin <- world.robin[world.robin$ISO3_CODE %in% isos_selected,]
  }
  
  # load data 
  dc[, SDG_region:= ifelse(SDGSimpleRegion1 != "Oceania", SDGSimpleRegion1, SDGSimpleRegion2)]
  dc_regions <- dc[, .(ISO3Code, SDG_region)]
  # stopifnot(all(dc_regions$SDG_region %in% names(colors_SDG)))
  # dc_regions[, fill_color := dplyr::recode(SDG_region, !!!colors_SDG)]
  
  dc_regions <- merge(dc_regions, data_in, by.x = "ISO3Code", by.y = "Country.Code", all.x = TRUE)
  dc_regions[, fill_color := color]
  dc_regions[is.na(fill_color), fill_color := color_not_selected]
  
  # countries outside UNIGME --- marked as grey 
  world.robin <- merge(world.robin, dc_regions, by.x = "ISO3_CODE", by.y = "ISO3Code", all = TRUE)
  world.robin$fill_color[is.na(world.robin$fill_color)] <- "grey"
  
  BDLINE_WIDTH <- 0.2
  LINE_COLOR <- "white" 
  
  # Set plotting limits
  bbox <- st_bbox(world.robin)
  
  # Plot using ggplot2 with geom_sf
  ggplot() +
    geom_sf(data = world.robin, aes(fill = fill_color), color = NA) +
    geom_sf(data = lks, fill = "white", color = LINE_COLOR) +
    geom_sf(data = bnd.line, linetype = "solid",  size = BDLINE_WIDTH, color = LINE_COLOR) +
    geom_sf(data = bnd.dash, linetype = "dashed", size = BDLINE_WIDTH, color = LINE_COLOR) +
    geom_sf(data = bnd.dot,  linetype = "dotted", size = BDLINE_WIDTH, color = LINE_COLOR) +
    geom_sf(data = bnd.ssd,  linetype = "dashed", size = BDLINE_WIDTH, color = LINE_COLOR) +
    geom_sf(data = rks,      linetype = "dashed", size = BDLINE_WIDTH, color = LINE_COLOR) +
    
    scale_fill_identity(guide = "legend", 
                        breaks = legend_label,
                        labels = names(legend_label), 
                        name = "") +
    
    coord_sf(xlim = c(bbox[['xmin']], bbox[['xmax']]), ylim = c(bbox[['ymin']], bbox[['ymax']])) +
    ggthemes::theme_map() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.justification = c("center"),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 9),
      plot.title = element_blank()
    )
  
  
  # Determine file extension and settings based on output format
  file_ext <- ifelse(output_format == "pdf", ".pdf", ".png")
  filename0 <- file.path(output.dir.fig, paste0("UNIGME_map_", ifelse(is.null(filename), length(isos_selected), filename), "_countries", file_ext))
  
  if(output_format == "pdf") {
    ggsave(filename = filename0, height = 4.5, width = 10, bg = "white", device = "pdf")
  } else {
    ggsave(filename = filename0, height = 4.5, width = 10, bg = "white", dpi = 300, device = "png")
  }
  message("Saved to ", filename0)
}

# 
plot.selected.countries(data_in = dt_ava, filename = "Figure 3 for report", zoom_in = TRUE, output_format = "png") 
# plot.selected.countries(data_in = dt_ava, filename = "Figure 3 for report", zoom_in = TRUE, output_format = "pdf") 