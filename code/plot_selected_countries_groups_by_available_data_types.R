# ==============================================================================
# Data Availability Mapping Script
# ==============================================================================
#
# Purpose:
#   Creates maps showing countries grouped by available data types for mortality indicators.
#   Visualizes data sources: survey data only, vital registration (VR) only, or both.
#
# Author: UNICEF IGME Team
# Created: 2025.06
# Last Modified: 2025.06
#
# Features:
#   - Categorizes countries by data availability (survey, VR, survey+VR)
#   - Color-coded map with custom legend for each data type
#   - Highlights data gaps and coverage for IGME reporting
#   - Uses UN cartography standards
#
# Dependencies:
#   - sf: spatial data handling
#   - ggplot2: map visualization
#   - data.table: data manipulation
#
# Data Sources:
#   - IGME database with series category information
#   - Country metadata with ISO codes
#
# Output:
#   Maps showing global data availability patterns for mortality estimation
# ==============================================================================

library("data.table")
library("ggplot2")
library("sf")

# working directory is project folder
work.dir <- file.path(USERPROFILE, "Dropbox/UNICEF Work/unmap/un_world_map")

map.rds.dir <- file.path(work.dir, "rds") # location of the shapefiles
output.dir.fig <- file.path(work.dir, "fig") # location to save the map

USERPROFILE <- Sys.getenv("USERPROFILE")
dc <- fread(file.path(USERPROFILE, "Dropbox/UN IGME data/2024 Round Estimation/Code/input/country.info.CME.csv"))


# import data  ------------------------------------------------------------
work_dir_report <- "D:/OneDrive - UNICEF/Documents - Child Mortality/IGME report/2025/Code_for_report"

dt_U5MR <- fread(file.path(work_dir_report, "db_U5MR_new_series_marked_2025.csv"))[Inclusion == 1]
dt_U5MR[, data_type  := "survey"]
dt_U5MR[Series.Category == "VR", data_type := "VR"]
dt_U5MR[Series.Category == "SVR", data_type := "VR"]
dt_U5MR[, data_ava := paste0(sort(unique(data_type)), collapse = "_"), by = Country.Code]
dt_U5MR[, table(data_ava)]
dt_ava <- unique(dt_U5MR[, .(Country.Code, Country.Name, data_ava)])
dt_ava[, color := dplyr::recode(data_ava, "survey" = "#F26A21", "VR" = "#80BD41", "survey_VR" = "#FFC20E")]


dt_ava[,.(Country.Code, color)]
# Country.Code   color
# 1:          AFG #F26A21
# 2:          ALB #FFC20E
# 3:          DZA #FFC20E
# 4:          AGO #F26A21
# 5:          AIA #80BD41

# Part II. function for the maps -------------------------------------------------------

plot.selected.countries <- function(
  data_in = dt_ava,
  isos_selected = NULL,
  zoom_in = FALSE,
  color_not_selected = "#D7D7D7", # color of the countries not selected
  filename = NULL       # allow you to add a note in the filename, e.g. hiv countries
  
  ){
  
  stopifnot("color" %in% colnames(data_in))
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
  dc_regions <- dc[ISO3Code != "LIE", .(ISO3Code, SDG_region)]
  # stopifnot(all(dc_regions$SDG_region %in% names(colors_SDG)))
  # dc_regions[, fill_color := dplyr::recode(SDG_region, !!!colors_SDG)]
  
  dc_regions <- merge(dc_regions, data_in, by.x = "ISO3Code", by.y = "Country.Code", all.x = TRUE)
  dc_regions[, fill_color := color]
  dc_regions[is.na(fill_color), fill_color := color_not_selected]
  dc_regions[, data_ava := factor(as.factor(data_ava), levels = c("survey", "survey_VR", "VR"))]
  setorder(dc_regions, data_ava)
  
  world.robin <- merge(world.robin, dc_regions, by.x = "ISO3_CODE", by.y = "ISO3Code", all = TRUE)
  world.robin$fill_color[is.na(world.robin$fill_color)] <- "darkgray"
  
  BDLINE_WIDTH <- 0.2 
  
  # Set plotting limits
  bbox <- st_bbox(world.robin)
  
  legend_order <- c("#F26A21" ="Survey and census only", "#FFC20E" =  "VR, survey and census", "#80BD41" = "VR",
                    "#D7D7D7" = "No included data", "darkgray" = "No data")
  world.robin$fill_color <- factor(as.factor(  world.robin$fill_color), levels = names(legend_order))
  
  # Plot using ggplot2 with geom_sf
  ggplot() +
    geom_sf(data = world.robin, aes(fill = fill_color), color = NA) +
    geom_sf(data = lks, fill = "white", color = "white") +
    geom_sf(data = bnd.line, linetype = "solid",  size = BDLINE_WIDTH, color = "white") +
    geom_sf(data = bnd.dash, linetype = "dashed", size = BDLINE_WIDTH, color = "white") +
    geom_sf(data = bnd.dot,  linetype = "dotted", size = BDLINE_WIDTH, color = "white") +
    geom_sf(data = bnd.ssd,  linetype = "dashed", size = BDLINE_WIDTH, color = "white") +
    geom_sf(data = rks,      linetype = "dashed", size = BDLINE_WIDTH, color = "white") +
    
    scale_fill_identity(guide = "legend", labels = legend_order, name = "") +

    coord_sf(xlim = c(bbox[['xmin']], bbox[['xmax']]), ylim = c(bbox[['ymin']], bbox[['ymax']])) +
    ggthemes::theme_map() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.justification = c("center"),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 9),
      plot.title = element_blank()
    ) + 
    guides(fill = guide_legend(nrow = 2, byrow = TRUE, order = 1))
  
  filename0 <- file.path(output.dir.fig, paste0("UNIGME_map_", ifelse(is.null(filename), length(isos_selected), filename), ".png"))
  ggsave(filename = filename0, height = 4.5, width = 10, bg = "white", dpi = 300)
  message("Saved to ", filename0)
}

plot.selected.countries(filename = "ava data types_included") 
