# a simpler script to only highlight countries on the map
# plot countries on the map, does not load estimates

# 2024.06

library("data.table")
library("ggplot2")
library("sf")

USERPROFILE <- Sys.getenv("USERPROFILE")  
# working directory is project folder
work.dir <- file.path(USERPROFILE, "Dropbox/UNICEF Work/unmap/un_world_map")

map.rds.dir <- file.path(work.dir, "rds") # location of the shapefiles
output.dir.fig <- file.path(work.dir, "fig") # location to save the map

# loading data 
source(file.path(USERPROFILE, "Dropbox/UNICEF Work/profile.R"))
source(file.path(dir_SP, "IGME report/2024/Code_for_figures/_basic_setting_for_plot.R"))
dir_fig_infographic <- file.path(work_dir, "Figures/_infographic")   # figure for the report

dir_input <- file.path(dir_IGME, "input")
dc <- fread(file.path(dir_IGME, "2024 Round Estimation/Code/input/country.info.CME.csv"))

# selected countries listed here: 
# plot certain countries

# Part II. function for the maps -------------------------------------------------------

plot.selected.countries <- function(
  isos_selected = NULL,
  zoom_in = FALSE,
  color_selected  = NULL, # if NULL, use the default SDG color scheme, only valid if `isos_selected` is not NULL
  color_not_selected = "#D7D7D7", # color of the countries not selected
  filename = NULL,       # allow you to add a note in the filename, e.g. hiv countries
  output_dir = output.dir.fig
  
  ){
  colors_SDG <- c("Sub-Saharan Africa"            = "#E2231A",
                  "Central and Southern Asia"     = "#F26A21",
                  "Eastern and South-Eastern Asia"   = "#FFC20E",
                  "Northern Africa and Western Asia" = "#00833D",
                  "Latin America and the Caribbean"  = "#80BD41",
                  "Europe and Northern America"   = "#1CABE2",
                  "Australia and New Zealand"     = "#0058AB",
                  "Oceania (exc. Australia and New Zealand)" = "#6A1E74"
  )
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
  stopifnot(all(dc_regions$SDG_region %in% names(colors_SDG)))
  dc_regions[, fill_color := dplyr::recode(SDG_region, !!!colors_SDG)]
  
  # color scheme 
  if(!is.null(isos_selected)){
    dc_regions[!ISO3Code %in% isos_selected, fill_color := color_not_selected]
    
    if(!is.null(color_selected)){
      dc_regions[ISO3Code %in% isos_selected, fill_color := color_selected]
    }
  }

  world.robin <- merge(world.robin, dc_regions, by.x = "ISO3_CODE", by.y = "ISO3Code", all = TRUE)
  world.robin$fill_color[is.na(world.robin$fill_color)] <- "gray"
  
  BDLINE_WIDTH <- 0.2 
  
  # Set plotting limits
  bbox <- st_bbox(world.robin)
  
  # Plot using ggplot2 with geom_sf
  ggplot() +
    geom_sf(data = world.robin, aes(fill = fill_color), color = NA) +
    geom_sf(data = lks, fill = "white", color = "white") +
    geom_sf(data = bnd.line, linetype = "solid",  size = BDLINE_WIDTH, color = "white") +
    geom_sf(data = bnd.dash, linetype = "dashed", size = BDLINE_WIDTH, color = "white") +
    geom_sf(data = bnd.dot,  linetype = "dotted", size = BDLINE_WIDTH, color = "white") +
    geom_sf(data = bnd.ssd,  linetype = "dashed", size = BDLINE_WIDTH, color = "white") +
    geom_sf(data = rks,      linetype = "dashed", size = BDLINE_WIDTH, color = "white") +
    scale_fill_identity() +
    coord_sf(xlim = c(bbox[['xmin']], bbox[['xmax']]), ylim = c(bbox[['ymin']], bbox[['ymax']])) +
    ggthemes::theme_map() +
    theme(
      legend.position = "none",
      legend.direction = "horizontal",
      legend.justification = c("center"),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 9),
      plot.title = element_blank()
    )
  
  
  filename0 <- file.path(output_dir, paste0("UNIGME_map_", ifelse(is.null(filename), length(isos_selected), filename), "_countries"))
  if(is.null(isos_selected))  filename0 <- file.path(output_dir, "UNIGME_map_all_countries")

  ggsave(filename = paste0(filename0, ".png"), height = 4.5, width = 10, bg = "white")
  ggsave(filename = paste0(filename0, ".pdf"), height = 4.5, width = 10)
  message("Saved to ", filename0)
}

# examples
plot.selected.countries(isos_selected = NULL)

WBRegions <- unique(dc$WBRegion4)
for (r in c("Low income", "High income")){
  plot.selected.countries(isos_selected = dc[WBRegion4 == r, ISO3Code], color_selected = "darkred",
                          filename = r, output_dir = dir_fig_infographic)
}

unique(dc$SDGSimpleRegion2)
for (r in c("Sub-Saharan Africa")){
  plot.selected.countries(isos_selected = dc[SDGSimpleRegion1 == r, ISO3Code], color_selected = "darkblue",
                          filename = r, output_dir = dir_fig_infographic)
}
for (r in c("Australia and New Zealand")){
  plot.selected.countries(isos_selected = dc[SDGSimpleRegion2 == r, ISO3Code], color_selected = "darkblue",
                          filename = r, output_dir = dir_fig_infographic)
}

FCSCountries <- unique(dc$FCSCountries1)
for (r in c("Fragile and Conflict-affected Situation", "non-FCS")){
  plot.selected.countries(isos_selected = dc[FCSCountries1 == r, ISO3Code], color_selected = "#F26A21",
                          filename = r, output_dir = dir_fig_infographic)
}

# SDG target 

dt_target <- fread("C:/Users/yanliu/Dropbox/UNICEF Work/Data Requests/UNICEF PD Health/UN IGME 2024 Countries Obs.Req.ARR and Status_long.csv")
dt_target[, table(Shortind, value)]
# Shortind Acceleration Needed Achieved On Track
# MR1t59                  49      140       11
# NMR                     65      125       10
# U5MR                    60      133        7

iso_miss_U5MR <- dt_target[Shortind == "U5MR" & value == "Acceleration Needed", ISO3Code]
iso_miss_NMR  <- dt_target[Shortind == "NMR" & value == "Acceleration Needed", ISO3Code]

plot.selected.countries(isos_selected = iso_miss_U5MR, color_selected = "darkred",
                        filename = "Miss U5MR SDG", output_dir = dir_fig_infographic)
plot.selected.countries(isos_selected = iso_miss_NMR, color_selected = "darkred",
                        filename = "Miss NMR SDG", output_dir = dir_fig_infographic)
