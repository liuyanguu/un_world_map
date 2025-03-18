# a simpler script to only highlight countries on the map
# plot countries on the map, does not load estimates

# 2024.06

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
dc <- fread(file.path(dir_IGME, "2024 Round Estimation/Code/input/country.info.CME.csv"))


# selected countries listed here: 
# plot certain countries groups 

dt_ava <- fread("D:/OneDrive - UNICEF/Documents - Child Mortality/IGME report/2024/Code_for_figures/draft/data_U5MR_ava_category.csv")
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
  zoom_in = FALSE,
  color_not_selected = "#D7D7D7", # color of the countries not selected
  filename = NULL       # allow you to add a note in the filename, e.g. hiv countries
  
  ){
  
  stopifnot("color" %in% colnames(data_in))
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
  # stopifnot(all(dc_regions$SDG_region %in% names(colors_SDG)))
  # dc_regions[, fill_color := dplyr::recode(SDG_region, !!!colors_SDG)]
  
  dc_regions <- merge(dc_regions, data_in, by.x = "ISO3Code", by.y = "Country.Code", all.x = TRUE)
  dc_regions[, fill_color := color]
  dc_regions[is.na(fill_color), fill_color := color_not_selected]

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
    
    scale_fill_identity(guide = "legend", labels = c("VR", "Survey only", "VR and survey"), name = "") +

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
  
  
  filename0 <- file.path(output.dir.fig, paste0("UNIGME_map_", ifelse(is.null(filename), length(isos_selected), filename), "_countries.png"))
  ggsave(filename = filename0, height = 4.5, width = 10, bg = "white", dpi = 300)
  message("Saved to ", filename0)
}

# # examples

# plot.selected.countries(isos_selected = CME.assistant::hiv.iso, filename = "hiv")
plot.selected.countries(isos_selected = NULL)
plot.selected.countries(isos_selected = c("ARG", "USA"), zoom_in = TRUE)


