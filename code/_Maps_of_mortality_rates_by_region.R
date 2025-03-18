# Map for any mortality rates listed below
# Data is pulled from "UNIGME202X_Country_Rates_&_Deaths.csv"
# can plot all countries or selected countries (regions)

# use `ggplot2` instead of base plot, `sf` instead of `sp`
# still has some issues with plotting Askai Chin using `geom_sf_pattern`

# 2023.08


colors_ind <- c(
  "U5MR"    = "Blues",
  "SBR"     = "Blues",
  "NMR"     = "Greens",
  "MR1t59"     = "OrRd",
  "MR1t11"  = "OrRd", 
  "CMR"     = "OrRd",  
  "10q10"   = "OrRd",
  "5q5"     = "Greens",
  "5q5"     = "Greens",
  "5q10"    = "Greens",  
  "5q15"    = "PuBu"
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

library("sf")
library("dplyr")
library("RColorBrewer")
library("ggplot2")
library("ggpattern")
library("data.table")

USERPROFILE <- Sys.getenv("USERPROFILE")

# working directory should be the project folder
work.dir <- file.path(USERPROFILE, "Dropbox/UNICEF Work/unmap/un_world_map")
map.rds.dir <- file.path(work.dir, "rds") # location of the shapefiles
output.dir.fig <- file.path(work.dir, "fig") # location to save the map


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

# load estimates
dir_CME_est <- file.path(USERPROFILE, "Dropbox/UNICEF Work/Data and charts for websites/Files 2023/CME/Estimates/")
dtc <- fread(file.path(dir_CME_est, "UNIGME2023_Country_Rates_&_Deaths.csv"))
dtc <- dtc[Year==2021.5 & Sex=="Total"]
dtc <- dtc[Shortind %in% inds]
dtc[, paste(range(round(Median)), collapse = "-"), by = Shortind]
dtc[, median(Median), by = Shortind] # get an idea of the range of indicators

dc <- fread(file.path(USERPROFILE, "Dropbox/UN IGME data/2023 Round Estimation/Code/input/country.info.CME.csv"))
dtc <- merge(dc[,.(ISO3Code, UNCode)], dtc, by.x = "ISO3Code", by.y = "ISO.Code")


# Data loading complete ---------------------------------------------------

# plot world or selected countries

# option 1. world map
ind0 <- "U5MR"
iso.subset.c = NULL

# option 2. region map
region0 <- "Eastern and South-Eastern Asia"
isos <- dc[SDGSimpleRegion2 == region0, unique(ISO3Code)]
iso.subset.c <- isos

make.world.map <- function(
    world.robin = world.robin.sf,
    ind0,
    iso.subset.c = NULL, # if NULL, plot world, otherwise, plot selected countries
    region.name  = "selected region", # a name used in filename for regional map
    plot.coastlines = FALSE, # do we want to show coastlines to see all small islands --- slow, set as FALSE, 
    save.pdf.also = FALSE   # want pdf or not, png is always saved 
    ){
  
  world.robin <- readRDS(file.path(map.rds.dir, "sp.world.robin.rds"))
  lks <- readRDS(file.path(map.rds.dir, "sp.lks.rds")) # lakes
  bnd <- readRDS(file.path(map.rds.dir, "sp.bnd.rds")) # borderline
  rks <- readRDS(file.path(map.rds.dir, "sp.RKS.boarder.rds")) # Kosovo
  
  world.robin <- st_as_sf(world.robin)
  lks <- st_as_sf(lks)
  bnd <- st_as_sf(bnd)
  rks <- st_as_sf(rks)
  
  stopifnot(ind0 %in% inds)
  color.palette <- dplyr::recode(ind0, !!!colors_ind)
  
  NumOfCategories <- 6 # Number of categories into which to split data, not including No data
  # should have 5 breaks: since NumOfCategories is 6
  category.breaks <- switch (ind0,
    # CME
    "U5MR"    = c(100, 75, 40, 25, 12),
    "NMR"     = c(40, 30, 20, 12, 5),
    # report:                         
    # "U5MR"    = c(100, 75, 50, 25, 10),
    # "NMR"     = c(35, 30, 25, 12, 5),
    "MR1t59"  = c(45, 35, 25, 15, 5),
    "IMR"     = c(25, 20, 12, 5, 2.5),
    "MR1t11"  = c(45, 35, 25, 15, 5),
    "CMR"     = c(15, 10, 7.5, 5, 2.5),
    "SBR"     = c(25, 20, 12, 5, 2.5),
    "5q5"     = c(15, 10, 7.5, 5, 2.5),
    "5q10"    = c(15, 10, 7.5, 5, 2.5),
    "5q15"    = c(15, 10, 7.5, 5, 2.5),
    "5q20"    = c(15, 10, 7.5, 5, 2.5)
  )
    
  # auto-create `legend.labels`, like: ">25" "20 to 25" "12 to 20" "5 to 12"  "2.5 to 5" "≤2.5" "No data"
  l1 <- category.breaks
  l2 <- shift(l1, 1)
  legend.labels <- paste(l1, "to", l2)
  legend.labels[1] <- paste0(">", l1[1])
  legend.labels[NumOfCategories] <- paste0("≤", l1[length(l1)])
  legend.labels[NumOfCategories+1] <- "No data"
  legend.title <- inds_rate_label_unit[[ind0]]
  
  
  
  
  # data --------------------------------------------------------------------
  # Prepare data
  indata <- dtc %>%
    filter(Shortind == ind0) %>%
    select(UNCode, ISO3Code, Median) %>%
    rename(M49COLOR = UNCode, vartomap = Median) %>% mutate(M49COLOR = as.character(M49COLOR))
  
  indata <- indata %>%
    mutate(vartomap = as.numeric(vartomap))
  
  # Attach data to world.robin sf object
  world.robin <- left_join(world.robin, indata, by = "M49COLOR")
  
  colors <- brewer.pal(NumOfCategories, color.palette) # (requires RColorBrewer package)
  colors <- colors[1:length(colors)] # eliminate the lightest hue as it tends not to map well (looks white in many palettes)
  colors <- rev(colors) # put the darkest hues first
  
  # Assign color codes based on category breaks
  world.robin <- world.robin %>%
    mutate(colorcode = case_when(
      vartomap >= category.breaks[1] ~ colors[1],
      vartomap >= category.breaks[2] ~ colors[2],
      vartomap >= category.breaks[3] ~ colors[3],
      vartomap >= category.breaks[4] ~ colors[4],
      vartomap >= category.breaks[5] ~ colors[5],
      vartomap < category.breaks[5] ~ colors[6],
      is.na(vartomap) ~ NoDataColor
    ))
  world.robin$colorcode[which(world.robin$TERR_NAME=="Greenland")] <- world.robin$colorcode[which(world.robin$TERR_NAME=="Denmark")] #Greenland should have same colorcode as Denmark
  world.robin$colorcode[which(world.robin$TERR_NAME=="Hong Kong")] <- world.robin$colorcode[which(world.robin$TERR_NAME=="China")] 
  world.robin$colorcode[which(world.robin$TERR_NAME=="Macro")]     <- world.robin$colorcode[which(world.robin$TERR_NAME=="China")] 
  
  
  table(world.robin$colorcode)
  
  # #08519C  #3182BD  #6BAED6  #9ECAE1  #C6DBEF  #EFF3FF darkgray 
  # 5        8       20       36       63      102       49 
    
    # note that not all legend categories exist
    legend.labels.exist <- legend.labels[which(colors %in% world.robin$colorcode)]
    if(!1 %in% which(colors %in% world.robin$colorcode)) legend.labels.exist[1] <- paste0(">", gsub( " .*$", "", legend.labels.exist[1]))
 
    # UN Cartography requires that the Askai Chin region be striped half in the color of China and half in the color of
    # Jammu-Kashmir (no data for most of UNPD purposes)
    # to do that, we create a separate polygon file that contains only the single region Aksai Chin and assign it the color of China for now
    # it will be layered on top of the map in a separate step
    ac <- world.robin[world.robin$TERR_NAME=="Aksai Chin",]
    ac$colorcode <- world.robin$colorcode[which(world.robin$TERR_NAME=="China")]
    
  
    # subset countries after setting colors 
    if(!is.null(iso.subset.c)) world.robin <- world.robin[world.robin$ISO3_CODE %in% iso.subset.c,]
    table(world.robin$colorcode)
    dt_color <- unique(data.table(fill_color = world.robin$colorcode, ISO3Code = world.robin$ISO3_CODE))
    
  
    # three styles of boundaries should be mapped: standard solid line, dashed line for undetermined boundaries, dotted for selected disputed boundaries
    # to do that, we create a separate dataframe with each containing boundaries of the same type
    bnd.line <- bnd[bnd$CARTOGRAPH=="International boundary line",]
    bnd.dash <- bnd[bnd$CARTOGRAPH=="Dashed boundary line" | bnd$CARTOGRAPH=="Undetermined international dashed boundary line",]
    bnd.dot <- bnd[bnd$CARTOGRAPH=="Dotted boundary line" | bnd$CARTOGRAPH=="Dotted boundary line (Abyei)",]
    bnd.ssd <- bnd[bnd$BDY_CNT01=="SDN" & bnd$BDY_CNT02=="SSD",] # Specify SSD-SDN boundaries and plot later to resolve issue of not showing in the original script 
    
    # Set plotting limits
    bbox <- st_bbox(world.robin)
    # x_min <- bbox[['xmin']]
    # x_max <- bbox[['xmax']]
    # y_min <- bbox[['ymin']]
    # y_max <- bbox[['ymax']]
    # 
    # # EAP is special, see hist(world.robin.df$long)
    # if("CHN" %in% world.robin$ISO3_CODE & !is.null(iso.subset.c)){
    #   x_min <- min(world.robin.df[world.robin.df$long>0,]$long)
    #   x_min_neg <- max(world.robin.df[world.robin.df$long<0,]$long)
    #   x_max <- max(world.robin.df[world.robin.df$long>0,]$long)
    # }
    
    BDLINE_WIDTH <- 0.05
    
    # Plot using ggplot2 with geom_sf
    gg <- ggplot() +
      geom_sf(data = world.robin, aes(fill = colorcode), color = NA) +
      geom_sf(data = lks, fill = "white", color = "white") +
      geom_sf(data = bnd.line, linetype = "solid",  linewidth = BDLINE_WIDTH, color = "white") +
      geom_sf(data = bnd.dash, linetype = "dashed", linewidth = BDLINE_WIDTH, color = "white") +
      geom_sf(data = bnd.dot,  linetype = "dotted", linewidth = BDLINE_WIDTH, color = "white") +
      geom_sf(data = bnd.ssd,  linetype = "dashed", linewidth = BDLINE_WIDTH, color = "white") +
      geom_sf(data = rks,      linetype = "dashed", linewidth = BDLINE_WIDTH, color = "white", fill = NA) +
      scale_fill_identity(guide = "legend", 
                          labels = legend.labels.exist, 
                          name = legend.title, drop = TRUE) +  # fill as it is , cannot keep all, 
      # coord_sf(xlim = c(x_min, x_max), ylim = c(y_min, y_max)) +
      coord_sf(xlim = c(bbox[['xmin']], bbox[['xmax']]), ylim = c(bbox[['ymin']], bbox[['ymax']])) +
      
      ggthemes::theme_map() +
      
      theme(legend.position = "right",
            # legend.justification = c("center"),
            legend.title = element_text(size=9),
            legend.text = element_text(size=9),
            plot.title = element_blank()) 
    
    # ggplot() +ggpattern::geom_sf_pattern(
    #   data = ac, aes(fill = colorcode[1]), pattern = "stripe", pattern_density = 0.05,    # Adjust stripe density
    #   pattern_angle = 45,       # Angle of stripes
    #   pattern_fill = ac$colorcode[1], # Color for stripes
    #   pattern_colour = NA       # No outline on stripes
    # ) +
    #   geom_sf(data = ac, color = "white", size = BDLINE_WIDTH, fill = NA)
    
      
    if("CHN" %in% world.robin$ISO3_CODE){
     gg <- gg + ggpattern::geom_sf_pattern(
       data = ac, aes(fill = colorcode[1]), pattern = "stripe", pattern_density = 0.001,    # Adjust stripe density
       pattern_angle = 45,       # Angle of stripes
       pattern_fill = ac$colorcode[1], # Color for stripes
       pattern_colour = NA       # No outline on stripes
     ) +  
       geom_sf(data = ac, color = "white", linewidth = BDLINE_WIDTH, fill = NA) 
    }

    # note: very slow
    if (plot.coastlines==TRUE) {
      gg <- gg + geom_sf(data = cst, linetype = "solid", linewidth = BDLINE_WIDTH, color = coastline.color)
    }
   
    ggmap_legend <- ggpubr::get_legend(gg)
    ggmap <- gg + theme(legend.position = "none")
    # group gg elements
    Grob0 <- grid::rectGrob(gp=grid::gpar(col="white"))
    
    if(is.null(iso.subset.c)){
    # glegend_col: `rel_heights` adjusts the vertical position of the legend: 
      glegend_col <- cowplot::plot_grid(Grob0, ggmap_legend, Grob0, ncol = 1, rel_heights = c(3,1,1))
      gg_group <- cowplot::plot_grid(glegend_col, ggmap, nrow = 1, rel_widths = c(1, 12))
    }
    
    # special for regional maps 
    if(!is.null(iso.subset.c)){
      glegend_col <- cowplot::plot_grid(Grob0, ggmap_legend, Grob0, ncol = 1, rel_heights = c(5,1,1))
      gg_group <- cowplot::plot_grid(glegend_col, ggmap, nrow = 1, rel_widths = c(1, 12))
      
      # for MDV, Maldives, downloaded from https://data.humdata.org/dataset/cod-ab-mdv?
      if("MDV" %in% iso.subset.c){
        sf.mdv <- st_read(file.path(map.dir, "script for unmap/mdv_admbnd_gov_20210329_shp/mdv_admbnda_adm0_gov_20210329.shp"))
        
        # Get the fill color for MDV from world.robin
        col.mdv <- world.robin$colorcode[which(world.robin$ISO3_CODE == "MDV")]
        
        # Create ggplot for MDV map
        gg.mdv <- ggplot() +
          geom_sf(data = sf.mdv, fill = col.mdv, color = "white") +
          theme_map() +
          theme(panel.border = element_rect(colour = "darkgrey", fill = NA))
        
        gg.mdv.l <- cowplot::plot_grid(Grob0, gg.mdv,  ncol = 2, rel_widths = c(2,1)) # a narrower map, MDV to the right
        
        glegend_col2 <- cowplot::plot_grid(Grob0, ggmap_legend, Grob0, gg.mdv.l, Grob0, 
                                           ncol = 1, rel_heights = c(1, 4, 1, 4,1)) 
        # gg_group <- cowplot::plot_grid(glegend_col, gg.mdv.l, ggmap, nrow = 1, rel_widths = c(1, 0.8, 10))
        gg_group <- cowplot::plot_grid(glegend_col2, ggmap, nrow = 1, rel_widths = c(3,  12))
      }
    }
    
  # Save --------------------------------------------------------------------
    
  
  if(is.null(iso.subset.c)){
    filename0 <- if(is.null(iso.subset.c)) paste0("UNIGME_Map_", ind0, "_", "world") 
    height0 <- 8
    width0 <- 16
  } else {
    range_x <- x_max - x_min
    range_y <- y_max - y_min
    x_y_ratio <- range_x / range_y
    
    filename0 <- paste0("UNIGME_Map_", ind0, "_", region.name)
    height0 <- 8
    width0 <- height0 * (x_y_ratio) * 1.2
  }
  filename_save_png <- file.path(output.dir.fig, paste0(filename0, ".jpg"))
  filename_save_pdf <- file.path(output.dir.fig, paste0(filename0, ".pdf"))
  
  ggsave(plot = gg_group, filename = filename_save_png, height = height0, width = width0,  dpi = 300)
  message("map saved to ", filename_save_png)
  
  if(save.pdf.also){
  ggsave(plot = gg_group, filename = filename_save_pdf, 
         height = height0, width = width0, dpi = 600)
    message("pdf map saved to ", filename_save_pdf)
  }
}

# option 1. world map
make.world.map(ind0 = "MR1t59", save.pdf.also = TRUE)

# run world maps for all indicators
# invisible(lapply(inds, make.world.map))


# option 2. region map
region0 <- "South Asia"
isos <- dc[UNICEFProgRegion1 == region0, unique(ISO3Code)]
make.world.map(ind0 = "MR1t59", iso.subset.c = isos, region.name = region0)


region0 <- "North America"
make.world.map(ind0 = "MR1t59", iso.subset.c = c("USA", "CAN"), region.name = region0)



# if want to run through all regions:
regions_UNICEF <- unique(dc$UNICEFProgRegion1)
plot.each.region <- function(region0){
  message("making maps for ", region0)
  isos <- dc[UNICEFProgRegion1 == region0, unique(ISO3Code)]
  make.world.map(ind0 = "SBR", iso.subset.c = isos, region.name = region0, save.pdf.also = TRUE)
}
# invisible(lapply(regions_UNICEF, plot.each.region))
