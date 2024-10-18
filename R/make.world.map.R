# main function

# plotting mortality indicators
make.world.map <- function(
    world.robin = world.robin.sf, 
    ind0, 
    dir.save = output.dir.fig,
    map_num = NULL
    ){
  
  stopifnot(ind0 %in% names(colors_ind))
  legend.title <- inds_rate_label_unit[[ind0]]
  color.palette <- dplyr::recode(ind0, !!!colors_ind)
  # map legend breaks
  category.breaks <- switch (ind0,
                             
                             "SBR"     = c(35, 30, 25, 12, 5),
                             "NMR"     = c(35, 30, 25, 12, 5),
                             "MR1t59"  = c(45, 30, 20, 13, 5),
                             "MR1t11"  = c(45, 35, 25, 15, 5),
                             "CMR"     = c(45, 35, 25, 15, 5),
                             "U5MR"    = c(100, 75, 50, 25, 10),
                             "5q5"     = c(15, 10, 7.5, 5, 2.5),
                             "5q10"    = c(15, 10, 7.5, 5, 2.5),
                             "5q15"    = c(15, 10, 7.5, 5, 2.5),
                             "10q10"   = c(20, 15, 10, 5, 2.5)
  )
  # Number of categories into which to split data, not including No data
  NumOfCategories <- length(category.breaks) + 1
  
  l1 <- category.breaks
  l2 <- shift(l1, 1)
  legend.labels <- paste(l1, "to", l2)
  legend.labels[1] <- paste0(">", l1[1])
  legend.labels[NumOfCategories] <- paste0("≤", l1[length(l1)])
  legend.labels[NumOfCategories+1] <- "No data"
  
  
  # data --------------------------------------------------------------------
  # Prepare data
  required_columns <- c("ISO3Code", "UNCode", "Shortind", "Median")
  stopifnot(all(required_columns %in% colnames(dtc)))
  
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
  # e.g. for U5MR: 
  # #08519C  #3182BD  #6BAED6  #9ECAE1  #C6DBEF  #EFF3FF darkgray 
  # 5        8       20       36       63      102       49 
  
  # UN Cartography requires that the Askai Chin region be striped half in the color of China and half in the color of
  # Jammu-Kashmir (no data for most of UNPD purposes)
  # to do that, we create a separate polygon file that contains only the single region Aksai Chin and assign it the color of China for now
  # it will be layered on top of the map in a separate step
  ac <- world.robin[world.robin$TERR_NAME=="Aksai Chin",]
  ac$colorcode <- world.robin$colorcode[which(world.robin$TERR_NAME=="China")]
  
  
  # three styles of boundaries should be mapped: standard solid line, dashed line for undetermined boundaries, dotted for selected disputed boundaries
  # to do that, we create a separate dataframe with each containing boundaries of the same type
  bnd.line <- bnd[bnd$CARTOGRAPH=="International boundary line",]
  bnd.dash <- bnd[bnd$CARTOGRAPH=="Dashed boundary line" | bnd$CARTOGRAPH=="Undetermined international dashed boundary line",]
  bnd.dot <- bnd[bnd$CARTOGRAPH=="Dotted boundary line" | bnd$CARTOGRAPH=="Dotted boundary line (Abyei)",]
  bnd.ssd <- bnd[bnd$BDY_CNT01=="SDN" & bnd$BDY_CNT02=="SSD",] # Specify SSD-SDN boundaries and plot later to resolve issue of not showing in the original script 
  
  # write the map function
  unpd.map <- function(){
    # Plot world.robin with filled colors and background
    plot(st_geometry(world.robin), border = NA, col = world.robin$colorcode, bg = background.color, reset = FALSE)
    
    # Extract coordinates for Aksai Chin region and plot it with hatching
    ac_coords <- st_coordinates(ac)
    polygon(ac_coords[,1], ac_coords[,2], col = ac$colorcode[1], border = NA, density = 130, angle = 45, lwd = 0.4)
    
    # Plot boundaries with different styles
    plot(st_geometry(lks), add = TRUE, col = background.color, border = boundary.color, lwd = 0.2, lty = 2)      # lakes
    plot(st_geometry(bnd.line), add = TRUE, col = boundary.color, lwd = 0.2, lty = 1) # Solid boundaries
    plot(st_geometry(bnd.dash), add = TRUE, col = boundary.color, lwd = 0.2, lty = 2) # Dashed boundaries
    plot(st_geometry(bnd.dot), add = TRUE, col = boundary.color, lwd = 0.2, lty = 3)  # Dotted boundaries
    plot(st_geometry(bnd.ssd), add = TRUE, col = boundary.color, lwd = 0.2, lty = 2)  # SSD-SDN boundary
    plot(st_geometry(rks), add = TRUE, border  = boundary.color, lwd = 0.2, lty = 2)  # Kosovo boundary
    
    # Plot lakes in the background color
    for (gp in unique(lks$Name)) {
      lk <- lks[lks$Name == gp, ]
      polygon(lk$long, lk$lat, col = background.color, border = boundary.color, lty = 1, lwd = 0.2)
    }
    
    # Optional coastlines
    if (plot.coastlines) {
      plot(st_geometry(cst), add = TRUE, col = coastline.color, lwd = 0.2, lty = 1)
    }
    
    legend(-16820000, -1000000, col=c(colors, NoDataColor), pt.bg = c(colors, NoDataColor), pch = 15, pt.cex = 2, cex = 0.7, 
           legend = legend.labels, title = legend.title, box.lty = 0, box.col = "white", bty = 'o', bg = 'white')
  }
  
  unpd.map()
  
  
  # Save --------------------------------------------------------------------
  filename0 <- paste0("Map", "_", ind0) # e.g. Map_U5MR.png
  if(!is.null(map_num))   filename0 <- paste0("Map ", map_num, "_", ind0) # e.g. Map 1_U5MR.png
  file_path <- file.path(dir.save, paste0(filename0, ".png"))
  
  png(file = file_path, width = 10, height = 4.5, units = "in", res = 300)
  par(mfrow = c(1,1), omi = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), mgp = c(2, 0.5, 0), 
      las = 0, mex = 1, cex = 1, cex.main = 1, cex.lab = 1, cex.axis = 1)
  unpd.map()
  dev.off() # close the png
  
  file_path <- file.path(dir.save, paste0(filename0, ".pdf"))
  
  # if use cairo_pdf, can print out the "≤" but resolution of lines is strange 
  # cairo_pdf(file = file_path, width = 10, height = 4.5)
  # par(mfrow = c(1,1), omi = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), mgp = c(2, 0.5, 0), 
  #     las = 0, mex = 1, cex = 1, cex.main = 1, cex.lab = 1, cex.axis = 1)
  # unpd.map()
  # dev.off() # close the pdf
  
  
  pdf(file = file_path, width = 10, height = 4.5)
  par(mfrow = c(1,1), omi = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), mgp = c(2, 0.5, 0), 
      las = 0, mex = 1, cex = 1, cex.main = 1, cex.lab = 1, cex.axis = 1)
  unpd.map()
  dev.off() # close the pdf
  
  message("map saved to ", file_path)
  
}


