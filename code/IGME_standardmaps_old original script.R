# ==============================================================================
# ⚠️  LEGACY SCRIPT - DEPRECATED ⚠️
# ==============================================================================
#
# ⚠️  THIS SCRIPT IS OUTDATED AND NO LONGER MAINTAINED
#
# Original Purpose:
#   Created world maps with country polygons colored by continuous variables
#   (e.g., Proportion Urban, TFR, life expectancy at birth)
#
# DEPRECATION NOTICE:
#   This script uses outdated packages and methods that are no longer supported:
#   - Uses 'sp' package (superseded by 'sf')
#   - Uses base R plotting (replaced with ggplot2)
#   - Uses 'rgdal' package (RETIRED as of Oct 2023)
#
# MIGRATION GUIDANCE:
#   ✅ Use instead: _Maps_of_mortality_rates_by_region.R
#   
#   New script provides:
#   - Modern 'sf' package for spatial data
#   - ggplot2 for better visualization
#   - Improved UN cartography compliance
#   - Better performance and maintainability
#   - Enhanced documentation
#
# Original Author: Sara Hertog
# Original Date: 29 September 2014
# Last Modified: 25 June 2015
# Deprecated: 2024
#
# Historical Note:
#   Shapefiles were originally stored in V:\unmap\2012SHP1M
#   Output formats were PDF (vector), PNG low-res (web), PNG high-res (Word/PPT)
# ==============================================================================

# Loading required packages - run this if you do not already have the packages installed
#install.packages(c("sp","RColorBrewer","ggplot2","rgdal","scales", "classInt"),dependencies=TRUE)


# Modify the fields below as needed to the specifications of your map
#####################################################################
#####################################################################
rm(list=ls())
USERPROFILE
set.path <- "C:/Users/lyhel/Dropbox/UNICEF Work/"
map.dir <- paste0(set.path, "unmap")# location of the shapefiles
#data.dir <- "C:/Users/dsharrow/Dropbox/2017 Reports/IGME Report/Data for charts/Wealth.Q5U5MR.csv"  # where to find the csv file with locid and variable to be mapped
out.dir <- "C:/Users/lyhel/Dropbox/UNICEF Work/unmap/fig_maps" # where to save the output map files
# name of the output png and pdf files that will contain your maps:
# map.name <- "U5MR" 
# map.name <- "NMR" 
map.name <- "mr10t19" 

# aesthetics
  # Sequential palette options:
  # "Blues","BuGn","BuPu","GnBu","Greens","Greys","Oranges","OrRd","PuBu","PuBuGn","PuRd","Purples","RdPu","Reds","YlGn","YlGnBu","YlOrBr","YlOrRd"
  #color.palette <- c("firebrick4","orange","dodgerblue","olivedrab3")
  # colors for polygons with data 
  # NoDataColor <- "#e6e8ed" # color for polygons with no data
  NoDataColor <- "darkgray" # color for polygons with no data
  boundary.color <- "white" # color for country boundaries
  background.color <- "white" # color for oceans, seas and lakes
  
  # NumOfCategories <- 6 # Number of categories into which to split data, not including No data # U5MR
  # category.breaks <- c(6,5,4,3,2,1) # break points from highest to lowest
  
  NumOfCategories <- 5 # Number of categories into which to split data, not including No data # NMR
  category.breaks <- c(5,4,3,2,1) # break points from highest to lowest
    # category.breaks <- c(15, 13, 10, 5, 2, 1) #5to9
    # category.breaks <- c(10, 8, 6, 4, 2, 1) #10to14
    # category.breaks <- c(40, 30, 20, 15, 10, 5) #15to24
    # category.breaks <- c(12, 10, 8, 6, 4, 2) #15to19
    # category.breaks <- c(20, 12, 8, 6, 4, 2) #20to24
  # category.breaks <- c(100,75,50,25,10) #U5MR
  #category.breaks <- c(75,50,25)
  #category.breaks <- c(40,30,20,12)
   #main.title <- "SDG target achievemnet of 12 deaths per 1,000 live births by 2030, by country"
  
  # legend.title <-"Under-five mortality rate \n (deaths per 1,000 live births)"
  # legend.title<-"Neonatal mortality rate \n (deaths per 1,000 live births)"
  legend.title<-"Probability of dying among adolescents \n aged 10-19 (deaths per 1,000 adolescents aged 10)"
    # legend.title <-"15 to 24"
    # legend.title <-"15 to 19"
    # legend.title <-"20 to 24"
  # legend.title <-"10 to 14"
  #legend.title<-"Probability of dying among children aged 5-14 years \n (deaths per 1,000 children aged 5)"
  # legend.title <-"Reduction in under-five mortality 1990 -2019 (%)"
  
  # legend.labels <-c(">100","75 to 100","50 to 75","25 to 50", "10 to 25","<=10","No data") # U5MR
  # legend.labels<-c(">35","25 to 35","12 to 25","5 to 12","<=5","No data") #NMR
  legend.labels<-c(">20","10 to 20","5 to 10","2.5 to 5","<=2.5","No data") #MR 10 to 19
    # legend.labels <-c(">10 to 12",">8 to 10",">6 to 8",">4 to 6", ">2 to 4", "<=2","No data") #
    # legend.labels <-c(">20" ,">12 to 20",">8 to 12",">6 to 8",">4 to 6", ">2 to 4", "<=2","No data") #
    # legend.labels <-c(">40",">30 to 40",">20 to 30",">15 to 20", ">10 to 15",">10 to 5", "<=5","No data") #
  # legend.labels <-c(">10",">8 to 10",">8 to 6",">4 to 6", ">2 to 4", "<=2","No data") #
  #legend.labels <-c(">=40",">30 to 40",">20 to 30",">12 to 20","<=12","No data")
  #legend.labels<-c(">=40",">30 to 40",">20 to 30",">10 to 20","<=10","No data")
  #legend.labels<-c(">30","20 to 30","10 to 20","5 to 10","1 to 5","<=1","No data")
  #legend.labels <- c("More than 75","More than half","More than 25","Less than 25","No data")
  source.text <- "Data source: UN Inter-agency Group for Child Mortality Estimation 2021"

  disclaimer.text <- "The boundaries and names shown and the designations used on this map do not imply official endorsement or acceptance by the United Nations.
Dotted line represents approximately the Line of Control in Jammu and Kashmir agreed upon by India and Pakistan. The final status of Jammu and Kashmir
has not yet been agreed upon by the parties. Final boundary between the Republic of Sudan and the Republic of South Sudan has not yet been determined."
  
  plot.coastlines <- FALSE # outline the coastlines with the same color as country boundaries? (TRUE or FALSE)
  plot.lakes <- TRUE # show lakes polygons for the 21 large lakes (TRUE or FALSE)
  plot.antarctica <- FALSE # show antarctica polygon (TRUE or FALSE)

# For standard map making, there is no need to modify code beyond this point
############################################################################
############################################################################

#memory.limit(size=4000) # double the memory allocation to accomodate large shape files

library("sp")
library("RColorBrewer")
library("ggplot2")
library("rgdal")
library("scales")
library("dplyr")


# Read in the UN cartography polygon shapefile (no antarctica)
  world.un <- readOGR(dsn = map.dir, layer = "un-world-2012-no-antartica-10pct")
  # convert to Robinson projection (the projection preferred by UN Cartography)
  proj4string(world.un) <- CRS("+proj=longlat +ellps=WGS84") # (requires sp package)
  world.robin <- spTransform(world.un, CRS("+proj=robin")) # (requires rgdal package)

# Read in the Un Cartography shapefile with country/area boundaries
  bnd.un <- readOGR(map.dir, "2012_UNGIWG_bnd_ln_01") 
  # convert to Robinson projection 
  proj4string(bnd.un) <- CRS("+proj=longlat +ellps=WGS84")
  bnd <- spTransform(bnd.un, CRS("+proj=robin"))
  
# Read in the Un Cartography shapefile with coastlines
  cst.un <- readOGR(map.dir, "2012_UNGIWG_cst_ln_01")  
  # convert to Robinson projection 
  proj4string(cst.un) <- CRS("+proj=longlat +ellps=WGS84")
  cst <- spTransform(cst.un, CRS("+proj=robin"))
  # remove Antarctica -- this is a bit clunky, but it works
  cst.df <- fortify(cst)
  cst.df <- cst.df[cst.df$lat>=-6285430,]
  cst.df$colorcode <- boundary.color
  cst.df <- cst.df[,c("id","colorcode")]
  cst.df$id <- as.numeric(cst.df$id)
  cst.df <- unique(cst.df)
  names(cst.df) <- c("OBJECTID","colorcode")
  cst <- merge(cst,cst.df,by="OBJECTID",all.x=FALSE,all.y=TRUE)

# Read in the Un Cartography shapefile with lakes
  lks.un <- readOGR(map.dir, "2012_UNGIWG_lks_ply_01")  
  # convert to Robinson projection 
  proj4string(lks.un) <- CRS("+proj=longlat +ellps=WGS84")
  lks <- spTransform(lks.un, CRS("+proj=robin"))
  lks.df <- fortify(lks)

# Read in the Un Cartography shapefile with Antarctica
  wld.un <- readOGR(map.dir, "un-world-2012-65pct")
  ant.un <- wld.un[wld.un$TERR_NAME=="Antarctica",]
  rm(wld.un)
  # convert to Robinson projection 
  proj4string(ant.un) <- CRS("+proj=longlat +ellps=WGS84")
  ant <- spTransform(ant.un, CRS("+proj=robin"))
  ant.df <- fortify(ant)
  ant.df$color.code <- NA
  if (plot.coastlines==TRUE){ ant.df$color.code <- boundary.color }
  
# Read in continuous variable to be mapped (csv file with LocID in second column and continuous var to be mapped in third column)
  input.data <- read.csv(file="/Users/lyhel/Dropbox/IGME 5-14/Estimates by age 2021/Aggregate results (median) 2021-12-02/Rates & Deaths_Country Summary.csv",stringsAsFactors = F)
 
  # indata<-filter(input.data,ISO3Code!="LIE") %>% # U5MR
  #   filter(X=="Median") %>%
  #   select(UNCode,ISO3Code, U5MR.2020) %>% 
  #   bind_rows(data.frame(UNCode=304, ISO3Code="GRL", U5MR.2020 = input.data$U5MR.2020[input.data$X=="Median" &input.data$ISO3Code=="DNK"])) %>%
  #   mutate(cat=ifelse(U5MR.2020<=10,1,
  #                     ifelse(U5MR.2020>10 & U5MR.2020<=25,2,
  #                            ifelse(U5MR.2020>25 & U5MR.2020<=50,3,
  #                                   ifelse(U5MR.2020>50 & U5MR.2020<=75,4,
  #                                          ifelse(U5MR.2020>75 & U5MR.2020<=100, 5, 
  #                                                 ifelse(U5MR.2020>100, 6, NA))))))) %>%
  #   rename(M49COLOR=UNCode, vartomap=cat, vartomap2=U5MR.2020)
  
  # indata<-filter(input.data,ISO3Code!="LIE") %>% # NMR
  #   filter(X=="Median") %>%
  #   select(UNCode,ISO3Code, NMR.2020) %>% 
  #   bind_rows(data.frame(UNCode=304, ISO3Code="GRL", NMR.2020 = input.data$NMR.2020[input.data$X=="Median" &input.data$ISO3Code=="DNK"])) %>%
  #   mutate(cat=ifelse(NMR.2020<=5,1,
  #                     ifelse(NMR.2020>5 & NMR.2020<=12,2,
  #                            ifelse(NMR.2020>12 & NMR.2020<=25,3,
  #                                   ifelse(NMR.2020>25 & NMR.2020<=35,4,
  #                                          ifelse(NMR.2020>35,5,NA)))))) %>%
  #   rename(M49COLOR=UNCode, vartomap=cat, vartomap2=NMR.2020) 
  
  indata<-filter(input.data,ISO3Code!="LIE") %>% # Mortality rate 10-19 
    filter(X=="Median") %>%
    select(UNCode,ISO3Code, X10q10.2020) %>% 
    bind_rows(data.frame(UNCode=304, ISO3Code="GRL", X10q10.2020 = input.data$X10q10.2020[input.data$X=="Median" &input.data$ISO3Code=="DNK"])) %>%
    mutate(cat=ifelse(X10q10.2020<=2.5,1,
                      ifelse(X10q10.2020>2.5 & X10q10.2020<=5,2,
                             ifelse(X10q10.2020>5 & X10q10.2020<=10,3,
                                    ifelse(X10q10.2020>10 & X10q10.2020<=20,4,
                                           ifelse(X10q10.2020>20,5,NA)))))) %>%
    rename(M49COLOR=UNCode, vartomap=cat, vartomap2=X10q10.2020)
  
  indata$vartomap  <- as.numeric(indata$vartomap) # ensure continuous var is read a numeric
  indata$vartomap2 <- as.numeric(indata$vartomap2) # ensure continuous var is read a numeric
  
library("classInt")
  
  # assign place marker aesthetics (type, color and size)
  pchs <- 19 # type of character to mark places (see ?pch for options)
  colors <- c("red3","salmon3","sandybrown","tan","lightgoldenrod") # colors of place markers
  pt.cexs <- c(3,2,1,0.5,0.25) # sizes of place markers

# Attach the continuous variable to be mapped to the polygon shapefile
  world.robin <- merge(world.robin,indata,by="M49COLOR",all.x=TRUE,all.y=FALSE) #attach the variable to be mapped to the polygon shapefile

  # get centroid of country polygons
  n.places <- length(world.robin$vartomap)
  places <- data.frame(matrix(ncol = 7, nrow = n.places))
  colnames(places) <- c("ISO3", "TERR_NAME", "long", "lat", "vartomap","vartomap2", "STATUS")
  
  places$ISO3 <- world.robin$ISO3_CODE
  places$TERR_NAME <- world.robin$TERR_NAME
  places$long <- coordinates(world.robin)[, 1]
  places$lat <- coordinates(world.robin)[, 2]
  places$vartomap <- world.robin$vartomap
  places$vartomap2 <- world.robin$vartomap2
  places$STATUS <- world.robin$STATUS
  tmp <- subset(indata, select=c(M49COLOR, ISO3Code))
  places <- merge(places, tmp, by.x="ISO3", by.y="ISO3Code", all.x=FALSE, all.y=FALSE)
  
  head(places)
  
  library(mapplots)
  library(reshape2)
  #Pie data structure
  
  # places<-merge(indata,places[,c("long","lat","ISO3", "STATUS", "TERR_NAME")],by.x="ISO3Code",by.y="ISO3",all.y=TRUE)
  # 
  # placespie<- places[!is.na(places$vartomap) & places$ISO3!="TWN" & places$ISO3!="XXX" & places$M49COLOR!= 136 & places$M49COLOR!= 238 & places$STATUS != "Sovereignty unsettled" & places$M49COLOR!= 574 &
  #                    places$STATUS == "Member State" & places$TERR_NAME !="Gaza Strip" & places$STATUS != "Occupied Palestinan Territory",]
  # 
  # placespie<-melt(placespie,id=c("ISO3Code","M49COLOR","vartomap","long","lat","STATUS","TERR_NAME"))
  # placespie<-placespie[order(placespie$ISO3Code),]
  # fix(placespie)
  # placespie$Data.source<-NA
  # placespie$Data.source[which(placespie$variable=="vartomap2")]<-"Neonatal Deaths"
  # placespie$Data.source[which(placespie$variable=="vartomap3")]<-"Post-neonatal Deaths"
  # 
  # labs=c("Neonatal deaths","Post-neonatal under-5 deaths (aged 1 to 59 months)")
  #   
  # tt<-make.xyz(placespie$long,placespie$lat,placespie$Freq,placespie$Data.source,FUN=sum)

# assign colors to each category to be mapped
  ## colors <- brewer.pal(NumOfCategories+1,color.palette) # (requires RColorBrewer package) 
  #colors <- brewer.pal(NumOfCategories,color.palette) # (requires RColorBrewer package) 
  ## colors <- colors[2:length(colors)] # eliminate the lightest hue as it tends not to map well (looks white in many palettes)
  #colors <- colors[1:length(colors)] # eliminate the lightest hue as it tends not to map well (looks white in many palettes)
  #colors <- rev(colors) # put the darkest hues first
  # color.palette<-"Blues"#PuRd"#Spectral"#OrRd"#RdYlBu"
  # colors <- brewer.pal((NumOfCategories+1),color.palette) # (requires RColorBrewer package) 
  # colors <- colors[2:(length(colors))] # eliminate the lightest hue as it tends not to map well (looks white in many palettes)
  # ##colors <- colors[1:length(colors)] # eliminate the lightest hue as it tends not to map well (looks white in many palettes)
  # colors <- rev(colors) # put the darkest hues first
  
  # colors <- c("#091e43","#1b3f73","#31608c","#5c77a6","#92acd9","#C7D3E5") #U5MR color  
  # colors <- c("#3d1f36","#67405A","#AC7881","#C7A2A4","#E8D1D1") #NMR color  
  colors <- c("#3d1f36","#67405A","#AC7881","#C7A2A4","#E8D1D1") #10-19 color  
  
  world.robin$colorcode <- NA
  for (i in 1:NumOfCategories){
    if (i==1){
      world.robin$colorcode[which(!is.na(world.robin$vartomap) & world.robin$vartomap>=category.breaks[1])] <- colors[1]
      } else if (i>1 & i<NumOfCategories) {
      world.robin$colorcode[which(!is.na(world.robin$vartomap) & world.robin$vartomap<category.breaks[i-1] & 
                                world.robin$vartomap>=category.breaks[i])] <- colors[i]
      } else if (i==NumOfCategories) {
      world.robin$colorcode[which(!is.na(world.robin$vartomap) & world.robin$vartomap<category.breaks[i-1])] <- colors[i]
      }
  }
  world.robin$colorcode[which(is.na(world.robin$vartomap))] <- NoDataColor
  
  #world.robin$colorcode[which(world.robin$TERR_NAME=="China")]
  
# UN Cartography requires that the Askai Chin region be striped half in the color of China and half in the color of 
# Jammu-Kashmir (no data for most of UNPD purposes)
# to do that, we create a separate polygon file that contains only the single region Aksai Chin and assign it the color of China for now
# it will be layered on top of the map in a separate step
  ac <- world.robin[world.robin$TERR_NAME=="Aksai Chin",]
  acf <- fortify(ac) # transform to data frame for more plotting options
  acf$colorcode <- world.robin$colorcode[which(world.robin$TERR_NAME=="China")]

# three styles of boundaries should be mapped: standard solid line, dashed line for undetermined boundaries, dotted for selected disputed boundaries
# to do that, we create a separate dataframe with each containing boundaries of the same type
  bnd.line <- bnd[bnd$CARTOGRAPH=="International boundary line",]
  bnd.dash <- bnd[bnd$CARTOGRAPH=="Dashed boundary line",]
  bnd.dot <- bnd[bnd$CARTOGRAPH=="Dotted boundary line" | bnd$CARTOGRAPH=="Dotted boundary line (Abyei)",]
  bnd.ssd <- bnd[bnd$BDY_CNT01=="SDN" & bnd$BDY_CNT02=="SSD",] # Specify SSD-SDN boundaries and plot later to resolve issue of not showing in the original script 

# write the map function
unpd.map <- function(){
    plot(world.robin,border=NA,col=world.robin$colorcode,bg=background.color) # plot country/area polygons
    polygon(acf$long,acf$lat,col=acf$colorcode[1],border=NA, density=130,angle=45,lwd=0.4) # plot Aksai Chin as striped region per UN Cartography requirements
    lines(bnd.line, col=boundary.color, lwd=0.2, lty=1) # plot solid boundaries
    lines(bnd.dot, col=boundary.color, lwd=0.2, lty=3) # plot dotted boundaries
    lines(bnd.dash, col=boundary.color, lwd=0.2, lty=2) # plot dashed boundaries
    lines(bnd.ssd, col=boundary.color, lwd=0.2, lty=2) # plot SSD-SDN boundary
    
    if (plot.lakes==TRUE) {
      lks.grp <- unique(lks.df$group)
      for (gp in lks.grp) {
        lk <- lks.df[lks.df$group==gp,]
         polygon(lk$long,lk$lat,col=background.color,border=boundary.color,lty=1,lwd=0.2) # plot lakes as background color
      }
    }
    if (plot.coastlines==TRUE) {
      lines(cst, col=boundary.color, lwd=0.2, lty=1) # plot coastlines as solid lines
      
    }
    if (plot.antarctica==TRUE) {
      ant.grp <- unique(ant.df$group)
      for (gp in ant.grp) {
        ant <- ant.df[ant.df$group==gp,]
        polygon(ant$long,ant$lat,col=NoDataColor,border=ant$color.code,lty=1,lwd=0.2)
      }
    }
    # plot dashed boundaries
		
	## add point layer
	library(mapplots)
	# define transparency function for named colors:
	colalpha <- function(color,alpha){
	 colalphai <- function(color,alpha){
	      paste(rgb(t(col2rgb(color)/255)),alpha,sep="")
		  }
	sapply(color,colalphai,alpha=alpha)
	}
	
	#places <- subset(places, is.na(places$vartomap2)==FALSE 
	#& places$ISO3!="TWN" & places$ISO3!="XXX" & places$M49COLOR!= 136 & places$M49COLOR!= 238 & places$STATUS != "Sovereignty unsettled" & places$M49COLOR!= 574 
	#& places$M49COLOR != "PT Territory" & places$TERR_NAME !="Gaza Strip" & places$STATUS != "Occupied Palestinan Territory")
	#places <- places[order(-places$vartomap2),]
	
	#draw.bubble(places$long, places$lat, places$vartomap2, maxradius=700000, pch=21, bg=colalpha("grey",90))
	#cols<-c("blue","forestgreen")
	
	 #library(maptools)
	#draw.pie(tt$x, tt$y, tt$z, radius=500000, col=alpha(cols,0.7), lty=0 )
 ##Label points to avoid overlaps using maptools function (simulated annealing optimization method)
#pointLabel(places2$long, places2$lat, places2$ISO3, method = "SANN", offset = 0, cex = .2)

	## alternative
	## radius <- sqrt( places$vartomap / pi ) 
	## symbols(places$long, places$lat, circles=radius, inches=0.35, fg="white", bg="red")
	## symbols(places$long, places$lat, squares=sqrt(places$vartomap), inches=0.5)


   #text(0,10000000, "Achievement of SDG target \n of a under-five mortality rate \n of 12 deaths per 1,000 live births in 2030", cex=0.8) # main title
     legend(-16820000, -1000000, col=c(colors, NoDataColor), pt.bg=c(colors, NoDataColor), pch=15, pt.cex=2, cex=0.7, 
     legend=legend.labels,
     title=legend.title, box.lty=0, box.col="white",
		 bty='o', bg='white')
     
     #legend(2020000*0.6, -5000000, col=cols, pt.bg=cols, pch=15, pt.cex=2, cex=0.7, 
    #        legend=labs,
    #        title="Under-5 deaths of which:", box.lty=0, box.col="white",
    #        bty='o', bg='white')

	#text(24820000*0.6, 3000000, "Under-five deaths\n(in million)", cex=0.7)
	#text(24820000*0.6, 3000000, "Neonatal deaths\n(in million)", cex=0.7)
	#legend.bubble(24820000*0.6, 1000000, z=max(places$vartomap2)/1000000, n=5, round = 1, maxradius=700000, 
	#				bty="n", inset=0.01, txt.cex=0.4, pch=21, pt.bg=colalpha("grey",95), bg= "white")  
		   
    # UN Cartography required disclaimers
    ## mtext(paste(source.text,"\n",disclaimer.text,sep=""), side=1,line=0,adj=0,cex=0.5)
  }

# Create the maps and save them to pdf and png output files

  # Use this low resolution png file for web displays 
  
  ## par(oma=c(0,0,0,0)) # no margins
  ## oma defines the space in lines, omd as a fraction of the device region, omi specifies the size in inches. 
  ## oma and omi take a four item vector where position one sets the bottom margin, position two the left margin, position three the top margin and position four the right margin. 
  ## The mar command represents the figure margins. The vector is in the same ordering of  the oma commands.  
  ## http://rgraphics.limnology.wisc.edu/rmargins_sf.php
  ## par(mfrow = c(1,1), omi=c(0.04,0.04,0.04,0.04), mar=c(0,0,0,0)+0.1, mgp=c(2,0.5,0),
png(file =paste0(out.dir,"/", "Map_",map.name,"_lowres.png"),width=10,height=4.5,units="in",res=200)  
par(mfrow = c(1,1), omi=c(0,0,0,0), mar=c(0,0,0,0), mgp=c(2,0.5,0),
      las=0, mex=1, cex=1, cex.main=1, cex.lab=1, cex.axis=1)
  unpd.map()
  dev.off() # close the png

  # Use this higher resolution png file for inserting into Word documents
  # png(file = paste0(out.dir,"/",map.name,"U5MR_lowres.png.png"),width=10,height=4.5,units="in",res=1000)
  # par(mfrow = c(1,1), omi=c(0,0,0,0), mar=c(0,0,0,0), mgp=c(2,0.5,0),
  #     las=0, mex=1, cex=1, cex.main=1, cex.lab=1, cex.axis=1)
  # unpd.map()
  # dev.off() # close the png

  # Use the pdf file for pubs going through the Graphic Design Unit
  pdf(file = paste0(out.dir,"/","Map_",map.name,".pdf"),width=10,height=4.5)
  par(mfrow = c(1,1), omi=c(0,0,0,0), mar=c(0,0,0,0), mgp=c(2,0.5,0),
      las=0, mex=1, cex=1, cex.main=1, cex.lab=1, cex.axis=1)
  unpd.map()
  dev.off() # close the pdf
