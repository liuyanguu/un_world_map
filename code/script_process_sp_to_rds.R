library("sp")
library("sf")
library("RColorBrewer")
library("ggplot2")
library("rgdal")  # for readOGR --- unfortunately this library is retired
library("scales")

# working directory is project folder
work.dir <- getwd()

map.shp.dir <- file.path(work.dir, "shp") # location of the shapefiles
map.rds.dir <- file.path(work.dir, "rds") # location to save the rds object
stopifnot(dir.exists(map.shp.dir))
stopifnot(dir.exists(map.rds.dir))


# Read in the UN cartography polygon shapefile (no antarctica)
# 10pct
world.un <- readOGR(dsn = map.shp.dir, layer = "un-world-2012-no-antartica-10pct")
# convert to Robinson projection (the projection preferred by UN Cartography)
proj4string(world.un) <- CRS("+proj=longlat +ellps=WGS84") # (requires sp package)
world.robin <- spTransform(world.un, CRS("+proj=robin")) # (requires rgdal package)

world.robin$ISO3_CODE[which(world.robin$TERR_NAME=="Greenland")] <- "DNK"
world.robin$ISO3_CODE[which(world.robin$TERR_NAME=="Hong Kong")] <- "CHN"

saveRDS(world.robin, file.path(map.rds.dir, "sp.world.robin.rds"))

# 35pct (if need higher resolution)
# Read in the UN cartography polygon shapefile (no antarctica)
world.un <- readOGR(dsn = map.shp.dir, layer = "un-world-2012-no-antartica-35pct")
# convert to Robinson projection (the projection preferred by UN Cartography)
proj4string(world.un) <- CRS("+proj=longlat +ellps=WGS84") # (requires sp package)
world.robin <- spTransform(world.un, CRS("+proj=robin")) # (requires rgdal package)

saveRDS(world.robin, file.path(map.rds.dir, "sp.world.robin.35pt.rds"))


# more elements -----------------------------------------------------------


# Read in the Un Cartography shapefile with country/area boundaries
bnd.un <- readOGR(map.shp.dir, "2012_UNGIWG_bnd_ln_01") 
# convert to Robinson projection 
proj4string(bnd.un) <- CRS("+proj=longlat +ellps=WGS84")
bnd <- spTransform(bnd.un, CRS("+proj=robin"))

saveRDS(bnd, file.path(map.rds.dir, "sp.bnd.rds"))

# Read in the Un Cartography shapefile with coastlines
cst.un <- readOGR(map.shp.dir, "2012_UNGIWG_cst_ln_01")  
# convert to Robinson projection 
proj4string(cst.un) <- CRS("+proj=longlat +ellps=WGS84")
cst <- spTransform(cst.un, CRS("+proj=robin"))
saveRDS(cst, file.path(map.rds.dir, "sp.cst.not.fortified.rds"))


# there is a color setting step 
# remove Antarctica -- this is a bit clunky, but it works
cst.df <- fortify(cst)
cst.df <- cst.df[cst.df$lat>=-6285430,]
boundary.color <- "white" # color for country boundaries
cst.df$colorcode <- boundary.color
cst.df <- cst.df[,c("id","colorcode")]
cst.df$id <- as.numeric(cst.df$id)
cst.df <- unique(cst.df)
names(cst.df) <- c("OBJECTID","colorcode")
cst <- merge(cst,cst.df,by="OBJECTID",all.x=FALSE,all.y=TRUE)

saveRDS(cst, file.path(map.rds.dir, "sp.cst.rds"))

# Read in the UN Cartography shapefile with lakes
lks.un <- readOGR(map.shp.dir, "2012_UNGIWG_lks_ply_01")  
# convert to Robinson projection 
proj4string(lks.un) <- CRS("+proj=longlat +ellps=WGS84")
lks <- spTransform(lks.un, CRS("+proj=robin"))

saveRDS(lks, file.path(map.rds.dir, "sp.lks.rds"))


# Antartica is not needed, but put it there for completeness

# wld.un <- readOGR(dsn = "C:/Users/lyhel/Dropbox/UNICEF Work/unmap", layer = "un-world-2012-65pct")
# ant.un <- wld.un[wld.un$TERR_NAME=="Antarctica",]
# # convert to Robinson projection 
# proj4string(ant.un) <- CRS("+proj=longlat +ellps=WGS84")
# ant <- spTransform(ant.un, CRS("+proj=robin"))
# saveRDS(ant, file.path(map.rds.dir, "sp.ant.rds"))


# RKS shapefiles
sp.rks <- rgdal::readOGR(file.path(map.shp.dir, "RKS/kosovo_administrative_boundaries_national_polygon.shp"))
# proj4string(sp.rks) <- CRS("+proj=longlat +ellps=WGS84") # (requires sp package)
sp.rks <- spTransform(sp.rks, CRS("+proj=robin")) 
saveRDS(sp.rks, file.path(map.rds.dir, "sp.RKS.boarder.rds"))


