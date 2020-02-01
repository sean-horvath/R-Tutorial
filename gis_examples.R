# This script contains a few examples of how to import geospatial data 
# and how to manipulate (crop, crs tranformations) and plot them using
# a few different packages.

setwd('C:/Users/seanm/Desktop/R-Tutorial/')


# levelplot example -------------------------------------------------------
# Levelplot in the rasterVis package is a great way to plot rasters and
# contours.  You can also add sp type data ontop of the rasters.

library(rasterVis)
library(maptools)
melt <- readRDS('data/Melt.RDS')
melt <- dropLayer(melt,seq(11:40)) #take a sample of the data
names(melt) <- paste('Year ',seq(1979,1988)) #give names to each layer in stack
levelplot(melt,margin=F,scales=list(draw=F)) #simple plot

# Now a more complete plot
data("wrld_simpl")
out <- wrld_simpl
out <- as(wrld_simpl, "SpatialLines")
out <- crop(out, extent(-180, 180, 60, 83.57027)) #crop so the crs transformation works ok
wrld_ice <- spTransform(out,crs(melt)) #project to same crs as melt

mapTheme <- rasterTheme(region=rev(brewer.pal(11,'Spectral')), #sets color theme
                        axis.line=list(col='transparent'))

levelplot(melt,margin=F,
          par.settings=mapTheme,
          main='Melt Onset over the Arctic',
          scales=list(draw=FALSE),
          colorkey=list(labels=list(cex=1.5),
                        title='Day of Year')) +
  layer(sp.polygons(wrld_ice, col="grey40",lwd=0.2),under=F)



# Shapefiles --------------------------------------------------------------

library(rgdal)
ecoregion_file <- list.files('data/us_eco_l3/',pattern='us_eco_l3.shp$',
                             recursive=TRUE,full.names=TRUE)
ecoregions <- readOGR(ecoregion_file)
class(ecoregions)
head(ecoregions)
spplot(ecoregions,'NA_L3NAME',colorkey=FALSE)

library(tmap)
tm_shape(ecoregions) + 
  tm_fill(col = "Shape_Area",
          style="kmeans",
          title="Area") +
  tm_borders()


# CSV Files ---------------------------------------------------------------

library(dplyr)
dams <- read.csv('data/NID2018_U.csv',header=T)
dams <- filter(dams,MAX_STORAGE>160290 & MAX_STORAGE<2e8)
dams <- filter(dams,!is.na(LONGITUDE) | !is.na(LATITUDE))

coordinates(dams) = ~LONGITUDE+LATITUDE
crs(dams) <- '+proj=longlat +datum=WGS84 +no_defs'
spplot(dams,'MAX_STORAGE')

dam_map <- tm_shape(dams) +
  tm_dots(size='MAX_STORAGE')

us_data <- USAboundaries::us_states()
state_list <- unique(us_data$name)
state_list <- subset(state_list,!(state_list%in%c('Alaska','Puerto Rico','Hawaii')))
us_data <- USAboundaries::us_states(states=state_list)

us_map <- tm_shape(us_data) +
  tm_polygons()

us_map + dam_map


# Geotiff Files -----------------------------------------------------------

library(raster)
library(rasterVis)
# detach(package:ggplot2,unload=T)
topo_data <- raster('data/NASA Topography.TIFF')
plot(topo_data)
levelplot(topo_data)

new_topo <- crop(topo_data,extent(-130,-60,24,55))

levelplot(new_topo) +
  layer(sp.polygons(as(us_data, 'Spatial'),lwd=2))

# NetCDF Files ------------------------------------------------------------

library(ncdf4)
atm_data <- nc_open('data/MERRA2_400.instM_2d_asm_Nx.201902.SUB.nc')

attributes(atm_data$var)$names

nc_variable <- ncvar_get(atm_data, attributes(atm_data$var)$names[1])
nc_lat <- ncvar_get(atm_data, attributes(atm_data$dim)$names[2])
nc_lon <- ncvar_get(atm_data, attributes(atm_data$dim)$names[3])

# close the connection
nc_close(atm_data)

# set the dimension names and values of your matrix to the appropriate latitude and longitude values
dimnames(nc_variable) <- list(lon=nc_lon, lat=nc_lat)

atm_df <- melt(nc_variable, value.name = "variable")
class(atm_df)
head(atm_df)

plot(raster('data/MERRA2_400.instM_2d_asm_Nx.201902.SUB.nc',
            varname='T10M'))
data(wrld_simpl)
wrld <- spTransform(wrld_simpl,crs(raster('data/MERRA2_400.instM_2d_asm_Nx.201902.SUB.nc',varname='T10M')))
plot(wrld,add=T)


# Synced Maps -------------------------------------------------------------

library(mapview)

m1 <- mapview(dams,zcol='PRIVATE_DAM',color=c('blue'),legend=TRUE)
m2 <- mapview(dams,zcol='MAX_STORAGE',cex='MAX_STORAGE')
m3 <- mapview(dams,zcol='YEAR_COMPLETED',at=seq(1835,2018),legend=TRUE)
sync(m1,m2,m3)
