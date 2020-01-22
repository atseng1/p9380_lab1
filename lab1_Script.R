# Can set working directory with command
getwd()
setwd("/Users/ashleytseng/OneDrive - cumc.columbia.edu/MPH/Spring 2020/EHSC P9380_Advanced GIS/Labs/Lab 1/p9380_lab1")

install.packages('maps')
install.packages('maptools')
install.packages('rgdal')
install.packages('RColorBrewer')
install.packages('classInt')


###############PROJECTIONS

library(maps) 

oldpar<-par()

world <- map("world", res=0)
str(world)
head(world$names)
plot(world)

states <- map("state", res=0)
str(states)
head(states$names)
plot(states)

library(maptools)

spworld <- map2SpatialLines(world, proj4string = CRS("+proj=longlat"))
spstates <- map2SpatialLines(states, proj4string = CRS("+proj=longlat"))

str(spworld, max.level=2)
str(spstates,max.level=2)

plot(spworld)
plot(spstates)

library(rgdal)

world.laea <- spTransform(spworld, CRS("+proj=laea +lat_0=0 +lon_0=0"))
states.laea <- spTransform(spstates, CRS("+proj=laea +lat_0=43.0758 +lon_0=-89.3976"))
#states.epsg <- spTransform(spstates, CRS("+init=epsg:3623"))


#Run Following Code chunk together


par(mfrow = c(2, 2), pty = "s", cex.axis = 0.5)

plot(spworld, axes = T)
title(main = "Longitude and\nLatitude")
plot(world.laea, axes = T)
title(main = "Lambert Azimuthal\nEqual Area")
plot(spstates, axes = T)
title(main = "Longitude and\nLatitude")
plot(states.laea, axes = T)
title(main = "Lambert Azimuthal\nEqual Area")


#SPATIAL REFERENCING



par(oldpar)

map.states <- map("state", plot = T, fill = T, res=0)
str(map.states)

list.names.states <- strsplit(map.states$names,":")
head(list.names.states, n=63)
View(list.names.states)

map.IDs <- sapply(list.names.states, function(x) x[1])
head(map.IDs, n=63)

polystates <- map2SpatialPolygons(map.states, IDs = map.IDs,proj4string = CRS("+proj=longlat"))

summary(polystates)

plot(polystates)

states.laea <- spTransform(polystates, CRS("+proj=laea +lat_0=43.0758 +lon_0=-89.3976"))
plot(states.laea)

sp.IDs <- sapply(slot(states.laea, "polygons"), function(x) slot(x,"ID"))
head(sp.IDs, n=50)

setwd("C:/Users/Jeremy/Desktop/Spring2019/Lab1_Spatial")
sat_verbal<- read.csv("sat_verbal.csv", stringsAsFactors = F,row.names = 1)
head(sat_verbal, n=50)

states.verbal <- SpatialPolygonsDataFrame(polystates,sat_verbal)
summary(states.verbal)

states.verbal.laea <- spTransform(states.verbal, CRS("+proj=laea +lat_0=43.0758 +lon_0=-89.3976"))

plot(states.verbal.laea)
summary(states.verbal.laea)



#MAPPING
library(RColorBrewer)
display.brewer.all()
display.brewer.pal(5, "Greys")

library(classInt)

plotvar <- states.verbal.laea$verbal
nclr <- 5
plotclr <- brewer.pal(nclr, "Greys")
plotclr
class <- classIntervals(plotvar, nclr, style = "quantile")
class
colcode <- findColours(class, plotclr, digits = 3)
colcode
plot(states.verbal.laea, col = colcode)


plotclr <- brewer.pal(nclr, "Purples")
class <- classIntervals(plotvar, nclr, style = "quantile")
colcode <- findColours(class, plotclr, digits = 3)
plot(states.verbal.laea, col = colcode, border = "grey",axes = F)
title(main = "SAT verbal scores in 2010 \n by Jeremy R. Porter", 
      sub = "Data Source: College Board")
legend("bottomleft", legend = names(attr(colcode,"table")), 
       fill = attr(colcode, "palette"), cex=0.55)

writeOGR(states.verbal.laea, dsn = "working_directory", layer = "sat_verbal", driver = "ESRI Shapefile")

rstudioapi::documentSave()




