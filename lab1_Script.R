# Can set working directory with command
getwd()
setwd("/Users/ashleytseng/OneDrive - cumc.columbia.edu/MPH/Spring 2020/EHSC P9380_Advanced GIS/Labs/Lab 1/p9380_lab1")

install.packages('maps')
install.packages('maptools')
install.packages('rgdal')
install.packages('RColorBrewer')
install.packages('classInt')


############### PROJECTIONS

library(maps) 
 
oldpar<-par()
## Clears plot parameters

world <- map("world", res=0)
## res=0 gives us the highest-resolution output of the world map
## All the points have been linked together to make it look like a continuous border
str(world)
head(world$names)
## What are the names associated with the objects? 
## "head" gives us a preview of the names
plot(world)
## Recall how points are used to connect lines, which are used to connect polygons


states <- map("state", res=0)
str(states)
## There are 11,687 total points used
head(states$names)
## Shows that each of the vertices are plotted by group name
plot(states)

## Note that we can't actually do anything to these points since they aren't spatial data (yet)
## Recall that the .dbf file of a shapefile is what provides the spatial data

library(maptools)

spworld <- map2SpatialLines(world, proj4string = CRS("+proj=longlat"))
spstates <- map2SpatialLines(states, proj4string = CRS("+proj=longlat"))
## Turns both of these into spatial line objects

str(spworld, max.level=2)
str(spstates, max.level=2)
## max.level determines how many numbers of lines to run
## spstates has 169 lines but R has only run 2 of those lines and given details/output for those 2

plot(spworld)
plot(spstates)


library(rgdal)

world.laea <- spTransform(spworld, CRS("+proj=laea +lat_0=0 +lon_0=0"))
## Changing coordinate system. Latitute and longitude are both centered on 0, 0
## Everything else will be projected around the center
states.laea <- spTransform(spstates, CRS("+proj=laea +lat_0=43.0758 +lon_0=-89.3976"))
## In order to spatially transform ("spTransform") something, the object must already be in spatial form
## Hence we call the spatial objects that we created earlier
states.epsg <- spTransform(spstates, CRS("+init=epsg:3623"))


#Run Following Code chunk together


par(mfrow = c(2, 2), pty = "s", cex.axis = 0.5)
## Setting parameters of a plot window with 2 rows and 2 columns, "s" for square, and the text size of
## the axis wil be 0.5

plot(spworld, axes = T)
title(main = "Longitude and\nLatitude")
plot(world.laea, axes = T)
title(main = "Lambert Azimuthal\nEqual Area")
plot(spstates, axes = T)
title(main = "Longitude and\nLatitude")
plot(states.laea, axes = T)
title(main = "Lambert Azimuthal\nEqual Area")





############### SPATIAL REFERENCING


par(oldpar)
## Resetting parameters since I don't want the 2x2 anymore

map.states <- map("state", plot = T, fill = T, res=0)
## This just fills in the entire area with black; no longer connects the points
str(map.states)
## Tells us the range of our data (likely the southernmost, westernmost, etc. points)
map.states$names
## We can look at all the state names in our data. We see that any land that isn't continuously
## connected to the state but is still part of it is listed as a separate state (e.g., Long Island)

list.names.states <- strsplit(map.states$names,":")
head(list.names.states, n=63)
View(list.names.states)

map.IDs <- sapply(list.names.states, function(x) x[1])
## Here we create a new object. "sapply" works across all of the 63 map names where the function x 
## is going to pull the first object (state name).
## i.e., we are pulling out the main state name and dropping the rest of the name
## These are going to become our map IDs
head(map.IDs, n=63)

polystates <- map2SpatialPolygons(map.states, IDs = map.IDs,proj4string = CRS("+proj=longlat"))
## We've created a new object called "polystates" which is a spatial polygon object

summary(polystates)

plot(polystates)
## This is essentially a shapefile. We set up this map so that we can join data to it.
## We have now created points, lines, and polygons - yay!

states.laea <- spTransform(polystates, CRS("+proj=laea +lat_0=43.0758 +lon_0=-89.3976"))
plot(states.laea)
## We projected the plot with the laea projection command

sp.IDs <- sapply(slot(states.laea, "polygons"), function(x) slot(x,"ID"))
## We want to burn in the ID structure into the polygons
## For each slot ID, which we created as the map IDs, we are going to burn in the actual IDs
## i.e., the lowercase state names are now our IDs
head(sp.IDs, n=50)

setwd("/Users/ashleytseng/OneDrive - cumc.columbia.edu/MPH/Spring 2020/EHSC P9380_Advanced GIS/Labs/Lab 1/p9380_lab1")
sat_verbal<- read.csv("sat_verbal.csv", stringsAsFactors = F, row.names = 1)
head(sat_verbal, n=50)

states.verbal <- SpatialPolygonsDataFrame(polystates,sat_verbal)
## By default, "SpatialPolygonsDataFrame" will try to connect the spatial polygon objects to file names
summary(states.verbal)

states.verbal.laea <- spTransform(states.verbal, CRS("+proj=laea +lat_0=43.0758 +lon_0=-89.3976"))

plot(states.verbal.laea)
summary(states.verbal.laea)





############### MAPPING
library(RColorBrewer)
display.brewer.all()
display.brewer.pal(5, "Greys")

library(classInt)

plotvar <- states.verbal.laea$verbal
nclr <- 5
plotclr <- brewer.pal(nclr, "Greys")
plotclr
class <- classIntervals(plotvar, nclr, style = "quantile")
## "classIntervals" tells R how we want to class the intervals
## In this case, the intervals are quantiles
class
colcode <- findColours(class, plotclr, digits = 3)
colcode
plot(states.verbal.laea, col = colcode)


plotclr <- brewer.pal(nclr, "Purples")
class <- classIntervals(plotvar, nclr, style = "quantile")
colcode <- findColours(class, plotclr, digits = 3)
plot(states.verbal.laea, col = colcode, border = "grey",axes = F)
title(main = "SAT verbal scores in 2010 \n by Ashley Tseng", 
      sub = "Data Source: College Board")
legend("bottomleft", legend = names(attr(colcode,"table")), 
       fill = attr(colcode, "palette"), cex=0.45)

writeOGR(states.verbal.laea, dsn = "working_directory", layer = "sat_verbal", driver = "ESRI Shapefile")

rstudioapi::documentSave()




###### LAB 1 ASSIGNMENT
setwd("/Users/ashleytseng/OneDrive - cumc.columbia.edu/MPH/Spring 2020/EHSC P9380_Advanced GIS/Labs/Lab 1/p9380_lab1")
sat_math = read.csv("sat_math.csv", stringsAsFactors = F, row.names = 1)
head(sat_math, n = 50)

states.math = SpatialPolygonsDataFrame(polystates, sat_math)
## By default, "SpatialPolygonsDataFrame" will try to connect the spatial polygon objects to file names
summary(states.math)

states.math.laea = spTransform(states.math, CRS("+proj=laea +lat_0=43.0758 +lon_0=-89.3976"))

plot(states.math.laea)
summary(states.math.laea)


library(classInt)

plotvarmath = states.math.laea$math
plotclrmath = brewer.pal(nclr, "Oranges")
nclrmath = 5
classmath = classIntervals(plotvarmath, nclrmath, style = "quantile")
colcodemath = findColours(classmath, plotclrmath, digits = 3)
plot(states.math.laea, col = colcodemath, border = "grey", axes = F)
title(main = "SAT Math Scores in 2010 \n by Ashley Tseng", 
      sub = "Data Source: College Board")
legend("bottomleft", legend = names(attr(colcodemath, "table")), 
       fill = attr(colcodemath, "palette"), cex=0.45)

writeOGR(states.math.laea, dsn = "working_directory", layer = "sat_math", driver = "ESRI Shapefile")

rstudioapi::documentSave()




