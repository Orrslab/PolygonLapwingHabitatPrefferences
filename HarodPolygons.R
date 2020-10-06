####1) Open libraries####
library(leaflet)
library(ptinpoly)

####2) Open KMLs####
# Open polygons:
Water <- st_read("C:/Users/miki/Desktop/Poly/Polyall/Water pond.kml")
AllTheArea <- st_read("C:/Users/miki/Desktop/Poly/Polyall/Field.kml")
Urban <- st_read("C:/Users/miki/Desktop/Poly/Polyall/Urban.kml")

####3)Remove the Z to create only polygons####
Water <- st_zm(Water, drop = TRUE, what = "ZM")
plot(Water[1]) 
Urban <- st_zm(Urban, drop = TRUE, what = "ZM")
plot(Urban[1]) 
AllTheArea <- st_zm(AllTheArea, drop = TRUE, what = "ZM")
plot(AllTheArea[1]) 

####4)Change the polygons to spatial polygons####
ATA1 <- as_Spatial(AllTheArea, cast = TRUE, IDs = paste0("Name", seq_along(from)))
Ur1 <- as_Spatial(Urban, cast = TRUE, IDs = paste0("Name", seq_along(from)))
Wa1 <- as_Spatial(Water, cast = TRUE, IDs = paste0("Name", seq_along(from)))

####5)Create a polygon for fields:####

Fields <- gDifference(ATA1, Ur1)
Fields <- gDifference(Fields, Wa1)

plot(Fields, col="green")

####6)Calculating area:####
FiAr<- area(Fields)/ 1000000
UrAr<- sum(area(Ur1))/ 1000000
WaAr<- sum(area(Wa1))/ 1000000

####7)Mapping only the polygons to see if they work####
ll<-leaflet() %>% addTiles() %>%
  addProviderTiles('Esri.WorldImagery') %>%
  addPolygons(data=Ur1,color = "grey", weight = 1, smoothFactor = 0.8,
              opacity = 1.0, fillOpacity = 0.7)%>%
  addPolygons(data=Fields,color = "green", weight = 1, smoothFactor = 0.8,
              opacity = 1.0, fillOpacity = 0.7)%>%
  addPolygons(data=Wa1,color = "blue", weight = 1, smoothFactor = 0.8,
              opacity = 1.0, fillOpacity = 0.7)
ll
####8)Working with tags####

#Tag 046.csv
setwd('C:\\Users\\miki\\Desktop\\Poly')
Tag46<- read.csv('Tag 046.csv', header = TRUE) 
unique(Tag46$date)
date2404 <- subset(Tag46,date == "2020-04-24" )
itm<-"+init=epsg:2039 +proj=tmerc +lat_0=31.73439361111111 +lon_0=35.20451694444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-48,55,52,0,0,0,0 +units=m +no_defs"
wgs84<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

coordinates(date2404)<-~X+Y
proj4string(date2404)<-CRS(itm)
llpd <- spTransform(date2404, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# Mapping to see if it works
ll<-leaflet() %>% addTiles() %>%
  addProviderTiles('Esri.WorldImagery')  %>%
  addPolygons(data=Ur1,color = "grey", weight = 1, smoothFactor = 0.8,
              opacity = 1.0, fillOpacity = 0.7)%>%
  addPolygons(data=Fields,color = "green", weight = 1, smoothFactor = 0.8,
              opacity = 1.0, fillOpacity = 0.7)%>%
  addPolygons(data=Wa1,color = "blue", weight = 1, smoothFactor = 0.8,
              opacity = 1.0, fillOpacity = 0.7) %>%
  addPolylines(data=llpd@coords, weight = 1, opacity = 1, color = "Black") %>%
  addCircles(data=llpd, weight = 5, fillOpacity = 1, color = "red")
  
ll
####9)Getting the location of the points and see if they overlap with the polygons####

# Creating the tag as maps:
TagTry <- Tag46
coordinates(TagTry)<-~X+Y
proj4string(TagTry)<-CRS(itm)
llpd <- spTransform(TagTry, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
p <- llpd
x <- Ur1
proj4string(x) # The coordinates need to be exact to overlap
# [1] " +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
proj4string(p)
# [1] " +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

# For urban:
plot(x)
plot(p, col="red" , add=TRUE) # The map works
res <- over(p, x) # Overlapping the data
names(res)
res$NAa <- is.na(res$Name)

# Creating a new column for the habitat:
Tag46$Hab <- NA
Tag46$Hab[res$NAa == "FALSE"] <- "Urban"

# For water ponds:
x2 <- Wa1
proj4string(x2)
# [1] " +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
proj4string(p)
# [1] " +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

plot(x2)
plot(p, col="red" , add=TRUE)
res2 <- over(p, x2)
res2$NAa <- is.na(res2$Name)
Tag46$Hab[res2$NAa == "FALSE"] <- "WaterPonds"

# For Fields:

x3 <- Fields
proj4string(x3)
# [1] " +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
proj4string(p)
# [1] " +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

plot(x3)
plot(p, col="red" , add=TRUE)
res3 <- over(p, x3)

# This value need to be a dataframe first:
names(res3)
res3 <- as.data.frame (res3)
res3$NAa <- is.na(res3$res3)
table(res$NAa)

Tag46$Hab[res3$NAa == "FALSE"] <- "Field"

table(Tag46$Hab) # Now I can know what habitat was each lapwing when!
