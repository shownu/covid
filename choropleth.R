library(rgdal)
library(RColorBrewer)
library(leaflet)

my_colors <- brewer.pal(9, "Greens")
my_colors <- colorRampPalette(my_colors)(5)

sf_nuts1 <- readOGR( 
  dsn= getwd() , 
  layer="c75a03ef-cd2c-4f59-a224-bf03cdd3f24c2020411-1-14x0nmk.zd23",
  verbose=FALSE
)
var_nuts1 <- as.numeric( NUTS$cases )

sf_nuts2 <- readOGR( 
  dsn= getwd() , 
  layer="2c2f23fe-79f5-4937-8e63-54a06a40b71b2020410-1-9kqbwq.54zqf",
  verbose=FALSE
)
var_nuts2 <- as.numeric( LADS$cases )

sf_nuts3 <- readOGR( ## slow one
  dsn= getwd() , 
  layer="f721de2d-d6aa-41e2-9a0d-2dbc7c8e76202020410-1-6dei90.91dnm",
  verbose=FALSE
)
var_nuts3 <- as.numeric( LADS$cases )

plot_chart <- function(sf, var) {
  class_of_country <- cut(var, 5)
  my_colors <- my_colors[as.numeric(class_of_country)]
  plot(sf, col=my_colors ,  bg = "#FFFFFF")
}

plot_chart(sf_nuts1, var_nuts1) # skeleton


shapeData <- readOGR( 
  dsn= getwd() , 
  layer="2c2f23fe-79f5-4937-8e63-54a06a40b71b2020410-1-9kqbwq.54zqf",
  verbose=FALSE
)

overlay_map <- function(sf) { # add var later
  shapeData <- spTransform(sf, CRS("+proj=longlat +ellps=GRS80"))
  leaflet()  %>% addTiles() %>% 
  setView(lat = 55, lng=0,zoom=6) %>% 
  addPolygons(data=shapeData, weight=2, col = 'green') 
}

overlay_map(sf_nuts1)
