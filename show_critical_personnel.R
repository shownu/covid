library(leaflet)

show_critical_personnel <- function(dat) {
  mytext <- paste(dat$`Home Town`, sep=" ") %>%
    lapply(htmltools::HTML)
  dat$lat <- as.numeric(dat$lat)
  dat$long <- as.numeric(dat$long)
  m <- leaflet(dat) %>% 
    addTiles()  %>% 
    setView( lat=55, lng=-3 , zoom=6) %>%
    addProviderTiles(providers$CartoDB.Voyager) %>%
    addCircleMarkers(dat$long, dat$lat, 
                     fillColor = "red", fillOpacity = 1, color="white", radius=2, stroke=F,
                     label = mytext,
                     labelOptions = labelOptions(textOnly=FALSE, style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "top", noHide=F)
    )
  m
}

show_critical_personnel(hometowns)
