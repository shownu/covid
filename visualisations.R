library(leaflet)

show_bduk_sites <- function(dat) {
  test2 <- dat
  mybins <- seq(0, max(test2$cases)+250, by=200)
  mypalette <- colorBin( palette="YlOrBr", domain=test2$cases, na.color="transparent", bins=mybins)
  mytext <- paste(test2$base, sep=" ") %>%
    lapply(htmltools::HTML)
  m <- leaflet(test2) %>% 
    addTiles()  %>% 
    setView( lat=55, lng=4, zoom=6) %>%
    addProviderTiles(providers$CartoDB.Voyager) %>%
    addCircleMarkers(~long, ~lat, 
                     fillColor = ~mypalette(cases), fillOpacity = 0.8, color="black", radius=10, stroke=TRUE, weight=1,
                     label = mytext,
                     labelOptions = labelOptions(textOnly=FALSE, style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "top", noHide=F)
    ) %>%
    addLegend( pal=mypalette, values=~cases, opacity=0.9, title = "Cases", position = "bottomleft" )
  m 
}


show_us_sites <- function(dat) {
  test2 <- dat
  mybins <- seq(0, max(test2$cases)+5000, by=5000)
  mypalette <- colorBin( palette="YlOrBr", domain=test2$cases, na.color="transparent", bins=mybins)
  mytext <- paste(test2$cases, "cases,", test2$deaths, "deaths", sep=" ") %>%
    lapply(htmltools::HTML)
  m <- leaflet(test2) %>% 
    addTiles()  %>% 
    setView( lat=40, lng=-100, zoom=4.5) %>%
    addProviderTiles(providers$CartoDB.Voyager) %>%
    addCircleMarkers(~long, ~lat, 
                     fillColor = ~mypalette(cases), fillOpacity = 0.8, color="black", radius=20, stroke=TRUE, weight=1,
                     label = mytext,
                     labelOptions = labelOptions(textOnly=FALSE, style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "left", noHide=T)
    ) %>%
    addLegend( pal=mypalette, values=~cases, opacity=0.9, title = "Cases", position = "bottomleft" )
  m 
}


show_uk_areas <- function(dat) {
  test2 <- dat
  mybins <- seq(0, max(test2$cases)+2500, by=2000)
  mypalette <- colorBin( palette="YlOrBr", domain=test2$cases, na.color="transparent", bins=mybins)
  mytext <- paste(test2$area, "-", test2$cases, "cases", sep=" ") %>%
    lapply(htmltools::HTML)
  m <- leaflet(test2) %>% 
    addTiles()  %>% 
    setView( lat=55, lng=-5.5 , zoom=6) %>%
    addProviderTiles(providers$CartoDB.Voyager) %>%
    addCircleMarkers(~long, ~lat, 
                     fillColor = ~mypalette(cases), fillOpacity = 0.8, color="black", radius=20, stroke=TRUE, weight=1,
                     label = mytext,
                     labelOptions = labelOptions(textOnly=FALSE, style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "right", noHide=T)
    ) %>%
    addLegend( pal=mypalette, values=~cases, opacity=0.9, title = "Cases", position = "bottomright" )
  m
}


show_bduk_personnel <- function(dat) {
  test2 <- dat
  mypalette <- colorFactor(c("red", "green"), domain=test2$status, na.color="transparent")
  mytext <- paste(test2$base, sep=" ") %>%
    lapply(htmltools::HTML)
  nbrs <- paste(test2$people, sep=" ") %>%
    lapply(htmltools::HTML)
  m <- leaflet(test2) %>% 
    addTiles()  %>% 
    setView( lat=55, lng=-3 , zoom=6) %>%
    addProviderTiles(providers$CartoDB.Voyager) %>%
    addCircleMarkers(~long, ~lat, 
                     fillColor = ~mypalette(status), fillOpacity = 1, color="white", radius=12, stroke=F,
                     label = mytext,
                     labelOptions = labelOptions(textOnly=FALSE, style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "bottom", noHide=F)
    ) %>%
    addCircleMarkers(~long, ~lat, 
                     fillColor = ~mypalette(status), fillOpacity = 0, color="white", radius=5, stroke=F,
                     label = nbrs,
                     labelOptions = labelOptions(textOnly=TRUE, style = list("font-weight" = "bold", padding = "3px 8px"), textsize = "13px", direction = "center", noHide=T)
    )
  
  m
}

show_bduk_sites(BDUK_SITES)

show_us_sites(US_DATA)

show_bduk_personnel(BDUK_PEOPLE)
