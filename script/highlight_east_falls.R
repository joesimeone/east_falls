

library(leaflet)

## -----------------------------------------------------------------------------
# Import Shape files ------
## -----------------------------------------------------------------------------

## Counties
pa_counties <- tigris::counties(state = "42", year = 2022)

## Zip
pa_zip <- tigris::zctas(state = "42", year = 2010)


## -----------------------------------------------------------------------------
# Change Projection and filter down  ----
## -----------------------------------------------------------------------------

## Philly
phl_co <- 
  pa_counties %>% 
  filter(str_detect(NAME, "Philadelphia"))

## Coerce projection 
phl_co <- 
  sf::st_transform(phl_co, 
                   crs = 4326)

## East Falls 
east_falls_zip <-
  pa_zip %>% 
  filter(ZCTA5CE10 == "19129")

## Coerce projection
east_falls_zip <- 
  sf::st_transform(east_falls_zip, 
                   crs = 4326)


## ----------------------------------------------------------------------------
# Make some maps  -----
## ----------------------------------------------------------------------------

east_falls_bb <- 
  leaflet() %>% 
  fitBounds(-75.28027 , 39.867,  -74.95576 ,
              40.13799) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addPolygons(data = phl_co,
              fillColor = "#eaf3ff",
              opacity = .15,
              color = "#eaf3ff"
  ) %>% 
  addPolygons(data = east_falls_zip,
              fillColor =  "#9a133d",
              dashArray = "3",
              fillOpacity = .55,
              color = "white",
              opacity = 1,
              weight = 1) 
