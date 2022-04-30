rm(list = ls())

library(shiny)
library(plotly)
library(leaflet)
library(rjson)

url <- 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'
counties <- rjson::fromJSON(file=url)

fips = read.csv("fips.csv")

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

fig <- plot_ly() %>% 
  add_trace(
    type="choropleth",
    geojson=counties,
    locations=fips$FIPS,
    text = fips$County,
    z=fips$Yield,
    colorscale="Viridis",
    zmin=0,
    zmax=200,
    marker=list(line=list(
      width=0))) %>% 
  colorbar(title = "Yield") %>% 
  layout(title = "Yield by County") %>% 
  layout(geo = g)
fig










# Old


renderPlotly({
  
  yield_map = plot_geo(usda_df, 
                       locationmode = 'USA-states', 
                       frame = ~Year) %>% 
    add_trace(type = "choropleth",
              geojson = file_js, # geojson file that was read in
              locations = ~FIPS_ST_CNTY_CD, # FIPS = county id codes
              z = ~bu.per.acre.Yield, 
              zmin = 0,
              zmax = max(usda_df$bu.per.acre.Yield),
              color = ~bu.per.acre.Yield,
              text = ~hover,
              hoverinfo = 'text',
              colors = brewer.pal(11, "RdYlGn")) %>%
    layout(geo = list(scope = 'usa'), # HAVING TROUBLE NARROWING THE SCOPE TO JUST TEXAS - Fabian
           font = list(family = "DM Sans"),
           title = "Grain Sorghum Yields\n 1970 - 2020") %>%
    style(hoverlabel = label) %>%
    config(displayModeBar = FALSE)
  
  yield_map 
  
  output$leaf = renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addProviderTiles("Stamen") %>% 
      setView(lat = 30.6, lng = -100, zoom = 5.35)  
    
  })
})

