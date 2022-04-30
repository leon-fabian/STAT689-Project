# Ram Pangaluri, Ashok Shanker, Fabian Leon
# STAT 689
# Data Visualization Dashboard

# Resources
# https://rstudio.github.io/shinydashboard/appearance.html
# https://rstudio.github.io/shinydashboard/structure.html

##### Code #####
library(shiny)
library(shinythemes)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(dplyr)
library(readr)
library(RColorBrewer)
library(leaflet)
library(ddplot)
library(ggmap)
library(readxl)
library(ddplot) # remotes::install_github("feddelegrand7/ddplot", build_vignettes = TRUE) #used to install ddplot
library(geojsonR)

# Read Dataset
data = read.csv("data/data.csv")
factors = c("Dataset", "Location", "County", "AgriLife.Region", "Region", "Irrigation", 
            "Hybrid", "Brand", "Maturity", "Previous.Crop")
numerics = c("DA", "PH", "EX", "MST", "bu.per.acre.Yield", "lbs.per.ac.Yield", "GY", "Lodging", "RowWidth", 
             "Location.Average.Yield", "plot.length", "rainfall", "Irrigation.Amount", 
             "Total.Moisture", "Population", "Days.from.Plant.to.Harvest")

integers = c("Number.of.Rows", "Year")
data[,factors] = lapply(data[,factors], as.factor)
data[,numerics] = lapply(data[,numerics], as.numeric)
data[,integers] = lapply(data[,integers], as.integer)

usda = data[which(data$Dataset == "USDA"),]
txar = data[which(data$Dataset == "TXAR"),]

traits = c("Year", "DA", "PH", "EX", "GY")

### For Choropleth Map
counties_df = read.csv("data/TX-FIPS.csv", fileEncoding = 'UTF-8-BOM') 
usda_df = usda %>% 
  inner_join(counties_df, by.x = County, by.y = County) %>%
  select(Year, County, FIPS, bu.per.acre.Yield) %>% 
  mutate(hover = paste0(County, "\n", bu.per.acre.Yield, " bu/ac"))
usda_df[, "FIPS_ST_CNTY_CD"] = as.character(usda_df[,"FIPS"])

fontStyle = list(family = "DM Sans", size = 15, color = "black")
label = list(bgcolor = "#EEEEEE", bordercolor = "transparent", font = fontStyle)


file_js = fromJSON(file = "https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json") # Get geojson file of Texas county geometry

g = list(scope = 'texas',
          projection = list(type = 'albers usa'),
          showlakes = TRUE,
          lakecolor = toRGB('white'))



#### For Racing Bar Chart
myTibble = as_tibble(txar %>%
                        distinct(Hybrid, Brand, Year) %>%
                        group_by(Year,Brand) %>%
                        summarize("Total Hybrids" = n()))


##### UI #####
ui <- dashboardPage(
  
  dashboardHeader(title = "Texas Grain Sorghum"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Texas AgriLife Variety Testing", 
               tabName = "intro", 
               icon = icon("grain", lib="glyphicon")),
      menuItem("Historical Yield Improvements", 
               tabName = "line", 
               icon = icon("signal", lib="glyphicon")),
      menuItem("Trait Relationships", 
               tabName = "traits", 
               icon = icon("leaf", lib="glyphicon")),
      menuItem("Map", 
               tabName = "Map", 
               icon = icon("globe", lib="glyphicon")),
      menuItem("Company Brands", 
               tabName = "brands", 
               icon = icon("object-align-left", lib="glyphicon")),
      menuItem("Statistical Analysis", 
               tabName = "stats", 
               icon = icon("stats", lib="glyphicon"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # Introduction
      tabItem(tabName = "intro",
              h2("Crop testing program at Texas AgriLife")
              # NEED TO ADD BACKGROUND ABOUT TEXAS AGRILIFE AND USDA AND GRAIN SORGHUM IN TEXAS
      ),
      
      # Line Graph
      tabItem(tabName = "line",
              h2("Yield Improvements over the Years"),
              fluidRow(
                box(plotlyOutput("graph1"))
                # ADD BOX WITH A TIMELINE EXPLAINING UPS AND DOWNS
              )
      ),
      
      # Scatter plots
      tabItem(tabName = "traits",
              h2("Correlations between metrics of hybrid performance"),
              fluidRow(
                box(selectInput("scat_x", label = h2("select x-axis"), 
                                choices = traits),
                    selectInput("scat_y", label = h2("select y-axis"), 
                                choices = traits)),
                box(plotOutput("scatter", height = 300))
              )
              # NEED TO ADD DISPLAY OF PEARSON'S CORRELATION AND/OR PLOT SMOOTHED LINE TO THE RELATIONSHIP
      ),
      
      # Choropleth map
      tabItem(tabName = "Map",
              #h2("Chloropleth map of average yield per county")
              fluidRow(box(plotlyOutput("map"))))
      ),
      
      # Racing Bars
      tabItem(tabName = "brands",
              h2("Accumulated numbers of Hybrids submitted over the years"),
              fluidRow(box(plotlyOutput("bars")))
      ),
      
      # LMM Statistical Analysis & Predictions
      tabItem(tabName = "stats",
              h2("Linear mixed model for factors influencing grain yields")
      )
    )
  )





####### Server #########
server = function(input, output) {
  
  # Line Graph
  output$graph1 = renderPlotly({
    p = ggplot(txar, aes(x = Year, y = GY)) +
      geom_smooth()
    ggplotly(p)
  })
  
  # Scatter Plot
  output$scatter = renderPlot({
    ggplot(txar, aes_string(x=input$scat_x, y=input$scat_y)) + 
      geom_point()
  })
  
  # Choropleth map
  output$map = renderPlotly({
    plot_ly() %>% 
      add_trace(
        type = "choropleth",
        geojson = counties,
        locations = usda_df$FIPS,
        frame = usda_df$Year,
        text = usda_df$hover,
        z = usda_df$bu.per.acre.Yield,
        colorscale = "Viridis",
        zmin = 0,
        zmax = max(usda_df$bu.per.acre.Yield),
        marker = list(line=list(
          width = 0))) %>% 
      colorbar(title = "Yield (bu/acre") %>% 
      layout(title = "USDA Average Yield by County") %>% 
      layout(geo = g)
  })    
    

  # Racing Bars
  output$bars = renderPlot({
    myTibble %>%
      barChartRace(
        x = "Total Hybrids",
        y = "Brand",
        time = "Year",
        title = "Popular Brands and their total hybrids over the Years",
        frameDur = 750,
        colorCategory = "Dark2",
        panelcol = "white",
        bgcol = "#DCDCDC",  # a light gray
        xgridlinecol = "#EBEBEBFF",
        timeLabelOpts = list(size = 16)
      )

  
  })

  # LMM Statistical Analysis & Predictions
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

