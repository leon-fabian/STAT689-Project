# Ram Pangaluri, Ashok Shanker, Fabian Leon
# STAT 689
# Data Visualization Dashboard

# Resources
# https://rstudio.github.io/shinydashboard/appearance.html
# https://rstudio.github.io/shinydashboard/structure.html

##### Load Libraries #####
library(shiny)
library(shinythemes)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(dplyr)
library(readr)
library(RColorBrewer)
# library(leaflet)
library(ddplot)
library(ggmap)
# library(readxl)
# library(ddplot) # remotes::install_github("feddelegrand7/ddplot", build_vignettes = TRUE) #used to install ddplot
# library(gganimate)
# library(r2d3)
library(geojsonR)
library(rjson)
library(knitr)


lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

##### Read Dataset #####
data = read.csv("data/data.csv", na.strings = c("",".","NA"))
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

##### For Choropleth Map #####
counties_df = read.csv("data/TX-FIPS.csv", fileEncoding = 'UTF-8-BOM') 
usda_df = usda %>% 
  inner_join(counties_df, by.x = County, by.y = County) %>%
  select(Year, County, FIPS, bu.per.acre.Yield) %>% 
  mutate(hover = paste0(County, " ", bu.per.acre.Yield, " bu/ac"))
usda_df[, "FIPS_ST_CNTY_CD"] = as.character(usda_df[,"FIPS"])
fontStyle = list(family = "DM Sans", size = 15, color = "black")
label = list(bgcolor = "#EEEEEE", bordercolor = "transparent", font = fontStyle)
file_js = fromJSON(file = "https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json") # Get geojson file of Texas county geometry


##### For Racing Bar Chart #####
myTibble = as_tibble(txar %>%
                        distinct(Hybrid, Brand, Year) %>%
                        group_by(Year,Brand) %>%
                        summarize("Total Hybrids" = n()))

# myTibble$`Total Hybrids` = as.numeric(myTibble$`Total Hybrids`)
# length(unique(myTibble$Brand))
# 
# myTibble = as_tibble(myTibble %>% 
#                        group_by(Brand) %>%
#                        mutate(cs = cumsum('Total Hybrids')))


##### For Effects Modelling and Predictions #####
factors = c("Maturity", "RowWidth", "Irrigation", "rainfall", 
            "Irrigation.Amount", "Population", "Days.from.Plant.to.Harvest")



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
      menuItem("Yield Factors", 
               tabName = "stats", 
               icon = icon("stats", lib="glyphicon"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # Introduction
      tabItem(tabName = "intro",
              h2("Grain Sorghum Performance in Texas: 1970-2021"),
              # home section
              includeMarkdown("home.md")
      ),
      
      # Line Graph
      tabItem(tabName = "line",
              h2("Yield Improvements over the Years"),
              fluidRow(
                box(plotlyOutput("graph1"))
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
      ),
      
      # Choropleth map
      tabItem(tabName = "Map",
              # includeHTML("images/finmap.html")
              h2("Chloropleth map of average yield per county"),
              fluidRow(htmlOutput('map'), height = '1000px', width = '600px')
              # fluidRow(box(plotlyOutput("map"))),
              # ## fluidRow(includeHTML("images/finmap.html"))
              # includeMarkdown("TXRegions.Rmd")
      ),
      
      # Racing Bars
      tabItem(tabName = "brands",
              h2("Accumulated numbers of Hybrids submitted over the years"),
              # fluidRow(includeHTML("barchartrace.html"))
              # fluidRow(box(plotlyOutput("bars")))
              fluidRow(box(htmlOutput("bars"), height = '700px', width = '600px'))
      ),
      
      # LM Statistical Analysis & Predictions
      tabItem(tabName = "stats",
              h2("Environmental variables and cultural practices affecting grain yields"),
              # fluidRow(
              #   box(selectInput("factor", label = h2("Select a factor"), 
              #                   choices = factors)),
              #   box(plotOutput("modeleffects", height = 300)))
              # includeMarkdown("statistical_analysis.Rmd"),
              fluidPage(
                uiOutput('markdown')
              )
      )
    )
  )
)


####### Server #########
server = function(input, output) {
  
  getPage<-function() {
    return(includeHTML("finmap.html"))
  }
  
  getBarChart<-function() {
    return(includeHTML("barchartrace.html"))
  }
  # Choropleth map
  output$map = renderUI({getPage()})
  
  # Racing bars
  output$bars = renderUI({getBarChart()})
  
  # Line Graph
  output$graph1 = renderPlotly({
    p = ggplot(txar, aes(x = Year, y = lbs.per.ac.Yield)) + 
      labs(x = "Year", y = "Grain Yield (lbs/ac)") +
      geom_smooth() 
      
    ggplotly(p)
    
  })
  
  # Scatter Plot
  output$scatter = renderPlot({
    ggplot(txar, aes_string(x = input$scat_x, y = input$scat_y)) + 
      geom_point() +
      geom_smooth()

  })
  
  # # Choropleth map
  # output$map = renderPlotly({
  #   plot_ly() %>% 
  #     add_trace(type = "choropleth",
  #               geojson = file_js,
  #               locations = usda_df$FIPS,
  #               frame = usda_df$Year,
  #               z = usda_df$bu.per.acre.Yield,
  #               colorscale = "Jet",
  #               zmin = 0,
  #               zmax = max(usda_df$bu.per.acre.Yield),
  #               marker = list(line = list(width = 0)),
  #               text = usda_df$hover) %>% 
  #     colorbar(title = "Yield (bu/acre)") %>% 
  #     layout(title = "USDA: Average Yield (bu/ac)", 
  #            geo = list(#scope = 'usa',
  #                       lonaxis = list(range = c(25, 38)),
  #                       lataxis = list(range = c(90, 105)),
  #                       projection = list(type = 'albers usa'),
  #                       showlakes = TRUE,
  #                       lakecolor = toRGB('white')))
  # })    
  #   

  # Racing Bars
  # output$bars = renderImage({
  #   anim = myTibble %>%
  #     barChartRace(
  #       x = "Total Hybrids",
  #       y = "Brand",
  #       time = "Year",
  #       title = "Popular Brands and their total hybrids over the Years",
  #       frameDur = 100,
  #       colorCategory = "Dark2",
  #       panelcol = "white",
  #       bgcol = "#DCDCDC",  # a light gray
  #       xgridlinecol = "#EBEBEBFF",
  #       timeLabelOpts = list(size = 16)
  #     )
  #   animate(anim) 
  #   list(src = "outfile.gif", contentType = "image/gif")
  # },
  # deleteFile = TRUE
  # )

  output$markdown <- renderUI({
    HTML(markdown::markdownToHTML(knit('statistical_analysis.Rmd', quiet = TRUE)))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

