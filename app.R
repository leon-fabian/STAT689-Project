# Ram Pangaluri, Ashok Shanker, Fabian Leon
# STAT 689
# Data Visualization Dashboard

# Resources
# https://rstudio.github.io/shinydashboard/appearance.html
# https://rstudio.github.io/shinydashboard/structure.html

library(shiny)
library(ggplot2)
library(shinydashboard)
library(plotly)

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
              # NEED TO DISPLAY PEARSON'S R AND/OR PLOT SMOOTHED LINE TO THE RELATIONSHIP
      ),
      
      # Chloropleth map
      tabItem(tabName = "Map"
              #h2("Chloropleth map of average yield per county")
      ),
      
      # Racing Bars
      tabItem(tabName = "brands",
              h2("Accumulated numbers of Hybrids submitted over the years")
      ),
      
      # LMM Statistical Analysis & Predictions
      tabItem(tabName = "stats",
              h2("Linear mixed model for factors influencing grain yields")
      )
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
  
  # Chloropleth map
  
  
  # Racing Bars
  

  # LMM Statistical Analysis & Predictions
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

