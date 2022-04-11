#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)


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


traits = c("DA", "PH", "EX", "GY")

##### UI ##### 
ui <- navbarPage("Texas Grain Sorghum",
                 
                 # Line Graph - Need a different layout to get rid of empty side panel
                 tabPanel("Historical Yield Improvements",
                          sidebarLayout(
                            sidebarPanel(),
                            mainPanel(
                              plotOutput("linegraph"),
                              sliderInput("slider", label = "Date Range",
                                          min = min(txar$Year),
                                          max = max(txar$Year),
                                          value=c(min(txar$Year), max(txar$Year)))
                            )
                          )
                 ),
                 
                 # Scatter
                 tabPanel("Relationships",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("scat_x", label = h2("select x-axis"), 
                                          choices = colnames(txar)),
                              selectInput("scat_y", label = h2("select y-axis"), 
                                          choices = colnames(txar))
                            ),
                            mainPanel(plotOutput("scatterplot"))
                          )
                 ),
                 
                 # Chloropleth map         
                 tabPanel("Map"
                          
                 ), 
                 
                 
                 # Racing Bars
                 tabPanel("Brands"
                 
                          ), 
                 
                 # LMM Statistical Analysis & Predictions
                 tabPanel("Dissecting Yield Factors")
)



# Server # 
server <- function(input, output) {
  
  # Line Graph 
    output$linegraph = renderPlot({
      ggplot(txar, aes(x = Year, y = GY)) + 
      geom_smooth() +
      coord_cartesian(xlim = input$slider) + 
      theme_minimal()
  })
  
  
  # Scatterplot
  output$scatterplot <- renderPlot({
    ggplot(txar, aes_string(x = input$scat_x, y = input$scat_y)) + 
      geom_point()
  })
  
  # Chloropleth map
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

