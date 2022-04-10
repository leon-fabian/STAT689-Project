library(shiny)
library(shinydashboard)
library(ggplot2)

FINALAGRI_data = read.csv("data/data.csv")

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(selectInput("scat_x", label = h2("select x-axis"), 
                      choices = colnames(FINALAGRI_data)),
          selectInput("scat_y", label = h2("select y-axis"), 
                      choices = colnames(FINALAGRI_data))),
      box(plotOutput("scatter", height = 300))
    )
    
  )
)

server <- function(input, output) {
  output$scatter<- renderPlot({
    ggplot(FINALAGRI_data, aes_string(x=input$scat_x, y=input$scat_y)) + geom_point()
  })
}


shinyApp(ui, server)
