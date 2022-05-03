library(shiny)

ui <- fluidPage(
  
  img(src="finalrace.gif", align = "left",height='250px',width='500px')
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)