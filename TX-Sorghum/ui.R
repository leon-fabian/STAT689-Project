#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(navbarPage("Texas Grain Sorghum",
                   tabPanel("Historical Yield Improvements"), # Line Graph
                   tabPanel("Relationships"), # Scatter
                   tabPanel("Map"), # chloropleth map
                   # leafletOutput("mymap"),
                   # p(),
                   # actionButton("recalc", "New Points"),
                   tabPanel("Brands"), # Racing Bars
                   tabPanel("Dissecting Yield Factors")
  )
  
)
