library(shiny)
library(DT)
library(leaflet)
library(leaflet.extras)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  titlePanel(title=div(img(src = "Logo.png", height = "50px"))),
  div(id = "vr-header"), 
    fluidRow(
      column(2, checkboxInput("doNameSearch", "Über Namen suchen:", FALSE))
    ),
    fluidRow(
      uiOutput("uiSearch")
    ),
    fluidRow(
      tags$br() 
    ),
    # Show a plot of the generated distribution
    fluidRow(
       column(5, DT::dataTableOutput("table")),
       column(7, leafletOutput("map", height = 1200))
    )
  )
)
