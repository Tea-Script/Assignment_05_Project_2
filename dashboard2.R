library(shiny)
library(shinydashboard)
library(tidyverse)
library(stringr)
library(dplyr)
library(ggplot2)
library(leaflet)


#build shiny app
header <- dashboardHeader(
  title="USDA Agriculture Data",
  titleWidth = 350             
)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction", tabName = "intro", icon = icon("th")),
    menuItem("About", tabName = "about", icon = icon("info")),
    menuItem("Data", tabName = "data", icon = icon("bar-chart-o")) 
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "intro",
            fluidRow(
              box(
                p("TODO")
              )
            )        
    ),
    tabItem(tabName = "about",
            fluidRow(
              box(width=10,
                  p("This app was created by Matthew D. Ciaramitaro as a part of the MA615 Tidy Final Project with help from Steven Tran and Praveen Kenderla. 
                    The application requires shiny, tidyverse, dplyr, shinydashboard, and leaflet. See the rest of the assignment at"),
                  a("https://github.com/PraveenKenderlagit/Assignment_05_Project_2")
                  )
            )        
            ),
    tabItem(tabName = "temperatures",
            fluidRow(
              box(
                title = "Time of Day",
                sliderInput(inputId = "hour", label="Hour (Military Time)", min=0, max=23, value=12, step=1)
              ),
              box(p("Use the slider to view trends in water and air temperature at the buoy at the selected time during the period of 1987 to 2016." ))
            ),
            fluidRow(
              box(plotOutput("series1")),
              box(plotOutput("series2"))          
              
            ),
            fluidRow(
              box(
                
                "The Pearson Correlation between the two time series of Air and water temperatures is ",
                textOutput("correlation"),
                "Because this number is closer to 1, there is a somewhat strong correlation between the changes in Air and Water temperatures."
                
                
              )
            )
    ),
    tabItem(tabName = "ttest",
            fluidRow(
              tabBox(title="Has the Mean Temperature Changed Since 1987?",
                     tabPanel(title="Air Temperature", plotOutput("ttesta"), textOutput("ttexta")),
                     tabPanel(title="Water Temperature",plotOutput("ttestw"), textOutput("ttextw"))
              )
            )
            
    )
    
    )
  
  
  )


ui <- dashboardPage(skin="green", header, sidebar, body)

server <- function(input, output) { 
  
  
}




shinyApp(ui, server)