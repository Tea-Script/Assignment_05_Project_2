library(shiny)
library(shinydashboard)
library(tidyverse)
library(stringr)
library(dplyr)
library(ggplot2)
library(leaflet)
gen_data <- function(){
  
  # Obtaining Data
  url1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=mlrf1h"
  url2 <- ".txt.gz&dir=data/historical/stdmet/"
  years <- c(1987:2016)
  urls <- str_c(url1, years, url2, sep = "")
  filenames <- str_c("mr", years, sep = "")
  N <- length(urls)
  for (i in 1:N){
    suppressMessages(
      assign(filenames[i], read_table(urls[i], col_names = TRUE))
    )
    file <- get(filenames[i])
    colnames(file)[1] <-"YYYY"
    if(!is.numeric(file[1,1])){
      file <- file[2:nrow(file),]
      file$YYYY <- as.numeric(file$YYYY)
    }
    
    file$YYYY <- ifelse(as.numeric(file$YYYY%/%100)<1, file$YYYY+1900, file$YYYY) #we add 1900 to the 2 digit years to match YYYY format
  }
  for (i in 1:N){
    # Combining Data 
    if(i == 1){
      MR <- file %>% select(YYYY, MM, DD, hh, ATMP, WTMP)
    }
    else{
      MR <- rbind.data.frame(MR, file %>% select(YYYY, MM, DD, hh, ATMP, WTMP))
    }
  }
    
  # Making Data Numeric
  MR$MM <- as.numeric(MR$MM)
  MR$DD <- as.numeric(MR$DD)
  MR$hh <- as.numeric(MR$hh)
  MR$ATMP <- as.numeric(MR$ATMP)
  MR$WTMP <- as.numeric(MR$WTMP)
  MR <- MR %>% 
    filter(ATMP<99) %>% 
    filter(WTMP<99)
  return(MR)
}
data <- gen_data() #organized data of every hour


get_data <- function(hour){
  return(data %>% filter(hh==hour)) #select this hour

}
dMR <- get_data(12)

get_air_series <- function(MR){
  #returns the data for the air series to be plotted
    return(
      MR %>% 
      select(YYYY, ATMP, MM, DD) %>% 
      mutate(date=as.Date(paste(as.character(MM), "/", as.character(DD), "/", as.character(YYYY), sep=""), format = "%m/%d/%Y"))
    )
}
get_water_series <- function(MR){ 
  #returns the data for the water series to be plotted
    return(
      MR %>% 
      select(YYYY, WTMP, MM, DD) %>% 
      mutate(date=as.Date(paste(as.character(MM), "/", as.character(DD), "/", as.character(YYYY), sep=""), format = "%m/%d/%Y"))
    )
}



#build shiny app
header <- dashboardHeader(
            title="NOAA Temperature Data",
            titleWidth = 350             
          )
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction", tabName = "intro", icon = icon("anchor")),
    menuItem("About us", tabName = "about", icon = icon("info")),
    menuItem("Charts and Analysis", tabName = "charts", icon = icon("bar-chart-o"), 
             menuSubItem("Temperature by Time of Day", tabName = "temperatures", icon = NULL), #include air temperature and sea temperature overlayed, option for time change
             menuSubItem("Changes in Mean Temperature", tabName = "ttest", icon = NULL) #Examines changes in mean air and mean sea temperature
    ),
    menuItem("Conclusions", tabName = "conc", icon = icon("server"))
    
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "intro",
      fluidRow(
        box(
          p("We have analysed data from NOAA Weather Station buoy 46035 at 57.026 N 177.738 in the Pacific Ocean. Use the tabs to navigate to the charts and analysis of the data.")
        ),
        box(leafletOutput("buoy", height = 500))
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
            
    ),
    
    tabItem(tabName = "conc")
    
    
  )
  
  
)


ui <- dashboardPage(skin="green", header, sidebar, body)
server <- function(input, output) { 
  
  output$series1 <- renderPlot({
    
    MR <- get_data(input$hour)
    W <-  get_water_series(MR)
    #render water temperature plot
    ggplot(data=W, aes(x=date, y=WTMP))+
      geom_line(color="red")+
      xlab("Year")+
      ylab("Temperature (Celsius)")+
      ggtitle("Water Temperature Over Time")+
      scale_x_date(date_labels = "%Y") 
   })
  output$series2 <- renderPlot({
    
    MR <- get_data(input$hour)
    A <- get_air_series(MR)
    #render water temperature plot
    ggplot(data=A, aes(x=date, y=ATMP))+
      geom_line(color="blue")+
      xlab("Year")+
      ylab("Temperature (Celsius)")+
      ggtitle("Air Temperature Over Time")+
      scale_x_date(date_labels = "%Y") 
  })
  output$correlation <- renderText({
    MR <- get_data(input$hour)
    A <- get_air_series(MR)
    W <- get_water_series(MR)
    cor(A %>% select(ATMP), W %>% select(WTMP), method="pearson")
    
  })
  output$buoy <- renderLeaflet(leaflet() %>% setView(lng=-177.738, lat=57.026, zoom = 3) %>% addMarkers(lng=-177.738, lat=57.026) %>% addTiles()) #map of buoy location
  
  
  
  
  
}




shinyApp(ui, server)