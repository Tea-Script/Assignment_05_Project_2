library(shiny)
library(shinydashboard)
library(tidyverse)
library(stringr)
library(dplyr)
library(ggplot2)
library(leaflet)
gen_data <- function(bool=F){
  
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
  if(bool){file <- file %>% dplyr::filter(hh==12)}
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
    dplyr::filter(ATMP<99) %>% 
    dplyr::filter(WTMP<99)
  return(MR)
}
datums <- gen_data() #organized data of every hour


get_data <- function(hour){
  return(datums %>% dplyr::filter(hh==hour)) #select this hour

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


#bug investigation
test <- function(){
  
  # Obtaining Data
  url1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=mlrf1h"
  url2 <- ".txt.gz&dir=data/historical/stdmet/"
  years <- c(1987:2016)
  urls <- str_c(url1, years, url2, sep = "")
  filenames <- str_c("mr", years, sep = "")
  N <- length(urls)
  
  ## Formatting Data
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
    file$YYYY <- ifelse(as.numeric(file$YYYY%/%100)<1, file$YYYY+1900, file$YYYY) 
    # Add 1900 to the 2 digit years to match YYYY format
    file <- file %>% dplyr::filter(hh==12)
    
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
    dplyr::filter(ATMP<99) %>% 
    dplyr::filter(WTMP<99)
  return(MR)
  
  
}
dMR <- test()

#build shiny app
header <- dashboardHeader(
            title="NOAA Temperature Data",
            titleWidth = 350             
          )
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction", tabName = "intro", icon = icon("anchor")),
    menuItem("About", tabName = "about", icon = icon("info")),
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
              tabBox(title="Has the Mean Temperature Changed Since 1987?", width = 500,
                     tabPanel(title="Air Temperature", plotOutput("ttesta"), "The p value of ", textOutput("ttexta"), "< .05 and indicates that there is a statistically significant difference in the means of air temperature in 1987 and 2016"),
                     tabPanel(title="Water Temperature",plotOutput("ttestw"), "The p value of ", textOutput("ttextw"), "< .05 and indicates that there is a statistically significant difference in the means of water temperature in 1987 and 2016")
              )
            )
            
    ),
    
    tabItem(tabName = "conc",
      fluidRow(
        box(width=500,
          h1("According to our Pearson Test for Correlation, the Air Temperatures and Water Temperatures at the buoy are strongly correlated, and there is a statistically significant difference between the air and water mean temperatures of 1987 and 2016. There has been a significant increase in both temperatures since 1987.")
          
        )
        
      )        
            
    )
    
    
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
  
  # Testing Mean Air Temperature
  MR <- dMR
  MR1987.ATMP <- as.vector((MR %>% dplyr::filter(YYYY==1987))[["ATMP"]])
  MR2016.ATMP <- as.vector((MR %>% dplyr::filter(YYYY==2016))[["ATMP"]])
  t <- t.test(MR1987.ATMP, MR2016.ATMP, na.action = na.omit)
  range <- seq(-8, 8,by=.1)
  cdf <- dt(range, t$parameter)
  output$ttesta <- renderPlot({
    plot(cdf ~ range, type ="l") 
    polygon(c( range[range <= t$statistic], t$statistic ),  c(cdf[range <= t$statistic], 0), col="blue") #create colored graph
  }) #cdf of t distribution
  output$ttexta <- renderText(paste(t$p.value))
  
  
  #Testing Mean Water Temperature
  MR1987.WTMP <- as.vector((MR %>% dplyr::filter(YYYY==1987))[["WTMP"]])
  MR2016.WTMP <- as.vector((MR %>% dplyr::filter(YYYY==2016))[["WTMP"]])
  s <- t.test(MR1987.WTMP, MR2016.WTMP, na.action = na.omit)
  cdf2 <- dt(range, s$parameter)
  output$ttestw <- renderPlot({
    plot(cdf2 ~ range, type ="l") 
    polygon(c( range[range <= s$statistic], s$statistic ),  c(cdf2[range <= s$statistic], 0), col="blue") #create colored graph
  }) #cdf of t distribution
  output$ttextw <- renderText(paste(s$p.value))
  
  
}




shinyApp(ui, server)