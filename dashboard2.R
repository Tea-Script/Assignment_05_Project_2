library(shiny)
library(shinydashboard)
library(tidyverse)
library(stringr)
library(dplyr)
library(readxl)
options(warn=-1)

#Read data from excel document
veg.1 <- read_xlsx("veg1.xlsx")
cnames.1 <- colnames(veg.1)
c <- apply(veg.1, 2, n_distinct)
e <- names(c[c>1])
veg.2 <- select(veg.1, e)
veg.3 <- dplyr::rename(veg.2,
                       Geo = `Geo Level`,
                       State = `State ANSI`,
                       Data = `Data Item`,
                       Category = `Domain Category`)
yy <- separate(veg.3, Category, into = c("label", "quant"), sep=",")
ru <- filter(yy, label=="RESTRICTED USE CHEMICAL")
ru1 <- ru %>% select(label, quant, Value) %>% unique()
CAS <- t(as.data.frame(strsplit(ru1$quant, "=")))
CAS[,2] <- substr(CAS[,2], 1, nchar(CAS[,2])-1)
ru2 <- cbind(ru1, CAS[,2])
row.names(ru2) <- 1:284
colnames(ru2) <- c(colnames(ru2)[1:length(colnames(ru2))-1], "CAS")

ru3 <- ru1 %>% separate (quant, c("ChemType", "ChemName"), sep = ":") %>%  mutate (Value = str_extract(Value, "^\\w+"))

ru4 <- unique(ru3[, c("label", "ChemType" , "ChemName", "Value")]) %>% na.omit(ru8) # Only takes the unique Chemical Names and corresponding values;

ru5 <- ru4[1:5, ]  # Clean the Value NA's and makes a table with corresponding chemical names;

ToixicityData <- tibble (TestedSpecies = c("NorwayRat", "HoneyBee", "FruitMoth", "NorwayRat", "BoolWeevil" ),
                         Effectmeasurement = c("Activity_General", "Mortality", "Mortality", "Mortality", "Mortality"),
                         LD50dose = c(4, 7, 7, 6, 4),
                         Dosage = c(23.3, 0.034,  0.011, 451, 0.0034),
                         Units = c("mg/kg", "ug/ul", "ug/org", "mg/kg", "ug/org"),
                         ObservedDurationiDays = c(0.0625, 1, 1, 1, 3)) # Rows extracted from search: 953, 190, 508, 52, 235

ru6 <- as.tibble(c(ru5, ToixicityData))
#we are going to use this tibble to allow users to examine a chemicals effect on a given animal.
print(ru6)
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
                ("This application is designed to allow a user to explore the data from the USDA on how chemicals placed on plants and other vegetables affect animals. We have data for 5 chemicals and the effect on a few different animals.")
              )
            )        
    ),
    tabItem(tabName = "about",
            fluidRow(
              box(width=10,
                  p("This app was created by Matthew D. Ciaramitaro as a part of the MA615 Tidy Final Project with help from Steven Tran and Praveen Kenderla. 
                    The application requires shiny, tidyverse, dplyr, shinydashboard, and readxl. See the rest of the assignment at"),
                  a("https://github.com/PraveenKenderlagit/Assignment_05_Project_2")
                  )
            )        
    ),
    tabItem(tabName = "data",
            #this row shall be designed so that a user can click on a chemical, then choose an animal to view the data on that chemical's effect on the animal.
            fluidRow(
              column(width=10,
                box(
                  selectInput("animals", choices=ru6$TestedSpecies, label="Choose Animal")
                ),
                box(
                  selectInput("chemicals", choices=c(), label="Choose Chemical")
                
                )
                
              ),
              tableOutput("table")
            )
    
    )
  
  
  )

)
ui <- dashboardPage(skin="green", header, sidebar, body)

server <- function(input, output, session) { 
  x <- reactive({dplyr::filter(ru6, TestedSpecies == input$animals)})
  reactive({print(x)})
  observe({
  x <- x()
  updateSelectInput(session, "chemicals",
                    label = "Choose Chemical",
                    choices = x$ChemName
  )})
  
  
  output$table <- renderTable({
    x <- x()
    dplyr::filter(x, ChemName == input$chemicals)})
  
}




shinyApp(ui, server)