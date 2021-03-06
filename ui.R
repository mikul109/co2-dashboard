library(shiny)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(leaflet)
library(dygraphs)

## grab data from github
raw_Data <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")

# world data
world_Data <- subset(raw_Data, iso_code == "OWID_WRL")

shinyUI(
  ## Main ui
  navbarPage(theme = shinytheme("sandstone"),
             title = HTML('<a style="text-decoration:none;
                          cursor:default;
                          font-family: Helvetica, sans-serif;
                          font-size:100%;
                          color:#FFFFFF;">
                          Fossil Fuel Emissions
                          </a>'),
             windowTitle = "Fossil Fuels",
             
  ## First tab
  tabPanel("World Map", 
          # ui/server for first tab
          leafletOutput(outputId = "map", height = 700 ),
           
          absolutePanel(id = "controls", class = "panel panel-default",
                    top = 95, right = 25, width = 250, fixed=TRUE,
                    draggable = TRUE, height = "auto",
                    
                    h3("CO2 Emissions (million tonnes)", style = "font-family: Courier New, monospace; 
                                  opacity: 0.65;", align = "center"),
                    h4(textOutput("total_co2"), align = "center"),
                    h4(textOutput("annual_co2"), align = "center"),
                    h6(textOutput("today_date"), align = "center"),
                    plotOutput("world_daily", height="130px", width="100%"),
                    plotOutput("world_cumulative", height="130px", width="100%"),
            style = "opacity: 0.65")
          ),
  
  ## Second tab
  tabPanel("World Plots",
  sidebarLayout(
             
    sidebarPanel(
      pickerInput(
        "plot", label=h5("Plot"), 
        choices=NULL, 
        options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
        multiple = FALSE),
      pickerInput(
        "forecast", label=h5("Model"), 
        choices=NULL, 
        options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
        multiple = FALSE),
      numericInput("step-ahead",label=h5("Years to Forecast"),0),
      plotlyOutput("w_pie"),
      sliderInput("year_w_pie", "Year", value = max(world_Data$year), min = min(world_Data$year), max = max(world_Data$year), sep = "")
      ),
    mainPanel(
      h4(textOutput("ann_mod")),
      h4(textOutput("ann_aic")),
      fluidRow(
        dygraphOutput("w_ann_line")
      ),
      br(),
      h4(textOutput("tot_mod")),
      h4(textOutput("tot_aic")),
      fluidRow(
        dygraphOutput("w_tot_line")
      )
    ))           
    ),   
           
  ## Third tab
  tabPanel("Country Plots",
  # ui for third tab
  sidebarLayout(
    
    sidebarPanel(
      pickerInput(
        "country", label=h5("Country"), 
        choices=NULL, 
        options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
        multiple = TRUE),
      pickerInput(
        "metric", label=h5("Metric"), 
        choices=NULL, 
        options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
        multiple = FALSE),
      pickerInput(
        "smoothing", label=h5("Moving Average"), 
        choices=NULL, 
        options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
        multiple = FALSE)
      ),
    
      
  # server for third tab
  mainPanel(
  tabsetPanel(type=c("tabs"),
    tabPanel("CO2 Emissions",
             pickerInput(
               "co2", label=h5("Type of Emission"), 
               choices=NULL, 
               options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
               multiple = FALSE),
      fluidRow(
        plotlyOutput("a1")
        ),
      fluidRow(
        plotlyOutput("a2")
      )),
      tabPanel("Industry Breakdown",
               pickerInput(
                 "industry", label=h5("Industry"), 
                 choices=NULL, 
                 options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                 multiple = FALSE),
      fluidRow(
        plotlyOutput("b1")
        ),
      fluidRow(
        plotlyOutput("b2")
      )),
      tabPanel("Other Emissions",
               pickerInput(
                 "emission", label=h5("Greenhouse Gas"), 
                 choices=NULL, 
                 options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                 multiple = FALSE),               
      fluidRow(
        plotlyOutput("c1")
        )
    )
    ))
  )),
  
  ## Fourth Tab
  tabPanel("Data", 
           # ui for fourth tab
           h4("", style = "font-family: Courier New, monospace; 
                                  opacity: 0.65;",
              
              # the '*' applies style to everything (except widgets)
              tags$style(HTML(
                "*{ font-family: Courier New, monospace; }"
              )),
           ),
          sliderInput("yeardt", "Year", value = max(world_Data$year), min = min(world_Data$year), max = max(world_Data$year), sep = ""),

          # server for fourth tab
          DT::dataTableOutput("mytable"),
  
          uiOutput("tab")
    )
  )
)  
