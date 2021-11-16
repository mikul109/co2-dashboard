# Data from:
# Our World in Data
# Link to data https://github.com/owid/co2-data

library(dplyr)
library(tidyr)
library(zoo) # rolling average
library(readr)
library(rgdal) # for readOGR function
library(rmapshaper) # for ms_simplify function
library(ezplot) # for labels
library(DT) # for datatable
library(forecast) # for time-series forecasting
library(dygraphs) # plot the forecasts

## grab data from github
raw_Data <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")

# world data
world_Data <- subset(raw_Data, iso_code == "OWID_WRL")
names(world_Data)[names(world_Data) == "other_industry_co2"] <- "other_co2"
world_Data <- world_Data %>% 
  group_by(country) %>% 
  mutate(cumulative_consumption_co2=cumsum(ifelse(is.na(consumption_co2), 0, consumption_co2)) + consumption_co2*0)

# remove non-countries that have no iso_code, Oceania, Africa, etc.
country_Data <- raw_Data[!(raw_Data$iso_code == ""),]

# remove outliers that have iso_codes, but are not countries
country_Data <- country_Data[!(country_Data$iso_code == "CXR"),] # Christmas Island
country_Data <- country_Data[!(country_Data$iso_code == "OWID_WRL"),] # World
country_Data <- country_Data[!(country_Data$iso_code == "BES"),] # Bonaire Sint Eustatius and Saba
country_Data <- country_Data[!(country_Data$iso_code == "OWID_KOS"),] # Kosovo

# rename countries to match geoson file
country_Data$country[country_Data$country == "Bahamas"] <- "The Bahamas"
country_Data$country[country_Data$country == "Cote d'Ivoire"] <- "Ivory Coast"
country_Data$country[country_Data$country == "Democratic Republic of Congo"] <- "Democratic Republic of the Congo"
country_Data$country[country_Data$country == "Congo"] <- "Republic of Congo" 
country_Data$country[country_Data$country == "Curacao"] <- "CuraÃ§ao"
country_Data$country[country_Data$country == "Czechia"] <- "Czech Republic"
country_Data$country[country_Data$country == "Faeroe Islands"] <- "Faroe Islands"
country_Data$country[country_Data$country == "Guinea-Bissau"] <- "Guinea Bissau"
country_Data$country[country_Data$country == "Hong Kong"] <- "Hong Kong S.A.R."
country_Data$country[country_Data$country ==  "North Macedonia"] <- "Macedonia"
country_Data$country[country_Data$country == "Serbia"] <- "Republic of Serbia"
country_Data$country[country_Data$country == "Eswatini"] <- "Swaziland"
country_Data$country[country_Data$country == "Timor"] <- "East Timor"
country_Data$country[country_Data$country == "Tanzania"] <- "United Republic of Tanzania"
country_Data$country[country_Data$country == "United States"] <- "United States of America"

# rename columns
names(country_Data)[names(country_Data) == "other_industry_co2"] <- "other_co2"
names(country_Data)[names(country_Data) == "ghg_per_capita"] <- "total_ghg_per_capita"

## add calculated columns 
# for co2
country_Data$cumulative_co2_per_capita <- (country_Data$cumulative_co2/country_Data$population)*1000000
country_Data$cumulative_co2_per_gdp <- (country_Data$cumulative_co2/country_Data$gdp)*1000000000

# for industry
country_Data$coal_co2_per_gdp <- (country_Data$coal_co2/country_Data$gdp)*1000000000
country_Data$cement_co2_per_gdp <- (country_Data$cement_co2/country_Data$gdp)*1000000000
country_Data$flaring_co2_per_gdp <- (country_Data$flaring_co2/country_Data$gdp)*1000000000
country_Data$gas_co2_per_gdp <- (country_Data$gas_co2/country_Data$gdp)*1000000000
country_Data$oil_co2_per_gdp <- (country_Data$oil_co2/country_Data$gdp)*1000000000
country_Data$other_co2_per_gdp <- (country_Data$other_co2/country_Data$gdp)*1000000000

# for industry
country_Data$cumulative_coal_co2_per_capita <- (country_Data$cumulative_coal_co2/country_Data$population)*1000000
country_Data$cumulative_coal_co2_per_gdp <- (country_Data$cumulative_coal_co2/country_Data$gdp)*1000000000
country_Data$cumulative_cement_co2_per_capita <- (country_Data$cumulative_cement_co2/country_Data$population)*1000000
country_Data$cumulative_cement_co2_per_gdp <- (country_Data$cumulative_cement_co2/country_Data$gdp)*1000000000
country_Data$cumulative_flaring_co2_per_capita <- (country_Data$cumulative_flaring_co2/country_Data$population)*1000000
country_Data$cumulative_flaring_co2_per_gdp <- (country_Data$cumulative_flaring_co2/country_Data$gdp)*1000000000
country_Data$cumulative_gas_co2_per_capita <- (country_Data$cumulative_gas_co2/country_Data$population)*1000000
country_Data$cumulative_gas_co2_per_gdp <- (country_Data$cumulative_gas_co2/country_Data$gdp)*1000000000
country_Data$cumulative_oil_co2_per_capita <- (country_Data$cumulative_oil_co2/country_Data$population)*1000000
country_Data$cumulative_oil_co2_per_gdp <- (country_Data$cumulative_oil_co2/country_Data$gdp)*1000000000
country_Data$cumulative_other_co2_per_capita <- (country_Data$cumulative_other_co2/country_Data$population)*1000000
country_Data$cumulative_other_co2_per_gdp <- (country_Data$cumulative_other_co2/country_Data$gdp)*1000000000

# for other emissions
country_Data$total_ghg_per_gdp <- (country_Data$total_ghg/country_Data$gdp)*1000000000
country_Data$methane_per_gdp <- (country_Data$methane/country_Data$gdp)*1000000000
country_Data$nitrous_oxide_per_gdp <- (country_Data$nitrous_oxide/country_Data$gdp)*1000000000

# for consumption-based co2
country_Data <- country_Data %>% group_by(country) %>% mutate(cumulative_consumption_co2=cumsum(ifelse(is.na(consumption_co2), 0, consumption_co2)) + consumption_co2*0)
country_Data$cumulative_consumption_co2_per_capita <- (country_Data$cumulative_consumption_co2/country_Data$population)*1000000
country_Data$cumulative_consumption_co2_per_gdp <- (country_Data$cumulative_consumption_co2/country_Data$gdp)*1000000000

## Data for latest year
current_Data <- 
  country_Data %>%
  group_by(country) %>%
  summarise(across(everything(),last))
# remove nulls for cumulative co2
#current_Data <- current_Data[!(current_Data$cumulative_co2 == ""),]

## Data for map. Current cumulative CO2 emissions 

cumulative_co2_Data <- current_Data[, c('iso_code', 'year', 'cumulative_co2')]

saveRDS(cumulative_co2_Data, "cumulative.rds")

### script to get borders for countries on the map
## This script only needs to be run once
## From https://datahub.io/core/geo-countries#r
#download.file("https://datahub.io/core/geo-countries/r/countries.geojson",
              #destfile = "countries.geojson")

  ## readOGR to read geojsons
#world_geojson = readOGR("countries.geojson") 

  ## ms_simplify reduce size of the file from 16MB to 2MB
#world_geojson = ms_simplify(world_geojson,
                            #sys = TRUE, # uses system's mapshaper installation
                              ## which speeds up process
                            #keep_shapes = TRUE) ## keeps small shapes that
  ## would otherwise be lost
  ## we can save this reduced size object and ...
#saveRDS(world_geojson, "countries_map.rds")
  ## delete the original as it is no longer needed
#system("rm countries.geojson")

## load data
cumulative = readRDS("cumulative.rds")
countries = readRDS("countries_map.rds")

## combine new polygons into dataset
mapData = countries

# perform right join to keep all countries available in the cumulative data
mapData@data = right_join(mapData@data, cumulative, by = c('ISO_A3' = 'iso_code'))

# only keep matching data
mapData@polygons = mapData@polygons[countries@data$ISO_A3 %in% cumulative$iso_code]



################ Build the charts ################ 
function(input, output, session) {
### Input
  
  
  ## Options for dropdowns 
  # Sorts country input data alphabetically
  countries <- sort(unique(country_Data$country))
  
  # use to 'translate' dropdown choice
  dropdown <- c("None", "Per Capita", "Per GDP", "None", "7 Years", "30 Years", "Production-based", "Consumption-based", "Total Greenhouse Gases", "Methane", "Nitrous Oxide", "Production-based CO2", "Consumption-based CO2", "Coal CO2", "Cement CO2", "Flaring CO2", "Gas CO2", "Oil CO2", "Other CO2", "NAIVE", "SIMPLE EXPONENTIAL SMOOTHING", "HOLT", "ETS", "ARIMA", "NEURAL NETWORK")
  data_column <-c("", "_per_capita", "_per_gdp", "", "7", "30", "co2", "consumption_co2", "total_ghg", "methane", "nitrous_oxide", "co2", "consumption_co2", "coal_co2", "cement_co2", "flaring_co2", "gas_co2", "oil_co2", "other_co2", "naive", "ses", "holt", "ets","auto.arima", "nnetar") 
  choice <- data.frame(dropdown, data_column)
  
  # Update for country input and metric input
  updatePickerInput(session, "country", choices=countries, selected = c("Brazil", "China", "India", "United States of America"))
  updatePickerInput(session, "metric", choices=c("None", "Per Capita", "Per GDP"), selected = "None")
  updatePickerInput(session, "smoothing", choices=c("None", "7 Years", "30 Years"), selected = "None")
  updatePickerInput(session, "industry", choices=c("coal", "cement", "flaring", "gas", "oil", "other"), selected = "coal")
  updatePickerInput(session, "emission", choices=c("Total Greenhouse Gases", "Methane", "Nitrous Oxide"), selected = "Total Greenhouse Gases")
  updatePickerInput(session, "co2", choices=c("Production-based", "Consumption-based"), selected = "Production-based")
  updatePickerInput(session, "plot", choices=c("Production-based CO2", "Consumption-based CO2", "Coal CO2", "Cement CO2", "Flaring CO2", "Gas CO2", "Oil CO2", "Other CO2"), selected = "Production-based CO2") 
  updatePickerInput(session, "forecast", choices=c("NAIVE", "SIMPLE EXPONENTIAL SMOOTHING", "HOLT", "ETS","ARIMA", "NEURAL NETWORK"), selected = "ARIMA") 
  updateNumericInput(session, "step-ahead", value = 10, min = 0, max = NULL, step = NULL)
  
  
  #### function to get ts forecasts
  get_forecast <- function(metric, plot, fc_model, h) {
    if (identical(plot, "")) return(NULL)
    annual_world <- world_Data[,c('year', ifelse(metric=="cumulative_", paste0("cumulative_", choice[match(plot,choice$dropdown),2]), paste0(choice[match(plot,choice$dropdown),2])))]
    ts_annual = ts(annual_world[,2], start=min(annual_world[,1]))
    
    # get forecast
    v <- c("NAIVE", "HOLT", "SIMPLE EXPONENTIAL SMOOTHING") 
    if (fc_model %in% v){
      forecast = do.call(choice[match(fc_model,choice$dropdown),2], list(ts_annual, h=h))
    } 
    else {
      model <- do.call(choice[match(fc_model,choice$dropdown),2], list(ts_annual))
      forecast <- forecast(model, h=h)
    }
    return(forecast)
  }
  
  
### Output

  ############# FIRST TAB #############
  ## World Map
  output$map <- renderLeaflet({
    # color palette
    pal <- colorNumeric("BuPu", NULL)
    # create leaflet plot
    leaf_map = leaflet(mapData,
                       width = 1040, 
                       height = 800,
                       options = leafletOptions(center = c(30,0),
                                                zoom=2,
                                                maxBounds = list(c(-90, -180),
                                                                 c(90,180)))
    )  %>% addTiles() 
    leaf_map = leaf_map %>%
      addPolygons(stroke = FALSE, 
                  smoothFactor = 0.3,
                  fillOpacity = 1,
                  fillColor = ~pal(log10(cumulative_co2)),
                  label = ~paste0(ADMIN," ", year, ": ",ez_labels(cumulative_co2, signif = 3))
      )
    leaf_map = leaf_map %>%
      addLegend(pal = pal,
                values = ~log10(cumulative_co2),
                title = "Total CO2 Emissions",
                position = c("bottomleft"),
                opacity = 1.0,
                labFormat = labelFormat(transform = function(x) round(10^x))
      )
    leaf_map
  })
  
  ## Absolute panel on world map
  current_world <- subset(world_Data, year == max(year))
  current_co2 <- current_world$co2
  output$annual_co2 <- renderText(paste(ez_labels(current_co2, signif = 3), "Annual co2 "))
  
  current_co2_total <- current_world$cumulative_co2
  output$total_co2 <- renderText(paste(ez_labels(current_co2_total, signif = 3), "Total co2 "))
  
  today_date <- max(subset(current_world, select = c(year)))
  format(today_date, format="%Y")
  output$today_date <- renderText(format(today_date, format="%Y"))
  
  output$world_daily <- renderPlot({
    ggplot(world_Data, aes(x = year, y = co2)) + 
      geom_line() + ylab("Annual co2") +  xlab(NULL)
  })
  
  output$world_cumulative <- renderPlot({
    ggplot(world_Data, aes(x = year, y = cumulative_co2)) + 
      geom_line() + ylab("Cumulative co2") +  xlab(NULL)
  })
  
  ############# SECOND TAB #############
  ## World Plots
  # pie chart
  output$w_pie <- renderPlotly({
    current_world <- world_Data[(world_Data$year == input$year_w_pie),]
    category <- c("coal_co2", "cement_co2", "flaring_co2", "gas_co2", "oil_co2", "other_co2")
    value <- c(as.integer(current_world["coal_co2"][1]), as.integer(current_world["cement_co2"][1]), as.integer(current_world["flaring_co2"][1]), as.integer(current_world["gas_co2"][1]), as.integer(current_world["oil_co2"][1]), as.integer(current_world["other_co2"][1]))
    pie_data <- data.frame(category, value)
    
    colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
    
    fig <- plot_ly(pie_data, labels = category, values = value, type = 'pie',
                   textposition = 'outside',
                   textinfo = 'percent',
                   
                   hoverinfo = 'text',
                   text = ~paste(category, ':', value, ' M'),
                   marker = list(colors = colors,
                                 line = list(color = '#FFFFFF', width = 1)),
                   #The 'pull' attribute can also be used to create space between the sectors
                   showlegend = TRUE)
    fig <- fig %>% layout(title = "CO2 Emissions",
                          paper_bgcolor='transparent',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig
  }) 
  
  output$ann_mod <- renderText({
    paste0("Model: ", input$forecast)
  })
  
  output$ann_aic <- renderText({
    paste0("AIC: ", ifelse(input$forecast %in% c("NAIVE", "NEURAL NETWORK"), "N/A", 
                           round(AIC(get_forecast("annual", input$plot, input$forecast, input$`step-ahead`)$model),2)))
  })
  
  # Annual line chart
  output$w_ann_line <- renderDygraph({
    
    forecast <- get_forecast("annual", input$plot, input$forecast, input$`step-ahead`)
    
    if (identical(input$forecast, "NEURAL NETWORK")){
      # plot forecast
      {cbind(actuals=forecast$x, forecast_mean=forecast$mean)} %>%
        dygraph(main=paste0("World Annual ", input$plot, " Emissions")) %>%
        dySeries("actuals", color = "black") %>%
        dySeries("forecast_mean", label = "forecast", color = "blue") %>%
        dyAxis("x", drawGrid = FALSE) %>%
        dyOptions(includeZero = TRUE, 
                  axisLineColor = "navy", 
                  gridLineColor = "lightblue")
    } else{
      # plot forecast
      {cbind(actuals=forecast$x, forecast_mean=forecast$mean,
             lower_95=forecast$lower[,"95%"], upper_95=forecast$upper[,"95%"])} %>%
        dygraph(main=paste0("World Annual ", input$plot, " Emissions")) %>%
        dySeries("actuals", color = "black") %>%
        dySeries(c("lower_95", "forecast_mean", "upper_95"),
                 label = "forecast", color = "blue") %>%
        dyAxis("x", drawGrid = FALSE) %>%
        dyOptions(includeZero = TRUE, 
                  axisLineColor = "navy", 
                  gridLineColor = "lightblue")
    }
  })

  output$tot_mod <- renderText({
    paste0("Model: ", input$forecast)
  })
  
  output$tot_aic <- renderText({
    paste0("AIC: ", ifelse(input$forecast %in% c("NAIVE", "NEURAL NETWORK"), "N/A", 
                      round(AIC(get_forecast("cumulative_", input$plot, input$forecast, input$`step-ahead`)$model),2)))
  })
  
  # Total line chart
  output$w_tot_line <- renderDygraph({
    
    forecast <- get_forecast("cumulative_", input$plot, input$forecast, input$`step-ahead`)
    
    if (identical(input$forecast, "NEURAL NETWORK")){
      # plot forecast
      {cbind(actuals=forecast$x, forecast_mean=forecast$mean)} %>%
        dygraph(main=paste0("World Total ", input$plot, " Emissions")) %>%
        dySeries("actuals", color = "black") %>%
        dySeries("forecast_mean", label = "forecast", color = "blue") %>%
        dyAxis("x", drawGrid = FALSE) %>%
        dyOptions(includeZero = TRUE, 
                  axisLineColor = "navy", 
                  gridLineColor = "lightblue")
    } else{
      # plot forecast
      {cbind(actuals=forecast$x, forecast_mean=forecast$mean,
             lower_95=forecast$lower[,"95%"], upper_95=forecast$upper[,"95%"])} %>%
        dygraph(main=paste0("World Total ", input$plot, " Emissions")) %>%
        dySeries("actuals", color = "black") %>%
        dySeries(c("lower_95", "forecast_mean", "upper_95"),
                 label = "forecast", color = "blue") %>%
        dyAxis("x", drawGrid = FALSE) %>%
        dyOptions(includeZero = TRUE, 
                  axisLineColor = "navy", 
                  gridLineColor = "lightblue")
    }
  })
  
  ############# THIRD TAB #############
  
  # Annual CO2
  output$a1 <- renderPlotly({
      req(input$country)
      if (identical(input$country, "")) return(NULL)
      annual_co2 <- filter(country_Data, country %in% input$country)[[paste0(choice[match(input$co2,choice$dropdown),2], choice[match(input$metric,choice$dropdown),2])]]
      if (input$smoothing != "None") {annual_co2 <- rollmean(annual_co2, k=strtoi(choice[match(input$smoothing,choice$dropdown),2]), fill=NA)}
      a <- ggplot(data = filter(country_Data, country %in% input$country)) + 
        geom_line(aes(year, 
                      annual_co2,
                      group = country, colour= country)) +
        labs(title = paste0("Annual ", input$co2, " Emissions (million tonnes)"),
             x = "",
             y = "",
             colour = "")
      
      ggplotly(a, tooltip = c("x", "y", "colour"))      
  })
  
  
  # Total CO2
  output$a2 <- renderPlotly({
    req(input$country)
    if (identical(input$country, "")) return(NULL)
    total_co2 <- filter(country_Data, country %in% input$country)[[paste0("cumulative_", choice[match(input$co2,choice$dropdown),2], choice[match(input$metric,choice$dropdown),2])]]
    if (input$smoothing != "None") {total_co2 <- rollmean(total_co2, k=strtoi(choice[match(input$smoothing,choice$dropdown),2]), fill=NA)}
    a <- ggplot(data = filter(country_Data, country %in% input$country)) + 
      geom_line(aes(year, 
                    total_co2,
                    group = country, colour= country)) +
      labs(title = paste0("Total ", input$co2, " Emissions (million tonnes)"),
           x = "",
           y = "",
           colour = "")
    
    ggplotly(a, tooltip = c("x", "y", "colour")) 
  })
 
  # Annual Production-based CO2 (Industry)
  output$b1 <- renderPlotly({
    req(input$country)
    if (identical(input$country, "")) return(NULL)
    annual_co2_industry <- filter(country_Data, country %in% input$country)[[paste0(input$industry, "_co2", choice[match(input$metric,choice$dropdown),2])]]
    if (input$smoothing != "None") {annual_co2_industry <- rollmean(annual_co2_industry, k=strtoi(choice[match(input$smoothing,choice$dropdown),2]), fill=NA)}
    a <- ggplot(data = filter(country_Data, country %in% input$country)) + 
      geom_line(aes(year, 
                    annual_co2_industry,
                    group = country, colour= country)) +
      labs(title = "Annual Production-based Emissions (million tonnes)",
           x = "",
           y = "",
           colour = "") 
    
    ggplotly(a, tooltip = c("x", "y", "colour"))
  })
  
  # Total Production-based CO2 (Industry)
  output$b2 <- renderPlotly({
    req(input$country)
    if (identical(input$country, "")) return(NULL)
    total_co2_industry <- filter(country_Data, country %in% input$country)[[paste0("cumulative_", input$industry, "_co2", choice[match(input$metric,choice$dropdown),2])]]
    if (input$smoothing != "None") {total_co2_industry <- rollmean(total_co2_industry, k=strtoi(choice[match(input$smoothing,choice$dropdown),2]), fill=NA)}
    a <- ggplot(data = filter(country_Data, country %in% input$country)) + 
      geom_line(aes(year, 
                    total_co2_industry,
                    group = country, colour= country)) +
      labs(title = "Total Production-based Emissions (million tonnes)",
           x = "",
           y = "",
           colour = "") 
    
    ggplotly(a, tooltip = c("x", "y", "colour"))
  })
  
  # Total other emissions
  output$c1 <- renderPlotly({
    req(input$country)
    if (identical(input$country, "")) return(NULL)
    total_emissions <- filter(country_Data, country %in% input$country)[[paste0(choice[match(input$emission,choice$dropdown),2], choice[match(input$metric,choice$dropdown),2])]]
    if (input$smoothing != "None") {total_emissions <- rollmean(total_emissions, k=strtoi(choice[match(input$smoothing,choice$dropdown),2]), fill=NA)}
    a <- ggplot(data = filter(country_Data, country %in% input$country)) + 
      geom_line(aes(year, 
                    total_emissions,
                    group = country, colour= country)) +
      labs(title = "Total Production-based Emissions (million tonnes)",
           x = "",
           y = "",
           colour = "") +
      xlim(1980, NA)
    
    ggplotly(a, tooltip = c("x", "y", "colour"))
  })
  
  ############# FOURTH TAB #############
  ## data table on last tab
  # There is also one for shiny, but using DT
  output$mytable = DT::renderDataTable(options = list(scrollX = TRUE), {
    country_Data[(country_Data$year == input$yeardt),]
  })
  
  # LINK TO DATA
  output$tab <- renderUI({
    url <- a("Our World in Data", href="https://github.com/owid/co2-data")
    tagList("Link to data:", url)
  })
}