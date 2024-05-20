library(shiny)
library(shinydashboard)
library(leaflet)
library(httr)
library(jsonlite)
library(ggplot2)
library(maps)
library(plotly)
library(tidyr)
library(dplyr)
library(quantmod)
library(dygraphs)
library(lubridate)
library(htmltools)
library(highcharter)  
library(readr)
library(openair)
city_data <- read.csv("in.csv")

bubble_data <- data.frame(
  Region = c("North America", "Europe", "Asia", "South America", "Africa"),
  Precipitation = c(1000, 800, 1200, 900, 700), # in mm
  Deforestation = c(20, 15, 25, 18, 12) # in percentage
)
temperature_data_world <- read.csv("GlobalLandTemperaturesByCity.csv")
# Preprocess latitude and longitude
temperature_data_world$Latitude <- as.numeric(sub("N|S", "", temperature_data_world$Latitude)) * ifelse(grepl("S", temperature_data_world$Latitude), -1, 1)
temperature_data_world$Longitude <- as.numeric(sub("E|W", "", temperature_data_world$Longitude)) * ifelse(grepl("W", temperature_data_world$Longitude), -1, 1)
View(temperature_data_world)

world_map <- map_data("world")
prec_data = read_csv("precipitation.csv")

temperature_data <- read_csv("GlobalLandTemperaturesByCountry.csv")

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(
    title = div("Weather Dashboard"),
    tags$li(
      class = "dropdown",
      style = "float: right; margin-right: 20px; margin-top: 10px;",
      selectInput("location", "Location:", choices = unique(city_data$city), selected = "Delhi"),
      
    ),
    tags$li(
      class = "dropdown",
      style = "float: right; margin-right: 30px; margin-top: 10px;",
      tags$span(verbatimTextOutput("current_temp_output"), style = "margin-left: 5px;")
    )
    
  ),
  dashboardSidebar(
    collapsed = FALSE,
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("World", tabName = "world"),
      menuItem("Indian city", tabName = "india"),
      menuItem("Contact us", tabName = "contact")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                column(width = 4, style = "margin: 2;",
                       div(style = "max-height: 300px; overflow: hidden;",
                           highchartOutput("temp_line_chart"))),
                column(width = 3, style = "margin: 2;",
                       div(style = "max-height: 300px; overflow: auto;",
                           highchartOutput("donut_chart"))),
                column(width = 5, style = "margin: 2;",
                       div(style = "max-height: 300px; overflow: ;",
                           plotlyOutput("stacked_bar_chart")))
              )
              ,
              fluidRow(
                column(width = 6,
                       div(style = "max-height: 180px; overflow: auto;"),
                       highchartOutput("co2_line_chart")),
                
                column(width = 6,
                       div(style = "max-height: 180px; overflow: auto;"),
                       selectInput("year", "Select Year", choices = unique(substr(temperature_data_world$dt, 1, 4))),
                       plotOutput("world_map_plot"))
                
              )
      ),
      tabItem(tabName = "world",
              fluidRow(
                column(width = 12, style = "margin: 2;",
                       div(style = "max-height: 400px; overflow: auto;",
                           dateRangeInput("dateRange", "Select Date Range:",
                                          start = "2000-01-01",
                                          end = "2024-12-31",
                                          min = "2000-01-01",
                                          max = "2024-12-31",
                                          format = "yyyy-mm-dd"),
                           
                           highchartOutput("sea_level_line_chart"))),
              ),
              fluidRow(
                column(width = 4,
                numericInput("precipitation_filter", "Select Precipitation Level", value = 300),
                tableOutput("filtered_table")),
                column(width=5,
                       numericInput("num_countries", "Number of Countries to Display:", value = 7, min = 1, max = 15),
                       highchartOutput("temp_max_chart")),
                column(width=3,
                       highchartOutput("bubble_chart"))
              )
      ),
      tabItem(tabName = "india",
              fluidRow(
                column(width = 12,
                       selectInput("city", "City:", choices = c("Delhi", "Mumbai", "Chennai", "Jammu"), 
                                   selected = "Delhi") )
              ),
              fluidRow(
                column(width = 6,
                       div(style = "max-height: 280px; overflow: auto;"),
                       highchartOutput("precipitation_plot")),
                column(width = 6,
                       div(style = "max-height: 280px; overflow: auto;"),
                       highchartOutput("temperature_plot"))
              ),
              fluidRow(
                column(width = 4,
                       div(style = "max-height: 280px; overflow: auto;"),
                       highchartOutput("density_plot")),
                column(width = 5,
                       plotOutput("boxplot")),
                column(width = 3,
                       plotOutput("windrose"))
                
              )
              
      ),
      tabItem(tabName = "contact",

      fluidRow(
        column(width = 4,
               h2("Jinal Suthar "),
               h4("Email:"),
               p("suthar.5@iitj.ac.in"),
               h4("GitHub:"),
               p("github.com/jinalsuthar25")
        ),
        column(width = 4,
               h2("Bikkavolu Prasanthi"),
               h4("Email:"),
               p("prasanthi.1@iit.ac.in"),
               h4("GitHub:"),
               p("github.com/Prasanthi1201")
        )
      ,
    
        column(width = 4,
               h2("Mamidipalli Divya Meghana"),
               h4("Email:"),
               p("mamidipalli.1@iitj.ac.in"),
               h4("GitHub:"),
               p("github.com/DivyaMeghana766")
        )
      
      )
    )
  )
))


# Define server logic
server <- function(input, output) {
  # Read city coordinates data
  city_data <- read.csv("in.csv")
  # Function to fetch latitude and longitude based on selected location
  get_coordinates <- reactive({
    selected_city <- input$location
    city_info <- city_data[city_data$city == selected_city, c("lat", "lng")]
    return(city_info)
  })
  # Function to fetch weather data from the API
  fetch_weather_data <- function(latitude, longitude) {
    url <- paste0("https://api.open-meteo.com/v1/forecast?latitude=", latitude, "&longitude=", longitude, "&current=temperature_2m,relative_humidity_2m,apparent_temperature,is_day,precipitation,rain,showers,snowfall,weather_code,cloud_cover,pressure_msl,surface_pressure,wind_speed_10m,wind_direction_10m,wind_gusts_10m&timezone=auto")
    
    response <- httr::GET(url)
    
    if (httr::http_error(response)) {
      stop("Failed to fetch data from the API.")
    }
    
    return(httr::content(response))
  }
  output$current_temp_output <- renderText({
    # Fetch weather data for selected location
    coordinates <- get_coordinates()
    weather_data <- fetch_weather_data(coordinates$lat, coordinates$lng)
    
    current_temp <- weather_data$current$temperature_2m
    paste("Current Temperature:", current_temp, "°C")
  })

  
  output$bubble_chart <- renderHighchart({
    hchart(height = 200, bubble_data, "scatter", hcaes(x = Precipitation, y = Deforestation, group = Region)) %>%
      hc_title(text = "Scatter Plot: Precipitation vs. Deforestation vs. Region") %>%
      hc_xAxis(title = list(text = "Precipitation (mm)")) %>%
      hc_yAxis(title = list(text = "Deforestation (%)")) %>%
      hc_tooltip(pointFormat = "Region: {point.Region}<br>Precipitation: {point.Precipitation} mm<br>Deforestation: {point.Deforestation}%<br>") %>%
      hc_plotOptions(series = list(marker = list(radius = 10, symbol = "circle"))) # Adjust marker size
  })
  
  sector_data <- data.frame(
    Sector = c("Energy", "Agriculture", "Industry", "Transportation", "Buildings", "Waste", "Land Use, Land-Use Change, and Forestry"),
    Emissions = c(25, 15, 20, 20, 10, 5, 5) # Emissions in percentage
  )
  
  # Calculate total emissions
  total_emissions <- sum(sector_data$Emissions)
  
  # Calculate percentage for each sector
  sector_data$Percentage <- sector_data$Emissions / total_emissions * 100
  
  # Create the variable pie chart
  output$donut_chart <- renderHighchart({
    hchart(height = 300, sector_data, "pie", hcaes(x = Sector, y = Percentage)) %>%
      hc_legend(enabled = TRUE)
  })
  
  
  # Read data from CSV file
  GlobalTemperatures <- read_csv("GlobalTemperatures.csv")
  
  
  # Filter data for years 1990 to 2015
  filtered_data <- GlobalTemperatures %>%
    filter(substr(dt, 1, 4) %in% as.character(1790:2015))
  
  # Take only the data for the first month of each year
  filtered_data <- filtered_data %>%
    group_by(year = substr(dt, 1, 4)) %>%
    slice(1)
  
  
  
  output$temp_line_chart <- renderHighchart({
    highchart(height = 300) %>%  # Set the height here (e.g., 300 pixels)
      
      hc_xAxis(categories = filtered_data$year) %>%
      hc_yAxis(title = list(text = "Temperature (°C)")) %>%
      hc_series(list(name = "Temperature", data = filtered_data$LandAverageTemperature, type = "line"))
  })
  
  
  observe({
    # Aggregate maximum temperatures for each country
    max_temperatures <- aggregate(AverageTemperature ~ Country, data = temperature_data, FUN = max)
    
    # Sort the temperatures in descending order and select the top 'x' countries
    top_x_temperatures <- head(max_temperatures[order(max_temperatures$AverageTemperature, decreasing = TRUE), ], input$num_countries)
    
    # Render bar chart
    output$temp_max_chart <- renderHighchart({
      highchart() %>%
        hc_title(text = paste("Top", input$num_countries, "Maximum Temperatures by Country")) %>%
        hc_xAxis(categories = top_x_temperatures$Country) %>%
        hc_yAxis(title = list(text = "Maximum Temperature")) %>%
        hc_series(list(name = "Maximum Temperature", data = top_x_temperatures$AverageTemperature, type = "bar"))
    })
  })
  
  
  # Load sea level change data
  sea_level_data <- read_csv("sealevel.csv", col_types = cols(Date = col_date(format = "%m/%d/%Y")))
  
  # Filter sea level data based on date range input
  filtered_sea_level_data <- reactive({
    req(input$dateRange)
    sea_level_data %>%
      filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
  })
 
  # Render sea level area chart
  output$sea_level_line_chart <- renderHighchart({
    filtered_sea_level_data() %>%
      hchart(height = 200, type = "area", hcaes(x = Date, y = Value, group = Country)) %>%
      hc_title(text = "Sea Level Change Over Time ") %>%
      hc_xAxis(title = list(text = "Date")) %>%
      hc_yAxis(title = list(text = "Change in Sea Level (mm)")) %>%
      hc_legend(layout = "vertical", align = "right", verticalAlign = "middle")
  })
  
  # Read data from CSV file
  co2_emission <- read_csv("co2_emmision.csv")
  
  # Subset data for the specified countries and years
  countries_of_interest <- c("India", "North America", "Australia", "China", "Mexico")
  years_of_interest <- as.character(1990:2020)
  
  # Select columns of interest by position
  co2_subset <- co2_emission %>%
    filter(`Data Source` %in% countries_of_interest) %>%
    select(`Data Source`, 35:ncol(co2_emission))
  
  # Reshape the data for plotting using tidyr::pivot_longer
  co2_melted <- co2_subset %>%
    pivot_longer(cols = -1, names_to = "Year", values_to = "CO2_Emission", values_drop_na = TRUE)
  
  # Convert Year to numeric for correct ordering
  co2_melted$Year <- as.numeric(co2_melted$Year)
  
  # Render line chart
  output$co2_line_chart <- renderHighchart({
    highchart(height=450) %>%
      hc_title(text = "CO2 Emissions from 1990 to 2020") %>%
      hc_xAxis(categories = unique(co2_melted$Year)) %>%
      hc_yAxis(title = list(text = "CO2 Emission")) %>%
      hc_add_series(data = co2_melted[co2_melted$`Data Source` == "India",]$CO2_Emission, name = "India") %>%
      hc_add_series(data = co2_melted[co2_melted$`Data Source` == "North America",]$CO2_Emission, name = "North America") %>%
      hc_add_series(data = co2_melted[co2_melted$`Data Source` == "Australia",]$CO2_Emission, name = "Australia") %>%
      hc_add_series(data = co2_melted[co2_melted$`Data Source` == "China",]$CO2_Emission, name = "China") %>%
      hc_add_series(data = co2_melted[co2_melted$`Data Source` == "Mexico",]$CO2_Emission, name = "United Kingdom")
  })
  
  # Read data from CSV file
  data <- read_csv("Climate-related_Disasters_Frequency.csv")
  
  # Filter out rows with unknown disaster types
  data_filtered <- filter(data, Indicator %in% c("Climate related disasters frequency, Number of Disasters: Drought", "Climate related disasters frequency, Number of Disasters: Flood", "Climate related disasters frequency, Number of Disasters: Landslide"))
  
  # Filter out incompatible columns and select only years from "F2000" to "F2020"
  data_filtered <- select(data_filtered, Country, starts_with("F20"), Indicator)
  
  # Reshape the data to long format
  data_long <- pivot_longer(data_filtered, cols = -c(Country, Indicator), names_to = "Year", values_to = "Frequency")
  
  # Extract year from column names and convert to numeric
  data_long$Year <- as.numeric(sub("^F([0-9]{4})$", "\\1", data_long$Year))
  
  # Filter years from 2000 to 2020
  data_long <- filter(data_long, Year >= 2000 & Year <= 2020)
  
  # Summarize frequencies for each year and disaster type
  data_summarized <- data_long %>%
    group_by(Year, Indicator) %>%
    summarise(Total_Frequency = sum(Frequency, na.rm = TRUE))
  
  # Pivot the data to wide format for plotting
  data_wide <- pivot_wider(data_summarized, names_from = Indicator, values_from = Total_Frequency, values_fill = 0)
  
  # Plot stacked bar chart for each year
  output$stacked_bar_chart <- renderPlotly({
    # Create a plotly bar chart
    plot_ly(data_wide, x = ~Year) %>%
      add_trace(y = ~`Climate related disasters frequency, Number of Disasters: Flood`, name = "Flood", type = "bar") %>%
      add_trace(y = ~`Climate related disasters frequency, Number of Disasters: Drought`, name = "Drought", type = "bar") %>%
      add_trace(y = ~`Climate related disasters frequency, Number of Disasters: Landslide`, name = "Landslide", type = "bar") %>%
      layout(title = "Climate Related Disasters",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Total Frequency"),
             barmode = "stack",
             height = 300)
  })
  
  ##########################################################################################################################################
  ################################################### 2ND PAGE #############################################################################
  
  
  observe({
    if (input$city == "Delhi") {
      weather <- readr::read_csv("delhi2019-2021.csv")
     
      # Remove last 6 characters from the time column
      weather$time <- substr(weather$time, 1, 10)
      
      
      
      # Render precipitation plot
      output$precipitation_plot <- renderHighchart({
        highchart() %>%
          hc_chart(type = "line") %>%
          hc_title(text = "Precipitation (2019-2022)") %>%
          hc_xAxis(categories = unique(weather$time), title = list(text = "Date")) %>%
          hc_yAxis(title = list(text = "Precipitation (mm)")) %>%
          hc_add_series(data = weather, type = "line", hcaes(x = time, y = `precipitation_sum (mm)`))
      })
      
      
      # Render temperature plot
      output$temperature_plot <- renderHighchart({
        highchart() %>%
          hc_title(text = "Temperature (2019-2022)") %>%
          hc_xAxis(categories = weather$time) %>%
          hc_yAxis(title = list(text = "Temperature (°C)")) %>%
          hc_series(list(name = "Max Temperature", data = weather$`temperature_2m_max (°C)`),
                    list(name = "Min Temperature", data = weather$`temperature_2m_min (°C)`),
                    list(name = "Mean Temperature", data = weather$`temperature_2m_mean (°C)`))
      })
      
      # Create density plot
      output$density_plot <- renderHighchart({
        # Filter out missing values
        weather_filtered <- weather[!is.na(weather$`shortwave_radiation_sum (MJ/m²)`), ]
        
        # Calculate density values
        density_values <- density(weather_filtered$`shortwave_radiation_sum (MJ/m²)`)
        
        # Create Highchart density plot
        highchart() %>%
          hc_chart(type = "area") %>%
          hc_title(text = "Density Plot of Shortwave Radiation Sum") %>%
          hc_xAxis(title = list(text = "Shortwave Radiation Sum (MJ/m²)")) %>%
          hc_yAxis(title = list(text = "Density")) %>%
          hc_add_series(data = density_values$y, type = "area", name = "Density", color = "#7cb5ec") %>%
          hc_plotOptions(area = list(fillColor = "#7cb5ec", marker = list(enabled = FALSE))) # Adjust area plot options as needed
      })
      
      
      
      # Define the list of row numbers and column names you want to select
      selected_rows <- c(2, 33, 61, 92, 122, 153, 367, 398, 427, 458, 488, 519, 733, 764, 792, 823, 853, 884, 
                         1173, 1190, 1192, 1216, 1282, 1272)  # Example row numbers
      selected_columns <- c("time", "sunshine_duration (s)")  # Example column names
      
      # Create a dataframe with the selected rows and columns
      data_box <- weather[selected_rows, selected_columns]
      
      data_box$time <- as.Date(data_box$time)
      View(data_box)
      output$boxplot <- renderPlot({
        ggplot(data_box, aes(x = factor(format(time, "%Y")), y = `sunshine_duration (s)`, fill = factor(format(time, "%Y")))) +
          geom_boxplot() +
          scale_fill_brewer(palette = "Set3") +  # Choose a color palette
          labs(x = "Year", y = "sunshine_duration (s)", title = "Sunshine Duration Boxplots by Year")
      })
      
      names(weather)[names(weather) == "wind_speed_10m_max (km/h)"] <- "ws"
      names(weather)[names(weather) == "wind_direction_10m_dominant (°)"] <- "wd"
      # Render wind rose plot
      output$windrose <- renderPlot({
        
        # Create wind rose plot    windRose(delhi_weather, ws.int = delhi_weather$wind_speed_10m_max (km/h), angle.scale  = delhi_weather$wind_direction_10m_dominant (°), type = "default")
        
        
        windRose(weather, ws.int = 8, angle.scale  = 60,cols= 'increment',paddle=FALSE, type = "default")
      })
      
    } 
    else if (input$city == "Mumbai"){
      weather <- readr::read_csv("mumbai2019-2022.csv")
     
      # Remove last 6 characters from the time column
      weather$time <- substr(weather$time, 1, 10)
      
      
      
      # Render precipitation plot
      output$precipitation_plot <- renderHighchart({
        highchart() %>%
          hc_chart(type = "line") %>%
          hc_title(text = "Precipitation (2019-2022)") %>%
          hc_xAxis(categories = unique(weather$time), title = list(text = "Date")) %>%
          hc_yAxis(title = list(text = "Precipitation (mm)")) %>%
          hc_add_series(data = weather, type = "line", hcaes(x = time, y = `precipitation_sum (mm)`))
      })
      
      
      # Render temperature plot
      output$temperature_plot <- renderHighchart({
        highchart() %>%
          hc_title(text = "Temperature (2019-2022)") %>%
          hc_xAxis(categories = weather$time) %>%
          hc_yAxis(title = list(text = "Temperature (°C)")) %>%
          hc_series(list(name = "Max Temperature", data = weather$`temperature_2m_max (°C)`),
                    list(name = "Min Temperature", data = weather$`temperature_2m_min (°C)`),
                    list(name = "Mean Temperature", data = weather$`temperature_2m_mean (°C)`))
      })
      
      # Create density plot
      output$density_plot <- renderHighchart({
        # Filter out missing values
        weather_filtered <- weather[!is.na(weather$`shortwave_radiation_sum (MJ/m²)`), ]
        
        # Calculate density values
        density_values <- density(weather_filtered$`shortwave_radiation_sum (MJ/m²)`)
        
        # Create Highchart density plot
        highchart() %>%
          hc_chart(type = "area") %>%
          hc_title(text = "Density Plot of Shortwave Radiation Sum") %>%
          hc_xAxis(title = list(text = "Shortwave Radiation Sum (MJ/m²)")) %>%
          hc_yAxis(title = list(text = "Density")) %>%
          hc_add_series(data = density_values$y, type = "area", name = "Density", color = "#7cb5ec") %>%
          hc_plotOptions(area = list(fillColor = "#7cb5ec", marker = list(enabled = FALSE))) # Adjust area plot options as needed
      })
      
      
      
      # Define the list of row numbers and column names you want to select
      selected_rows <- c(2, 33, 61, 92, 122, 153, 367, 398, 427, 458, 488, 519, 733, 764, 792, 823, 853, 884, 
                         1173, 1190, 1192, 1216, 1282, 1272)  # Example row numbers
      selected_columns <- c("time", "sunshine_duration (s)")  # Example column names
      
      # Create a dataframe with the selected rows and columns
      data_box <- weather[selected_rows, selected_columns]
      
      data_box$time <- as.Date(data_box$time)
      View(data_box)
      output$boxplot <- renderPlot({
        ggplot(data_box, aes(x = factor(format(time, "%Y")), y = `sunshine_duration (s)`, fill = factor(format(time, "%Y")))) +
          geom_boxplot() +
          scale_fill_brewer(palette = "Set3") +  # Choose a color palette
          labs(x = "Year", y = "sunshine_duration (s)", title = "Sunshine Duration Boxplots by Year")
      })
      
      names(weather)[names(weather) == "wind_speed_10m_max (km/h)"] <- "ws"
      names(weather)[names(weather) == "wind_direction_10m_dominant (°)"] <- "wd"
      # Render wind rose plot
      output$windrose <- renderPlot({
        
        # Create wind rose plot    windRose(delhi_weather, ws.int = delhi_weather$wind_speed_10m_max (km/h), angle.scale  = delhi_weather$wind_direction_10m_dominant (°), type = "default")
        
        
        windRose(weather, ws.int = 8, angle.scale  = 60,cols= 'increment',paddle=FALSE, type = "default")
      })
    }
    else if (input$city == "Chennai")
    {
      weather <- readr::read_csv("chennai2019-2022.csv")
      
      # Remove last 6 characters from the time column
      weather$time <- substr(weather$time, 1, 10)
      
      
      # Render precipitation plot
      output$precipitation_plot <- renderHighchart({
        highchart() %>%
          hc_chart(type = "line") %>%
          hc_title(text = "Precipitation (2019-2022)") %>%
          hc_xAxis(categories = unique(weather$time), title = list(text = "Date")) %>%
          hc_yAxis(title = list(text = "Precipitation (mm)")) %>%
          hc_add_series(data = weather, type = "line", hcaes(x = time, y = `precipitation_sum (mm)`))
      })
      
      
      # Render temperature plot
      output$temperature_plot <- renderHighchart({
        highchart() %>%
          hc_title(text = "Temperature (2019-2022)") %>%
          hc_xAxis(categories = weather$time) %>%
          hc_yAxis(title = list(text = "Temperature (°C)")) %>%
          hc_series(list(name = "Max Temperature", data = weather$`temperature_2m_max (°C)`),
                    list(name = "Min Temperature", data = weather$`temperature_2m_min (°C)`),
                    list(name = "Mean Temperature", data = weather$`temperature_2m_mean (°C)`))
      })
      
      # Create density plot
      output$density_plot <- renderHighchart({
        # Filter out missing values
        weather_filtered <- weather[!is.na(weather$`shortwave_radiation_sum (MJ/m²)`), ]
        
        # Calculate density values
        density_values <- density(weather_filtered$`shortwave_radiation_sum (MJ/m²)`)
        
        # Create Highchart density plot
        highchart() %>%
          hc_chart(type = "area") %>%
          hc_title(text = "Density Plot of Shortwave Radiation Sum") %>%
          hc_xAxis(title = list(text = "Shortwave Radiation Sum (MJ/m²)")) %>%
          hc_yAxis(title = list(text = "Density")) %>%
          hc_add_series(data = density_values$y, type = "area", name = "Density", color = "#7cb5ec") %>%
          hc_plotOptions(area = list(fillColor = "#7cb5ec", marker = list(enabled = FALSE))) # Adjust area plot options as needed
      })
      
      
      
      # Define the list of row numbers and column names you want to select
      selected_rows <- c(2, 33, 61, 92, 122, 153, 367, 398, 427, 458, 488, 519, 733, 764, 792, 823, 853, 884, 
                         1173, 1190, 1192, 1216, 1282, 1272)  # Example row numbers
      selected_columns <- c("time", "sunshine_duration (s)")  # Example column names
      
      # Create a dataframe with the selected rows and columns
      data_box <- weather[selected_rows, selected_columns]
      
      data_box$time <- as.Date(data_box$time)
      View(data_box)
      output$boxplot <- renderPlot({
        ggplot(data_box, aes(x = factor(format(time, "%Y")), y = `sunshine_duration (s)`, fill = factor(format(time, "%Y")))) +
          geom_boxplot() +
          scale_fill_brewer(palette = "Set3") +  # Choose a color palette
          labs(x = "Year", y = "sunshine_duration (s)", title = "Sunshine Duration Boxplots by Year")
      })
      
      names(weather)[names(weather) == "wind_speed_10m_max (km/h)"] <- "ws"
      names(weather)[names(weather) == "wind_direction_10m_dominant (°)"] <- "wd"
      # Render wind rose plot
      output$windrose <- renderPlot({
        
        # Create wind rose plot    windRose(delhi_weather, ws.int = delhi_weather$wind_speed_10m_max (km/h), angle.scale  = delhi_weather$wind_direction_10m_dominant (°), type = "default")
        
        
        windRose(weather, ws.int = 8, angle.scale  = 60,cols= 'increment',paddle=FALSE, type = "default")
      })
      
    }
    
    else if (input$city == "Jammu")
    {
      weather <- readr::read_csv("jammu2019-2022.csv")
      
      # Remove last 6 characters from the time column
      weather$time <- substr(weather$time, 1, 10)
      
      
      
      # Render precipitation plot
      output$precipitation_plot <- renderHighchart({
        highchart() %>%
          hc_chart(type = "line") %>%
          hc_title(text = "Precipitation (2019-2022)") %>%
          hc_xAxis(categories = unique(weather$time), title = list(text = "Date")) %>%
          hc_yAxis(title = list(text = "Precipitation (mm)")) %>%
          hc_add_series(data = weather, type = "line", hcaes(x = time, y = `precipitation_sum (mm)`))
      })
      
      
      # Render temperature plot
      output$temperature_plot <- renderHighchart({
        highchart() %>%
          hc_title(text = "Temperature (2019-2022)") %>%
          hc_xAxis(categories = weather$time) %>%
          hc_yAxis(title = list(text = "Temperature (°C)")) %>%
          hc_series(list(name = "Max Temperature", data = weather$`temperature_2m_max (°C)`),
                    list(name = "Min Temperature", data = weather$`temperature_2m_min (°C)`),
                    list(name = "Mean Temperature", data = weather$`temperature_2m_mean (°C)`))
      })
      
      # Create density plot
      output$density_plot <- renderHighchart({
        # Filter out missing values
        weather_filtered <- weather[!is.na(weather$`shortwave_radiation_sum (MJ/m²)`), ]
        
        # Calculate density values
        density_values <- density(weather_filtered$`shortwave_radiation_sum (MJ/m²)`)
        
        # Create Highchart density plot
        highchart() %>%
          hc_chart(type = "area") %>%
          hc_title(text = "Density Plot of Shortwave Radiation Sum") %>%
          hc_xAxis(title = list(text = "Shortwave Radiation Sum (MJ/m²)")) %>%
          hc_yAxis(title = list(text = "Density")) %>%
          hc_add_series(data = density_values$y, type = "area", name = "Density", color = "#7cb5ec") %>%
          hc_plotOptions(area = list(fillColor = "#7cb5ec", marker = list(enabled = FALSE))) # Adjust area plot options as needed
      })
      
      
      
      # Define the list of row numbers and column names you want to select
      selected_rows <- c(2, 33, 61, 92, 122, 153, 367, 398, 427, 458, 488, 519, 733, 764, 792, 823, 853, 884, 
                         1173, 1190, 1192, 1216, 1282, 1272)  # Example row numbers
      selected_columns <- c("time", "sunshine_duration (s)")  # Example column names
      
      # Create a dataframe with the selected rows and columns
      data_box <- weather[selected_rows, selected_columns]
      
      data_box$time <- as.Date(data_box$time)
      View(data_box)
      output$boxplot <- renderPlot({
        ggplot(data_box, aes(x = factor(format(time, "%Y")), y = `sunshine_duration (s)`, fill = factor(format(time, "%Y")))) +
          geom_boxplot() +
          scale_fill_brewer(palette = "Set3") +  # Choose a color palette
          labs(x = "Year", y = "sunshine_duration (s)", title = "Sunshine Duration Boxplots by Year")
      })
      
      names(weather)[names(weather) == "wind_speed_10m_max (km/h)"] <- "ws"
      names(weather)[names(weather) == "wind_direction_10m_dominant (°)"] <- "wd"
      # Render wind rose plot
      output$windrose <- renderPlot({
        
        # Create wind rose plot    windRose(delhi_weather, ws.int = delhi_weather$wind_speed_10m_max (km/h), angle.scale  = delhi_weather$wind_direction_10m_dominant (°), type = "default")
        
        
        windRose(weather, ws.int = 8, angle.scale  = 60,cols= 'increment',paddle=FALSE, type = "default")
      })
    }
    
  })
    
    
 
  
  ######################################### WORLD MAP ###################################################################
  
  # Create world map
  
  filtered_data2 <- reactive({
    temperature_data_world %>%
      filter(substr(dt, 1, 4) == input$year)
  })
  # Generate world temperature heatmap
  output$world_map_plot <- renderPlot({
    # Get world map data
    world_map <- map_data("world")
    
    # Merge temperature data with world map data
    merged_data <- merge(world_map, filtered_data2(), by.x = c("region"), by.y = c("Country"), all.x = TRUE)
    
    # Create the plot
    ggplot() +
      geom_map(data = merged_data, map = merged_data,
               aes(x = long, y = lat, map_id = region, fill = AverageTemperature),
               color = "black", size = 0.15) +
      scale_fill_gradient(name = "Temperature (°C)", low = "blue", high = "red", guide = "legend") +
      labs(title = "World Temperature Heatmap", x = "Longitude", y = "Latitude") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  

  #################################3RECORDS#########################################
  
  filtered_data3 <- reactive({
    filtered <- prec_data %>%
      select("Country Name", "Country Code", "2020") %>%
      rename(Precipitation = "2020")
    
    # Check for numeric values in the Precipitation column
    if (!is.numeric(filtered$Precipitation)) {
      filtered$Precipitation <- as.numeric(as.character(filtered$Precipitation))
    }
    
    # Apply filter
    filtered <- filtered %>%
      filter(!is.na(Precipitation), Precipitation >= input$precipitation_filter) %>%
      head(9)  # Limit to 10 rows
    
    return(filtered)
  })
  

  # Render the filtered table
  output$filtered_table <- renderTable({
    filtered_data3()
  })
  
  
}


# Run the application
shinyApp(ui = ui, server = server)


