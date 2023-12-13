# Library's
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(ggplot2)

# Read the accident data and get rid of numbers by county
accident <- read.csv("accident.csv")
accident$COUNTYNAME <- gsub("\\s*\\(\\d+\\)", "", accident$COUNTYNAME)

# Read the vehicle data
vehicles <- read.csv("vehicle.csv")

# Organize data for bar charts 
aggregated_data <- vehicles %>%
  group_by(ST_CASE) %>%
  summarize(
    TotalVehicles = n(),
    SpeedingVehicles = sum(SPEEDRELNAME %in% c("Yes, Too Fast for Conditions",
                                               "Yes, Exceeded Speed Limit",
                                               "Yes, Too Fast for Conditions",
                                               "Yes, Specifics Unknown")),
    NonSpeedingVehicles = sum(SPEEDRELNAME %in% c("No",
                                                  "No Driver Present/Unknown if Driver Present",
                                                  "Reported as Unknown")),
    DrinkingDrivers = sum(DR_DRINKNAME == "Yes"),
    NonDrinkingDrivers = sum(DR_DRINKNAME == "No")
  )

# Merge Data 
merged_data_0 <- merge(accident, vehicles, by = "ST_CASE")
merged_data <- merge(merged_data_0, aggregated_data, by = "ST_CASE")


# Define UI for the Shiny application
#Header Title and Sidebar names 
ui <- dashboardPage(
  dashboardHeader(title = "2021 Car Accident Info Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Accident Map", tabName = "map_tab"),
      menuItem("Vehicle Map", tabName = "vehicle_tab"),
      menuItem("Speeding vs Not Speeding", tabName = "speed_tab"),
      menuItem("Drinking vs Not Drinking", tabName = "drinking_tab")
    )
  ),
  dashboardBody(
    tabItems(
      # First tab Accident Map
      tabItem(
        tabName = "map_tab",
        fluidPage(
          titlePanel("Car Accident Location"),
          sidebarLayout(
            sidebarPanel(
              selectInput("Accident_State", "Select State", unique(accident$STATENAME)),
              selectInput("County", "Select County", "All"),
              br(),
              tableOutput("accidentCountTable")
            ),
            mainPanel(
              leafletOutput("map", width = "100%", height = "600px")
            )
          )
        )
      ),
      # Second tab Vehicle Map
      tabItem(
        tabName = "vehicle_tab",
        fluidPage(
          titlePanel("Specific Vehicle Type Location"),
          sidebarLayout(
            sidebarPanel(
              selectInput("VehicleType", "Select Vehicle Type", unique(vehicles$VPICMAKENAME)),
              selectInput("Vehicle_State", "Select State", unique(vehicles$STATENAME)),
              br(),
              tableOutput("vehicleCountTable")
            ),
            mainPanel(
              leafletOutput("vehicle_map", width = "100%", height = "600px")
            )
          )
        )
      ),
      # Third tab Speeding vs Not Speeding
      tabItem(
        tabName = "speed_tab",
        fluidPage(
          titlePanel("Speeding Affecting Accidents"),
          sidebarLayout(
            sidebarPanel(
              selectInput("Speed_State", "Select State", unique(accident$STATENAME)),
            ),
            mainPanel(
              plotOutput("speed_death_plot", width = "100%", height = "500px")
            )
          )
        )
      ),
      #Fourth Tab Drinking vs Non Drinking 
      tabItem(
        tabName = "drinking_tab",
        fluidPage(
          titlePanel("Accidents Involving Drinking vs. Non-Drinking Drivers"),
          sidebarLayout(
            sidebarPanel(
              selectInput("Drinking_State", "Select State", unique(accident$STATENAME)),
            ),
            mainPanel(
              plotOutput("drinking_plot", width = "100%", height = "500px")
            )
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Observer for updating county choices based on selected state for Location Map 1
  observe({
    selected_state <- input$Accident_State
    if (!is.null(selected_state) && selected_state != "All") {
      counties <- unique(accident$COUNTYNAME[accident$STATENAME == selected_state])
      updateSelectInput(session, "County", choices = c("All", counties))
    } else {
      updateSelectInput(session, "County", choices = "All")
    }
  })
  
  # Observer for updating vehicle type choices in vehicle map 
  observe({
    updateSelectInput(session, "VehicleType", choices = unique(vehicles$VPICMAKENAME))
  })
  
  # Reactive function for filtered accident data for map 1
  filtered_accident <- reactive({
    filtered1 <- accident
    
    if (!is.null(input$Accident_State) && input$Accident_State != "All") {
      filtered1 <- filter(filtered1, STATENAME == input$Accident_State)
    }
    
    if (!is.null(input$County) && input$County != "All") {
      filtered1 <- filter(filtered1, COUNTYNAME == input$County)
    }
    
    filtered1
  })
  
  # Reactive function for filtered vehicle data for map 2 
  filtered_vehicles <- reactive({
    filtered2 <- merged_data
    
    if (!is.null(input$VehicleType) && input$VehicleType != "All") {
      filtered2 <- filter(filtered2, VPICMAKENAME == input$VehicleType)
    }
    
    if (!is.null(input$Vehicle_State) && input$Vehicle_State != "All") {
      filtered2 <- filter(filtered2, STATENAME.x == input$Vehicle_State)
    }
    
    filtered2
  })
  
  # Reactive function for filtered speed data bar tab 3
  filtered_speed_data <- reactive({
    filtered <- merged_data
    
    if (!is.null(input$Speed_State) && input$Speed_State != "All") {
      filtered <- filter(filtered, STATENAME.x == input$Speed_State)
    }
    
    if (!is.null(input$Speed_Limit) && input$Speed_Limit != "All") {
      filtered <- filter(filtered, VSPD_LIM == as.numeric(input$Speed_Limit))
    }
    
    
    filtered <- mutate(filtered, Speeding = ifelse(SpeedingVehicles > 0, "Speeding", "Not Speeding"))
    
    filtered
  })
  
  filtered_drinking_data <- reactive({
    filtered3 <- merged_data
    
    if (!is.null(input$Drinking_State) && input$Drinking_State != "All") {
      filtered3 <- filter(filtered3, STATENAME.x == input$Drinking_State)
    }
    
    
    filtered3 <- mutate(filtered3, Drinking = ifelse(DrinkingDrivers > 0, "Drinking", "Not Drinking"))
    
    filtered3
  })
  
  # leaflet map for accidents 1 tab 1
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -95.7129, lat = 37.0902, zoom = 3.5) %>%
      addMarkers(data = filtered_accident(),
                 lat = ~LATITUDE,
                 lng = ~LONGITUD,
                 popup = ~paste("Accident ID:", ST_CASE, 
                                "<br>", "Description:", HARM_EVNAME,
                                "<br>", "Weather:", WEATHERNAME))
  })
  
  # leaflet map for vehicles map 2 tab 2 
  output$vehicle_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -95.7129, lat = 37.0902, zoom = 3.5) %>%
      addMarkers(data = filtered_vehicles(),
                 lat = ~LATITUDE,
                 lng = ~LONGITUD,
                 popup = ~paste("Vehicle ID:", VEH_NO,
                                "<br>", "Type:", VPICMODELNAME,
                                "<br>", "Make:", VPICMAKENAME))
  })
  
  # table for accident counts tab 1
  output$accidentCountTable <- renderTable({
    data <- filtered_accident()
    if (!is.null(input$Accident_State) && input$Accident_State != "All") {
      state_accidents <- data %>%
        group_by("State" = STATENAME) %>%
        summarise("Number of Accidents" = n())
      state_accidents
    } else {
      NULL
    }
  })
  
  # table for vehicle counts tab 2 
  output$vehicleCountTable <- renderTable({
    data <- filtered_vehicles()
    if (!is.null(input$VehicleType) && input$VehicleType != "All") {
      vehicle_counts <- data %>%
        group_by("Vehicle Type" = VPICMAKENAME) %>%
        summarise("Number of Vehicles" = n())
      vehicle_counts
    } else {
      NULL
    }
  })
  
  # bar plot tab 3 
  output$speed_death_plot <- renderPlot({
    data <- filtered_speed_data()
    if (!is.null(input$Speed_State) && input$Speed_State != "All") {
      ggplot(data, aes(x = Speeding, fill = Speeding)) +
        geom_bar() +
        labs(title = "Speeding vs. Not Speeding Accidents",
             x = "Speeding Status",
             y = "Number of Accidents") +
        theme_minimal()
    }
  })
  
  # bar plot tab 4
  output$drinking_plot <- renderPlot({
    data <- filtered_drinking_data()
    if (!is.null(input$Drinking_State) && input$Drinking_State != "All") {
      ggplot(data, aes(x = Drinking, fill = Drinking)) +
        geom_bar() +
        labs(title = "Accidents Involving Drinking vs. Non-Drinking Drivers",
             x = "Drinking Status",
             y = "Number of Accidents") +
        theme_minimal()
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


