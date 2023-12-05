library(tidyr)
library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)
library(tigris)
library(RColorBrewer)
library(DT)
library(ggplot2)
# Define the function to create a map based on the input year outside the server function
create_success_map <- function(input_year, data) {
  # Load ZCTA data for California
  zctas_ca <- zctas(cb = FALSE, state = "CA", year = 2010) %>% 
    st_as_sf() %>% 
    mutate(ZCTA5CE10 = as.character(ZCTA5CE10))
  
  # Extract unique ZIP codes from your data
  unique_zips <- unique(data$postal_code)
  
  # Filter ZCTA data for these unique ZIP codes
  zctas_ca <- zctas_ca %>% filter(ZCTA5CE10 %in% unique_zips)
  
  
  # Filter the restaurant data for the input year and calculate success
  successful_restaurants <- data %>%
    filter(year == input_year) %>%
    mutate(postal_code = as.character(postal_code)) %>%
    filter(success==1) %>%
    group_by(postal_code) %>%
    summarise(success_count = n(), .groups = 'drop') # Count successful restaurants
  
  # Merge the ZCTA geometries with the successful restaurants data
  merged_data <- zctas_ca %>% 
    left_join(successful_restaurants, by = c("ZCTA5CE10" = "postal_code"))
  
  # Replace NA values with 0 in the success_count
  merged_data$success_count[is.na(merged_data$success_count)] <- 0
  
  # Determine the max success_count value
  max_success <- max(merged_data$success_count, na.rm = TRUE)
  
  
  # Create a color palette function with a more versatile color scale
  pal <- colorBin(palette = "viridis", domain = merged_data$success_count, bins = seq(0, max_success, length.out = 14), na.color = "transparent")
  
  # Plotting the map with the enhanced color scale and adding a legend
  map <- leaflet(data = merged_data) %>%
    addProviderTiles(providers$OpenStreetMap) %>%
    addPolygons(fillColor = ~pal(success_count),
                fillOpacity = 1, color = "#444444", weight = 0.5,
                label = ~paste("Zip:", ZCTA5CE10, "<br>", "Successful Restaurants:", success_count),
                labelOptions = labelOptions(direction = 'auto')) %>%
    addLegend(pal = pal, values = ~success_count, title = "Number of Successful Restaurants",
              position = "bottomright", labFormat = labelFormat(suffix = "")) %>%
    setView(lng = -119.6982, lat = 34.4208, zoom = 10)  # Center on Santa Barbara
  
  
  return(map)
}


create_income_map <- function(input_year, data) {
  # Load ZCTA data for California
  zctas_ca <- zctas(cb = FALSE, state = "CA", year = 2010) %>% 
    st_as_sf() %>% 
    mutate(ZCTA5CE10 = as.character(ZCTA5CE10))
  
  # Extract unique ZIP codes from your data
  unique_zips <- unique(data$postal_code)
  
  # Filter ZCTA data for these unique ZIP codes
  zctas_ca <- zctas_ca %>% filter(ZCTA5CE10 %in% unique_zips)
  
  # Filter the restaurant data for the input year and get income
  income_data <- data %>%
    filter(year == input_year) %>%
    mutate(postal_code = as.character(postal_code)) %>%
    distinct(postal_code, .keep_all = TRUE) %>%
    select(postal_code, income_c)  # Select the desired columns
  
  # Merge the ZCTA geometries with the income data
  merged_data <- zctas_ca %>% 
    left_join(income_data, by = c("ZCTA5CE10" = "postal_code"))
  
  # Replace NA values with 0 in the income_c
  merged_data$income_c[is.na(merged_data$income_c)] <- 0
  
  # Determine the range of income_c values
  income_range <- range(merged_data$income_c, na.rm = TRUE)
  
  # Create a color palette function with a fixed color scale
  pal <- colorBin(palette = "Blues", domain = merged_data$income_c, bins = seq(income_range[1], income_range[2], length.out = 9), na.color = "transparent")
  
  # Plotting the map with the fixed color scale
  map <- leaflet(data = merged_data) %>%
    addProviderTiles(providers$OpenStreetMap) %>%
    addPolygons(fillColor = ~pal(income_c),
                fillOpacity = 1, color = "#444444", weight = 0.5,
                label = ~paste("Zip:", ZCTA5CE10, "<br>", "Income Level:", income_c),
                labelOptions = labelOptions(direction = 'auto')) %>%
    addLegend(pal = pal, values = ~income_c, title = "Income Level",
              position = "bottomright", labFormat = labelFormat(suffix = "")) %>%
    setView(lng = -119.6982, lat = 34.4208, zoom = 10)  # Center on Santa Barbara
  
  return(map)
}






data_final_4 <- read.csv("./data_final_4.csv")
df<-data_final_4
santa_distance <- read.csv("./Santa_distance.csv")










ui <- fluidPage(
  titlePanel("Key Drivers of Restaurant Success in Santa Barbara: 2019-2021"),
  tabsetPanel(
    # Tab for the first app: Restaurant Success and Income Maps
    tabPanel("Restaurant Success and Income Maps",
             titlePanel("Map for Restaurant Success and Income"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("yearInput", "Select Year", choices = unique(df$year)),
                 radioButtons("mapType", "Choose Map Type",
                              choices = list("Success Count" = "success", "Income Level" = "income"),
                              selected = "success"),
                 actionButton("filterButton", "Generate Map")
               ),
               mainPanel(
                 leafletOutput("map", height = "600px"),
                 width = 9
               )
             )
    ),
    
    # Tab for the second app: Analysis of Restaurant Success Factors
    tabPanel("Analysis of Restaurant Success Factors",
             titlePanel("Analysis of Restaurant Success Factors"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("yearInput2", "Select Year", choices = unique(df$year)),
                 actionButton("filterButton2", "Show Data")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Table", DT::dataTableOutput("successTable")),
                   tabPanel("Price Range", plotlyOutput("pricePlot"), plotlyOutput("priceCountPlot")),
                   tabPanel("Table Service", plotlyOutput("servicePlot"), plotlyOutput("serviceCountPlot")),
                   tabPanel("Quiet Level", plotlyOutput("quietPlot"), plotlyOutput("quietCountPlot"))
                 )
               )
             )
    ),
    
    # Tab for the third app: Trips Analysis
    tabPanel("Trips Analysis",
             titlePanel("Trips Analysis"),
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("selectedTrips", "Select Number of Trips:",
                                    choices = c("1" = "Number.of.Trips..1",
                                                "1-3" = "Number.of.Trips.1.3",
                                                "3-5" = "Number.of.Trips.3.5",
                                                "5-10" = "Number.of.Trips.5.10")),
                 checkboxInput("showPopulation", "Show Population Staying at Home", FALSE)
               ),
               mainPanel(
                 plotOutput("distancePlot")
               )
             )
    )
  )
)


server <- function(input, output, session) {
  
  # Logic from the first app: Restaurant Success and Income Maps
  observeEvent(input$filterButton, {
    if(input$mapType == "success") {
      output$map <- renderLeaflet({
        create_success_map(input$yearInput, df)
      })
    } else if(input$mapType == "income") {
      output$map <- renderLeaflet({
        create_income_map(input$yearInput, df)
      })
    }
  })
  
  
  filteredData <- eventReactive(input$filterButton2, {
    df %>%
      filter(year == input$yearInput2, !is.na(RestaurantsPriceRange2)) %>%
      select(QuietLevel, RestaurantsPriceRange2, RestaurantsTableService, success)
  })
  
  # Output for the successTable using DT
  output$successTable <- DT::renderDataTable({
    DT::datatable(filteredData(), options = list(pageLength = 10, searchHighlight = TRUE), rownames = FALSE)
  })
  
  # Output for the quiet level vs success plot
  output$quietPlot <- renderPlotly({
    req(filteredData())
    # Proportion calculation for 'QuietLevel' vs 'success'
    ggplotly(
      ggplot(filteredData(), aes(x = as.factor(QuietLevel), fill = as.factor(success))) +
        geom_bar(position = "fill") +
        labs(x = "Quiet Level", y = "Proportion of Success", fill = "Success") +
        scale_y_continuous(labels = scales::percent_format()) +
        theme_minimal()
    )
  })
  
  # Output for the price range vs success plot
  output$pricePlot <- renderPlotly({
    req(filteredData())
    # Proportion calculation for 'RestaurantsPriceRange2' vs 'success'
    ggplotly(
      ggplot(filteredData(), aes(x = RestaurantsPriceRange2, fill = as.factor(success))) +
        geom_bar(position = "fill") +
        labs(x = "Restaurant Price Range", y = "Proportion of Success") +
        scale_y_continuous(labels = scales::percent_format())
    )
  })
  
  # Output for the table service vs success plot
  output$servicePlot <- renderPlotly({
    req(filteredData())
    # Proportion calculation for 'RestaurantsTableService' vs 'success'
    ggplotly(
      ggplot(filteredData(), aes(x = as.factor(RestaurantsTableService), fill = as.factor(success))) +
        geom_bar(position = "fill") +
        labs(x = "Table Service", y = "Proportion of Success") +
        scale_y_continuous(labels = scales::percent_format())
    )
  })
  
  # Output for the total count of price ranges
  output$priceCountPlot <- renderPlotly({
    req(filteredData())
    ggplotly(
      ggplot(filteredData(), aes(x = RestaurantsPriceRange2)) +
        geom_bar(fill = 'skyblue') +
        labs(x = "Restaurant Price Range", y = "Total Count") +
        theme_minimal()
    )
  })
  
  # Output for the total count of table service options
  output$serviceCountPlot <- renderPlotly({
    req(filteredData())
    ggplotly(
      ggplot(filteredData(), aes(x = as.factor(RestaurantsTableService))) +
        geom_bar(fill = 'skyblue') +
        labs(x = "Table Service", y = "Total Count") +
        theme_minimal()
    )
  })
  
  # Output for the total count of quiet levels
  output$quietCountPlot <- renderPlotly({
    req(filteredData())
    ggplotly(
      ggplot(filteredData(), aes(x = as.factor(QuietLevel))) +
        geom_bar(fill = 'skyblue') +
        labs(x = "Quiet Level", y = "Total Count") +
        theme_minimal()
    )
  })
  
  
  # Logic for the third app: Trips Analysis
  output$distancePlot <- renderPlot({
    # Ensure Date is in the correct format and sort the data by Date
    santa_distance$Date <- as.Date(santa_distance$Date)
    sortedData <- santa_distance[order(santa_distance$Date), ]
    
    # Filter the data to include only dates up to a certain point, if needed
    filteredData <- sortedData[sortedData$Date<as.Date("2022-01-01"),] # Apply any necessary filtering
    
    # Start the plot with the Date on the x-axis
    p <- ggplot(filteredData, aes(x = Date))
    
    # Define a set of colors for the lines, if needed
    lineColors <- setNames(c("blue", "green", "red", "purple"), 
                           c("Number.of.Trips..1", "Number.of.Trips.1.3", 
                             "Number.of.Trips.3.5", "Number.of.Trips.5.10"))
    
    # Add smoothed lines for each selected trip range
    for (i in input$selectedTrips) {
      p <- p + geom_smooth(aes_string(y = i, colour = factor(i)), method = "loess", se = FALSE)
    }
    
    # Optionally add Population Staying at Home data
    if (input$showPopulation) {
      p <- p + geom_smooth(aes(y = Population.Staying.at.Home, colour = "Population staying at home"), 
                           method = "loess", se = FALSE)
    }
    
    # Set the scale for the colors and add a legend
    p <- p + scale_colour_manual(values = c(lineColors, "Population staying at home" = "black"), 
                                 name = "Data Type")
    
    # Set labels for axes
    p <- p + labs(y = "Normalized Number of Trips / Population Staying at Home", x = "Date")
    
    p
  })
}

# Finally, run the app with the combined UI and server
shinyApp(ui = ui, server = server)
