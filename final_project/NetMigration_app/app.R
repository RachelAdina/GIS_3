
library(shiny)

# Define UI 
ui <- fluidPage(

    # Application title
    titlePanel("United States Net Domestic Migration by Year"),

    # Sidebar with a slider input for years 
    sidebarLayout(
        sidebarPanel(
            sliderInput("year",
                        "Year",
                        min = 2014,
                        max = 2018,
                        step=1,
                        sep='',
                        value = 2018)
        ),

        # Show maps
        mainPanel(
           leafletOutput('county_map'),
           leafletOutput('state_map'),
           leafletOutput('region_map')
        )
    )
)

# Define server logic
server <- function(input, output) {
    
    output$county_map <- renderLeaflet({
        
        counties_by_year <- filter(counties_merged,
                                 year == input$year)
        
        pal <- colorQuantile('RdYlBu', counties_merged$net_migration, n=6)
        
        leaflet(data = counties_by_year) %>%
            addPolygons(fillColor = ~pal(net_migration),
                        weight=1,
                        opacity=1, 
                        color='black',
                        fillOpacity=0.8,
                        highlight = highlightOptions(
                            weight = 2,
                            color = 'white',
                            fillOpacity=0.9,
                            bringToFront=TRUE),
                        label = counties_merged$net_migration) %>%
            addLegend(pal = pal, values = ~net_migration, opacity = 0.8, 
                      title='County Migrants', position='bottomright')
    })
    
    output$state_map <- renderLeaflet({
        
        states_by_year <- filter(states_merged,
                                 year == input$year)
        
        pal <- colorQuantile('RdYlBu', states_merged$net_migrants, n=6)
        
        leaflet(data = states_by_year) %>%
            addPolygons(fillColor = ~pal(net_migrants),
                        weight=2,
                        opacity=1, 
                        color='black',
                        fillOpacity=0.8,
                        highlight = highlightOptions(
                            weight = 4,
                            color = 'white',
                            fillOpacity=0.9,
                            bringToFront=TRUE),
                        label = states_merged$net_migrants) %>%
            addLegend(pal = pal, values = ~net_migrants, opacity = 0.8, 
                      title='State Migrants', position='bottomright')
    })
    
    output$region_map <- renderLeaflet({
        
        regions_by_year <- filter(regions_merged,
                                 year == input$year)
        
        pal <- colorQuantile('RdYlBu', regions_merged$net_migrants, n=4)
        
        leaflet(data = regions_by_year) %>%
            addPolygons(fillColor = ~pal(net_migrants),
                        weight=2,
                        opacity=1, 
                        color='black',
                        fillOpacity=0.8,
                        highlight = highlightOptions(
                            weight = 4,
                            color = 'white',
                            fillOpacity=0.9,
                            bringToFront=TRUE),
                        label = regions_merged$net_migrants) %>%
            addLegend(pal = pal, values = ~net_migrants, opacity = 0.8, 
                      title='Region Migrants', position='bottomright')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
