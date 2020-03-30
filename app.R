library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(leaflet)
library(plotly)
source("leaftlet_nrc.r")
source("viri_health.r")


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
    #get_leaflet_map(),
    # 
    # box(
    #     title = "COVID-91 in Canada", background = "maroon", solidHeader = TRUE, width = "95%", height = "95%",
    #     #plotOutput("plot4", height = 250)
    #     #leafletOutput("mymap" , width = "90%", height = "750")
    # ),
    
    tabBox(
        title = "COVID-91 in Canada",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", 
        width = "100%",
        #height = "250px",
        tabPanel(title = "Map", leafletOutput("mymap" , width = "100%", height = "750")),
        
        tabPanel("International", 
                 
                 
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(inputId = "case_type", "Case Type:",
                                     c("Confirmed" = "Confirmed",
                                       "Deaths" = "Deaths",
                                       "Recovered" = "Recovered")),
                         numericInput(inputId = "min_cases", label = "minimum cases", value = 75, min = 1, max = 10000),
                         numericInput(inputId = "num_label", label = "number of labels", value = 10, min = 0, max = 25)
                     ),
                     
                         mainPanel(
                            plotOutput("inter", height = 750)
                         )
                     )
                 ),
        tabPanel("Age", plotOutput("agehist", height = 750)),
        tabPanel("Tests", plotOutput("test_plot", height = 750))
    )
    
    #,
    #p()#,
    #actionButton("recalc", "New points")
)

server <- function(input, output, session) {
    
    #data <- read_viri_health_data()

    # data$cases <-
    # bind_cols (data$cases,
    #     st_coordinates(data$cases) %>%
    #     as_tibble() %>%
    #     rename(Long := X, Lat := Y)
    #     )



    #data_circles <- data$cases %>% aggregate_points() %>% as_tibble()
    #data_circles <- get_viri_health_cases_circles()
    #data_points <- data$cases %>% as_tibble() %>% select(Long, Lat, City)

    
    
    #%>% as.matrix()
    points <- eventReactive(input$recalc, {
        data_points
        #cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
    }, ignoreNULL = FALSE)
    #pal(data_circles$size)
    output$mymap <- renderLeaflet({
         get_leaflet_map() %>% 
            #addPolygons() %>%  
             addCircles(data = get_viri_health_cases_circles(
                 size_adjust = 5000
             ), radius = ~ Size , fillOpacity = 0.8, popup = ~ Popup, color = "#0F0F0F" , 
             fillColor  =  ~ colorQuantile("YlOrRd", Size)(Size)   , # pal(Size) , 
             fill = TRUE )
                        
                        
                         #%>%
             #addMarkers(data = data_points, popup = ~City)
    })
    
    output$inter<-renderPlot({
        
        
        
        get_covid_19_growth_compare(TYPE_CASE =input$case_type, MIN_COUNT_START = input$min_cases, NUM_COUNTRIES_LABEL = input$num_label)
        
    })
    output$agehist <- renderPlot({
        get_viri_health_age_hist()
    })
    
    output$test_plot <- renderPlot({
        get_tests_plot()
    })
    
    
}

shinyApp(ui, server)
