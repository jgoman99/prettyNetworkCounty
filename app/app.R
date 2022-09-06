library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinycssloaders)
library(shinyjs)
library(plotly)
library(dplyr)
library(sf)
library(ggplot2)
source("base.R")


ui <- fluidPage(
  tags$style(type="text/css",
             ".recalculating {opacity: 1.0;}"
  ),useShinyjs(),
  h3("Pretty County Networks", align='left'),
  p("Each iteration adds adjacent US county with smallest GDP", align = 'left'),
  p("More iterations take longer to load. 3,000 iterations takes about 6 minutes"),
  sliderInput("iterationNumber","Iterations",min=1,max=nrow(counties_shp),value=100, step = 10),
  actionButton("calculate","Calculate!"), br(),br(),
  #progressBar(id = "pb4", value = 0, display_pct = T),
  actionButton("plot","Plot!"),
  

  # will need to learn how to properly size things at some point
  plotlyOutput('map',width='100%',height='400px')
)


server <- function(input, output, session) {
  # start num
  start_num <- 1
  # calculate bool
  calculate_bool <- FALSE
  # plot bool
  plot_bool <- FALSE
  # hide widgets
  hide('map')
  hide('plot')
  # assets
  base_shp <- find_smallest_gdp_path(current_node = sample(nrow(counties_shp),1),counties_shp = counties_shp,
                                                          iteration_number = 1)
  dat <- reactiveValues(
    data=base_shp %>% filter(order<=start_num)
  )
  
  observeEvent(input$plot,{
    plot_bool <<- TRUE
    show('map')
  })
  observeEvent(input$calculate,{
    calculate_bool <<- TRUE
    start_num <<- 1
    plot_bool <<- FALSE
    hide('map')
    hide('plot')
  })
  output$map <- renderPlotly({
     p <- ggplot(data=dat$data) + geom_sf() +
      theme(
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
      ) 
    ggplotly(p) %>% config(displayModeBar = F)
  })
  
  observe({

    invalidateLater(100)
    if (calculate_bool)
    {

      base_shp <<- find_smallest_gdp_path(current_node = sample(nrow(counties_shp),1),counties_shp = counties_shp,
                                         iteration_number = input$iterationNumber)
      calculate_bool <<- FALSE
      show('plot')
    }
    if (plot_bool){
      start_num <<- start_num +1
      dat$data <- base_shp %>% filter(order<=start_num)
    }
    

  })
  

  

}


shinyApp(ui = ui, server = server)
