library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinycssloaders)
library(shinyjs)
library(plotly)
library(dplyr)
library(sf)
library(ggplot2)
library(igraph)

# works. now we need efficiency
## speed/ negotiate setting up data
## speed up plotting


# bug fix
sf::sf_use_s2(FALSE)

# not in
`%notin%` <- Negate(`%in%`)

# load assets
counties_shp <- read_sf("assets/counties_simplified.geojson")
counties_gdp <- read.csv("assets/gdp.csv")
counties_shp$geofips <- as.numeric(counties_shp$geofips)
counties_shp <- left_join(counties_shp,counties_gdp,by='geofips')
counties_shp$gdp_2020 <- as.numeric(counties_shp$gdp_2020)
counties_shp$state_name <- as.numeric(counties_shp$state_name)

counties_shp <- counties_shp %>% filter(state_name %in% c(1,4:13,16:56)) %>% filter(geofips %notin% c(36085,25019,53055))

# replace na with 0 
counties_shp[is.na(counties_shp$gdp_2020),'gdp_2020'] <- 0

# change this later
popup_sb <- paste0("GDP: ", as.character(counties_shp$gdp_2020))

find_smallest_gdp_path <- function(current_node=1,counties_shp)
{
  # note 2: convert to igraph, solve igraph, convert back
  touching_list <- st_touches(counties_shp)
  g = graph.adjlist(touching_list)
  # set node value
  g <- g %>% set_vertex_attr("gdp",value=counties_shp$gdp_2020)
  
  ordered_list <- c(current_node)
  current_list <- ordered_list
  
  # helper func
  all_in_list <- function(l1,l2)
  {
    return(all(l1 %in% l2))
  }
  
  s0 = Sys.time()
  while(length(ordered_list)<length(g))
  {
    print(paste0("iteration: ", length(ordered_list)))
    # get nearest nodes to cluster
    # this is slow area
    s1 = Sys.time()
    neighbors_vertex_list <- adjacent_vertices(g,current_list)
    print(paste0("Time to get adjacent verticies: ", Sys.time()-s1))
    s2 = Sys.time()
    # speed up by only callign reduce verticies if exceeds limit
    if (s2-s1 > .05)
    {
      current_list <- current_list[!unlist(lapply(neighbors_vertex_list,all_in_list,l2=ordered_list))]
      
      print(paste0("Time to get reduce current_list: ", Sys.time()-s2))
    }
    
    neighbors_list = unique(unlist(neighbors_vertex_list))
    
    
    # remove nodes already in cluster
    # super fast
    neighbors_list = neighbors_list[neighbors_list %notin% ordered_list]
    current_node = as.numeric(neighbors_list[V(g)[neighbors_list]$gdp==min(V(g)[neighbors_list]$gdp)])[1]
    current_list <- c(current_list,current_node)
    ordered_list <- c(ordered_list,current_node)
    print(paste0('time elapsed: ', Sys.time()-s0))
    
    ## for testing
    if (length(ordered_list)>500)
    {
      break
    }
    
  }
  
  counties_shp$order = NA
  counties_shp$order[ordered_list] <- 1:(length(ordered_list))
  return(counties_shp)
}


ui <- fluidPage(
  tags$style(type="text/css",
             ".recalculating {opacity: 1.0;}"
  ),

  # will need to learn how to properly size things at some point
  plotlyOutput('map',width='100%',height='600px')
)


server <- function(input, output, session) {
  # start num
  start_num <- 1
  # assets
  counties_shp <- find_smallest_gdp_path(current_node = sample(nrow(counties_shp),1),counties_shp = counties_shp)
  dat <- reactiveValues(
    data=counties_shp %>% filter(order<=start_num)
  )
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
    invalidateLater(1)
    start_num <<- start_num +1
    print(paste("The current time is", Sys.time()))
    dat$data <- counties_shp %>% filter(order<=start_num)
  })
  

  

}


shinyApp(ui = ui, server = server)
