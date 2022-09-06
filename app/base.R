library(dplyr)
library(sf)
library(igraph)
library(shiny)




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


find_smallest_gdp_path <- function(current_node=1,counties_shp,iteration_number)
{
  withProgress(
    message = 'Calculation in progress',
    detail ='calculating path',
    value = 0,
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
        incProgress(1/iteration_number)
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
        if (length(ordered_list)>iteration_number)
        {
          break
        }
        
      }
      
      counties_shp$order = NA
      counties_shp$order[ordered_list] <- 1:(length(ordered_list))
      return(counties_shp)
    }
  )

}