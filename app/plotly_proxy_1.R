
library(shiny)
library(plotly)
library(rjson)
library(sf)

url <- 'https://raw.githubusercontent.com/plotly/datasets/master/election.geojson'
geojson <- rjson::fromJSON(file=url)
url2<- "https://raw.githubusercontent.com/plotly/datasets/master/election.csv"
df <- read.csv(url2)
g <- list(
  fitbounds = "locations",
  visible = FALSE
)
ui <- fluidPage(
  plotlyOutput('map')
)
server <- function(input,output,session)
{
  output$map <- renderPlotly(
    {
      fig <- plot_ly() 
      fig <- fig %>% add_trace(
        type="choropleth",
        geojson=geojson,
        locations=df$district,
        z=df$Bergeron,
        colorscale="Viridis",
        featureidkey="properties.district"
      )
      fig <- fig %>% layout(
        geo = g
      )
      # fig <- fig %>% colorbar(title = "Bergeron Votes")
      # fig <- fig %>% layout(
      #   title = "2013 Montreal Election"
      # )
      fig
      
    }
  )
}

shinyApp(ui = ui, server = server)
