#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(haven)
library(fs)
library(readxl)
library(lubridate)
library(kableExtra)
library(janitor)
library(leaflet)
library(statesRcontiguous)
library(sp)
library(rgdal)
library(raster)
library(tidyverse)
library(moderndive)


ui <- fluidPage(
  tabsetPanel(
    tabPanel("Summary", fluid = TRUE,
    titlePanel("Polling Data from NYT's Upshot"), 
    h5("Click on any one of the districts polled by the New York Times! In the popup you will
     find information about the Republican advantage compared to the actual vote margin experienced
     by the candidates against their Democratic opponents. Also included are the titles of the winning
     candidate and their respective party. In the downbar, you will find a model comparing the turnout score
     against the final weight for voters in the final wave of polling in that district. This plot also
     has a linear model to easily identify relationships in different congressional districts.")), 
    
    tabPanel("Map & Plot", fluid = TRUE,
    titlePanel("Click a District!"), 
    leafletOutput("mymap", height = "350px"),
    uiOutput("tab"), 
  plotOutput("trial", height = "200px"))
  ))


districts <- read_xlsx("districts.xlsx")

districts <- districts %>%
  filter(!is.na(district.name)) %>% 
  mutate(district.name = paste0("Congressional District ", district.name))
  
shp <- districts %>% 
  left_join(shp_all_us_congressional_districts)

shp <- st_as_sf(shp)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  pal <- colorFactor(palette = c("red", "blue", "#A0A0A0"), 
                     levels = c("Republican", "Democrat", "Undecided"))
  
  output$mymap <- renderLeaflet({
    leaflet(shp) %>%
      addProviderTiles(provider = "CartoDB") %>% 
      setView(lat = 40, lng = -100, zoom = 4) %>% 
      addPolygons(highlight = highlightOptions(bringToFront = TRUE), 
                  popup = paste0(shp$state.short.name, "-", shp$district.name, "<br>", "<br>", 
                        "Republican Advantage: ", shp$rep.adv*100,"%", "<br>",
                        "Actual Advantage: ", shp$actual.adv*100,"%", "<br>",
                        "Winner: ", shp$win.name),
                  color = pal(shp$win.party), 
                  layerId = shp$wave.number)
    })
  
  observeEvent(input$mymap_shape_click, {
    if(is.null(click))
      return()
    else
      {
      click <- input$mymap_shape_click
      path <- paste0("https://raw.githubusercontent.com/TheUpshot/2018-live-poll-results/master/data/elections-poll-", click$id,".csv")
      df <- read_csv(path)
      
      output$trial <- renderPlot({
        file <- df %>%
          ggplot(aes(x = as.numeric(turnout_score), y = as.numeric(final_weight))) +
          geom_point() +
          labs(x = "Turnout Score", y = "Final Weight") +
          geom_smooth(method = "lm", se = FALSE)
        return(file)
        path <- NULL
      })
      
      }
  })
  
  
  url <- a("sonyakalara", href="https://github.com/sonyakalara/181113_NYTupshot")
  output$tab <- renderUI(tagList("Link to github repository:", url))
}


# Run the application 
shinyApp(ui = ui, server = server)