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

  
ui <- fluidPage(
  
  titlePanel("Polling Data from NYT's Upshot"), 
  
  leafletOutput("mymap"),
  uiOutput("tab"), 
  uiOutput("trial")
  )


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
      
      output$trial <- renderTable({
        
        file <- df %>%  
          transmute(response, final_weight, likely = as.factor(likely)) %>% 
          mutate(likely = fct_relevel(likely, c("Already voted", "Almost certain","Very likely", "Somewhat likely", "Not very likely", "Not at all likely"))) %>% 
          group_by(likely, response) %>% 
          tally(wt = final_weight) %>%
          filter(! likely == "[DO NOT READ] Don't know/Refused") %>% 
          spread(key = response, value = n) 
        
        file[is.na(file)] <- 0
        
        file <- file %>% 
          mutate(all = Dem + Rep + Und) %>% 
          mutate(Dem = Dem/all,
                 Rep = Rep/all,
                 Und = Und/all) %>% 
          adorn_pct_formatting(digits = 0, affix_sign = TRUE) %>% 
          dplyr::select(likely, Dem, Rep, Und) 
        
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