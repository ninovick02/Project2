library(shiny)
library(shinyalert)
library(bslib)
library(tidyverse)
library(janitor)
library(knitr)

source("helper.R")

ui <- sidebarLayout(
  
  titlePanel(" "),
  
    sidebarPanel(
      
    ),
    mainPanel(
      
      navset_card_underline(
        
        nav_panel("About", includeMarkdown("about.md"), card(
          card_image("www/shiny.svg", height = "300px")
        )),
        
        nav_panel("Data Download", tableOutput("filter_table")),
        
        nav_panel("Data", DT::dataTableOutput("data")),
        
        nav_panel(
          "Reference",
          markdown(
            glue::glue(
              "These data were obtained from [IMDB](http://www.imdb.com/) and [Rotten Tomatoes](https://www.rottentomatoes.com/).
  
        The data represent {nrow(movies)} randomly sampled movies released between 1972 to 2014 in the United States.
        "
            )
          )
        )
      )
      
    )
  )
)


server <- function(input, output, session){
  
}
  

  
shinyApp(ui = ui, server = server)