library(shiny)
library(shinyalert)
library(bslib)
library(sortable)
library(tidyverse)
library(janitor)
library(knitr)
library(DT)

ui <- page_sidebar(
  title = "title panel",
  sidebar = sidebar("sidebar"),
    
    navset_card_underline(
      
      nav_panel("About", #includeMarkdown("README.md"), 
                card(
                  card_image("www/Mobile Phone.png", height = "600px")
                )),
      
      nav_panel("Data Download", DT::dataTableOutput("filter_table")),
      
      nav_panel("Data Exploration", 
                card(radioButtons(
                  "explore_type", 
                  label = "Select the type of Exploration",
                  choices = list(
                    "Categorical Data Summaries" = "cat_sum", 
                    "Numeric Value Summaries" = "num_sum", 
                    "Plots" = "visual",
                    "Numeric Correlatoin Heatmap" = "heatmap"
                  ) 
                ),
                conditionalPanel("input.explore_type",
                                 helpText("dynamic_card_output"))
                
                )
                
                
      )
    ))

server <- function(input, output, session){
  
}

shinyApp(ui = ui, server = server)