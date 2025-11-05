library(shiny)
library(shinyalert)
library(bslib)
library(sortable)
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
          card_image("www/Mobile Phone.png", height = "300px")
        )),
        
        nav_panel("Data Download", DT::datatableOutput("filter_table")),
        
        nav_panel("Data Exploration", 
                  card(radioButtons(
                    "explore_type", 
                     label = "Select the type of Exploration"),
                     choices = list("Categorical Data Summaries" = "cat_sum", 
                                    "Numeric Value Summaries" = "num_sum", 
                                    "Plots" = "visual",
                                    "Numeric Correlatoin Heatmap" = "heatmap") 
                       ),
                  conditionalPanel("input.explore_type",
                                   uiOutput("dynamic_card_output"))
                  
                  
                  
                  
                  ),
        
        # nav_panel(
        #   "Reference",
        #   markdown(
        #     glue::glue(
        #       "These data were obtained from [IMDB](http://www.imdb.com/) and [Rotten Tomatoes](https://www.rottentomatoes.com/).
        # 
        # The data represent {nrow(movies)} randomly sampled movies released between 1972 to 2014 in the United States.
        # "
          #   )
          # )
        # )
      
      
    )
  )
)


server <- function(input, output, session){
  
  output$dynamic_card_output <- renderUI({
    
    # Use a switch statement for clean, multi-way branching logic
    switch(input$ui_choice,
           
           "cat_sum" = {
             # If input is "A", render Card A (e.g., a fluidRow with content)
             fluidRow(
               h3("Categorical Data Summary"),
               p("Provides One-Way and Two-Way Tables for categorical data"),
               selectInput("cat_var1" ,"Select Variable", choices = sapply(names(cat_vars), clean_label), selected = names(cat_vars[1]), ),
               actionButton("make_2_way", "Click to Add a Variable", ),
               conditionalPanel("input.make_2_way",
                 selectInput("cat_var2", "Select Second Variable", choices = sapply(names(cat_vars), clean_label), selected = cat_vars[2]),
               ),
               actionButton("make_table", "Create Contingency Table"),
               tableOutput("way_table")
             )
           },
           
           "num_sum" = {
             # If input is "B", render Card B (e.g., a specialized input block)
             card(
               card_header("Numeric Value Summary"),
               checkboxGroupInput("num_vars", "Select Numeric Variables to Summarize", choices = clean_label(num_vars)),
               checkboxGroupInput("stats", "Select Summary Statistics to add",
                                  choice = c("Minimum", "Q1", "Median", "Q3", "Maximum", 
                                             "Range", "Standard Deviation", "Variance", "Count")),
               actionButton("other_quant", "Click to add other percentile options"),
               conditionalPanel("input.other_quant",
                                h3("Enter Numbers for additional percentile statistics"),
                                textInput("num_input", 'Enter numbers (0–1, separated by commas):", "'),
                                actionButton("submit_nums", "Submit"),
                                textOutput("num_feedback")),
               actionButton("grouped_num", "Click here to Group by Categorical Variables"),
               conditionalPanel("input.group_num",
                                fluidRow(
                                  h3("Choose and order your variables"),
                                  orderInput(
                                    "available_vars", "Available variables:",
                                    items = sapply(names(cat_vars), clean_label), connect = "selected_vars",
                                    as_source = TRUE, as_target = FALSE
                                  ),
                                  orderInput(
                                    "selected_vars", "Selected variables (drag here):",
                                    items = NULL, connect = "available_vars",
                                    as_source = TRUE, as_target = TRUE
                                  ),
                                  actionButton("submit_order", "Submit"),
                                  textOutput("order_feedback")
                                )
                              ),
               actionButton("make_num_sum", "Create Table"),
               renderTable("num_sum_tab")
             )
           },
           
           "visual" = {
             # If input is "C", render Card C (e.g., a DT table output)
             div(
               h3("Plot Data"),
               radioButtons("graph_type", "Select Type of Graph",
                            choiceNames = c("Bar chart", "Density Plot", "Box Plot", "Jitter Plot", "Scatter Plot", "Numeric Correlation Heat Map"), 
                            choiceValues = c("bar", "density", "box", "jitter", "scatter", "correlation")),
               
             )
           },
           
           # Optional: Default message if no choice is made or recognized
           div("Please select an option to view the corresponding card.")
      )
    
    })
  
    observeEvent(input$submit_nums, {
      # Split text input into numeric vector
      nums <- as.numeric(unlist(strsplit(input$num_input, ",")))
      
      # Check for invalid entries
      if (any(is.na(nums)) || any(nums < 0 | nums > 1)) {
        output$num_feedback <- renderText("Please enter only valid numbers between 0 and 1.")
      } else {
        output$num_feedback <- renderText(paste("You entered:", paste(nums, collapse = ", ")))
      }
    })
    
    observeEvent(input$submit_order, {
      selected_order <- input$selected_vars
      if (length(selected_order) == 0) {
        output$order_feedback <- renderText("You didn’t select any variables yet.")
      } else {
        output$order_feedback <- renderText(
          paste("You chose this order:", paste(selected_order, collapse = " -> "))
        )
      }
    })
    
}  

  
shinyApp(ui = ui, server = server)