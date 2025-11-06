library(shiny)
library(shinyalert)
library(bslib)
library(sortable)
library(tidyverse)
library(janitor)
library(knitr)
library(DT)
#library(litedown)

source("helper.R")

ui <- sidebarLayout(
  
  titlePanel(" "),
  
    sidebarPanel(
      
    ),
    mainPanel(
      
      navset_card_underline(
        
        nav_panel("About", #includeMarkdown("README.md"), 
                  card(
          card_image("www/Mobile Phone.png", height = "300px")
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
                                   uiOutput("dynamic_card_output"))
                  
                  )
                  
                  
                  )
        
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

              req(cat_vars)
              req(nice_cat_vars)
             
              fluidRow(
                h3("Categorical Data Summary"),
                p("Provides One-Way and Two-Way Tables for categorical data"),
                selectInput("cat_var1" ,"Select Variable",
                            choiceNames = nice_cat_vars,
                            choiceValues = cat_vars),
                actionButton("make_2_way", "Click to Add a Variable"),
                conditionalPanel("input.make_2_way",
                  selectInput("cat_var2", "Select Second Variable",
                              choiceNames = nice_cat_vars,
                              choiceValues = cat_vars)
               ),
                actionButton("make_table", "Create Contingency Table"),
                tableOutput("way_table")
              )
           },
           
           "num_sum" = {
             # If input is "B", render Card B (e.g., a specialized input block)
             
             req(cat_vars)
             req(nice_cat_vars)
             req(num_vars)
             req(nice_num_vars)
             
             card(
                card_header("Numeric Value Summary"),
               
                checkboxGroupInput("num_vars", "Select Numeric Variables to Summarize",
                                   choiceNames = nice_num_vars,
                                   choiceValues = num_vars),
               
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
                                     items = nice_cat_vars, connect = "selected_vars",
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
             )           },
           
           "visual" = {
              # If input is "C", render Card C (e.g., a DT table output)
              req(cat_vars)
              req(nice_cat_vars)
              req(name_lookup_table)
              
              div(
                h3("Plot Data"),
                selectInput("x_var", "Select X variable", choiceNames = name_lookup_table[[1]], choiceValues = name_lookup_table[[2]]),
                selectInput("y_var", "Select Y variable", choiceNames = name_lookup_table[[1]], choiceValues = name_lookup_table[[2]]),
                selectInput("fill", "Group By: ", choiceNames = nice_cat_names, choiceValues = cat_vars),
                actionButton("add_facet", "Click Here to add faceting"),
                conditionalPanel("input.add_facet", 
                                 selectInput("facet1", "Select Variable to Facet by",
                                             choiceNames = nice_cat_vars, choiceValues = cat_vars),
                                 selectInput("facet2", "Select Second Faceting Variable if Desired",
                                             choiceNames = nice_cat_vars, choiceValues = cat_vars)
                )
                
              )
           },
           "heatmap" = {
             renderPlot("corr")
           }
           
           # Optional: Default message if no choice is made or recognized
           #div("Please select an option to view the corresponding card.")
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
