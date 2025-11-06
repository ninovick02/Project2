library(shiny)
library(shinyalert)
library(bslib)
library(sortable)
library(tidyverse)
library(janitor)
library(knitr)
library(DT)
library(litedown)

source("helper.R")

ui <- page_sidebar(
  
  title = "title",
  
  sidebar = sidebar(h3("Filter Data"),
                    
                    helpText("Use these to filter the Data. You must select "),
                    
                    
                    checkboxGroupInput("filter_dev", "Choose which Device Models to include",
                                       choices = c("Google Pixel 5", "iPhone 12", "OnePlus 9", "Samsung Galaxy s21", "Xiaomi Mi 11"),
                                       selected = c("Google Pixel 5", "iPhone 12", "OnePlus 9", "Samsung Galaxy s21", "Xiaomi Mi 11")),
                    checkboxGroupInput("filter_sys", "Choose which Operating Systems to include",
                                       choices = c("Android", "iOS"), selected = c("Android", "iOS")),
                    checkboxGroupInput("filter_gender", "Choose which Genders to include",
                                       choices = c("Male", "Female"), selected = c("Male", "Female")),
                    checkboxGroupInput("filter_behavior", "Choose which User Behavior Class to include (Phone Usage)",
                                       choices = c("Light Usage", "Mild Usage", "Moderate Usage", "Heavy Usage", "Extreme Usage"),
                                       selected = c("Light Usage", "Mild Usage", "Moderate Usage", "Heavy Usage", "Extreme Usage")
                                       ),
                    br(),
                    
                    actionButton("open_filter1", "Filter by a Numeric Variable?"),
                    conditionalPanel("input.open_filter1",
                                     selectInput("num_filter_var1", "Select Numerical Variable to filter by", 
                                                 choices = c(setNames(num_vars, nice_num_vars))),
                                     
                                    uiOutput("filtered_num1")
                                     
                    ),
                    
                    br(),
                    
                    actionButton("open_filter2", "Filter by a Second Numeric Variable?"),
                    
                    uiOutput("second_num_filter"),             
                    uiOutput("filtered_num2"),
                    
                    
                    br(),
                    br(),
                    
                    
                    actionButton("start_filter", "Click to Apply Filter choices"),
                    
                    actionButton("reset_df", "Click to Reset Data")                 
                    ),
  
      
  navset_card_underline(
    
    nav_panel("About", includeMarkdown("README.md"), 
              card(
      card_image("www/Mobile Phone.png", height = "300px")
    )),
    
    nav_panel("Data Download", 
              downloadButton("download_filtered", "Download this Data Table"),
              DT::dataTableOutput("filter_table")),
    
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


server <- function(input, output, session){
  
  
  
  # ============ Side bar ===================================
  output$second_num_filter <- renderUI({
    req(input$open_filter2)        # wait until button is clicked
    req(input$num_filter_var1)      # wait until first variable is selected
    
    selectInput(
      "num_filter_var2",
      "Select Second Numerical Variable by which to Filter",
      choices = setNames(
        num_vars[ num_vars != input$num_filter_var1 ], 
        nice_num_vars[ num_vars != input$num_filter_var1 ]
      )
    )
  })
  
  output$filtered_num1 <- renderUI({
    req(input$num_filter_var1)
    data_col <- df[[input$num_filter_var1]]
    
    sliderInput("range1", paste("Select range for", clean_label(input$num_filter_var1)),
                min = min(data_col),
                max = max(data_col),
                value = c(min(data_col), max(data_col)))
    
  })
  
  output$filtered_num2 <- renderUI({
    req(input$num_filter_var2)
    
    data_col <- df[[input$num_filter_var2]]
    
    sliderInput("range2", paste("Select range for", clean_label(input$num_filter_var2)),
                min = min(data_col),
                max = max(data_col),
                value = c(min(data_col), max(data_col)))
  })
  
  filtered_data <- reactiveVal(df)  # start with full dataset
  
  # Apply filters
  observeEvent(input$start_filter, {
    dat <- df
    
    if(!is.null(input$filter_gender))
      dat <- filter(dat, gender %in% input$filter_gender)
    
    if(!is.null(input$filter_dev))
      dat <- filter(dat, device_model %in% input$filter_dev)
    
    if(!is.null(input$filter_sys))
      dat <- filter(dat, operating_system %in% input$filter_sys)
    
    if(!is.null(input$filter_behavior))
      dat <- filter(dat, user_behavior_class %in% input$filter_behavior)
    
    if (!is.null(input$num_filter_var1) && !is.null(input$range1))
      dat <- filter(dat, .data[[input$num_filter_var1]] >= input$range1[1],
                           .data[[input$num_filter_var1]] <= input$range1[2])
    
    if (!is.null(input$num_filter_var2) && !is.null(input$range2))
      dat <- filter(dat, .data[[input$num_filter_var2]] >= input$range2[1],
                           .data[[input$num_filter_var2]] <= input$range2[2])
    
    filtered_data(dat)  # update reactive value
  })
  
  # Reset data
  observeEvent(input$reset_df, {
    filtered_data(df)
  })
  
  # ============ Data Table =============================
  
  output$filter_table <- renderDataTable({
    
    dat <- filtered_data()
    names(dat) <- sapply(names(dat), clean_label)
    dat
  })
  
  output$download_filtered <- downloadHandler(
    filename = function() { "filtered_data.csv" },
    content = function(file) {
      write.csv(df_filtered(), file, row.names = FALSE)
    }
  )
  
  # ============== Data Exploration ======================
  output$dynamic_card_output <- renderUI({
    
    validate(
      need(!is.null(input$ui_choice), "Please select button.")
    )
    
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
