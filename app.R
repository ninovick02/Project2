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
  
  title = "Exploring Mobile Phone Usage",
  
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
    
    nav_panel("About",  
              card(
                tabPanel(
                  # About section content
                  h2("Mobile Device Usage and User Behavior App"),
                  
                  p("This app is made to let you view and explore the data that describes mobile
                    device usage. The data behind this app comes from the ",
                    a("Mobile Device Usage and User Behavior Dataset on Kaggle",
                      href = "https://www.kaggle.com/datasets/valakhorasani/mobile-device-usage-and-user-behavior-dataset/data",
                      target = "_blank"),
                    ", curated by", (strong("Valakhorasani")), ". There are 700 observations and 10 variables of interest including device model, app usage time,
                    and battery drain. You can learn more about the dataset and 
    download it directly from Kaggle through the link provided."),
                    
                  br(),
                  
                  p("The sidebar on the left is your main control panel. Use it to filter the data 
    based on its categorical data: device models, operating system, user behavior characteristics, or gender.
    You can also select up to 2 numerical variables to filter by a range of values.
    Your selections in the sidebar will automatically update the visualizations and 
    summaries in the main panel once you apply your filters. You can also reset your filter!"),
                  
                  h3("App Organization"),
                  
                  tags$ul(
                    tags$li(strong("About:"), " This page, where you are now, explains the purpose of the app, 
            the dataset, and how to use the interface."),
                    tags$li(strong("Download data:"), " View or download a table of the data filtered by the sidebar selections. "),
                    tags$li(strong("Data Exploraton:"), " Explore data with tables, charts, and plots allowing you to visualize patterns present in the data.")
                  ),
                
      card_image("www/Mobile Phone.png", height = "275px", width = "200px")
    ))),
    
    nav_panel("Data Download", 
              downloadButton("download_filtered", "Download this Data Table"),
              DT::dataTableOutput("filter_table")),
    
    nav_panel("Data Exploration", 
              card(radioButtons(
                "explore_type", 
                 label = "Select the type of Exploration",
                 choices = list(
                                "One Way Contingency Table" = "cat_sum1", 
                                "Two Way Contingency Table" = "cat_sum2",
                                "Numeric Value Summaries" = "num_sum", 
                                "Plots" = "visual",
                                "Numeric Correlation Heatmap" = "heatmap"
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
  
  
  # ------------------ UI --------------------------------
  output$dynamic_card_output <- renderUI({
    
    validate(
      need(!is.null(input$explore_type), "Please select button.")
    )
    
    # Use a switch statement for clean, multi-way branching logic
    switch(input$explore_type,
           "cat_sum1" = {
              # If input is "A", render Card A (e.g., a fluidRow with content)

              req(cat_vars)
              req(nice_cat_vars)
             
              fluidRow(
                h3("One Way Contingency Table"),
                
                selectInput("cat_var1" ,"Select Variable",
                            choices = setNames(cat_vars, nice_cat_vars)),
                br(),
                
                actionButton("make_table1", "Create Contingency Table"),
                tableOutput("one_way_table")
              )
           },
           
           "cat_sum2" = {
             fluidRow(
               h3("Two Way Contingency Table"),
               selectInput("cat_var2", "Select First Variable",
                           choices = setNames(cat_vars, nice_cat_vars)),
               selectInput("cat_var3", "Select Second Variable",
                          choices = setNames(cat_vars, nice_cat_vars)),
               actionButton("make_table2", "Create Contingency Table"),
               tableOutput("two_way_table")
            )
             
             
           },
           
           "num_sum" = {
             # If input is "B", render Card B (e.g., a specialized input block)
             
             req(cat_vars)
             req(nice_cat_vars)
             req(num_vars)
             req(nice_num_vars)
             
             fluidRow(
                h3("Numeric Value Summary"),
               
                checkboxGroupInput("num_vars", "Select Numeric Variables to Summarize",
                                   choices = setNames(num_vars, nice_num_vars)),
               
                checkboxGroupInput("stats", "Select Summary Statistics to add",
                                   choices = c("Minimum", "Q1", "Median", "Q3", "Maximum",
                                              "Range", "Standard Deviation", "Variance", "Count")),
               
                actionButton("grouped_num", "Click here to Group by Categorical Variable"),

                conditionalPanel("input.grouped_num",
               
                                 fluidRow(
                                   h3("Choose and order your variable"),
                                   
                                  selectInput("group_cat", "Select Categorical Variable to group", 
                                              choices =c("None" = "None", setNames(cat_vars, nice_cat_vars)),
                                              selected = "None")
                                 )
                               ),
                actionButton("make_num_sum", "Create Table"),
               
                tableOutput("num_sum_tab")
             )           
             },
           
           "visual" = {
              # If input is "C", render Card C (e.g., a DT table output)
              req(cat_vars)
              req(nice_cat_vars)
              req(name_lookup_table)
              
              div(
                h3("Plot Data"),
                selectInput("x_var", "Select X variable", choices = setNames(vars, nice_vars)),
                selectInput("y_var", "Select Y variable", choices = c("None" = "None", setNames(num_vars, nice_num_vars)), selected = "None"),
                selectInput("fill", "Group By: ", choices = c("None" = "None", setNames(cat_vars, nice_cat_vars)), selected = "None"),
                actionButton("add_facet", "Click Here to add faceting"),
                conditionalPanel("input.add_facet", 
                                 selectInput("facet1", "Select Variable to Facet by",
                                             choices = c("None" = "None", setNames(cat_vars, nice_cat_vars)), selected = "None"),
                                 selectInput("facet2", "Select Second Faceting Variable if Desired",
                                             choices = c("None" = "None", setNames(cat_vars, nice_cat_vars), selected = "None"))
                ),
                actionButton("make_plot", "Click to make plot"),
                
                plotOutput("plot")
              )
           },
           "heatmap" = {
             card(
             plotOutput("corr"))
           }
           
      )
    
    })
  
    #-------------------------------Server ------------
    
    
    oneway_table_data <- eventReactive(input$make_table1, {
      return(make_way_tables(df, col_names = input$cat_var1))
      
      # sides <- c(input$cat_var1, input$cat_var2)
      # make_way_tables(df, sides)
    })
    
    # Render it in the UI
    output$one_way_table <- renderTable({
      oneway_table_data()
    })
    
    twoway_table_data <- eventReactive(input$make_table2, {
      return(make_way_tables(df, col_names = c(input$cat_var2, input$cat3)))

    })
    
    # Render it in the UI
    output$two_way_table <- renderTable({
      twoway_table_data()
    })
    
    
    
    num_sum_data <- eventReactive(input$make_num_sum,{
      
                                  
                                  vars_s <- input$num_vars
                                  stats <- input$stats
                                  groups <- input$group_cat
                                  #df, vars, groups = NULL, stats = c("Minimum", "Q1", "Median", "Q3", "Maximum"), quantiles
                                 

                                  if (length(groups) == 0 || groups == "None") {groups <- NULL}
                                  
                                  out <- make_summaries(
                                    df = df, vars = vars_s, stats = stats, 
                                   groups = groups
                                  )
                                  
                                  print(str(out))  #  diagnostic: see what’s being returned
                                  return(out)
                                  
                                  }
    )
    
    output$num_sum_tab <- renderTable({
      num_sum_data()
    })
    
    
    plot_plot <- eventReactive(input$make_plot, {
      
      req(input$x_var)
      
      # --- Handle optional inputs ---
      group <- if (input$fill %in% c("None", "")){ NULL} else {input$fill}
      y_var <- if (input$y_var %in% c("None", "")) {NULL} else {input$y_var}
      facet1 <- if (input$facet1 %in% c("None", "")) {NULL} else {input$facet1}
      facet2 <- if (input$facet2 %in% c("None", "")) NULL else {input$facet2}
      
      facet <- c(facet1, facet2)
      facet <- facet[!sapply(facet, is.null)]  # Remove NULLs
      
      # --- Determine type and variables ---
      if(is.null(y_var)){  
        # Only x_var is selected
        if(is.factor(df[[input$x_var]])){
          type <- "bar"
          vars <- input$x_var
        } else {
          type <- "density"
          vars <- input$x_var
        }
      } else {
        # Both x and y are selected
        if(is.factor(df[[input$x_var]])){
          type <- "box"
          vars <- y_var
          group = input$x_var
        } else {
          type <- "scatter"
          vars <- c(input$x_var, y_var)
        }
      }
      
      # Call make_visuals with cleaned arguments
      make_visuals(df = df, vars = vars, group = group, facet = facet, type = type)
      
    })
    
    # --- Render the plot ---
    output$plot <- renderPlot({
      plot_plot()
    })
    
    
    
    output$corr <- renderPlot({
      make_visuals(df, type = "correlation", vars = vars)
    })
}  

  
shinyApp(ui = ui, server = server)
