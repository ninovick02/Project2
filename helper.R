df_raw <- read_csv("user_behavior_dataset.csv")
df <- df_raw|>
  mutate(across(where(is.character), as.factor)) |> 
  mutate(`User Behavior Class` = 
           factor(x = `User Behavior Class`, 
                  labels = c("Light Usage", "Mild Usage", "Moderate Usage", "Heavy Usage", "Extreme Usage"))) |>
  clean_names()

clean_label <- function(name) {
  # 1. Replace all underscores with spaces
  clean <- gsub("_", " ", name)
  # 2. Capitalize the first letter of each word (Title Case)
  s <- strsplit(clean, " ")[[1]]
  s <- paste0(toupper(substring(s, 1, 1)), substring(s, 2))
  return(paste(s, collapse = " "))
}

single_var_chart <- function(df, col_name){
  p <- ggplot(data = df, aes(x = .data[[col_name]])) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          plot.title = element_text(hjust = .5))
  
  if(is.numeric(df[[col_name]])){
    result <- p +
      geom_histogram(aes(y = after_stat(density)), fill = "skyblue", 
                     color = "white", bins = 30) +
      
      geom_density(linewidth = 1.2, color = "red") + 
      
      labs(title = paste("Distribution of", col_name), # Updated title
           x = col_name,
           y = "Density")  # Updated y-axis label
  }
   else{
    result <- p +
       labs(title = paste("Count of", col_name),
         x = col_name,
         y = "Count") +
    geom_bar(aes(fill = .data[[col_name]]))
   }
  return(result)
}

make_way_tables <- function(col_names = ...){
  #Only allow options to be categorical data
  temp <- ...
  tabyl(df, temp)
}

make_filtered_tables <- function(df, col_names, groups = NULL, conds = c(NULL, NULL)){
  
  if(!is.null(conds)){
    df <- df |> 
      filter(
        if_all(
          .cols = where(is.numeric),
          .fns = ~ .x >= conds[1]  & .x <= conds[2])
      )
  }
  
  if (!is.null(groups)){
    df <- df |> group_by(across(all_of(groups)))
  }
  
  result <- df |> select(col_names)
  
  return(result)
}

make_summaries <- function(df, vars, groups = NULL, stats = c("Minimum", "Q1", "Median", "Q3", "Maximum"), quantiles = NULL){
  
  funct_choice <-  list( "Minimum" = ~ min(.x),
                         "Q1" = ~ quantile(.x, .25),
                         "Median" = ~ median(.x),
                         "Mean" = ~ mean(.x),
                         "Q3" = ~ quantile(.x, .75),
                         "Maximum" = ~ max(.x),
                         "Standard Deviation" = ~ sd(.x),
                         "Variance" = ~ var(.x),
                         "Range" = ~ range(.x),
                         "Count" = ~ n()
                         )
  functs <- funct_choice[stats]
  
  if(!is.null(quantiles)){
    names(quantiles) <- paste0("p", quantiles * 100)
    quantiles <- quantiles
    
    for(i in 1:length(quantiles)){
      j = i
      if(quantiles[i] <= .25){
        if(!("Minimum" %in% names(functs))){j<- j - 1}
        functs <- append(functs, ~ quantile(.x, quantiles[i]), after = j)
        names(functs)[[j + 1]] <- names(quantiles)[i]
      }
      if(quantiles[i] > .25 & quantiles[i] <= .5){
        if("Q1" %in% names(functs)){j = j + 1}
        if(!("Minimum" %in% names(functs))){j<- j - 1}
        functs <- append(functs, ~ quantile(.x, quantiles[i]), after = j)
        names(functs)[[j + 1]] <- names(quantiles)[i]
      }
      if(quantiles[i] > .5 & quantiles[i] <= .75){
        if(!is.null(intersect(c("Q1", "Median", "Mean"), names(functs)))){j = j + length(intersect(c("Q1, Median, Mean"), names(functs)))}
        if(!("Minimum" %in% names(functs))){j <- j - 1}
        functs <- append(functs, ~ quantile(.x, quantiles[i]), after = j)
        names(functs)[[j + 1]] <- names(quantiles)[i]
      }
      if(quantiles[i] >= 1){
        if(!is.null(intersect(c("Q1", "Median", "Mean", "Q3"), names(functs)))){j = j + length(intersect(c("Q1, Median, Mean", "Q3"), names(functs)))}
        if(!("Minimum" %in% names(functs))){j <- j - 1}
        functs <- append(functs, ~ quantile(.x, quantiles[i]), after = j)
        names(functs)[[j + 1]] <- names(quantiles)[i]
      }
    }
    
  }
  
  if (!is.null(groups)){
    df <- df |> group_by(across(all_of(groups)))
  }

    summary_table_wide <- df |> 
      summarize(
        across(
          .cols = {{ vars }},
          .fns = functs
        )
      )
    
    summary_table_long <- summary_table_wide |> 
      pivot_longer(
          cols = !all_of(groups), 
          names_to = c("Variable", "Statistic"), 
          names_sep = "_",
          values_to = "Value"
        )
    
    summary_table <- summary_table_long |>
      pivot_wider(
        id_cols = c(all_of(groups), "Variable"),
        names_from = "Statistic", 
        values_from = "Value"
      )
    
    return(summary_table)
  
}

#' Helper to determine if a variable in a data frame is effectively discrete (categorical).
#' @param df The data frame.
#' @param var The unquoted column name (as a string).
#' @return Logical (TRUE if categorical).
is_discrete <- function(df, var) {
  # Check if the column is a factor, character, or logical type
  is.factor(df[[var]]) || is.character(df[[var]]) || is.logical(df[[var]])
}

#' Helper to apply facetting based on the 'facet' argument length.
#' @param p The ggplot object.
#' @param facet A character vector of length 1 or 2.
#' @return The ggplot object with faceting applied, or the original plot.
apply_facets <- function(p, facet) {
  if (is.null(facet)) {
    return(p)
  }
  
  if (length(facet) == 1) {
    p <- p + facet_wrap(sym(facet[1]))
  } else if (length(facet) == 2) {
    # Facet grid requires the format rows ~ columns
    p <- p + facet_grid(sym(facet[1]) ~ sym(facet[2]))
  } else {
    warning("Facet argument should be of length 1 (wrap) or 2 (grid). Facetting skipped.")
  }
  return(p)
}

# =================================================================
# MAIN PLOTTING FUNCTION
# =================================================================

#' Generates various ggplot2 visualizations based on input variables and type.
#'
#' @param df The data frame.
#' @param vars A character vector of length 1 (univariate) or 2 (bivariate).
#' @param group An optional character string for the grouping variable (must be categorical).
#' @param facet An optional character vector of length 1 or 2 for faceting.
#' @param type A character string specifying the plot type (e.g., "bar", "hist", "scatter").
#' @return A ggplot object.
make_visuals <- function(df, vars, group = NULL, facet = NULL, type) {

  # --- Start Plot Construction ---
  p <- ggplot(df)
  
  # =================================================================
  # CASE 1: UNIVARIATE (LENGTH 1)
  # =================================================================
  if (length(vars) == 1) {
    var1 <- vars
    is_cat <- is.factor(vars)
    
    # --- Aesthetics Setup ---
    base_aes <- "x = var1"

    
    # --- Categorical Plots (Bar / Pie) ---
    if (is_cat) {
      
      # If grouping is applied, set the fill aesthetic
      if (!is.null(group)) {
        fill_aes <- "fill = group"
      }else{fill_aes <- "fill = var1"}
      
        p <- p + 
          geom_bar(aes(sym(base_aes), sym(fill_aes)), position = if(!null(group)){"dodge"}else{"stack"}) +
          if(!is.null(group)){
            labs(title = paste("Clustered Bar Chart of", clean_label(var1), "by", clean_label(group)),
                 fill = clean_label(group)) 
          }else{
            labs(title = paste("Bar Chart of", clean_label(var1), sep = " "))
          }
          + labs(xlab = clean_label(var1), ylab = "Count")
      # --- Numeric Plots (Hist, Density, Box, Jitter) ---
    } else {
      
      if (!is.null(group)) {
        fill_aes <- "alpha = .5, fill = group"
      }else{fill_aes <- "fill = 'skyblue'"}
      # Prepare position argument for grouped numeric plots
      pos <- if(!is.null(group)){ "identity"} else{ "stack"}
    
      if (type == "density"){
        p <- p + 
          p <- p + 
            geom_histogram(aes(sym(base_aes), sym(fill_aes))) +
            if(!is.null(group)){
              labs(title = paste("Smoothed Histogram of", clean_label(var1), "by", clean_label(group)),
                   fill = clean_label(group)) 
            }else{
              labs(title = paste("Smoothed Histogram of", clean_label(var1), sep = " "))
            }
          + labs(xlab = clean_label(var1), ylab = "Density")
        
      }  else if (type == "box") {
        # Box plots: If grouped, the group variable moves to the X-axis for side-by-side comparison
        p <- p + if(!is.null(group)){
          geom_boxplot(aes(x = group, y = var1, fill = group)) + 
            labs(x = clean_label(group), y = clean_label(var1), 
                 title = paste("Box Plot of", clean_label(var1), "by", clean_label(group)))
          } else {
            geom_boxplot(aes(x = factor(1), y = var1)) + 
              labs(x = " ", y = clean_label(var1), 
                   title = paste("Box Plot of", clean_label(var1)))
        } 
        
      } else if (type == "jitter" && !is.null(group)) {
        # Jitter plots: Require a categorical X-axis (the group)
        p <- p + geom_jitter(aes(x = group, y = var1, color = group), width = .25) + 
          labs(title = paste("Jitter Plot of", clean_label(var1), "by", clean_label(group)))
        
      } else {
        stop(paste("Invalid or unsupported 'type' for the selected variables and groups:", type))
      }
    } 
  }
}


