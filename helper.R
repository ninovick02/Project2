df <- read_csv("user_behavior_dataset.csv")
df <- df|>
  mutate(across(where(is.character), as.factor)) |> 
  mutate(`User Behavior Class` = 
           factor(x = `User Behavior Class`, 
                  labels = c("Light Usage", "Mild Usage", "Moderate Usage", "Heavy Usage", "Extreme Usage")))


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



