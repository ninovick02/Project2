library(tidyverse)
library(janitor)
library(knitr)
library(stats)

df_raw <- read_csv("user_behavior_dataset.csv")
df <- df_raw|>
  mutate(across(where(is.character), as.factor)) |> 
  mutate(`User Behavior Class` = 
           factor(x = `User Behavior Class`, 
                  labels = c("Light Usage", "Mild Usage", "Moderate Usage", "Heavy Usage", "Extreme Usage"))) |>
  clean_names()



name_lookup_table <- tibble("label" = names(df_raw), "var" = names(df))

clean_label <- function(name) {
  if(name %in% names(df)){
    lab <- name_lookup_table$label[which(name_lookup_table$var == name)]
    return(lab)
  }else{
    # 1. Replace all underscores with spaces
    clean <- gsub("_", " ", name)
    # 2. Capitalize the first letter of each word (Title Case)
    s <- strsplit(clean, " ")[[1]]
    s <- paste0(toupper(substring(s, 1, 1)), substring(s, 2))
    return(paste(s, collapse = " "))
  }
 
}


cat_vars <- df |> select(where(is.factor)) |> names()
nice_cat_vars <- sapply(cat_vars, clean_label)
num_vars <- df |> select(where(is.numeric)) |> select(-user_id) |> names()
nice_num_vars <- sapply(num_vars, clean_label)
df_always <- df

vars <- c(cat_vars, num_vars)
nice_vars <- c(nice_cat_vars, nice_num_vars)

# single_var_chart <- function(df, col_name){
#   p <- ggplot(data = df, aes(x = .data[[col_name]])) +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1), 
#           plot.title = element_text(hjust = .5))
#   
#   if(is.numeric(df[[col_name]])){
#     result <- p +
#       geom_histogram(aes(y = after_stat(density)), fill = "skyblue", 
#                      color = "white", bins = 30) +
#       
#       geom_density(linewidth = 1.2, color = "red") + 
#       
#       labs(title = paste("Distribution of", col_name), # Updated title
#            x = col_name,
#            y = "Density")  # Updated y-axis label
#   }
#    else{
#     result <- p +
#        labs(title = paste("Count of", col_name),
#          x = col_name,
#          y = "Count") +
#     geom_bar(aes(fill = .data[[col_name]]))
#    }
#   return(result)
# }

make_way_tables <- function(df, col_names){
  #Only allow options to be categorical data
  
  f = paste("~", paste(col_names, collapse="+"))
  result <- xtabs(f, data=df)
  
  for(i in 1:length(col_names)){
    names(dimnames(result))[i] <- clean_label(col_names[i])
  }
  
  return(result)
}

# make_way_tables <- function(df, ...){
#   #Only allow options to be categorical data
#   
#   result <- tabyl(df, ...)
#   #colnames(result)[1] <- clean_label(col_names)
#   
#   return(result)
# }

make_filtered_tables <- function(df, col_names, groups = NULL, conds = NULL){
  
  if(!is.null(conds)){
    df <- df |> 
      filter(conds)
  }
  
  columns_to_select <- c(groups, setdiff(col_names, groups))

  result <- df |> 
    select(all_of(columns_to_select))
  
  if (!is.null(groups)){
    result <- result |> group_by(across(all_of(groups))) |> arrange(across(all_of(groups)))
  }
  
  result <- result |> rename_with(~ sapply(.x, clean_label), everything())
  
  return(result)
}

make_summaries <- function(df, vars, groups = NULL, stats = c("Minimum", "Q1", "Median", "Q3", "Maximum"), quantiles = NULL){
  
  funct_choice <-  list( "Minimum" = function(x) min(x),
                         "Q1" = function(x) unname(quantile(x, .25)),
                         "Median" = function(x) median(x),
                         "Mean" = function(x) mean(x),
                         "Q3" = function(x) unname(quantile(x, .75)),
                         "Maximum" = function(x) max(x),
                         "Range" = function(x) max(x) - min(x),
                         "Standard Deviation" = function(x) sd(x),
                         "Variance" = function(x) var(x),
                         "Count" = function(x) length(x)
                         )
  functs <- funct_choice[stats]
  
  if(!is.null(quantiles)){
    names(quantiles) <- paste0("p", quantiles * 100)
    quantiles <- quantiles
    
    for(i in 1:length(quantiles)){
      j = i
      if(quantiles[i] <= .25){
        if(!("Minimum" %in% names(functs))){j<- j - 1}
        functs <- append(functs, function(x) unname(quantile(x, quantiles[i]), after = j))
        names(functs)[[j + 1]] <- names(quantiles)[i]
      }
      if(quantiles[i] > .25 & quantiles[i] <= .5){
        if("Q1" %in% names(functs)){j = j + 1}
        if(!("Minimum" %in% names(functs))){j<- j - 1}
        functs <- append(functs, function(x) unname(quantile(x, quantiles[i]), after = j))
        names(functs)[[j + 1]] <- names(quantiles)[i]
      }
      if(quantiles[i] > .5 & quantiles[i] <= .75){
        if(!is.null(intersect(c("Q1", "Median", "Mean"), names(functs)))){j = j + length(intersect(c("Q1, Median, Mean"), names(functs)))}
        if(!("Minimum" %in% names(functs))){j <- j - 1}
        functs <- append(functs, function(x) unname(quantile(x, quantiles[i]), after = j))
        names(functs)[[j + 1]] <- names(quantiles)[i]
      }
      if(quantiles[i] >= 1){
        if(!is.null(intersect(c("Q1", "Median", "Mean", "Q3"), names(functs)))){j = j + length(intersect(c("Q1, Median, Mean", "Q3"), names(functs)))}
        if(!("Minimum" %in% names(functs))){j <- j - 1}
        functs <- append(functs, function(x) unname(quantile(x, quantiles[i]), after = j))
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
          names_pattern = "(.*)_(.*)",
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

make_visuals <- function(df, vars, group = NULL, facet = NULL, type, conds = NULL) {
  
  if(!is.null(conds)){df <- df |> filter(conds)}
  # --- Start Plot Construction ---
  p <- ggplot(df)
  
  # CASE 1: UNIVARIATE (LENGTH 1)
  if (length(vars) == 1) {
    var1 <- vars
    is_cat <- is.factor(df[[vars]])
    
    # --- Aesthetics Setup ---
    base_aes <- var1
    
    # --- Categorical Plots (Bar) ---
    if (is_cat) {
      
      # If grouping is applied, set the fill aesthetic
      if (!is.null(group)) {
        fill_aes <- group
      }else{fill_aes <- var1}
      
      p <- p +
        geom_bar(aes_string(x = base_aes, fill = fill_aes), position = if(!is.null(group)){"dodge"}else{"stack"})
        if(!is.null(group)){
          p <- p + labs(title = paste("Clustered Bar Chart of", clean_label(var1), "by", clean_label(group)),
               fill = clean_label(group), 
               x = clean_label(var1), y = "Count") 
        }else{
          p <- p + labs(title = paste("Bar Chart of", clean_label(var1), sep = " "), 
                        x = clean_label(var1), y = "Count", fill = clean_label(fill_aes))
        }
      # --- Numeric Plots (Hist, Density, Box, Jitter) ---
    } else {
    
      if (type == "density"){
            if(!is.null(group)){
              p <- p + geom_density(aes_string(var1, alpha = .5, fill = group))+ 
                labs(title = paste("Smoothed Histogram of", clean_label(var1), "by", clean_label(group)),
                   fill = clean_label(group)) 
            }else{
              p <- p + geom_density(aes_string(var1), fill = "skyblue") +
                labs(title = paste("Smoothed Histogram of", clean_label(var1), sep = " "))
            }
          p <- p + labs(x = clean_label(var1), y = "Density")
        
      }  else if (type == "box") {
        # Box plots: If grouped, the group variable moves to the X-axis for side-by-side comparison
        if(!is.null(group)){
          p <- p + geom_boxplot(aes_string(x = group, y = var1, fill = group)) + 
            labs(x = clean_label(group), y = clean_label(var1), 
                 title = paste("Box Plot of", clean_label(var1), "by", clean_label(group)))
          } else {
            p <- p + geom_boxplot(aes_string(x = factor(1), y = var1)) + 
              labs(x = " ", y = clean_label(var1), fill = clean_label(group),
                   title = paste("Box Plot of", clean_label(var1)))
        } 
        p <- p + theme(legend.position = "none")
        
      } else if (type == "jitter" && !is.null(group)) {
        # Jitter plots: Require a categorical X-axis (the group)
        p <- p + geom_jitter(aes_string(x = group, y = var1, color = group), width = .25) + 
          labs(title = paste("Jitter Plot of", clean_label(var1), "by", clean_label(group))) +
          theme(legend.position = "none")
        
      } else {
        stop(paste("Invalid or unsupported 'type' for the selected variables and groups:", type))
      }
    } 
  
  # CASE 2: BIVARIATE (LENGTH 2) - SCATTERPLOT
  } else if (length(vars) >= 2) {
    var_x <- vars[1]
    var_y <- vars[2]
    
    if(type == "correlation"){
      df$user_behavior_class <- as.numeric(df$user_behavior_class)
      }
    
  
    if(type == "scatter"){  
      if(!is.null(group)){
        p <- p +
          geom_point(aes_string(x = var_x, y = var_y, color = group),  alpha = 0.7) +
          geom_smooth(aes_string(x = var_x, y = var_y, color = group),  alpha = 0.4, method = lm)
      }else{
        p <- p +
          geom_point(aes_string(x = var_x, y = var_y), alpha = 0.7) +
          geom_smooth(aes_string(x = var_x, y = var_y), alpha = 0.4, method = lm)
      }
      p <- p +
        labs(x = clean_label(var_x),
             y = clean_label(var_y),
             color = clean_label(group),
             title = paste("Scatter Plot:", clean_label(var_y), "vs", clean_label(var_x), if(!is.null(group)){paste("(By", clean_label(group), ")")}))
      
    }else if(type == "correlation"){
      num_df <- df |> select(all_of(vars)) |> select(where(is.numeric))
      corr_mat <- num_df |> cor(use = "pairwise.complete.obs")
      
      for(j in 1:2){
          dimnames(corr_mat)[[j]] <- sapply(names(num_df), clean_label)
       }
      
      corr_long <- as.data.frame(corr_mat) |>
        tibble::rownames_to_column("var1") |>   # preserves row names as a column
        pivot_longer(
          cols = -var1,
          names_to = "var2",
          values_to = "correlation"
        )
      
      q <- ggplot(corr_long, aes(x = var1, y = var2, fill = correlation)) +
        geom_tile(color = "white") +  # white grid lines between tiles
        scale_fill_gradient2(
          low = "blue", mid = "purple", high = "red",
          midpoint = 0, limits = c(-1, 1)
        ) +
        labs(
          title = "Correlation Matrix",
          fill = "Correlation"
        ) +
        theme_minimal() +
        theme(
          axis.title = element_blank(),
          panel.grid = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1), 
          plot.title = element_text(hjust = .5))
      
      return(q)
    }
  }
  
  
  # --- Apply Facets (Final Step) ---
  if(!is.null(facet)){
    if(length(facet) == 1){
      p <- p + facet_wrap(as.formula(paste("~", facet))) +
        labs(subtitle = paste("Per", clean_label(facet))) 
    }else if(length(facet) == 2){
      p <- p + facet_grid(as.formula(paste(facet[1], "~", facet[2]))) + 
        labs(subtitle = paste("Per Intersects of", clean_label(facet[1]), "by", clean_label(facet[2])))
    }
    p <- p + theme(plot.subtitle = element_text(hjust = .5))
  }
  
  final_plot <- p +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          plot.title = element_text(hjust = .5))
  
  return(final_plot)
}


