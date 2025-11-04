library(tidyverse)
library(GGally)

#Loading data
df <- read_csv("user_behavior_dataset.csv")

#Separating between numerical and categorical data
num <- df |> select(where(is.numeric)) |> select(-c(`User ID`, `User Behavior Class`))
cat <- df |> select(where(is.character), `User Behavior Class`) |>
  mutate(across(where(is.character), as.factor)) |> 
  mutate(`User Behavior Class` = factor(x = `User Behavior Class`, 
                labels = c("Light Usage", "Mild Usage", "Moderate Usage", "Heavy Usage", "Extreme Usage")))
user_id <- df |> select(`User ID`)

vars <- tibble(cat, num)
ggpairs(vars)

#Categorical data---------------------------------------------

#Creating 1 way contingency tables
lapply(cat, table)

#Creating simple bar charts
lapply(names(cat), function(col_name) {
  ggplot(data = cat, aes(x = .data[[col_name]], fill = .data[[col_name]])) +
    labs(title = paste("Count of", col_name),
         x = col_name,
         y = "Count") +
    geom_bar() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          plot.title = element_text(hjust = .5))
})

#Creating all possible 2 way table combinations
for (i in names(cat)){
  for (j in names(cat)){
    if(i != j){
      result <- table(cat[[i]], cat[[j]])
      print(paste(i, "vs", j))
      print(result)
    }
  }
}

#Numerical data--------------------------------------

#5 numerber summaries

lapply(num, summary)

lapply(names(num), function(col_name) {
  ggplot(data = num, aes(x = .data[[col_name]])) + 

    geom_histogram(aes(y = after_stat(density)), fill = "skyblue", 
                   color = "white", bins = 30) +
    
    geom_density(linewidth = 1.2, color = "red") + 
    
    labs(title = paste("Distribution of", col_name), # Updated title
         x = col_name,
         y = "Density") + # Updated y-axis label
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          plot.title = element_text(hjust = 0.5))
})


