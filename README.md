# Project2

# Project Purpose

The key purpose of this project, created for **ST558**, is to build an **R Shiny website** that explores a dataset on mobile device usage and user behavior. The app allows interactive exploration of the data, visualizations, and summaries to better understand patterns across devices and users.

# Files Overview

## `explore_data.qmd`

This Quarto document explores the dataset in a **static fashion**, allowing us to verify that the analyses are created correctly. All functionality in this document is intended to translate directly into the Shiny app.

## `helpers.R`

This file:

- Loads the dataset for the app
- Creates additional variables for easier analysis
- Contains data exploration functions

> Note: Writing some of these functions took nearly as much time as building the app itself, so their utility is debatable—but they helped organize the code.

## `data.csv`

The dataset being analyzed in this project. The data comes from the [Mobile Device Usage and User Behavior Dataset on Kaggle](https://www.kaggle.com/datasets/valakhorasani/mobile-device-usage-and-user-behavior-dataset/data), curated by **Valakhorasani**.

## `www/` folder

Contains the single image (`Mobile Phone.png`) used in the app interface.

# Credits

Dataset courtesy of **Valakhorasani** on [Kaggle](https://www.kaggle.com/datasets/valakhorasani/mobile-device-usage-and-user-behavior-dataset/data).