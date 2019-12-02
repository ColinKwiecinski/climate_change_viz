# Analysis code script for my INFO 201 Ax Extra Credit Assignment
# Examines the climate change dataset posted by Berkley Earth on Kaggle.com
# Has functions to create three lineplots to show trends in the time series data
# By Colin Kwiecinski, INFO 201 AU19

library(dplyr)
library(plotly)
library(lintr)
library(lubridate)
library(knitr)

# Import data and reformat date information to be more usable.
main_df <- read.csv("GlobalTemperatures.csv", stringsAsFactors = FALSE)
colnames(main_df)[1] <- "date"
main_df$date <- as.Date(main_df$date, "%Y-%m-%d")
main_df <- main_df %>%
  mutate(year = year(date)) %>%
  group_by(year) 

# Creates a plot of land temp vs time. Option to select certain start year
plot_landavgtemp <- function(df, startYear) {
  df <- df %>%
    summarise(temp = mean(LandAverageTemperature, na.rm = TRUE)) %>%
    filter(year > startYear)
  
  p <- plot_ly(
    data = df,
    x = df$year,
    y = df$temp,
    type = "scatter",
    mode = "lines",
    name = "Temperature"
  ) %>%
    add_lines(
      y = ~ fitted(loess(df$temp ~ df$year)),
      line = list(color = "green"),
      name = "Loess Curve"
    ) %>%
    layout(
      title = paste("Average Land Temperature per year Since", startYear),
      xaxis = list(title = "Year"),
      yaxis = list(title = "Temperature in C")
    )
  return(p)
}
avg_landtemp_line <- plot_landavgtemp(main_df, 1750)
avg_landtemp_line

# Creates a plot of water and land temp vs time. Option to select certain start year
plot_wateravgtemp <- function(df, startYear) {
  if (startYear < 1850) {
    startYear <- 1850
  }
  df <- df %>%
    summarise(temp = mean(LandAndOceanAverageTemperature, na.rm = TRUE)) %>%
    filter(year > startYear)
  
  p <- plot_ly(
    data = df,
    x = df$year,
    y = df$temp,
    type = "scatter",
    mode = "lines",
    name = "Temperature"
  ) %>%
    add_lines(
      y = ~ fitted(loess(df$temp ~ df$year)),
      line = list(color = "green"),
      name = "Loess Curve"
    ) %>%
    layout(
      title = paste("Average Land and Ocean Temperature per year Since", startYear),
      xaxis = list(title = "Year"),
      yaxis = list(title = "Temperature in C")
    )
  return(p)
}
avg_watertemp_line <- plot_wateravgtemp(main_df, 1850)
avg_watertemp_line

# Creates a plot of max land temp vs time. Option to select certain start year
plot_landmaxtemp <- function(df, startYear) {
  if (startYear < 1850) {
    startYear <- 1850
  }
  df <- df %>%
    summarise(temp = mean(LandMaxTemperature, na.rm = TRUE)) %>%
    filter(year > startYear)
  
  p <- plot_ly(
    data = df,
    x = df$year,
    y = df$temp,
    type = "scatter",
    mode = "lines",
    name = "Temperature"
  ) %>%
    add_lines(
      y = ~ fitted(loess(df$temp ~ df$year)),
      line = list(color = "green"),
      name = "Loess Curve"
    ) %>%
    layout(
      title = paste("Max Land Temperature per year Since", startYear),
      xaxis = list(title = "Year"),
      yaxis = list(title = "Temperature in C")
    )
  return(p)
}
avg_maxtemp_line <- plot_landmaxtemp(main_df, 1850)
avg_maxtemp_line
