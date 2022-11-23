# 2. Load packages ----
# Data Manipulation
library(dplyr)    # data.frames
library(sf)       # Spatial

# Interactive Data Viz
library(leaflet)  # Maps
library(dygraphs) # Charts
library(DT)       # tables
library(rvest)    # webscraping

# Shiny
library(dqshiny)    # auto complete
library(shiny)       # Starting Reactivity
library(shinythemes) # themes


#Load Spatial Data
counties = readRDS("./data/counties.rds")

# Read in COVID-19 Timesries from URL
read_covid19 = function(){
  url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
  read.csv(url, stringsAsFactors = FALSE) %>% 
    mutate(date = as.Date(date), 
           fips = as.numeric(fips), 
           name = paste0(county, " County, ", state))
}

# Join County Data with 
today_centroids = function(counties, covid_data){
  filter(covid_data, date == max(date)) %>% 
    left_join(st_centroid(counties), by = 'fips') %>% 
    na.omit() %>% 
    mutate(size = abs(cases - mean(cases)) / sd(cases)) %>% 
    st_as_sf()
}

#covid19 = read_covid19()
#today   = today_centroids(counties, covid19)

# The graph requires you COVID data and a FIP code as input
make_graph  = function(covid19, FIP){
  
  subset = filter(covid19, fips == FIP)
  rownames(subset) <- subset$date
  
  # Fit and exponetial model for fun
  exponential.model <- lm(log(cases)~ date, data = subset)
  # use the model to predict a what a expoential curve would look like
  subset$expCases = ceiling(exp(predict(exponential.model, list(date = subset$date))))
  
  
  # !!!! This is were you put you code!!!!
  
  dygraph(data = select(subset, cases, deaths, expCases),
          main = paste0("COVID-19 Trend: ", subset$name[1]),
          ylab = 'Number of Cases/Deaths', 
          xlab = 'Date') %>% 
    dyHighlight(highlightCircleSize = 4, 
                highlightSeriesBackgroundAlpha = 0.2,
                highlightSeriesOpts = list(strokeWidth = 2)) %>% 
    dyOptions(colors = c("blue", "red", "black"))
}

make_graph (covid19, 8031)

basemap(today)
