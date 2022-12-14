
remotes::install_github("daqana/dqshiny")

# 1. Install missing packages ----
list.of.packages <- c("dplyr", "sf", 
                      "leaflet", "dygraphs", "DT", 
                      'shiny', 'rvest',"shinythemes", "rsconnect")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
