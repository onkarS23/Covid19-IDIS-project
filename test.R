url ='https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
# Here we are reading the URL directly using the url variable created above
covid19 = read.csv(url, stringsAsFactors = FALSE)
head(covid19)
library(dplyr)

covid19  = covid19 %>% 
  mutate(date = as.Date(date), # Override date with formated date
         name = paste0(county, " County, ", state)) # Make a new text string

## Lets check!
str(covid19)

# Filter the covid19 data to only those records were fips equals 6037
test = filter(covid19, fips == 6037)

# Plot the data
plot(x = test$date, # define the x axis variable using all the dates in test
     y = test$cases, # define the y axis variable
     type  = "l", # we wnat a line "l" plot
     xlab = "Date", ylab = "Cases", # refine the x and y labels
     main = test$name[1]) # define the plot title.

library(sf) # Attach sf library

counties = readRDS("./data/counties.rds") # read rds data using the path to your resource

plot(counties$geometry) # plot the county geometries

today = filter(covid19, date == max(date)) %>% # filter all data to the maximum datae
  left_join(st_centroid(counties), by = 'fips') %>%  # cast county geometries to centroid, and join by fip
  na.omit() %>% # remove NA values
  mutate(size = abs(cases - mean(cases)) / sd(cases)) # Compute a scaled case count

# Plot and size (cex) the cnetroid data
plot(today$geometry, cex = today$size)