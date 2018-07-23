###RScript for Q3 of Data Incubator Challenge

###DI PLOT TWO BELOW

#set working directory
setwd("C:/Users/Jouki/Dropbox/Work/DS/DI_Chall/data/Q3/")
#setwd("Dropbox/Data Science/DI_Chall/data/Q3/")

#load some pacakges
library("readr")
library("tidyverse")
library("ggplot2")
library("data.table")
library("dplyr")
library("lubridate")
library("rvest")
library("stringi")
library("ggmap")
library("fiftystater")

#####################################creating median income data
#scrape data for median income by state, omit DC and Puerto Rico,
#convert state names to abbreviations
url <- read_html("https://en.wikipedia.org/wiki/List_of_U.S._states_by_income")
wagetable <- url %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div/table[2]') %>%
  html_table()
wagetable <- na.omit(wagetable)
wagetable$state <- state.abb[match(wagetable$State, state.name)]

#remove $ from median income
for (i in c(3:7)) {
  wagetable[[i]] <- substr(wagetable[[i]], 2, length(wagetable[[i]]))
}

#remove punctuation from median income and convert char to num
for (i in 1:length(wagetable)) {
  wagetable[[i]] <- gsub('[[:punct:]]','',wagetable[[i]])
}
for (i in c(3:7)) {
  wagetable[[i]] <- as.numeric(wagetable[[i]])
}
names(wagetable) <- c("trash1", "state_full", "y2015", "y2014", "y2013", "y2012", "y2011", "state")
wage2015 <- subset(wagetable, select=c(3,8))


######################################curating the housing market data
#reseting the wd
setwd("C:/Users/Jouki/Dropbox/Work/DS/DI_Chall/data/Q3/")

d <- read.csv("housingdata/RDC_InventoryCoreMetrics_County_Hist.csv", strip.white = TRUE)
d <- subset(d, select = c(Month, CountyName, Median.Listing.Price, Active.Listing.Count, Days.on.Market, New.Listing.Count, Price.Increase.Count, Price.Decrease.Count, Avg.Listing.Price, Total.Listing.Count))
#dropping a noisy row
d = d[-74001,]
#splitting apart county/state info into two columns and keeping only city
d$loc <- data.frame(do.call('rbind', strsplit(as.character(d$CountyName),', ',fixed=TRUE)))
d$City <- d$loc[,1]
d$State <- d$loc[,2]
d <- subset(d, select = c(Month, Median.Listing.Price, Active.Listing.Count, Days.on.Market, New.Listing.Count, Price.Increase.Count, Price.Decrease.Count, Avg.Listing.Price, Total.Listing.Count, City, State))
names(d) <- c("date", "medPrice", "activeCount", "daysMarket", "newCount", "increaseCount", "decreaseCount", "avgPrice", "totalCount", "city", "state") 
d$date <- as.POSIXct(d$date, format = "%Y-%m-%d %H:%M:%S")
d$month <- month(d$date)
d$year <- year(d$date)
d$state <- trimws(d$state, which = c("left"))

###############merging tables

#lets put some of those datasets together
#need to prep these sets to merge
#get rid of NA
sapply(d, function(x) sum(is.na(x)))
sapply(wagetable, function(x) sum(is.na(x)))

  #keeping only one year to merge, for mapping, for the DI Challenge
  d2 <- filter(d, year==2015)
  d2$state <- as.factor(d2$state)
  d3 <- d2 %>%
    group_by(state) %>%
    summarise(houseAvg = mean(medPrice))

  wage2015$state <- as.character(wage2015$state)
  d2$state <- as.character(d2$state)
  
  #merging together one year to put onto a map
  d4 <- left_join(d3, wage2015, by = c("state"))
  #creating my new variable
  d4 = d4 %>% mutate(ratio = houseAvg / y2015)
  d4 <- na.omit(d4)
  d4$state <- tolower(state.name[match(d4$state,state.abb)])
  d4 <- as.data.frame(d4)
  
  #creating the map plot
  states_map <- map_data("state")  
  
  #setting the wd for graphs
  setwd("C:/Users/Jouki/Dropbox/Work/DS/DI_Chall/visualizations/Q3/")

  p <- ggplot(d4, aes(map_id = state))+
    geom_map(aes(fill = ratio), color="black", map = fifty_states)+
    expand_limits(x = fifty_states$long, y = fifty_states$lat)+
    scale_fill_gradient(low="purple", high="dark blue")+
    theme(plot.title = element_text(hjust = 0.5))+
    ggtitle("Ratio of Median House Price Over Median Income - 2015")+
    scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) +
    labs(x = "", y = "") + theme(legend.position = "bottom", 
                                 panel.background = element_blank())
  p
  ggsave("2015map.jpg", width = 5, height = 5)