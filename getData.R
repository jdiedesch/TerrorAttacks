library(rvest)
library(stringr)
library(dplyr)
library(lubridate)

monthFunc <- function(idx) {
  if (idx <= 12) m <- idx
  if (idx > 12 && idx <= 24) m <- idx - 12
  if (idx > 24 && idx <= 36) m <- idx - 24
  if (idx > 36) m <- idx - 36
  
  m
  
}

numbCleaner <- function(val) {
  if (is.na(val)) return(NA)
  
  clean <- as.numeric(val)
  if (is.na(clean)) {
    
    # paren with extra space
    if (str_detect(val, " \\(")) {
      clean <- substr(val, 1, str_locate(val, " \\(") - 1)
      clean <- as.numeric(clean)
      
      clean2 <- substr(val, 
                       str_locate(val, " \\(")[1] + 2,
                       str_locate(val, "\\)")[1] - 1)
      
      clean2 <- as.numeric(clean2)
      clean <- ifelse(is.na(clean2), clean, clean + clean2)
      
    }
    
  }
  
  # get first part when there's a +
  if (is.na(clean)) {
    if (str_detect(val, "\\+")) {
      clean <- substr(val, 1, str_locate(val, "\\+") - 1)
      clean <- as.numeric(clean)
    }
  }
  
  # em dash
  if (is.na(clean)) {
    if (str_detect(val, "\u2013")) {
      clean <- substr(val, 1, str_locate(val, "\u2013") - 1)
      clean <- as.numeric(clean)
    }
  }
  
  # regular dasg
  if (is.na(clean)) {
    if (str_detect(val, "-")) {
      clean <- substr(val, 1, str_locate(val, "-") - 1)
      clean <- as.numeric(clean)
    }
  }
  
  # tight paren (i.e. no extra space)
  if (is.na(clean)) {
    if (str_detect(val, "\\(")) {
      clean <- substr(val, 1, str_locate(val, "\\(") - 1)
      clean <- as.numeric(clean)
    }
  }
  
  clean
}

getWikipediaData <- function() {

  # prior to april 2016, wikipedia used a different naming convention for tables
  partialMonths <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                     "Sep", "Oct", "Nov", "Dec")
  
  # unchanging portion of URL
  baseUrl <- "https://en.wikipedia.org/wiki/List_of_terrorist_incidents_in_"
  
  # url tails of the form 'month_year'
  urlTails <- paste0(month.name,
                     "_", 
                     c(rep(2015, 12), rep(2016, 12), rep(2017, 12), rep(2018, 4)))
  tgtUrls <- paste0(baseUrl, urlTails)
  
  # xpath node tails
  
  node2015 <- paste0("terrorIncidents", c(rep(2015, 12)), partialMonths)
  node2016a <- paste0("terrorIncidents", 2016, partialMonths[1:4])
  node2016b <- paste0("terrorIncidents", 2016, month.name[5:12])
  node2017 <- paste0("terrorIncidents", 2017, month.name)
  node2018 <- paste0("terrorIncidents", 2018, month.name[1:4])
  
  nodeTails <- c(node2015, node2016a, node2016b, node2017, node2018)
  
  # xpaths for extracting tables
  xPaths <- paste0('//*[@id=', shQuote(nodeTails), "]")
  
  # storage frame for holding incidents
  terrorFrame <- data.frame()
  
  # loop through and get data
  
  for (i in 1:length(tgtUrls)) {
    
    m <- monthFunc(i)
    y <- substr(tgtUrls[i], str_length(tgtUrls[i]) - 3, str_length(tgtUrls[i]))
    
    # read in the html
    htmlPage <- read_html(tgtUrls[i])
    
    # wikipedia carried over february for naming the tables in march and april 2018
    xp <- xPaths[i]
    
    # extract the table and add month & year
    dataTbl <- html_node(htmlPage, xpath = xp)
    
    if (class(dataTbl) == "xml_missing"){ 
      dataTbl <- html_node(htmlPage, xpath = oldXp)
    }
    
    else {
      oldXp <- xp
    }
    
    dataTbl <- dataTbl %>%
      html_table(fill = TRUE) %>%
      mutate(month = m,
             year = y)
    
    # store the new data
    terrorFrame <- rbind(terrorFrame, dataTbl)
  }
  
  # clean up the death totals
  terrorFrame$cleanDeaths <- apply(as.matrix(terrorFrame$Dead), 1, numbCleaner)
  
  
  
  # 2011 to 2015 Data -------------------------------------------------------
  
  urlTails2 <- paste0(c("January-June_",  "July-December_"), 
                      c(rep(2011, 2), rep(2012, 2), rep(2013, 2), rep(2014, 2)))
  tgtUrls2 <- paste0(baseUrl, urlTails2)
  
  node2011 <- paste0("terrorIncidents", rep(2011, 12), partialMonths)
  node2012 <- paste0("terrorIncidents", rep(2012, 12), partialMonths)
  node2013 <- paste0("terrorIncidents", rep(2013, 12), partialMonths)
  node2014 <- paste0("terrorIncidents", rep(2014, 12), partialMonths)
  
  nodeTails2 <- c(node2011, node2012, node2013, node2014)
  
  
  # xpaths for extracting tables
  xPaths2 <- paste0('//*[@id=', shQuote(nodeTails2), "]")
  
  
  # storage frame for holding incidents
  # terrorFrame2 <- data.frame()
  terrorFrame2 <- list()
  
  # loop through and get data
  cnt <- 1
  
  for (i in 1:length(tgtUrls2)) {
    for (j in 1:6){
      
      m <- monthFunc(cnt)
      y <- substr(tgtUrls2[i], str_length(tgtUrls2[i]) - 3, str_length(tgtUrls2[i]))
      
      # read in the html
      htmlPage <- read_html(tgtUrls2[i])
      
      # wikipedia carried over february for naming the tables in march and april 2018
      xp <- xPaths2[cnt]
      
      # extract the table and add month & year
      dataTbl <- html_node(htmlPage, xpath = xp) %>%
        html_table(fill = TRUE) %>%
        mutate(month = m,
               year = y)
      
      if (y < 2014) {
        colnames(dataTbl) <- c("Date", "Type", "Dead", "Injured", "Location",
                               "Details", "Perpetrator", "State", "Non-State", 
                               "month", "year")
        dataTbl <- select(dataTbl, -State, -`Non-State`) %>%
          mutate(`Part of` = NA)
      }
      
      # store the new data
      terrorFrame2 <- rbind(terrorFrame2, dataTbl)
      
      cnt <- cnt + 1
    }
  }
  
  # clean up the death totals
  terrorFrame2$cleanDeaths <- apply(as.matrix(terrorFrame2$Dead), 1, numbCleaner)
  
  terrorEvents <- rbind(terrorFrame2, terrorFrame)
  terrorEvents$cleanDay <- apply(as.matrix(terrorEvents$Date), 1, numbCleaner)
  
  terrorEvents
}