

#FUNCTION WEB2STRUC (set wd to loc)
library(rvest)
require(rvest)

setwd("C:/Users/Gustav/Documents/R/polisen")
web2struc <- function(page) {
  
  url_part1 <- "https://polisen.se/Aktuellt/Handelser/Handelsearkiv/?p="
  url_part2 <- "&tbSearch=&ddl=0&tbFrom=&tbTo="
  url_path  <-  paste(url_part1, page,url_part2, sep = "")
  
  unstructured_data   <- read_html(url_path)
  structured_web_data <- html_nodes(unstructured_data, xpath = "//h3/a" )
  text_extract        <- html_text(structured_web_data)
  event_details <- NULL
 for (i in 1:length(text_extract)){
   extract         <- unlist(strsplit(text_extract[i], ",")) # Extract categories (date, area, category)
   event_area     <- tail(extract, n = 1)
   
   event_date      <- extract[1]                             # Event date raw format
   date_breakdown  <- unlist(strsplit(event_date, " "))      # Date brakdown to date ad time separated
   date_breakdown2 <- unlist(strsplit(date_breakdown[1], "-"))#date breakdown to year, month, date
   event_year      <- date_breakdown2[1]
   event_month     <- date_breakdown2[2]
   event_day       <- date_breakdown2[3]
   date_breakdown[1]  <- as.Date(date_breakdown[1])
   event_weekday   <- weekdays.Date(as.Date(date_breakdown[1]))
   
   
   hour_breakdown  <- unlist(strsplit(date_breakdown[2], ":"))
   event_hour      <- hour_breakdown[1]
   event_minute   <- hour_breakdown[2]
   
   
   
       #event category dependent on detail level in extract
        if (length(extract) == 4) {
          event_category <- paste(extract[2], extract[3], sep= ",")
          
        }
        else if (length(extract) == 5){
          event_category <- paste(extract[2], extract[3], extract[4], sep= ",")
          print("5")
        }
        else{     
          event_category <- extract[2]
          
        }
   
   ID <- paste(substr(event_category,2,5),event_year, event_month, event_day, event_hour, event_minute, sep = "")
   
    event_details <- rbind(event_details, c(ID, event_year, event_month, event_day, event_weekday, event_hour, event_minute, event_area, event_category))
       
    }
    
  structured_event_details <- event_details
  return(structured_event_details)
}




setwd("C:/Users/Gustav/Documents/R/polisen")


pages <- seq(1,49)
total_events <- NULL
for (i in pages){
  list_events <-web2struc(i)
  total_events <- rbind(total_events, list_events)
}
total_events

write.table(total_events, file ="historical_crimes2.csv", sep = ",", col.names = TRUE)


