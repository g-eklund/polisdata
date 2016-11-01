rawdata <- read.csv("historical_crimes2.csv", header = TRUE, sep = ",")
event_date <- paste (rawdata$Year, rawdata$Month, rawdata$Day, sep = "-")
event_date <- as.Date(event_date)
event_weekday <- weekdays(event_date)





id_and_dates <- cbind(rawdata$ID, event_date)
id_and_dates <- as.data.frame(id_and_dates)







#addeddate <- cbind(rawdata$ID, date, rawdata$Year, rawdata$Month, rawdata$Day, rawdata$Hour, rawdata$Minute, rawdata$Area, rawdata$Category)
#addeddate <- as.data.frame(addeddate)
#addeddate <- unique(addeddate)
#summary(addeddate)
