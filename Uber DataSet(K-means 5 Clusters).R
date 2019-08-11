apr14 <-read.csv("./datafiles/uber/uber-raw-data-apr14.csv")
may14 <-read.csv("./datafiles/uber/uber-raw-data-may14.csv")
jun14 <-read.csv("./datafiles/uber/uber-raw-data-jun14.csv")
jul14 <-read.csv("./datafiles/uber/uber-raw-data-jul14.csv")
aug14 <-read.csv("./datafiles/uber/uber-raw-data-aug14.csv")
sep14 <-read.csv("./datafiles/uber/uber-raw-data-sep14.csv")
library(dplyr)
data14 <- bind_rows(apr14,may14,jun14,jul14,aug14,sep14)
sum(is.na(data14))

library(lubridate)
data14$Date.Time <- mdy_hms(data14$Date.Time)
print(head(data14))
data14$year <- factor(year(data14$Date.Time))
data14$month <- factor(month(data14$Date.Time))
data14$day <- factor(day(data14$Date.Time))
data14$weekday <- factor(wday(data14$Date.Time))
data14$hour <- factor(hour(data14$Date.Time))
data14$minute <- factor(minute(data14$Date.Time))
data14$second <- factor(second(data14$Date.Time))
print(head(data14))

set.seed(20)
clusters <- kmeans(data14[,2:3],5)
str(clusters)
data14$burough =clusters$cluster

library(DT)
data14$month <- as.double(data14$month)
month_borough_14 <- count_(data14, vars = c('month', 'burough'), sort = TRUE) %>%
arrange(desc(month,burough))
print(datatable(month_borough_14))


