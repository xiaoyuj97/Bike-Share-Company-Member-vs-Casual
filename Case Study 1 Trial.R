#to start with
setwd("/Users/xiaoyujin/Desktop/GDA/Case Study 1 Data")
getwd()
library(tidyverse)

#split into member and casual-function1
raw202301<-read.csv("202301-divvy-tripdata.csv")
raw202301split<-split(raw202301,raw202301$member_casual)
member202301<-raw202301split$member
head(member202301)
str(member202301)
casual202301<-raw202301split$casual
head(casual202301)

table(is.na(member202301))
table(is.na(casual202301))
casual202301complete <- casual202301[complete.cases(casual202301), ]
member202301complete <- member202301[complete.cases(member202301), ]
nrow(member202301complete)

#duration column-function2
library(lubridate)
casual202301duration<-ymd_hms(casual202301complete$ended_at)-ymd_hms(casual202301complete$started_at)
member202301duration<-ymd_hms(member202301complete$ended_at)-ymd_hms(member202301complete$started_at)
length(casual202301duration)
#route column
casual202301route<-paste(casual202301complete$start_station_name,
                         casual202301complete$end_station_name,
                         sep=" ")
head(casual202301route)
member202301route<-paste(member202301complete$start_station_name,
                         member202301complete$end_station_name,
                         sep=" ")
str(casual202301route)
#dataset to analyze
casual202301all<-cbind(casual202301complete,casual202301duration,casual202301route)
member202301all<-cbind(member202301complete,member202301duration,member202301route)
head(member202301all)

#descriptive
table(is.na(casual202301complete$start_station_name))
table(is.na(casual202301complete$end_station_name))

#function3
#poplar starting station for casual
casual202301start <- table(casual202301complete$start_station_name)
casual202301start<-data.frame(casual202301start)
head(casual202301start)
casual202301start_sorted <- casual202301start %>%
  arrange(desc(Freq))
casual202301start_sorted100<-head(casual202301start_sorted,100)
#poplar end station for casual
casual202301end <- table(casual202301complete$end_station_name)
casual202301end<-data.frame(casual202301end)
head(casual202301end)
casual202301end_sorted <- casual202301end %>%
  arrange(desc(Freq))
casual202301end_sorted100<-head(casual202301end_sorted,100)
#poplar starting station for member
member202301start <- table(member202301complete$start_station_name)
member202301start<-data.frame(member202301start)
head(member202301start)
member202301start_sorted <- member202301start %>%
  arrange(desc(Freq))
member202301start_sorted100<-head(member202301start_sorted,100)
#poplar end station for member
member202301end <- table(member202301complete$end_station_name)
member202301end<-data.frame(member202301end)
head(member202301end)
member202301end_sorted <- member202301end %>%
  arrange(desc(Freq))
member202301end_sorted100<-head(member202301end_sorted,100)

#most popular stations cbind
mostpopular100stations<-cbind(casual202301start_sorted100,
                              casual202301end_sorted100,
                              member202301start_sorted100,
                              member202301end_sorted100)
mostpopular100stations1<-mostpopular100stations[-1,]
colnames(mostpopular100stations1)<-c("casualstart","Freq1","casualend","Freq2","memberstart","Freq3","memberend","Freq4")
head(mostpopular100stations1)

#function4
#most popular route for casual
casual202301route
casual202301routefreq <- table(casual202301route)
casual202301routefreq<-data.frame(casual202301routefreq)
head(casual202301routefreq)
casual202301routefreq<-casual202301routefreq[-1,]
casual202301routefreq_sorted <- casual202301routefreq %>%
  arrange(desc(Freq))
casual202301routefreq_sorted10<-head(casual202301routefreq_sorted,10)
casual202301routefreq_sorted10
#most popular route for member
member202301route
member202301routefreq <- table(member202301route)
member202301routefreq<-data.frame(member202301routefreq)
head(member202301routefreq)
member202301routefreq<-member202301routefreq[-1,]
member202301routefreq_sorted <- member202301routefreq %>%
  arrange(desc(Freq))
member202301routefreq_sorted10<-head(member202301routefreq_sorted,10)
member202301routefreq_sorted10


#most popular route for member and casual: tableau

#part 5: rideable_type difference
table(casual202301all$rideable_type)
table(member202301all$rideable_type)


#function6: combine data and change names
casual202301allforcom<-casual202301all
member202301allforcom<-member202301all
colnames(casual202301allforcom)<-c("ride_id","rideable_type","started_at","ended_at",
                             "start_station_name","start_station_id",
                             "end_station_name","end_station_id",
                             "start_lat","start_lng",
                             "end_lat","end_lng",
                             "member_casual","duration","route")
colnames(member202301allforcom)<-c("ride_id","rideable_type","started_at","ended_at",
                                   "start_station_name","start_station_id",
                                   "end_station_name","end_station_id",
                                   "start_lat","start_lng",
                                   "end_lat","end_lng",
                                   "member_casual","duration","route")
raw202301complete<-rbind(member202301allforcom,casual202301allforcom)
raw202301complete$duration<-as.numeric(raw202301complete$duration)

#plot the duration vs. member_casual
ggplot(raw202301complete, aes(x = duration, fill = member_casual)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("red", "blue")) +
  xlim(1,1000)

#step 8: duration vs. member_casual: summary data
str(casual202301duration)
summary(as.numeric(raw202301complete$casual202301duration))
summary(as.numeric(raw202301complete$member202301duration))







#other datasets to work on
raw202212<-read.csv("202212-divvy-tripdata.csv")
raw202211<-read.csv("202211-divvy-tripdata.csv")
raw202210<-read.csv("202210-divvy-tripdata.csv")
raw202209<-read.csv("202209-divvy-publictripdata.csv")
raw202208<-read.csv("202208-divvy-tripdata.csv")
raw202207<-read.csv("202207-divvy-tripdata.csv")
raw202206<-read.csv("202206-divvy-tripdata.csv")
raw202205<-read.csv("202205-divvy-tripdata.csv")
raw202204<-read.csv("202204-divvy-tripdata.csv")
raw202203<-read.csv("202203-divvy-tripdata.csv")
raw202202<-read.csv("202202-divvy-tripdata.csv")
raw202201<-read.csv("202201-divvy-tripdata.csv")
all13months<-rbind(raw202301,raw202201,raw202202,raw202203,raw202204,raw202205,
                   raw202206,raw202207,raw202208,raw202209,raw202210,raw202211,
                   raw202212)




