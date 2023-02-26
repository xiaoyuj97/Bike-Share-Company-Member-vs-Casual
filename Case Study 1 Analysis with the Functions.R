setwd("/Users/xiaoyujin/Desktop/GDA/Case Study 1 Data")
getwd()
library(tidyverse)
library(lubridate)

source("Case Study 1 Functions.R")

#funciton1: split member and casual
data<-clean_data("202201-divvy-tripdata.csv")
member_data_complete <- data$member_data_complete
casual_data_complete <- data$casual_data_complete
casual_data_complete

#funciton2: add duration and route
casual202201complete <- add_duration_and_route_columns(casual_data_complete)
member202201complete <- add_duration_and_route_columns(member_data_complete)

#function3: most popular start and end station for member and casual
casual202201start_sorted100 <- create_top_station_table(casual202201complete, "start", 100)
casual202201end_sorted100 <- create_top_station_table(casual202201complete, "end", 100)
member202201start_sorted100 <- create_top_station_table(member202201complete, "start", 100)
member202201end_sorted100 <- create_top_station_table(member202201complete, "end", 100)

#most popular stations cbind
mostpopular100stations<-cbind(casual202201start_sorted100,
                              casual202201end_sorted100,
                              member202201start_sorted100,
                              member202201end_sorted100)

#function4: most popular route
casual_popular_routes <- popular_route_freq(casual202201complete)
member_popular_routes <- popular_route_freq(member202201complete)

#part 5: rideable_type difference
table(casual202301all$rideable_type)
table(member202301all$rideable_type)

#function6: combine data and change names
raw202301complete <- combine_data(casual202201complete, member202201complete)
raw202301complete

#part 7: plot duration vs. member/casual
ggplot(raw202301complete, aes(x = duration, fill = member_casual)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("red", "blue")) +
  xlim(1,1500)

#step 8: duration vs. member_casual: summary data
str(casual202301duration)
summary(as.numeric(casual202201complete$duration))
summary(as.numeric(member202201complete$duration))

#t test to tell if significant
numcausaldur<-as.numeric(casual202201complete$duration)
nummemberdur<-as.numeric(member202201complete$duration)
t.test(numcausaldur,nummemberdur, alternative = c("two.sided"), 
       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
#significant in this case-202201






