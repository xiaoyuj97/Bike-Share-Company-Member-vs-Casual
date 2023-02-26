setwd("/Users/xiaoyujin/Desktop/GDA/Case Study 1 Data")
getwd()
library(tidyverse)

#split into member and casual
raw202301<-read.csv("202301-divvy-tripdata.csv")


# function1 to auto split into member and casual
clean_data <- function(filename) {
  raw_data <- read.csv(filename)
  raw_data_split <- split(raw_data, raw_data$member_casual)
  member_data <- raw_data_split$member
  casual_data <- raw_data_split$casual
  member_data_complete <- member_data[complete.cases(member_data), ]
  casual_data_complete <- casual_data[complete.cases(casual_data), ]
  return(list(member_data_complete = member_data_complete, 
              casual_data_complete = casual_data_complete))
}

#use the function
#clean_data("202301-divvy-tripdata.csv")
#member_data_complete <- data$member_data_complete
#casual_data_complete <- data$casual_data_complete

#function2 to produce duration and routes
add_duration_and_route_columns <- function(data) {
  
  # Add duration column
  duration <- ymd_hms(data$ended_at) - ymd_hms(data$started_at)
  data <- cbind(data, duration)
  
  # Add route column
  route <- paste(data$start_station_name, data$end_station_name, sep = " ")
  data <- cbind(data, route)
  
  return(data)
}

#use the function
#casual202301complete <- add_duration_and_route_columns(casual202301complete)
#member202301complete <- add_duration_and_route_columns(member202301complete)

#function3: top stations
create_top_station_table <- function(data, station_type, num_top_stations) {
table_data <- table(data[[paste0(station_type, "_station_name")]])
table_df <- data.frame(table_data)
sorted_df <- table_df %>%
  arrange(desc(Freq))
top_sorted_df <- head(sorted_df, num_top_stations)
return(top_sorted_df)
}

#use the function
#casual202301start_sorted100 <- create_top_station_table(casual202301complete, "start", 100)
#casual202301end_sorted100 <- create_top_station_table(casual202301complete, "end", 100)
#member202301start_sorted100 <- create_top_station_table(member202301complete, "start", 100)
#member202301end_sorted100 <- create_top_station_table(member202301complete, "end", 100)


#function4: most popular route
popular_route_freq <- function(route) {
  # combine start and end station names into a single string
  route_string <- paste(route$start_station_name, route$end_station_name, sep = " ")
  
  # create frequency table and remove first row if left-most cell is empty
  freq_table <- table(route_string)
  if (freq_table[1] == "") {
    freq_table <- freq_table[-1]
  }
  
  # create a data frame from frequency table and sort by frequency in descending order
  freq_df <- data.frame(table = names(freq_table), freq = as.numeric(freq_table))
  freq_df_sorted <- freq_df %>% arrange(desc(freq))
  
  # return the top 10 most popular routes
  return(head(freq_df_sorted, 10))
}


# use the functions
#casual_popular_routes <- popular_route_freq(casual202301complete)
#member_popular_routes <- popular_route_freq(member202301complete)


#function6: combine data and change names
combine_data <- function(casual_data, member_data) {
  colnames(casual_data) <- c("ride_id","rideable_type","started_at","ended_at",
                             "start_station_name","start_station_id",
                             "end_station_name","end_station_id",
                             "start_lat","start_lng",
                             "end_lat","end_lng",
                             "member_casual","duration","route")
  
  colnames(member_data) <- c("ride_id","rideable_type","started_at","ended_at",
                   "start_station_name","start_station_id",
                   "end_station_name","end_station_id",
                   "start_lat","start_lng",
                   "end_lat","end_lng",
                   "member_casual","duration","route")
  combined_data <- rbind(member_data, casual_data)
  combined_data$duration <- as.numeric(combined_data$duration)
  return(combined_data)
}

# Usage:
#raw202301complete <- combine_data(casual202301all, member202301all)







