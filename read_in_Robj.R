library(readr)
library(stringr)
library(parallel)

#data_raw <- as.data.frame(read_tsv("../data.txt", n_max = 10000))
# full data frame is 10,884,539 rows and 27 variables
# all of the website data is from 2015 
dest_raw <- as.data.frame(read_tsv("../dest.txt", n_max = 10000))
# full data frame is 10,884,539 rows and 27 variables

data_raw$user_location_latitude <- as.numeric(data_raw$user_location_latitude)
data_raw$user_location_longitude <- as.numeric(data_raw$user_location_longitude)

# convert month and day to day of 2015
conv_month_day <- function(month, day, year){
  days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  if(year == 2016){
    out = sum(days_in_month[1:(month - 1)]) + day + 365
  }else{
    out = sum(days_in_month[1:(month - 1)]) + day
  }
  return(out)
}


nrows_data_raw <- nrow(data_raw)

cl <- makeCluster(3, type = "FORK")

data_raw$srch_day_of2015 <- parSapply(cl, 1:nrows_data_raw, 
                                      function(i) conv_month_day(
                                        as.numeric(str_sub(data_raw$date_time[i], start = 6, end = 7)), 
                                        as.numeric(str_sub(data_raw$date_time[i], start = 9, end = 10)),
                                        as.numeric(str_sub(data_raw$date_time[i], start = 1, end = 4))
                                      )
)
data_raw$srch_month <- parSapply(cl, 1:nrows_data_raw, 
                                 function(i)
                                   as.numeric(str_sub(data_raw$date_time[i], start = 6, end = 7
                                   )))

#col 13&14
data_raw = subset(data_raw, !nchar(srch_ci)!=5)
data_raw$length_of_stay <- parSapply(cl, 1:nrows_data_raw, 
                                     function(i) conv_month_day(
                                       as.numeric(str_sub(data_raw$srch_co[i], start = 6, end = 7)), 
                                       as.numeric(str_sub(data_raw$srch_co[i], start = 9, end = 10)),
                                       as.numeric(str_sub(data_raw$srch_co[i], start = 1, end = 4))) - 
                                       conv_month_day(as.numeric(str_sub(data_raw$srch_ci[i], start = 6, end = 7)),
                                                      as.numeric(str_sub(data_raw$srch_ci[i], start = 9, end = 10)),
                                                      as.numeric(str_sub(data_raw$srch_ci[i], start = 1, end = 4))
                                       ))
data_raw = subset(data_raw, !length_of_stay < 0)
stopCluster(cl)


dom_data <- subset(data_raw, !hotel_country != "UNITED STATES OF AMERICA")
dom_data <- subset(dom_data, !user_location_country != "UNITED STATES OF AMERICA")

require(dplyr)

# drops the search IDs that don't have a corresponding entry in dest_raw, and vice versa
joined_dest_dom_data <- inner_join(dom_data, dest_raw, by = "srch_destination_id")
joined_dest_dom_data <- as.data.frame(mutate_at(joined_dest_dom_data, vars(srch_destination_latitude, srch_destination_longitude), funs(as.numeric(.))))

save(joined_dest_dom_data, file = "joined_dest_dom_data.rda")


# ggplot(subset(data_raw, user_location_country == "UNITED STATES OF AMERICA")) +
#     geom_point(aes(x = user_location_longitude, y = user_location_latitude))

save(data_raw, file = "data_10000_cases.rda")