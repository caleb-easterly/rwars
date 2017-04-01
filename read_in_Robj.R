require(readr)
require(stringr)

data_raw <- read_tsv("../data.txt", n_max = 10000)
data_raw$user_location_latitude <- as.numeric(data_raw$user_location_latitude)
data_raw$user_location_longitude <- as.numeric(data_raw$user_location_longitude)

nrows_data_raw <- nrow(data_raw)

# convert month and day to day of 2015
conv_month_day <- function(month, day){
    days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    sum(days_in_month[1:(month - 1)]) + day
}

require(parallel)
cl <- makeCluster(3, type = "FORK")
data_raw$day_of2015 <- parSapply(cl, 1:nrows_data_raw, 
                                 function(i) conv_month_day(
                                     as.numeric(str_sub(data_raw$date_time[i], start = 6, end = 7)), 
                                     as.numeric(str_sub(data_raw$date_time[i], start = 9, end = 10))
                                 )
)
stopCluster(cl)

# full data frame is 10,884,539 rows and 27 variables
# all of the website data is from 2015 
dest_raw <- read_tsv("../dest.txt", n_max = 10000)



ggplot(subset(data_raw, user_location_country == "UNITED STATES OF AMERICA")) +
    geom_point(aes(x = user_location_longitude, y = user_location_latitude))

save(data_raw, file = "data_10000_cases.rda")
