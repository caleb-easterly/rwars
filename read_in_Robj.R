library(readr)
library(stringr)
library(parallel)
require(dplyr)

data_raw <- as.data.frame(read_tsv("../data.txt"))

# full data frame is 10,884,539 rows and 27 variables
# all of the website data is from 2015 
dest_raw <- as.data.frame(read_tsv("../dest.txt"))
dest_raw <- select(dest_raw, -starts_with("popular"))

# full data frame is 10,884,539 rows and 27 variables

data_raw$user_location_latitude <- as.numeric(data_raw$user_location_latitude)
data_raw$user_location_longitude <- as.numeric(data_raw$user_location_longitude)

#subset to domestic data only, remove all rows with missing user_longitude or latitude
dom_data <- subset(data_raw, 
                   hotel_country == "UNITED STATES OF AMERICA" & 
                     user_location_country == "UNITED STATES OF AMERICA" &
                       !is.na(user_location_latitude) & !is.na(user_location_longitude))


# drops the search IDs that don't have a corresponding entry in dest_raw, and vice versa
dom_data <- inner_join(dom_data, dest_raw, by = "srch_destination_id")
dom_data <- as.data.frame(mutate_at(dom_data, 
                                                vars(srch_destination_latitude, 
                                                     srch_destination_longitude), 
                                                funs(as.numeric(.))))

# convert month and day to day since Jan 1, 2015
days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
days_in_month_cumsum <- cumsum(days_in_month)

# string sub
get_month <- function(vect) {
  as.numeric(substr(vect, 6, 7))
}
get_day <- function(vect) {
  as.numeric(substr(vect, 9, 10))
}
get_year <- function(vect){
  as.numeric(substr(vect, 1, 4))
}

dom_data$srch_month <- get_month(dom_data$date_time)
dom_data$srch_day <- get_day(dom_data$date_time)
dom_data$srch_year <- get_year(dom_data$date_time)

# check-in, check-out
dom_data$ci_year <- get_year(dom_data$srch_ci)
dom_data$ci_day <- get_day(dom_data$srch_ci)
dom_data$ci_month <- get_month(dom_data$srch_ci)
dom_data$co_year <- get_year(dom_data$srch_co)
dom_data$co_day <- get_day(dom_data$srch_co)
dom_data$co_month <- get_month(dom_data$srch_co)

dom_data <- mutate(dom_data, 
                   srch_day_since_jan1_2015 = days_in_month_cumsum[srch_month] + 
                     srch_day + 
                     (srch_year > 2015) * 365 + (srch_month >= 2) * (srch_year > 2015) * 1,
                   ci_day_since_jan1_2015 = days_in_month_cumsum[ci_month] + 
                     ci_day + 
                     (ci_year > 2015) * 365 + (ci_month >= 2) * (ci_year > 2015) * 1,
                  co_day_since_jan1_2015 = days_in_month_cumsum[co_month] + 
                    co_day + 
                    (co_year > 2015) * 365 + (co_month >= 2) * (co_year > 2015) * 1)

dom_data <- mutate(dom_data,
                   length_of_stay = co_day_since_jan1_2015 - ci_day_since_jan1_2015)
dom_data <- subset(dom_data, length_of_stay > 0)

# make sure data types are what we need
dom_data$prop_starrating <- as.numeric(dom_data$prop_starrating)
dom_data$prop_is_branded <- as.factor(dom_data$prop_is_branded)
dom_data$popularity_band <- as.factor(dom_data$popularity_band)
dom_data$cnt <- as.numeric(dom_data$cnt)
dom_data$srch_adults_cnt <- as.numeric(dom_data$srch_adults_cnt)
dom_data$srch_children_cnt <- as.numeric(dom_data$srch_children_cnt)
dom_data$srch_rm_cnt <- as.numeric(dom_data$srch_children_cnt)

# from geocode("cape_flattery")[1, 1]
farthest_west <- -124.7144
filt_dom_data <- dplyr::filter(dom_data,    
                                srch_destination_longitude > farthest_west)

# remove unimportant variables
joined_varimp_dom_data <- dplyr::select(filt_dom_data,
                                      -date_time,
                                      -site_name,
                                      -user_location_country,
                                      -user_location_city,
                                      -is_mobile,
                                      -srch_ci,
                                      -srch_co,
                                      -hotel_country,
                                      -hotel_id,
                                      -distance_band,
                                      -srch_destination_name
                                      )

# select training and testing 

set.seed(1010)
nrows_clean <- nrow(joined_varimp_dom_data)
size_of_train <- round(8/10 * nrows_clean)
train_indices <- base::sample(1:nrows_clean, size = size_of_train, replace = FALSE)

train_df <- joined_varimp_dom_data[train_indices, ]
test_df <- joined_varimp_dom_data[-train_indices, ]
all_df <- joined_varimp_dom_data

save(train_df, file = "../train_df.rda")
save(test_df, file = "../test_df.rda")
save(all_df, file = "../all_df.rda")

small_dat <- sample_n(train_df, 10000)
save(small_dat, file = "small_dat.rda")
    
# <<<<<<< HEAD
# =======
# #col 13&14
# data_raw = subset(data_raw, !nchar(srch_ci)!=5)
# data_raw$length_of_stay <- parSapply(cl, 1:nrows_data_raw, 
#                                      function(i) conv_month_day(
#                                        as.numeric(str_sub(data_raw$srch_co[i], start = 6, end = 7)), 
#                                        as.numeric(str_sub(data_raw$srch_co[i], start = 9, end = 10)),
#                                        as.numeric(str_sub(data_raw$srch_co[i], start = 1, end = 4))) - 
#                                        conv_month_day(as.numeric(str_sub(data_raw$srch_ci[i], start = 6, end = 7)),
#                                                       as.numeric(str_sub(data_raw$srch_ci[i], start = 9, end = 10)),
#                                                       as.numeric(str_sub(data_raw$srch_ci[i], start = 1, end = 4))
#                                        ))
# data_raw = subset(data_raw, !length_of_stay < 0)
# stopCluster(cl)
# 
# data_raw<-subset(data_raw, !(nchar(srch_ci))!=5)
# 
#                                        
# dom_data <- subset(data_raw, !hotel_country != "UNITED STATES OF AMERICA")
# dom_data <- subset(dom_data, !user_location_country != "UNITED STATES OF AMERICA")
# >>>>>>> 126b3d38a8d2440c96bc3dfc6efdf25fe18dd1af

# save(joined_dest_dom_data, file = "joined_dest_dom_data.rda")

# save(data_raw, file = "data_10000_cases.rda")


### basement
# dom_data <- mutate(dom_data, 
#                    ci_day_since_jan1_2015 = days_in_month_cumsum[ci_month] + 
#                      ci_day + 
#                      (ci_year > 2015) * 365 + (ci_month >= 2) * (ci_year > 2015) * 1)

# dom_data$length_of_stay <- conv_month_day(
#                               get_month(srch_co),
#                               get_day(srch_co),
#                               get_year(srch_co)
#                             ) - conv_month_day(
#                               get_month(srch_ci),
#                               get_day(srch_ci),
#                               get_year(srch_ci)
#                             )
# 
# <<<<<<< HEAD
#col 13&14
# data_raw = subset(data_raw, !nchar(srch_ci)!=5)
# data_raw$length_of_stay <- parSapply(cl, 1:nrows_data_raw, 
#                                      function(i) conv_month_day(
#                                        as.numeric(str_sub(data_raw$srch_co[i], 
#                                                           start = 6, end = 7)), 
#                                        as.numeric(str_sub(data_raw$srch_co[i], 
#                                                           start = 9, end = 10)),
#                                        as.numeric(str_sub(data_raw$srch_co[i], 
#                                                           start = 1, end = 4))) - 
#                                        conv_month_day(
#                                            as.numeric(str_sub(data_raw$srch_ci[i], 
#                                                               start = 6, end = 7)),
#                                            as.numeric(str_sub(data_raw$srch_ci[i], 
#                                                               start = 9, end = 10)),
#                                            as.numeric(str_sub(data_raw$srch_ci[i], 
#                                                               start = 1, end = 4))
#                                        ))

# stopCluster(cl)

# 
# dom_data <- subset(data_raw, 
#                    hotel_country == "UNITED STATES OF AMERICA" & 
#                        user_location_country == "UNITED STATES OF AMERICA")
# 
# require(dplyr)
# 
# # drops the search IDs that don't have a corresponding entry in dest_raw, and vice versa
# joined_dest_dom_data <- inner_join(dom_data, dest_raw, by = "srch_destination_id")
# joined_dest_dom_data <- as.data.frame(mutate_at(joined_dest_dom_data, 
#                                                 vars(srch_destination_latitude, 
#                                                      srch_destination_longitude), 
#                                                 funs(as.numeric(.))))

# restrict to contiguous united states by longitude
# require(ggmap)

# conv_month_day <- function(month, day, year, days_in_month){
#   # if (is.null(month) | is.null(day) | is.null(year)) return(-1) 
#   if (year == 2016) {
#     out = sum(days_in_month[1:(month - 1)]) + day + 365
#   } else {
#     out = sum(days_in_month[1:(month - 1)]) + day
#   }
#   return(out)
# }

# cl <- makeCluster(3, type = "FORK")

# dom_data$srch_day_from_jan1_2015 <- conv_month_day(get_month(data_raw$date_time),
#                                            get_day(data_raw$date_time),
#                                            get_year(data_raw$date_time), days_in_month)
#     

#     parSapply(cl, 1:nrows_data_raw, 
#                                       function(i) conv_month_day(
#                                         as.numeric(str_sub(data_raw$date_time[i], 
#                                                            start = 6, end = 7)), 
#                                         as.numeric(str_sub(data_raw$date_time[i], 
#                                                            start = 9, end = 10)),
#                                         as.numeric(str_sub(data_raw$date_time[i], 
#                                                            start = 1, end = 4))
#                                       )
# )
# data_raw$srch_month <- parSapply(cl, 1:nrows_data_raw, 
#                                  function(i)
#                                    as.numeric(str_sub(data_raw$date_time[i], 
#                                                       start = 6, end = 7
#                                    )))

# dom_data <- mutate(dom_data, 
#                    srch_day_since_jan1_2015 = days_in_month_cumsum[srch_month] + 
#                      srch_day + 
#                      (srch_year > 2015) * 365 + (srch_month >= 2) * (srch_year > 2015) * 1)
# =======
# save(data_raw, file = "data_10000_cases.rda")
# >>>>>>> 126b3d38a8d2440c96bc3dfc6efdf25fe18dd1af
