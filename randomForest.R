require(randomForestSRC)

load("small_dat.rda") # data frame of same name

small_train_ind <- sample(1:nrow(small_dat), 8000)
small_train <- small_dat[small_train_ind, ]
small_test <- small_dat[-small_train_ind, ]

mult_reg_tree <- rfsrc(Multivar(srch_destination_longitude, srch_destination_latitude) ~ 
                           user_location_longitude + user_location_latitude + srch_month + 
                           length_of_stay + srch_adults_cnt + srch_children_cnt + srch_rm_cnt + is_booking, 
                            data = small_dat)

minnesota_person <- subset(small_test, user_location_region == "MN")

predictions_mn <- predict(mult_reg_tree, newdata = minnesota_person)
output_values_m <- data.frame("long" = predictions_mn$regrOutput$srch_destination_longitude$predicted,
            "lat" = predictions_mn$regrOutput$srch_destination_latitude$predicted)

# use vimp
map <- get_map(location='united states', zoom = 4,
               color = "bw")

ggmap(map, extent = 'device') + 
    geom_point(data = output_values, aes(x = long, y = lat), size = 3) + 
    geom_point(aes(x = minnesota_person$user_location_longitude, y = minnesota_person$user_location_latitude, color = "prediction"), size = 3) + 
    geom_point(aes(x = minnesota_person$srch_destination_longitude, y = minnesota_person$srch_destination_latitude, color = "truth"), size = 3)
