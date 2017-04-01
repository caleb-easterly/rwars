require(randomForestSRC)

load("small_dat.rda") # data frame of same name

small_train_ind <- sample(1:nrow(small_dat), 8000)
small_train <- small_dat[small_train_ind, ]
small_test <- small_dat[-small_train_ind, ]

mult_reg_tree <- rfsrc(Multivar(srch_destination_longitude, srch_destination_latitude) ~ 
                           user_location_longitude + user_location_latitude + srch_month + 
                           length_of_stay + srch_adults_cnt + srch_children_cnt + srch_rm_cnt + is_booking, 
                            data = small_dat)

minnesota_person <- subset(small_test, user_location_region == "MN" &
                               srch_month == "2" & srch_adults_cnt ==2 )

predictions <- predict(mult_reg_tree, newdata = minnesota_person)
output_values <- c(predictions$regrOutput$srch_destination_longitude$predicted,
            predictions$regrOutput$srch_destination_latitude$predicted)
