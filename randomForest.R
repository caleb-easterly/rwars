require(randomForestSRC)

load("small_dat.rda") # data frame of same name
mult_reg_tree <- rfsrc(Multivar(srch_destination_longitude, srch_destination_latitude) ~ 
                           user_location_longitude + user_location_latitude + 
                           length_of_stay + srch_adults_cnt + srch_children_cnt + srch_rm_cnt, 
                            data = small_dat)

