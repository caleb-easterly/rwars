require(randomForestSRC)

load("joined_dest_dom_data.rda") # data frame of same name
mult_reg_tree <- rfsrc(Multivar(srch_destination_longitude, srch_destination_latitude) ~ 
                            user_location_longitude + user_location_latitude, 
                            data = joined_dest_dom_data)

