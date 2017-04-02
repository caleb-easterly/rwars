library(randomForestSRC)
library(dplyr)


load("../bookers_df.rda")
nbookers <- nrow(bookers_df)

set.seed(1010)
n_small_bookers <- 500000
n_train <- round(8/10 * n_small_bookers)

small_bookers <- sample_n(bookers_df, n_small_bookers)
train_ids <- sample(1:n_small_bookers, n_train)
train_temp_df <- small_bookers[train_ids, ]
test_temp_df <- small_bookers[-train_ids, ]

mult_reg_tree <- rfsrc(Multivar(srch_destination_longitude, srch_destination_latitude) ~ 
                         user_location_longitude + user_location_latitude + srch_month + 
                         length_of_stay + srch_adults_cnt +
                         srch_children_cnt + srch_rm_cnt + is_booking + ci_month,
                       seed = -29,
                       ntree = 100,
                       data = train_temp_df)

# save(mult_reg_tree, file = "mult_reg_tree.rda")

test = predict(mult_reg_tree, newdata = test_temp_df)
output = matrix(c(test$regrOutput$srch_destination_longitude$predicted,
                  test$regrOutput$srch_destination_latitude$predicted), ncol = 2)



ppl <- cbind(test_temp_df, "pred_long" = output[, 1], "pred_lat" = output[, 2])
ppl2 <- subset(ppl, srch_adults_cnt == 2 & srch_children_cnt == 0 & 
                 ci_month == 3 & length_of_stay == 2 &
                 user_location_region == "MN")  
ppl3 <- select(ppl2, 
               user_location_latitude, 
               user_location_longitude, 
               srch_destination_latitude,
               srch_destination_longitude,
               pred_long,
               pred_lat)

ppl_mn_1rm_2adults <- ppl3
save(ppl_mn_1rm_2adults, file = "ppl_mn_1rm_2adults.rda")


deg2rad <- function(deg){
  deg * pi/180
}

#uses http://stackoverflow.com/questions/27928/calculate-distance-between-two-latitude-longitude-points-haversine-formula
offsetDis = function(lon1, lat1, lon2, lat2){
  dLat <- deg2rad(lat2 - lat1)
  dLon <- deg2rad(lon2 - lon1)
  R <- 6371
  p = pi/180
  a <- sin(dLat/2) * sin(dLat/2) + 
    cos(deg2rad(lat1)) * cos(deg2rad(lat2)) *
    sin(dLon/2) * sin(dLon/2)
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  d <- R*c
  return(d)
}



truth = matrix(c(test_temp_df$srch_destination_longitude, 
                 test_temp_df$srch_destination_latitude), ncol = 2)

offDis = rep(0, nrow(output))
for(i in 1:nrow(output)){
  offDis[i] = offsetDis(truth[i,][1], truth[i,][2], output[i,][1], output[i,][2])
}

# var_imp <- vimp(mult_reg_tree)







