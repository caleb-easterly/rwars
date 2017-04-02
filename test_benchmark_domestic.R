library(randomForestSRC)

load("../test_df.rda")
load("../train_df.rda") 

train_temp_df <- train_df
test_temp_df <- test_df

mult_reg_tree <- rfsrc(Multivar(srch_destination_longitude, srch_destination_latitude) ~ 
                           user_location_longitude + user_location_latitude + srch_month + 
                           length_of_stay + srch_adults_cnt +
                           srch_children_cnt + srch_rm_cnt + is_booking + prop_is_branded + 
                           prop_starrating + popularity_band, 
                       nsplit = 4,
                       nodesize = 20,
                       seed = -29,
                       ntree = 100,
                       data = train_temp_df,
                       importance = "none",
                       na.action = "na.omit")

test = predict(mult_reg_tree, newdata = test_temp_df)
output = matrix(c(test$regrOutput$srch_destination_longitude$predicted, test$regrOutput$srch_destination_latitude$predicted), ncol = 2)

offsetDis = function(lon1, lat1, lon2, lat2){
  p = pi/180
  a = 0.5 - cos((lat2 - lat1) * p)/2 + 
    cos(lat1 * p) * cos(lat2 * p) * (1 - cos((lon2 - lon1) * p))/2
  return(12742 * asin(a^2))  #km
}

truth = matrix(c(test_temp_df$srch_destination_longitude, 
                       test_temp_df$srch_destination_latitude), ncol = 2)

offDis = rep(0, nrow(output))
for(i in 1:nrow(output)){
  offDis[i] = offsetDis(truth[i,][1], truth[i,][2], output[i,][1], output[i,][2])
}







