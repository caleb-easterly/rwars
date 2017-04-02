# map of us

require(ggmap)
require(ggplot2)
require(dplyr)

load("joined_dest_dom_data.rda")

clean_jdd <- select(joined_dest_dom_data, -starts_with("popular"))


# melt data so that 
require(reshape2)
clean_jdd <- subset(clean_jdd, user_location_region == "MN" & complete.cases(clean_jdd))
lonlat <- select(clean_jdd, user_location_longitude, user_location_latitude, srch_destination_longitude, srch_destination_latitude, user_id, is_booking)

lonlat2 <- data.frame(
    "lat" = as.numeric(c(lonlat$user_location_latitude,lonlat$srch_destination_latitude)),
    "lon" = as.numeric(c(lonlat$user_location_longitude,
                         lonlat$srch_destination_longitude)),
    "loctype" = rep(c("usr", "search"), each = nrow(lonlat)),
    "user_id" = as.factor(rep(lonlat$user_id, 2)),
    "lookbook" = as.factor(rep(lonlat$is_booking, 2))
)



# qmplot(user_location_longitude, user_location_latitude, data = clean_jdd)

map <- get_map(location=c(-130, 23, -62, 51), zoom = 4, maptype = "watercolor",
               source = "stamen",
               color = "bw")

ggmap(map, extent = 'device') + 
    geom_path(aes(x = lon, y = lat, group = user_id, color = lookbook),
              data = lonlat2, size = 0.8) + 
    geom_point(aes(x = lon, y = lat, color = loctype),
               data = lonlat2, size = 2, shape = 1) +
    theme(legend.position = "none") + 
    scale_color_manual(values = c("dodgerblue", "darkorange", "purple", "black"))
ggsave("travel_from_mn.png", scale = 2)
