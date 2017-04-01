# adapted from http://stackoverflow.com/questions/21708488/get-country-and-continent-from-longitude-and-latitude-point-in-r

reverseGeoCode <- function(latlng) {
    require("XML")
    require("httr")
    latlng    <- as.numeric(latlng)
    latlngStr <- gsub(' ','%20', paste(round(latlng,2), collapse=","))
    url   <- "http://maps.google.com"
    path  <- "/maps/api/geocode/xml"
    query <- list(sensor="false",latlng=latlngStr)
    response <- GET(url, path=path, query=query)
    if (response$status !=200) {
        print(paste("HTTP Error:",response$status),quote=F)
        return(c(NA,NA))
    }
    xml    <- xmlInternalTreeParse(content(response,type="text"))
    status <- xmlValue(getNodeSet(xml,"//status")[[1]])
    if (status != "OK"){
        print(paste("Query Failed:",status),quote=F)
        return(c(NA,NA))
    }
    xPath   <- '//result[1]/address_component[type="country"]/long_name[1]'
    country <- xmlValue(getNodeSet(xml,xPath)[[1]])
    xPath   <- '//result[1]/address_component[type="administrative_area_level_1"]/long_name[1]'
    state   <- xmlValue(getNodeSet(xml,xPath)[[1]])
    return(state)
}

testdf <- matrix(c(36.06783, -94.17365, 36.7, -110), byrow = TRUE, nrow = 2)
sapply(1:2, function(i) reverseGeoCode(testdf[i, ]))

