library(stringr)
library(plyr)
library(sf)
library(dplyr)
library(circular)

# Loading 911 Call Dataset
data <- read.csv("data.csv")
names(data)[1] <- "lat"

# Loading Hospital Location Data 
hosp <- read.csv("hospital.csv")

# Distance calculator function
hs_dist <- function(lat1, lon1, lat2, lon2)
{
        # The math module contains a function named 
        # rad which converts from degrees to radians. 
        lon1 = rad(lon1) 
        lon2 = rad(lon2) 
        lat1 = rad(lat1) 
        lat2 = rad(lat2) 
        
        # Haversine formula  
        dlon = lon2 - lon1  
        dlat = lat2 - lat1 
        a = sin(dlat / 2)**2 + cos(lat1) * cos(lat2) * sin(dlon / 2)**2
        
        c = 2 * asin(sqrt(a))  
        
        # Radius of earth in kilometers. 
        r = 6371
        
        # calculate the result 
        return(c * r) 
}

# Allocation Function

allot <- function(lat, lon)
{
        min_dist = hs_dist(lat, lon, hosp[[4]][1], hosp[[5]][1])
        min_index = 1
        for(i in 2:nrow(hosp))
        {
                dist = hs_dist(lat, lon, hosp[[4]][i], hosp[[5]][i])
                if(dist < min_dist)
                {
                        min_dist <- dist
                        min_index <- i
                }
        }
        return(hosp[min_index,])
}

# Allocating Hospital to each emergency

for(i in 1:length(data$lat))
{
        alloted_hospital <- allot(data$lat[i],data$lon[i])
        data$hospital_no[i] <- alloted_hospital[[1]]
        data$hospital_name[i] = alloted_hospital[[2]]
        data$hospital_lat[i] = alloted_hospital[[4]]
        data$hospital_lon[i] = alloted_hospital[[5]]
}

write.csv(data, "final_23.csv")