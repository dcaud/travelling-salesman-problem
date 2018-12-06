# Author: Marinus Louw
# Original Author: Andrew B. Collier <andrew@exegetic.biz> | @datawookie
# Link to original article: https://datawookie.netlify.com/blog/2018/05/travelling-salesman-with-ggmap/
# Citations: D. Kahle and H. Wickham. ggmap: Spatial Visualization with ggplot2. The R Journal, 5(1), 144-161.
# URL http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf

# DATA SETUP ========================================================================================================

# Google Maps API key
KEY = "AIzaSy...."

addresses = c(
  "115 St Andrews Dr, Durban North, 4051, South Africa",
  "67 Boshoff St, Pietermaritzburg, 3201, South Africa",
  "4 Paul Ave, Fairview, Empangeni, 3880, South Africa",
  "166 Kerk St, Vryheid, 3100, South Africa",
  "9 Margaret St, Ixopo, 3276, South Africa",
  "16 Poort road, Ladysmith, 3370, South Africa"
)

# LIBRARIES =========================================================================================================

library(gmapsdistance)
library(dplyr)
library(ggmap)
library(TSP)
library(ggplot2)

# GEOCODING =======================================================================================================

# Assign the Google Maps API key to the gmapsdistance and ggmap packages, respectively
set.api.key(KEY)
register_google(key = KEY)

# Create a dataframe of addresses for use in the geocode command
df <- data.frame(addresses,
                 stringsAsFactors = FALSE)

# Use the dplyr package's pipe operator to add the latitude and longitude of the addresses to your dataframe
addresses <- tbl_df(df) %>% mutate_geocode(addresses)

# DATAFRAME =========================================================================================================

# Add latlon and latlonggmap columns for use in the gmapsdistance and ggmap packages, respectively, later
addresses <- addresses %>%
  mutate(latlon = sprintf("%s+%s", lat, lon)) %>%
  mutate(latlonggmap = sprintf("%s,%s", lat, lon)) %>%
  mutate(label = LETTERS[1:nrow(.)]) %>%
  select(label, everything())

# TIMES =========================================================================================================

# Calculate the travel times between addresses by using the gmapdistance command from the gmapdistance package.
# For gmapdistance to access the Google Maps API, register a API key, from Google's free trail at https://developers.google.com/maps/documentation/javascript/get-api-key
#
times <- gmapsdistance(origin = addresses$latlon,
                       destination = addresses$latlon,
                       combinations = "all",
                       mode = "driving")$Time[, -1]

# Convert the time (in seconds) to minutes.
#
times <- as.matrix(times) / 60
# Rename the columns and rows
#
colnames(times) <- addresses$label
rownames(times) <- addresses$label

# Convert the matrix to a distance matrix.
#
times <- as.dist(times)

# TRAVELLING SALESMAN ===============================================================================================

# Use TSP to solve the travelling salesman problem (TSP)
#
tsp <- TSP(times)
#
methods <- c(
  "nearest_insertion",
  "farthest_insertion",
  "cheapest_insertion",
  "arbitrary_insertion",
  "nn",
  "repetitive_nn",
  "two_opt"
)
#
tours <- methods %>% map(function(method) {
  solve_TSP(tsp, method)
})
#
tour <- solve_TSP(tsp)
#
# Set the order of locations for the tour.
#
tour_order <- as.integer(tour)
#
# Sort the addresses.
#
addresses <- addresses[tour_order,]

# BUILD ROUTE =======================================================================================================

# Use ggmap's route command to build the route
route <- lapply(seq(nrow(addresses) - 1), function(n) {
  route(addresses$latlonggmap[n], addresses$latlonggmap[n+1], structure = "route") %>%
    mutate(section = n)
})

route <- route %>% bind_rows()

# MAP ROUTE =========================================================================================================

# Use the ggmap package to create a map of the optimal route
map <- get_map(location = c(lon = mean(route$lon), lat = mean(route$lat)), zoom = 7, maptype = "roadmap")

addresses[[3]] <- as.numeric(addresses[[3]])
addresses[[4]] <- as.numeric(addresses[[4]])

ggmap(map) +
  geom_path(data = route, 
            aes(x = lon, y = lat),  
            colour = "blue", 
            size = 1, 
            alpha = 0.5) +
  geom_point(data = addresses, 
             aes(x = lon, y = lat), 
             size = 3, 
             alpha = 0.75) +
  labs(x = "Longitude", y = "Latitude")
