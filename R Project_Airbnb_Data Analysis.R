#----------------------------------------------------------------
#  # R project_2 MSIS 2506 Fall 2019
#  # Group Members - Apurva Shekhar, Karishma Vsrodia, Ritu Ranjani Ravi Shankar, Sanjana Ramakandath, Vidhi Gandhi

#  # Project - Analysis of Airbnb - 2 years ago data
#  # Data sourced from - Kaggle.com
#  # 
# Download the train.csv file from the link -
# https://www.kaggle.com/stevezhenghp/airbnb-price-prediction
#
#----------------------------------------------------------------

## - Loading data from CSV file into R


#getwd()  -- to check for the current working directory
#setwd()  -- set the working directory to where the file was downloaded

#Libraries needed

# Loading data into r
if (!require(ggmap)) install.packages("ggmap")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(stringr)) install.packages("stringr")

library(VIM)
library(stringr)
library(lubridate)
library(ggplot2)
library(data.table)
library(ggvis)
library(ggpubr)
library(readr)
library(ggmap)
library(zipcode)
library(gridExtra)
library(plyr)
install.packages("dplyr",dependencies = TRUE)
require(dplyr)
library(dplyr)
devtools::install_github("dkahle/ggmap")     # -- Installed from link -:https://github.com/dkahle/ggmap

# you will have to generate an API key in Google (generate an API key. Didn't post our API as the API was public)

##Reading data from csv

# - replace the "C:/Users/ritur/Desktop/R Books and materials/train.csv" with local directory.

airbnb_maindata <- read.csv("C:/Users/ritur/Desktop/R Books and materials/train.csv", header=TRUE, sep=",")

# check if the data has loaded properly

head(airbnb_maindata, n=20)

# increasing the print option

options(max.print=999999)
-----------------------------------------------------------------------------------------------------------------
  ## - Data Preprocessing
  
  #Find out the number of missing values in the entire dataset and plot to get a better understanding
  
  missingvalue_plot <- aggr(airbnb_maindata, col=c('navyblue','yellow'),
                            numbers=TRUE, sortVars=TRUE,
                            labels=names(airbnb_maindata), cex.axis=.5,
                            gap=3, ylab=c("Missing data","Pattern"))

#imputing mean for numaric variables

for(i in 1:ncol(airbnb_maindata)){
  airbnb_maindata[is.na(airbnb_maindata[,i]), i] <- mean(airbnb_maindata[,i], na.rm = TRUE)
}

# Mode imputing for categorical variables - from the graph we see that the categorical variables don't have
# missing values in them, hence mode imputing is not needed.

# could also check the missing values for categorical variables as below:

#Check for NA in property_type (Categorical variable) - should return 74111 as there are no NAs.

impute_property_type <- airbnb_maindata[,'property_type']
NA_imput_property_type = 0
for(i in 1:length(impute_property_type)){
  if(is.na(impute_property_type[i]) == TRUE){
    print('NA')
  }
  else{
    NA_imput_property_type = NA_imput_property_type + 1
  }
}
print(NA_imput_property_type)

# repeat this for all other categorical variables to see that there are no NAs or NULL in these columns.
-----------------------------------------------------------------------------------------------------------------
  ## 1) Factors affecting the price of Airbnb
  -----------------------------------------------------------------------------------------------------------------
  ## Preparing inputs for linear regression of Price ~ Number of Amenities, accomodates, room-type, city,etc .....
  
  # Counting the number of amenities in each row from dataset and creating a new column in the datset
  
  airbnb_maindata$num_aminities = str_count(airbnb_maindata$amenities,',') + 1

# counting the length of name and description

airbnb_maindata$num_description = str_count(airbnb_maindata$description,' ') + 1

airbnb_maindata$num_name = str_count(airbnb_maindata$name,' ') + 1

# check if the columns have been created

View(airbnb_maindata)

# create a subset from the main data to contain inputs for regression

airbnb_price_prediction <- data.frame(airbnb_maindata$log_price,airbnb_maindata$room_type,airbnb_maindata$num_aminities,airbnb_maindata$accommodates,airbnb_maindata$bathrooms,
                                      airbnb_maindata$bed_type,airbnb_maindata$cancellation_policy,airbnb_maindata$cleaning_fee,airbnb_maindata$city,airbnb_maindata$num_description,airbnb_maindata$host_identity_verified,
                                      airbnb_maindata$instant_bookable,airbnb_maindata$num_name,airbnb_maindata$number_of_reviews,airbnb_maindata$review_scores_rating,
                                      airbnb_maindata$bedrooms,airbnb_maindata$beds)

# check if the dataframe is created

View(airbnb_price_prediction)

colnames(airbnb_price_prediction)

# Run regression using the airbnb_price_prediction dataframe

regression_model = lm(airbnb_price_prediction$airbnb_maindata.log_price ~ ., data = airbnb_price_prediction)

summary(regression_model)

# 9x9 matrix plot to see how each variable affects other variables in the dataset

#subset data for matrix plot

matrix_plot <- data.frame(airbnb_maindata$log_price,airbnb_maindata$num_aminities,airbnb_maindata$accommodates,airbnb_maindata$bathrooms,
                          airbnb_maindata$num_description,airbnb_maindata$num_name,airbnb_maindata$number_of_reviews,
                          airbnb_maindata$bedrooms,airbnb_maindata$beds)

# plot using pairs()
-----------------------------------------------------------------------------------------------------------------
  # since the data is huge this plot takes time to load -- refer the document for the output of the plot
  
  #  pairs(matrix_plot[1:9],panel=panel.smooth)
  -----------------------------------------------------------------------------------------------------------------
  # 2) See if price and demand are correlated
  -----------------------------------------------------------------------------------------------------------------
  # splitting year from first review and last review column
  
  first_review_year <- year(as.Date(airbnb_maindata$first_review, format = "%m/%d/%Y"))
last_review_year <- year(as.Date(airbnb_maindata$last_review, format = "%m/%d/%Y"))

# create subset of data to see price and demand relationship
# omit rows that has no first review or lst review date

airbnb_demand <- na.omit(data.frame(exp(airbnb_maindata$log_price),airbnb_maindata$number_of_reviews,first_review_year,last_review_year,airbnb_maindata$city))

View(airbnb_demand)
#Create subsets for individual cities - NYC, SF, LA, Chicago, Boston, DC

demand_NYC <- subset(airbnb_demand, airbnb_demand$airbnb_maindata.city == 'NYC' & airbnb_demand$first_review_year == 2017 & airbnb_demand$last_review_year == 2017)
View(demand_NYC)

demand_LA <- subset(airbnb_demand, airbnb_demand$airbnb_maindata.city == 'LA' & airbnb_demand$first_review_year == 2017 & airbnb_demand$last_review_year == 2017)
View(demand_LA)
demand_SF <- subset(airbnb_demand, airbnb_demand$airbnb_maindata.city == 'SF' & airbnb_demand$first_review_year == 2017 & airbnb_demand$last_review_year == 2017)
View(demand_SF)
demand_Chicago <- subset(airbnb_demand, airbnb_demand$airbnb_maindata.city == 'Chicago' & airbnb_demand$first_review_year == 2017 & airbnb_demand$last_review_year == 2017)
View(demand_Chicago)

demand_Boston <- subset(airbnb_demand, airbnb_demand$airbnb_maindata.city == 'Boston' & airbnb_demand$first_review_year == 2017 & airbnb_demand$last_review_year == 2017)
View(demand_Boston)

demand_DC <- subset(airbnb_demand, airbnb_demand$airbnb_maindata.city == 'DC' & airbnb_demand$first_review_year == 2017 & airbnb_demand$last_review_year == 2017)
View(demand_DC)

# plot price vs number of reviews (assumed as number of bookings) to see the demand relationship
-----------------------------------------------------------------------------------------------------------------
  # changing columns of the demand datsets
  colnames(demand_NYC)

setnames(demand_NYC, old=c("exp.airbnb_maindata.log_price.","airbnb_maindata.number_of_reviews","first_review_year","last_review_year","airbnb_maindata.city"), new=c("Price","Number_of_reviews","First_Review","Last_Review","City"))
head(demand_NYC, n=3)

setnames(demand_LA, old=c("exp.airbnb_maindata.log_price.","airbnb_maindata.number_of_reviews","first_review_year","last_review_year","airbnb_maindata.city"), new=c("Price","Number_of_reviews","First_Review","Last_Review","City"))
head(demand_LA, n=3)

setnames(demand_SF, old=c("exp.airbnb_maindata.log_price.","airbnb_maindata.number_of_reviews","first_review_year","last_review_year","airbnb_maindata.city"), new=c("Price","Number_of_reviews","First_Review","Last_Review","City"))
head(demand_SF, n=3)

setnames(demand_Chicago, old=c("exp.airbnb_maindata.log_price.","airbnb_maindata.number_of_reviews","first_review_year","last_review_year","airbnb_maindata.city"), new=c("Price","Number_of_reviews","First_Review","Last_Review","City"))
head(demand_Chicago, n=3)

setnames(demand_Boston, old=c("exp.airbnb_maindata.log_price.","airbnb_maindata.number_of_reviews","first_review_year","last_review_year","airbnb_maindata.city"), new=c("Price","Number_of_reviews","First_Review","Last_Review","City"))
head(demand_Boston, n=3)

setnames(demand_DC, old=c("exp.airbnb_maindata.log_price.","airbnb_maindata.number_of_reviews","first_review_year","last_review_year","airbnb_maindata.city"), new=c("Price","Number_of_reviews","First_Review","Last_Review","City"))
head(demand_DC, n=3)

-----------------------------------------------------------------------------------------------------------------
  # price vs demand - Boston
  
  p <- ggplot(demand_Boston, aes(x = Number_of_reviews , y = Price)) + geom_point()    # if both X and Y axes are fixed for all layers.
print(p + geom_smooth(method=lm) + ggtitle("Price Vs Demand - Boston") +labs(y= "Price", x = "No.of Reviews"))


#price vs demand - LA

p <- ggplot(demand_LA, aes(x = Number_of_reviews , y = Price)) + geom_point()    # if both X and Y axes are fixed for all layers.
print(p + geom_smooth(method=lm) + ggtitle("Price Vs Demand - LA") +labs(y= "Price", x = "No.of Reviews"))
#p + xlim(5, 20) + ylim(0, 50)

#price vs Demand - NYC

p <- ggplot(demand_NYC, aes(x = Number_of_reviews , y = Price)) + geom_point()    # if both X and Y axes are fixed for all layers.
print(p + geom_smooth(method=lm) + ggtitle("Price Vs Demand - NYC") +labs(y= "Price", x = "No.of Reviews"))


#price vs Demand - SF

p <- ggplot(demand_SF, aes(x = Number_of_reviews , y = Price)) + geom_point()
# if both X and Y axes are fixed for all layers.
print(p + geom_smooth(method=lm) + ggtitle("Price Vs Demand - SF") +labs(y= "Price", x = "No.of Reviews"))


#price vs Demand - DC

p <- ggplot(demand_DC, aes(x = Number_of_reviews , y = Price)) + geom_point()  # if both X and Y axes are fixed for all layers.
print(p + geom_smooth(method=lm) + ggtitle("Price Vs Demand - DC") +labs(y= "Price", x = "No.of Reviews"))


# price vs demand - Chicago

p <- ggplot(demand_Chicago, aes(x = Number_of_reviews , y = Price)) + geom_point()   # if both X and Y axes are fixed for all layers.
print(p + geom_smooth(method=lm) + ggtitle("Price Vs Demand - Chicago") +labs(y= "Price", x = "No.of Reviews"))


# 3) Airbnb lisitngs growth
-----------------------------------------------------------------------------------------------------------------
  # Extracting the required dataframe
  
  airbnbPopularity <- data.frame(airbnb_maindata$city, airbnb_maindata$host_since, airbnb_maindata$id)

View(airbnbPopularity)

# Data cleaning of the airbnbPopularity dataframe

airbnbPopularity <- airbnbPopularity %>%
  na_if("") %>%
  na.omit

# Formatting Date
airbnbPopularity$airbnb_maindata.host_since <- format(as.Date(airbnbPopularity$airbnb_maindata.host_since), "%Y")

airbnbPopularity$airbnb_maindata.host_since <- year(as.Date(airbnbPopularity$airbnb_maindata.host_since, format = "%m/%d/%Y"))

# Grouping data by city and year

airbnbPopularity <- rename(count(airbnbPopularity, airbnb_maindata.city, airbnb_maindata.host_since), Freq = n)

# Plotting the Airbnb listing growth graph

ggplot(airbnbPopularity, aes(x=airbnb_maindata.host_since, y=Freq, color=airbnb_maindata.city, group=airbnb_maindata.city )) + geom_line(size=1) + labs(x = "Years", y = "Number of Hosts", title = "GROWTH RATE OF NEW HOSTS ON AIRBNB", colour = "City")


# 4) Density population of Airbnb and its relation with the significance of location/price
-----------------------------------------------------------------------------------------------
  # creating table
  table_data <- select (airbnb_maindata,city,zipcode )
print(table_data)

#caculating mode of log price of Santa Monica Neighbourhood
santa_monica_price <- airbnb_maindata %>%
  select(city,log_price,neighbourhood) %>%
  filter(neighbourhood == "Santa Monica") %>%
  arrange(city)


#caculating mode of log price near LAX airport
airport_airbnb_price <- airbnb_maindata %>%
  select(city,log_price,zipcode) %>%
  filter(zipcode == "90045") %>%
  arrange(city)

# To get count of Air Bnb in each city
airbnb_count_city <- table(airbnb_maindata$city)
print(airbnb_count_city)

# To generate a bar plot to show the count of AirBnB in each city
barplot(airbnb_count_city,
        main = "Number of AirBnB in each City",
        xlab = "City",
        ylab = "Count of AirBnB",
        names.arg = c("BO", "CHI", "DC","LA", "NYC" ,"SF"),
        col = "lightblue1",
        horiz = TRUE)

# Calculating density of Airbnb rental per square mile
#(total number of Airbnb rental/ total area of city)
# Squaremile per city info is based on google data
density_Boston  <- 3468/89.63
density_Chicago  <- 3719 / 234
density_DC  <- 5688/ 68.34
density_LA  <- 22453/ 503
density_NYC  <- 32349/ 468
density_SF  <- 6434/ 46.87


#Creating a table consisting of density of each city
density <- matrix(c(density_Boston,density_Chicago,density_DC,density_LA,density_NYC,density_SF ),ncol=6,byrow=TRUE)
colnames(density) <- c("Boston", "Chicago", "DC", "LA", "NYC", "SF")
density <- as.table(density)


#Generating barplot for the density of Airbnb rental
barplot(density,
        main = "Density of Airbnb Rentals",
        xlab = "Name of Cities",
        ylab = "Airbnb Rental Per Square Mile",
        col = "#8AACEB",
        names.arg = c("Boston", "Chicago", "DC", "LA", "NYC", "SF"),
        space = 1
)

# plotting density of AirBnB  based on zipcodes
---------------------------------------------------------------------------------------
  # SAN FRANSISCO
  area_map <- ggmap(get_googlemap(center = c(lon = -122.431618682397, lat = 37.7720044825607),
                                  zoom = 12, scale = 1,
                                  maptype ='terrain',
                                  color = 'color'))

area_map + geom_point(aes(x = longitude, y = latitude, colour = zipcode), data = airbnb_maindata, size = 0.5, show.legend = FALSE , colour = 'red') +
  theme(legend.position="none")

# BOSTON
area_map <- ggmap(get_googlemap(center = c(lon = -71.04094378, lat = 42.30775512),
                                zoom = 12, scale = 1,
                                maptype ='terrain',
                                color = 'color'))

area_map + geom_point(aes(x = longitude, y = latitude, colour = zipcode), data = airbnb_maindata, size = 0.5 , show.legend = FALSE , colour = 'purple4') +
  theme(legend.position="none")

#NEW YORK
area_map <- ggmap(get_googlemap(center = c(lon = -73.935242, lat = 40.730610),
                                zoom = 11, scale = 1,
                                maptype ='terrain',
                                color = 'color'))

area_map + geom_point(aes(x = longitude, y = latitude, colour = zipcode), data = airbnb_maindata, size = 0.5 , show.legend = FALSE , colour = 'blue') +
  theme(legend.position="none")

# CHICAGO
area_map <- ggmap(get_googlemap(center = c(lon = -87.61740869, lat = 41.85104749),
                                zoom = 12, scale = 1,
                                maptype ='terrain',
                                color = 'color'))

area_map + geom_point(aes(x = longitude, y = latitude, colour = zipcode), data = airbnb_maindata, size = 1 , show.legend = FALSE , colour = 'turquoise4') +
  theme(legend.position="none")

#DC
area_map <- ggmap(get_googlemap(center = c(lon = -76.99769614, lat = 38.89283222),
                                zoom = 12, scale = 1,
                                maptype ='terrain',
                                color = 'color'))

area_map + geom_point(aes(x = longitude, y = latitude, colour = zipcode), data = airbnb_maindata, size = 0.5, show.legend = FALSE , colour = 'royalblue4') +
  theme(legend.position="none")

#LA
area_map <- ggmap(get_googlemap(center = c(lon = -118.2604389, lat = 34.04673741),
                                zoom = 12, scale = 1,
                                maptype ='terrain',
                                color = 'color'))

area_map + geom_point(aes(x = longitude, y = latitude, colour = zipcode), data = airbnb_maindata, size = 1, show.legend = FALSE , colour = 'indianred') +
  theme(legend.position="none")

# 5) Variation of Review scores in each city
# a) Does price vary in according to review scores in each city
# b) Which room_type is preferred
# c) Which month saw highest numbers of hosts join Airbnb in each city
-----------------------------------------------------------------------------------------------------------------
  # Clean zipcode column and add a new column with clean values.
  airbnb_maindata$clean_zipcode = clean.zipcodes(airbnb_maindata$zipcode)
# Add a new column that contains year of starting as a host.
airbnb_maindata$start_year = format(as.Date(airbnb_maindata$host_since, format="%Y-%m-%d"),"%Y")
airbnb_maindata$start_month = format(as.Date(airbnb_maindata$host_since, format="%Y-%m-%d"),"%m")
# Divide the data by city for city level analysis.
airbnb_boston <- filter(airbnb_maindata, city == "Boston")
airbnb_chicago <- filter(airbnb_maindata, city == "Chicago")
airbnb_dc <- filter(airbnb_maindata, city == "DC")
airbnb_la <- filter(airbnb_maindata, city == "LA")
airbnb_nyc <- filter(airbnb_maindata, city == "NYC")
airbnb_sf <- filter(airbnb_maindata, city == "SF")

# 1. Plot listing graph against each city to find out distribution of listings across cities.
plot(airbnb_maindata$city)
# Summary: Bar plot shows that NYC has the most number of listings followed by LA and SF respectively.

# 2. Plot rating graph for each city to find out the highest rated city.
plot(airbnb_maindata$city, airbnb_maindata$review_scores_rating,
     main = "Rating per City",
     xlab = "City",
     ylab = "Rating",
     type = "b",
     col = "coral2")
# Summary: The box-plot shows that SF, LA and DC are the highest rated cities. These cities have biggest
# fraction of listings that are very highly rated.

# 3. Plot rating against log_price for each city to find out how price varies for different rating in
# different cities.

# 3.0 Across all Cities
all_with_ratings <- filter(airbnb_maindata, !is.na(review_scores_rating))
all_price_dist <- ggplot(data=airbnb_maindata, aes(x=log_price)) + geom_histogram(aes(y = stat(count)), fill="darkred") + ggtitle("All")
all_rating_dist <- ggplot(data=airbnb_maindata, aes(x=review_scores_rating)) + geom_histogram(aes(y = stat(count)), fill="darkred") + ggtitle("All")
all_price_vs_rating <- ggplot(data=all_with_ratings, aes(x=review_scores_rating, y=log_price)) + stat_summary(fun.y="mean", geom="bar", fill="darkred") + ggtitle("All")

# 3.1 - Boston
boston_with_ratings <- filter(airbnb_boston, !is.na(review_scores_rating))
boston_price_dist <- ggplot(data=airbnb_boston, aes(x=log_price)) + geom_histogram(aes(y = stat(count)), fill="blue") + ggtitle("Boston")
boston_rating_dist <- ggplot(data=airbnb_boston, aes(x=review_scores_rating)) + geom_histogram(aes(y = stat(count)), fill="blue") + ggtitle("Boston")
boston_price_vs_rating <- ggplot(data=boston_with_ratings, aes(x=review_scores_rating, y=log_price)) + stat_summary(fun.y="mean", geom="bar", fill="blue") + ggtitle("Boston")
# grid.arrange(boston_price_dist, boston_rating_dist, boston_price_vs_rating, nrow = 2)

# 3.2 Chicago
chicago_with_ratings <- filter(airbnb_chicago, !is.na(review_scores_rating))
chicago_price_dist <- ggplot(data=airbnb_chicago, aes(x=log_price)) + geom_histogram(aes(y = stat(count)), fill="orangered1") + ggtitle("Chicago")
chicago_rating_dist <- ggplot(data=airbnb_chicago, aes(x=review_scores_rating)) + geom_histogram(aes(y = stat(count)), fill="orangered1") + ggtitle("Chicago")
chicago_price_vs_rating <- ggplot(data=chicago_with_ratings, aes(x=review_scores_rating, y=log_price)) + stat_summary(fun.y="mean", geom="bar", fill="orangered1") + ggtitle("Chicago")
# grid.arrange(chicago_price_dist, chicago_rating_dist, chicago_price_vs_rating, nrow = 2)

# 3.3 DC
dc_with_ratings <- filter(airbnb_dc, !is.na(review_scores_rating))
dc_price_dist <- ggplot(data=airbnb_dc, aes(x=log_price)) + geom_histogram(aes(y = stat(count)), fill="maroon2") + ggtitle("DC")
dc_rating_dist <- ggplot(data=airbnb_dc, aes(x=review_scores_rating)) + geom_histogram(aes(y = stat(count)), fill="maroon2") + ggtitle("DC")
dc_price_vs_rating <- ggplot(data=dc_with_ratings, aes(x=review_scores_rating, y=log_price)) + stat_summary(fun.y="mean", geom="bar", fill="maroon2")+ ggtitle("DC")
# grid.arrange(dc_price_dist, dc_rating_dist, dc_price_vs_rating, nrow = 2)

# 3.4 LA
la_with_ratings <- filter(airbnb_la, !is.na(review_scores_rating))
la_price_dist <- ggplot(data=airbnb_la, aes(x=log_price)) + geom_histogram(aes(y = stat(count)), fill="darkgreen") + ggtitle("LA")
la_rating_dist <- ggplot(data=airbnb_la, aes(x=review_scores_rating)) + geom_histogram(aes(y = stat(count)), fill="darkgreen")+ ggtitle("LA")
la_price_vs_rating <- ggplot(data=la_with_ratings, aes(x=review_scores_rating, y=log_price)) + stat_summary(fun.y="mean", geom="bar", fill="darkgreen") + ggtitle("LA")
# grid.arrange(la_price_dist, la_rating_dist, la_price_vs_rating, nrow = 2)

# 3.5 NYC
nyc_with_ratings <- filter(airbnb_nyc, !is.na(review_scores_rating))
nyc_price_dist <- ggplot(data=airbnb_nyc, aes(x=log_price)) + geom_histogram(aes(y = stat(count)), fill="purple") + ggtitle("NYC")
nyc_rating_dist <- ggplot(data=airbnb_nyc, aes(x=review_scores_rating)) + geom_histogram(aes(y = stat(count)), fill="purple") + ggtitle("NYC")
nyc_price_vs_rating <- ggplot(data=nyc_with_ratings, aes(x=review_scores_rating, y=log_price)) + stat_summary(fun.y="mean", geom="bar", fill="purple") + ggtitle("NYC")
# grid.arrange(nyc_price_dist, nyc_rating_dist, nyc_price_vs_rating, nrow = 2)

# 3.6 SF
sf_with_ratings <- filter(airbnb_sf, !is.na(review_scores_rating))
sf_price_dist <- ggplot(data=airbnb_sf, aes(x=log_price)) + geom_histogram(aes(y = stat(count)), fill="lightblue") + ggtitle("SF")
sf_rating_dist <- ggplot(data=airbnb_sf, aes(x=review_scores_rating)) + geom_histogram(aes(y = stat(count)), fill="lightblue") + ggtitle("SF")
sf_price_vs_rating <- ggplot(data=sf_with_ratings, aes(x=review_scores_rating, y=log_price)) + stat_summary(fun.y="mean", geom="bar", fill="lightblue") + ggtitle("SF")
# grid.arrange(sf_price_dist, sf_rating_dist, sf_price_vs_rating, nrow = 2)

grid.arrange(all_price_dist, boston_price_dist, chicago_price_dist, dc_price_dist, la_price_dist, nyc_price_dist, sf_price_dist, nrow = 3)
# Boston and Chicago have variety of options on all price points. Chicago has the cheapest
# available options.
grid.arrange(all_rating_dist, boston_rating_dist, chicago_rating_dist, dc_rating_dist, la_rating_dist, nyc_rating_dist, sf_rating_dist, nrow = 3)
# A large fraction of listings have high rating across all cities. This could either mean that
# customers usually log into the system to give good ratings or that hosts with low ratings quickly
# go out of the market.
grid.arrange(all_price_vs_rating, boston_price_vs_rating, chicago_price_vs_rating, dc_price_vs_rating, la_price_vs_rating, nyc_price_vs_rating, sf_price_vs_rating, nrow = 3)
# Log price decreases with review scores at the top of the spectrum. The lower half of review spectrum is
# quite unpredictable. Sometimes the price of very poorly rated properties is more than highly rated properties as well.


# 4. Plot rating against (property_type, room_type) in various cities to find out whats most popular.

# 4.0
all_property_type_dist <- ggplot(data=airbnb_boston, aes(x=property_type)) + geom_bar(fill="darkred") + ggtitle("All")
all_room_type_dist <- ggplot(data=airbnb_boston, aes(x=room_type)) + geom_bar(fill="darkred") + ggtitle("All")
all_rating_vs_property_type <- ggplot(data=boston_with_ratings, aes(x=property_type, y=review_scores_rating)) + stat_summary(fun.y="mean", geom="bar", fill="darkred") + ggtitle("All")
all_rating_vs_room_type <- ggplot(data=boston_with_ratings, aes(x=room_type, y=review_scores_rating)) + stat_summary(fun.y="mean", geom="bar", fill="darkred") + ggtitle("All")

# 4.1
boston_property_type_dist <- ggplot(data=airbnb_boston, aes(x=property_type)) + geom_bar(fill="blue") + ggtitle("Boston")
boston_room_type_dist <- ggplot(data=airbnb_boston, aes(x=room_type)) + geom_bar(fill="blue") + ggtitle("Boston")
boston_rating_vs_property_type <- ggplot(data=boston_with_ratings, aes(x=property_type, y=review_scores_rating)) + stat_summary(fun.y="mean", geom="bar", fill="blue") + ggtitle("Boston")
boston_rating_vs_room_type <-ggplot(data=boston_with_ratings, aes(x=room_type, y=review_scores_rating)) + stat_summary(fun.y="mean", geom="bar", fill="blue") + ggtitle("Boston")

# 4.2
chicago_property_type_dist <- ggplot(data=airbnb_chicago, aes(x=property_type)) + geom_bar(fill="orangered1") + ggtitle("Chicago")
chicago_room_type_dist <- ggplot(data=airbnb_chicago, aes(x=room_type)) + geom_bar(fill="orangered1") + ggtitle("Chicago")
chicago_rating_vs_property_type <- ggplot(data=chicago_with_ratings, aes(x=property_type, y=review_scores_rating)) + stat_summary(fun.y="mean", geom="bar", fill="orangered1") + ggtitle("Chicago")
chicago_rating_vs_room_type <-ggplot(data=chicago_with_ratings, aes(x=room_type, y=review_scores_rating)) + stat_summary(fun.y="mean", geom="bar", fill="orangered1") + ggtitle("Chicago")

# 4.3
dc_property_type_dist <- ggplot(data=airbnb_dc, aes(x=property_type)) + geom_bar(fill="maroon2") + ggtitle("DC")
dc_room_type_dist <- ggplot(data=airbnb_dc, aes(x=room_type)) + geom_bar(fill="maroon2") + ggtitle("DC")
dc_rating_vs_property_type <- ggplot(data=dc_with_ratings, aes(x=property_type, y=review_scores_rating)) + stat_summary(fun.y="mean", geom="bar", fill="maroon2") + ggtitle("DC")
dc_rating_vs_room_type <-ggplot(data=dc_with_ratings, aes(x=room_type, y=review_scores_rating)) + stat_summary(fun.y="mean", geom="bar", fill="maroon2") + ggtitle("DC")

# 4.4
la_property_type_dist <- ggplot(data=airbnb_la, aes(x=property_type)) + geom_bar(fill="darkgreen") + ggtitle("LA")
la_room_type_dist <- ggplot(data=airbnb_la, aes(x=room_type)) + geom_bar(fill="darkgreen") + ggtitle("LA")
la_rating_vs_property_type <- ggplot(data=la_with_ratings, aes(x=property_type, y=review_scores_rating)) + stat_summary(fun.y="mean", geom="bar", fill="darkgreen") + ggtitle("LA")
la_rating_vs_room_type <-ggplot(data=la_with_ratings, aes(x=room_type, y=review_scores_rating)) + stat_summary(fun.y="mean", geom="bar", fill="darkgreen") + ggtitle("LA")

# 4.5
nyc_property_type_dist <- ggplot(data=airbnb_nyc, aes(x=property_type)) + geom_bar(fill="purple") + ggtitle("NYC")
nyc_room_type_dist <- ggplot(data=airbnb_nyc, aes(x=room_type)) + geom_bar(fill="purple") + ggtitle("NYC")
nyc_rating_vs_property_type <- ggplot(data=nyc_with_ratings, aes(x=property_type, y=review_scores_rating)) + stat_summary(fun.y="mean", geom="bar", fill="purple") + ggtitle("NYC")
nyc_rating_vs_room_type <-ggplot(data=nyc_with_ratings, aes(x=room_type, y=review_scores_rating)) + stat_summary(fun.y="mean", geom="bar", fill="purple") + ggtitle("NYC")

# 4.6
sf_property_type_dist <- ggplot(data=airbnb_sf, aes(x=property_type)) + geom_bar(fill="lightblue") + ggtitle("SF") + theme(legend.position="top")
sf_room_type_dist <- ggplot(data=airbnb_sf, aes(x=room_type)) + geom_bar(fill="lightblue") + ggtitle("SF")
sf_rating_vs_property_type <- ggplot(data=sf_with_ratings, aes(x=property_type, y=review_scores_rating)) + stat_summary(fun.y="mean", geom="bar", fill="lightblue") + ggtitle("SF") + theme(legend.position="top")
sf_rating_vs_room_type <-ggplot(data=sf_with_ratings, aes(x=room_type, y=review_scores_rating)) + stat_summary(fun.y="mean", geom="bar", fill="lightblue") + ggtitle("SF")

grid.arrange(all_property_type_dist, boston_property_type_dist, chicago_property_type_dist, dc_property_type_dist, la_property_type_dist, nyc_property_type_dist, sf_property_type_dist, nrow = 3)
grid.arrange(all_room_type_dist, boston_room_type_dist, chicago_room_type_dist, dc_room_type_dist, la_room_type_dist, nyc_room_type_dist, sf_room_type_dist, nrow = 3)
# All cities have most listing for "Entire home". Plus, the ratio of number of "Entire home" listings to
# "Private room" listings is similar across all cities except New York. New York has an exceptionally
# large number of "Private Room" listinngs. This can be attributed to the lack of space and a large
# supply of "Private Room" in New York. Also, LA has the most number of "Shared room" options,
# closely followed by New York.
grid.arrange(all_rating_vs_property_type, boston_rating_vs_property_type, chicago_rating_vs_property_type, dc_rating_vs_property_type, la_rating_vs_property_type, nyc_rating_vs_property_type, sf_rating_vs_property_type, nrow = 3)
grid.arrange(all_rating_vs_room_type, boston_rating_vs_room_type, chicago_rating_vs_room_type, dc_rating_vs_room_type, la_rating_vs_room_type, nyc_rating_vs_room_type, sf_rating_vs_room_type, nrow = 3)
# Shared rooms are highly rated in SF, even though the most pricey listings are in NY. This might due
# to more travellers / young travellers coming to SF for exploring the area.

# 5. Rating vs host year

# 5.0
all_rating_vs_start_year <- ggplot(data=all_with_ratings, aes(x=start_year, y=review_scores_rating)) + stat_summary(fun.y="mean", geom="bar", fill="darkred") + ggtitle("All")
all_rating_vs_start_month <- ggplot(data=all_with_ratings, aes(x=start_month, y=review_scores_rating)) + stat_summary(fun.y="mean", geom="bar", fill="darkred") + ggtitle("All")
all_join_month_dist <- ggplot(data=airbnb_maindata, aes(x=start_month)) + geom_bar(fill="darkred") + ggtitle("All")

# 5.1
boston_rating_vs_start_year <- ggplot(data=boston_with_ratings, aes(x=start_year, y=review_scores_rating)) + stat_summary(fun.y="mean", geom="bar", fill="blue") + ggtitle("Boston")
boston_rating_vs_start_month <- ggplot(data=boston_with_ratings, aes(x=start_month, y=review_scores_rating)) + stat_summary(fun.y="mean", geom="bar", fill="blue") + ggtitle("Boston")
boston_join_month_dist <- ggplot(data=airbnb_boston, aes(x=start_month)) + geom_bar(fill="blue") + ggtitle("Boston")

# 5.2
chicago_rating_vs_start_year <- ggplot(data=chicago_with_ratings, aes(x=start_year, y=review_scores_rating)) + stat_summary(fun.y="mean", geom="bar", fill="orangered1") + ggtitle("Chicago")
chicago_rating_vs_start_month <- ggplot(data=chicago_with_ratings, aes(x=start_month, y=review_scores_rating)) + stat_summary(fun.y="mean", geom="bar", fill="orangered1") + ggtitle("Chicago")
chicago_join_month_dist <- ggplot(data=airbnb_chicago, aes(x=start_month)) + geom_bar(fill="orangered1") + ggtitle("Chicago")

# 5.3
dc_rating_vs_start_year <- ggplot(data=dc_with_ratings, aes(x=start_year, y=review_scores_rating)) + stat_summary(fun.y="mean", geom="bar", fill="maroon2") + ggtitle("DC")
dc_rating_vs_start_month <- ggplot(data=dc_with_ratings, aes(x=start_month, y=review_scores_rating)) + stat_summary(fun.y="mean", geom="bar", fill="maroon2") + ggtitle("DC")
dc_join_month_dist <- ggplot(data=airbnb_dc, aes(x=start_month)) + geom_bar(fill="maroon2") + ggtitle("DC")

# 5.4
la_rating_vs_start_year <- ggplot(data=la_with_ratings, aes(x=start_year, y=review_scores_rating)) + stat_summary(fun.y="mean", geom="bar", fill="darkgreen") + ggtitle("LA")
la_rating_vs_start_month <- ggplot(data=la_with_ratings, aes(x=start_month, y=review_scores_rating)) + stat_summary(fun.y="mean", geom="bar", fill="darkgreen") + ggtitle("LA")
la_join_month_dist <- ggplot(data=airbnb_la, aes(x=start_month)) + geom_bar(fill="darkgreen") + ggtitle("LA")

# 5.5
nyc_rating_vs_start_year <- ggplot(data=nyc_with_ratings, aes(x=start_year, y=review_scores_rating)) + stat_summary(fun.y="mean", geom="bar", fill="purple") + ggtitle("NYC")
nyc_rating_vs_start_month <- ggplot(data=nyc_with_ratings, aes(x=start_month, y=review_scores_rating)) + stat_summary(fun.y="mean", geom="bar", fill="purple") + ggtitle("NYC")
nyc_join_month_dist <- ggplot(data=airbnb_nyc, aes(x=start_month)) + geom_bar(fill="purple") + ggtitle("NYC")

# 5.6
sf_rating_vs_start_year <- ggplot(data=sf_with_ratings, aes(x=start_year, y=review_scores_rating)) + stat_summary(fun.y="mean", geom="bar", fill="lightblue") + ggtitle("SF")
sf_rating_vs_start_month <- ggplot(data=sf_with_ratings, aes(x=start_month, y=review_scores_rating)) + stat_summary(fun.y="mean", geom="bar", fill="lightblue") + ggtitle("SF")
sf_join_month_dist <- ggplot(data=airbnb_sf, aes(x=start_month)) + geom_bar(fill="lightblue") + ggtitle("SF")

grid.arrange(all_rating_vs_start_year, boston_rating_vs_start_year, chicago_rating_vs_start_year, dc_rating_vs_start_year, la_rating_vs_start_year, nyc_rating_vs_start_year, sf_rating_vs_start_year, nrow = 3)
grid.arrange(all_rating_vs_start_month, boston_rating_vs_start_month, chicago_rating_vs_start_month, dc_rating_vs_start_month, la_rating_vs_start_month, nyc_rating_vs_start_month, sf_rating_vs_start_month, nrow = 3)
grid.arrange(all_join_month_dist, boston_join_month_dist, chicago_join_month_dist, dc_join_month_dist, la_join_month_dist, nyc_join_month_dist, sf_join_month_dist, nrow = 3)
# Across all cities, the number of people who join as hosts is relatively high in summer. This trend seems to be in line with the fact that many
#foreigner tourist visit the US during summer. In the USA, NYC, LA and SF are major tourist attractions in the US.


# 6) Variation of price w.r.t to cities
# a) Count of Airbnbs per city
-----------------------------------------------------------------------------------------------------------------
  #Summary of all the six cities wrt log price
  by(airbnb_maindata$log_price,airbnb_maindata$city, summary)

# Visualize the distribution of price for different cities using a boxplot
ggboxplot(airbnb_maindata, x = "city", y = "log_price",
          color = "city",
          ylab = "Log Price", xlab = "City", title = "Price Vs City")

#Plot of all six cities wrt log price
ggplot(airbnb_maindata, aes(airbnb_maindata$log_price)) +
  geom_histogram(binwidth = 1, color = 'black', fill = '#099DD9') +
  xlim(0,8) +
  facet_wrap(~airbnb_maindata$city, ncol = 3, scales = "free")

#Segregating data for each city
City_2 <- airbnb_maindata %>%
  select(city, log_price,property_type) %>%
  filter(log_price != "NA", city != "NA") %>%
  arrange(city)
NYC_Price <- City_2 %>%
  filter(city == 'NYC') %>%
  arrange(city)
Boston_Price <- City_2 %>%
  filter(city == 'Boston') %>%
  arrange(city)
LA_Price <- City_2 %>%
  filter(city == 'LA') %>%
  arrange(city)
SF_Price <- City_2 %>%
  filter(city == 'SF') %>%
  arrange(city)
Chicago_Price <- City_2 %>%
  filter(city == 'Chicago') %>%
  arrange(city)
DC_Price <- City_2 %>%
  filter(city == 'DC') %>%
  arrange(city)

#Summary for NYC and plot of log price vs count of airbnbs in NYC
summary(NYC_Price)
NYC_Price %>% ggvis(x = ~log_price, fill:="blue" )  %>% layer_histograms(center = 0) %>% add_axis("x", title = "NYC Log Price") %>%
  add_axis("y", title = "Count of airbnbs in NYC")
#Summary for Boston and plot of log price vs count of airbnbs in Boston
summary(Boston_Price)
Boston_Price %>% ggvis(x = ~log_price, fill:="Orange" )  %>% layer_histograms(center = 0) %>% add_axis("x", title = "Boston Log Price") %>%
  add_axis("y", title = "Count of airbnbs in Boston")
#Summary for SF and plot of log price vs count of airbnbs in SF
summary(SF_Price)
SF_Price %>% ggvis(x = ~log_price, fill:="Green" )  %>% layer_histograms(center = 0) %>% add_axis("x", title = "SF Log Price") %>%
  add_axis("y", title = "Count of airbnbs in SF")
#Summary for DC and plot of log price vs count of airbnbs in DC
summary(DC_Price)
DC_Price %>% ggvis(x = ~log_price, fill:="Yellow" )  %>% layer_histograms(center = 0) %>% add_axis("x", title = "DC Log Price") %>%
  add_axis("y", title = "Count of airbnbs in DC")
#Summary for Chicago and plot of log price vs count of airbnbs in Chicago
summary(Chicago_Price)
Chicago_Price %>% ggvis(x = ~log_price, fill:="Grey" )  %>% layer_histograms(center = 0) %>% add_axis("x", title = "Chicago Log Price") %>%
  add_axis("y", title = "Count of airbnbs in Chicago")
