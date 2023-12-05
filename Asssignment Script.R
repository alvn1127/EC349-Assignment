#Pre-Processing Yelp Academic Data for the Assignment
library(jsonlite)

#Clear
cat("\014")  
rm(list=ls())

#Set Directory as appropriate
#setwd("C/...")
setwd("C:/Users/Alvin's PC/Desktop/Assignment")

#Load Different Data
business_data <- stream_in(file("yelp_academic_dataset_business.json")) #note that stream_in reads the json lines (as the files are json lines, not json)
review_data  <- load("C:/Users/Alvin's PC/Desktop/Assignment/yelp_review_small.Rda") 
user_data <- load("C:/Users/Alvin's PC/Desktop/Assignment/yelp_user_small.Rda")




#cleaning data 
install.packages("tidyverse")
library(tidyverse)
glimpse(user_data_small)
glimpse(tip_data)
glimpse(business_data)
glimpse(checkin_data)
glimpse(review_data_small)

#Tidying  User_data_small dataset
install.packages("lubridate")
library(lubridate)
user_data_small <- user_data_small %>%
  mutate(elite = str_replace(elite, "20,20", "2020"),#fixing year 2020 for elite members showing as "20,20"
         elite_years = ifelse(elite == "" | is.na(elite), 0, str_count(elite, ",") + 1), #creating a new variable elite_years, number of years a user was an elite member
         yelping_since = as.Date(yelping_since, format = "%Y-%m-%d"), #Converting yelping_since into date format, removing hour, min and seconds
         year_started = year(yelping_since), #extracting year from yelping_since
         friends_number = ifelse(friends == "None", 0, str_count(friends, ",") + 1)) #creating a new variable friends_number, number of friends the user has


#Cleaning Categories, using the main categories I create a dummy variable and give a 1 if it is in the category column  
business_data <- business_data %>% 
  mutate(Active_Life = if_else(str_detect(categories, "Active Life"), 1, 0),
         Arts_Entertainment = if_else(str_detect(categories, "Arts & Entertainment"), 1, 0),
         Automotive = if_else(str_detect(categories, "Automotive"), 1, 0),
         Beauty_Spas = if_else(str_detect(categories, "Beauty & Spas"), 1, 0),
         Education = if_else(str_detect(categories, "Education"), 1, 0),
         Event_Planning_Services = if_else(str_detect(categories, "Event Planning & Services"), 1, 0),
         Financial_Services = if_else(str_detect(categories, "Financial Services"), 1, 0),
         Food = if_else(str_detect(categories, "Food"), 1, 0),
         Health_Medical = if_else(str_detect(categories, "Health & Medical"), 1, 0),
         Home_Services = if_else(str_detect(categories, "Home Services"), 1, 0),
         Hotels_Travel = if_else(str_detect(categories, "Hotels & Travel"), 1, 0),
         Local_Flavor = if_else(str_detect(categories, "Local Flavor"), 1, 0),
         Local_Services = if_else(str_detect(categories, "Local Services"), 1, 0),
         Mass_Media = if_else(str_detect(categories, "Mass Media"), 1, 0),
         Nightlife = if_else(str_detect(categories, "Nightlife"), 1, 0),
         Pets = if_else(str_detect(categories, "Pets"), 1, 0),
         Pets = if_else(str_detect(categories, "Pets"), 1, 0),
         Professional_Services = if_else(str_detect(categories, "Professional Services"), 1, 0),
         Public_Services_Government = if_else(str_detect(categories, "Public Services & Government"), 1, 0),
         Real_Estate = if_else(str_detect(categories, "Real Estate"), 1, 0),
         Religious_Organizations = if_else(str_detect(categories, "Religious Organizations"), 1, 0),
         Restaurants = if_else(str_detect(categories, "Restaurants"), 1, 0),
         Shopping = if_else(str_detect(categories, "Shopping"), 1, 0)
  )
#for businesses that did not include any categories, they were removed
business_data <- business_data %>% 
  filter(!is.na(categories))

#Cleaning hours
# Creating a function to calculate the number of hours open 
calculate_hours_open <- function(opening_time) {
  if (is.na(opening_time) || opening_time == "NA") {
    return(0)
  } else {
    times <- strsplit(opening_time, "-")[[1]]
    open <- as.POSIXct(times[1], format = "%H:%M")
    close <- as.POSIXct(times[2], format = "%H:%M")
    return(as.numeric(difftime(close, open, units = "hours")))
  }
}

# Apply the function to each day of the week
business_data <- business_data %>%
  mutate(hoursMonday = sapply(hours$Monday, calculate_hours_open),
         hoursTuesday = sapply(hours$Tuesday, calculate_hours_open),
         hoursWednesday = sapply(hours$Wednesday, calculate_hours_open),
         hoursThursday = sapply(hours$Thursday, calculate_hours_open),
         hoursFriday = sapply(hours$Friday, calculate_hours_open),
         hoursSaturday = sapply(hours$Saturday, calculate_hours_open),
         hoursSunday = sapply(hours$Sunday, calculate_hours_open))


#Renaming review_data_small stars
colnames(review_data_small)[colnames(review_data_small) == "stars"] <- "review_stars"

#Combining datasets
install.packages("glmnet")
library(glmnet)
review_user_data <- inner_join(review_data_small %>% select(user_id, business_id, review_stars, useful, funny, cool),
                               user_data_small %>% select(user_id, review_count, useful, funny, cool, fans, average_stars, 
                                                          compliment_hot, compliment_more, compliment_profile, 
                                                          compliment_cute, compliment_list, compliment_note, 
                                                          compliment_plain, compliment_cool, compliment_funny, 
                                                          compliment_writer, compliment_photos, elite_years, 
                                                          year_started, friends_number),
                               by = "user_id")

final_dataset <- inner_join(review_user_data,
                            business_data %>% select(business_id, stars, review_count, is_open, Active_Life, Arts_Entertainment,
                                                     Automotive, Beauty_Spas, Education, Event_Planning_Services, Financial_Services,
                                                     Food, Health_Medical, Home_Services, Hotels_Travel, Local_Flavor, Local_Services,
                                                     Mass_Media, Nightlife, Pets, Professional_Services, Public_Services_Government,
                                                     Real_Estate, Religious_Organizations, Restaurants, Shopping, hoursMonday, hoursTuesday,
                                                     hoursWednesday, hoursThursday, hoursFriday, hoursSaturday, hoursSunday),
                            by = "business_id")




#Separating final_dataset into training and test
library(caret)
set.seed(1)
final_partition <- createDataPartition(y=final_dataset$review_stars, p=0.75)
final_training_set <- final_dataset[final_partition[[1]], ]
final_test_set <- final_dataset[-final_partition[[1]], ]
vars_excluded <- c("review_id", "user_id", "business_id", "city", "state", "review_stars")
x <- as.matrix(final_training_set[,setdiff(names(final_training_set), vars_excluded)])
y <- final_training_set$review_stars

x_test <- as.matrix(final_test_set[, setdiff(names(final_test_set), vars_excluded)])
y_test <- final_test_set$review_stars

lasso <- glmnet(x, y, alpha=1) #LASSO model

#Validation for LASSO
cv_lasso <- cv.glmnet(x, y, alpha = 1)
best_lambda_lasso <- cv_lasso$lambda.min #best_lamda_lasso = 0.002032159568
lasso <- glmnet(x, y, alpha = 1, lambda = best_lambda_lasso, thresh = 1e-12)
lasso.prediction <- predict(lasso, newx = as.matrix(x_test), s = best_lambda_lasso)
lasso.mse <- mean((lasso.prediction - y_test )^2) #lasso MSE=1.174839
print(cv_lasso)
plot(cv_lasso)

coef_lasso <- as.matrix(coef(cv_lasso, s= best_lambda_lasso))
coef_lasso_df <- as.data.frame(coef_lasso) #Dataframe showing the coefficients of all the variables
save(coef_lasso_df, file = "coef_lasso_df.RData")


ridge <- glmnet(x,y, alpha = 0) #Ridge model
cv_ridge <- cv.glmnet(x, y, alpha = 0)
best_lambda_ridge <- cv_ridge$lambda.min #best_lamda = 0.08594496
ridge <- glmnet(x, y, alpha = 0, lambda = best_lambda_ridge, thresh = 1e-12)
ridge.prediction <- predict(ridge, newx = as.matrix(x_test), s = best_lambda_ridge)
ridge.mse <- mean((ridge.prediction - y_test )^2) #Ridge MSE = 1.177312
print(cv_ridge)
plot(cv_ridge)

coef_ridge <- as.matrix(coef(cv_ridge, s= best_lambda_ridge))
coef_ridge_df <- as.data.frame(coef_ridge)
save(coef_ridge_df, file = "coef_ridge_df.RData")

#Bagging, Random Forest, Boosting
install.packages("ggplot2")
install.packages("ipred")
install.packages("adabag")
install.packages("randomForest")

library(ggplot2)
library(ipred)
library(adabag) #AdaBoost
library(randomForest) #randomforest

#Mutating "stars" to factor variable 
final_dataset <- final_dataset %>% 
  mutate(review_stars=as.factor(review_stars))


#doing same partition but with factor variable
set.seed(1)
final_partition_rf <- createDataPartition(y=final_dataset$review_stars, p=0.75)
final_training_set_rf <- final_dataset[final_partition_rf[[1]], ]
final_test_set_rf <- final_dataset[-final_partition_rf[[1]], ]




#Random Forest
rf_model_50 <- randomForest(review_stars~., data=final_training_set_rf, ntree=50) #random forest with 50 trees
pred_rf_test_50 = predict(rf_model_50, final_test_set_rf)
mean(rf_model_50[["err.rate"]]) #error rate = 0.5841689

actual_values <- final_test_set_rf$review_stars
test_set_error_rate_50 <- mean(pred_rf_test_50 != actual_values) # test set error rate (50 trees) = 0.416707


rf_model_100 <- randomForest(review_stars~., data=final_training_set_rf, ntree=100) #random forest with 100 trees
pred_rf_test_100 = predict(rf_model_100, final_test_set_rf)
mean(rf_model_100[["err.rate"]]) #error rate = 0.5740321

actual_values <- final_test_set_rf$review_stars
test_set_error_rate_100 <- mean(pred_rf_test_100 != actual_values) # test set error rate (100 trees) = 0.4146344

#Bagging
bag <- bagging(review_stars~., data=final_training_set_rf, nbagg = 50,
               coob = TRUE, control = rpart.control(minsplit = 2, cp = 0.1)
)


bag

#Boosting
model_adaboost <- boosting(review_stars~., data=final_training_set_rf, boos=TRUE, mfinal=50)
summary(model_adaboost)
pred_ada_test = predict(model_adaboost, test)
pred_ada_test


#Attempting to tidy business_data$attributes, ended up not including
glimpse(business_data$attributes)
unique_price_values <- unique(business_data$attributes$RestaurantsPriceRange2)
print(unique_price_values) # finding all the unique price range values
unique_wifi <- unique(business_data$attributes$WiFi)
print(unique_wifi) # finding all the unique wifi responses
unique_alcohol <- unique(business_data$attributes$Alcohol)
print(unique_alcohol) # finding all the unique Alcohol responses
unique_BusinessParking <- unique(business_data$attributes$BusinessParking)
print(unique_BusinessParking) # finding all the unique Business Parking responses
unique_ages_allowed <- unique (business_data$attributes$AgesAllowed)
print(unique_ages_allowed)

#setting True=1, False = 0, NA = -1 for the attributes dataframe, apart from non-numeric columns
business_data$attributes <- business_data$attributes %>%
  mutate(across(where(is.character), ~ ifelse(. == "True", 1, ifelse(. == "False", 0, .))))

non_numeric_columns <- c('RestaurantsPriceRange2', 'BusinessParking', 'Ambience', 'BestNights', 
                         'GoodForMeal', 'HairSpecializesIn', 'Music', 'AgesAllowed', 'Alcohol', 
                         'BYOBCorkage', 'NoiseLevel', 'RestaurantsAttire', 'Smoking', 'WiFi')

# Convert NAs to -1 in all other columns
business_data$attributes <- business_data$attributes %>%
  mutate(across(-all_of(non_numeric_columns), ~ ifelse(is.na(.), -1, .)))

#Converting NA and "None" to -1 in ResturantsPriceRange2
business_data$attributes <- business_data$attributes %>%
  mutate(RestaurantsPriceRange2 = ifelse(is.na(RestaurantsPriceRange2) | RestaurantsPriceRange2 == "None", -1, as.numeric(RestaurantsPriceRange2)))

#Cleaning 'Alcohol'
business_data$attributes <- business_data$attributes %>%
  mutate(Alcohol = case_when(
    Alcohol %in% c("u'none'", "'none'", "None") ~ 0, #No alcohol = 0
    Alcohol %in% c("'beer_and_wine'", "u'beer_and_wine'") ~ 1, #beer and wine = 1
    Alcohol %in% c("u'full_bar'", "'full_bar'") ~ 2, #full bar = 2
    is.na(Alcohol) ~ -1, #missing = -1
  ))

#Cleaning 'WiFi'
business_data$attributes <- business_data$attributes %>%
  mutate(WiFi = case_when(
    WiFi %in% c("u'no'", "'no'", "None") ~ 0, #No WiFi = 0
    WiFi %in% c("u'paid'", "'paid'") ~ 1, #Paid WiFi = 1
    WiFi %in% c("'u'free'", "'free") ~ 2, #Free WiFi= 2
    is.na(WiFi) ~ -1, #missing = -1
  ))

#Cleaning 'RestaurantsAttire
business_data$attributes <- business_data$attributes %>%
  mutate(RestaurantsAttire = case_when(
    RestaurantsAttire %in% c("None") ~ 0, #No Dress Code = 0
    RestaurantsAttire %in% c("u'casual'", "'casual'") ~ 1, #Casual Attire= 1
    RestaurantsAttire %in% c("'u'dressy'", "'dressy") ~ 2, #Dressy Attire= 2
    RestaurantsAttire %in% c("'u'formal'", "'formal") ~ 3, #Formal Attire= 3
    is.na(RestaurantsAttire) ~ -1, #missing = -1
  ))


#Cleaning 'NoiseLevel'
business_data$attributes <- business_data$attributes %>%
  mutate(NoiseLevel = case_when(
    NoiseLevel %in% c("None") ~ 0, #No Noise level = 0
    NoiseLevel %in% c("u'quiet'", "'quiet'") ~ 1, #Quiet Noise level= 1
    NoiseLevel %in% c("'u'average'", "'average") ~ 2, #Average noise level= 2
    NoiseLevel %in% c("'u'loud'", "'loud") ~ 3, #Loud noise level= 3
    NoiseLevel %in% c("'u'very_loud'", "'very_loud") ~ 4, #very loud noise level= 3
    is.na(NoiseLevel) ~ -1, #missing noise level = -1
  ))

business_data$attributes <- business_data$attributes %>%
  mutate(
    ByAppointmentOnly = as.double(ByAppointmentOnly),
    BusinessAcceptsCreditCards = as.double(BusinessAcceptsCreditCards),
    BikeParking = as.double(BikeParking),
    CoatCheck = as.double(CoatCheck),
    RestaurantsTakeOut = as.double(RestaurantsTakeOut),
    RestaurantsDelivery = as.double(RestaurantsDelivery),
    Caters = as.double(Caters),
    WheelchairAccessible = as.double(WheelchairAccessible),
    HappyHour = as.double(HappyHour),
    OutdoorSeating = as.double(OutdoorSeating),
    HasTV = as.double(HasTV),
    RestaurantsReservations = as.double(RestaurantsReservations),
    GoodForKids = as.double(GoodForKids),
    RestaurantsTableService = as.double(RestaurantsTableService),
    RestaurantsGoodForGroups = as.double(RestaurantsGoodForGroups),
    DriveThru = as.double(DriveThru),
    BusinessAcceptsBitcoin = as.double(BusinessAcceptsBitcoin),
    GoodForDancing = as.double(GoodForDancing),
    AcceptsInsurance = as.double(AcceptsInsurance),
  )     





