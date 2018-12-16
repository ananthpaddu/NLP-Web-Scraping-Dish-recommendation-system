#MGMT 590 (SECTION 2) GROUP 11 PROJECT STATISTICAL MODELING CODE

#clearing Global Environment
rm(list = ls())

#obtaining working directory
getwd()

#import necessary libraries for reading files, data and string manipulation
library(readr)
library(dplyr)
library(stringr)

#reading, and cleaning Restaurant Price data file of all San Francisco restaurants into R
price_data <- read.csv(file = "Web_data/Price.csv", sep = "|")
price_data$Price <- gsub(pattern = ' - \\$+', replacement = '', x = price_data$Price)

#subsetting price column which contain $ strings into a categorized price column
price_data$Price_fig[price_data$Price=='$'] <- 1
price_data$Price_fig[price_data$Price=='$$'] <- 2
price_data$Price_fig[price_data$Price=='$$$'] <- 3
price_data$Price_fig[price_data$Price=='$$$$'] <- 4

#reading sentiment analysis result dataset from TripAdvisor
rating_data <- read_delim("~/Web_data/output2.csv",
                                    "\t", 
                                    escape_double = FALSE, 
                                    col_types = cols(Restaurant = col_skip(),
                                                     X1 = col_skip()), trim_ws = TRUE)

#descriptive statistics on price_data
summary(price_data)
#rating_data$rest_name <- split(x = rating_data$restaurant_name, '\r')[1]

#cleaning restaurant_name column objects 
foo <- data.frame(do.call('rbind', strsplit(as.character(rating_data$restaurant_name),'\r\n',fixed=TRUE)))
rating_data$rest_name <- trimws(gsub(pattern = '0 ', replacement = '', x = foo$X1))

#merging rating_data and restaurant price datasets into one 
complete_data <- merge(rating_data, price_data, by.x = 'rest_name', by.y = 'Restaurant_name') 

#remove duplicated elements/rows from merged dataset and performing descriptive statistics 
deduped_data <- unique(complete_data)
summary(deduped_data)


#
deduped_data$rest_name <- NULL
deduped_data$restaurant_name <- NULL
deduped_data$Price <- NULL


#checking for column names of newly merged dataset
colnames(deduped_data)
#[1] "noodle"                    "pizza"                     "roll"                      "burger"                   
#[5] "taco"                      "sandwich"                  "Overall_rating"            "restaurant_food_rating"   
#[9] "restaurant_rating"         "restaurant_service_rating" "restaurant_value_rating"   "Price_fig" 


#correlation check between overall_rating and restaurant_rating variables in deduped _data
cor(deduped_data$Overall_rating, deduped_data$restaurant_rating)

#correlation plot
plot(deduped_data$Overall_rating, deduped_data$restaurant_rating)



# t-test for checking if dish sentiment keywords were significant on food_rating
t.test(deduped_data$restaurant_food_rating, deduped_data$noodle)
t.test(deduped_data$restaurant_food_rating, deduped_data$pizza)
t.test(deduped_data$restaurant_food_rating, deduped_data$roll)
t.test(deduped_data$restaurant_food_rating, deduped_data$burger)
t.test(deduped_data$restaurant_food_rating, deduped_data$taco)
t.test(deduped_data$restaurant_food_rating, deduped_data$sandwich)

# Remove dish sentiment from food rating
#for noodle
orth_model <- lm(restaurant_food_rating~noodle, data = deduped_data)
deduped_data$restaurant_food_rating_adj_noodle <- orth_model$residual

#for pizza
orth_model <- lm(restaurant_food_rating~pizza, data = deduped_data)
deduped_data$restaurant_food_rating_adj_pizza <- orth_model$residual

#for roll
orth_model <- lm(restaurant_food_rating~roll, data = deduped_data)
deduped_data$restaurant_food_rating_adj_roll <- orth_model$residual

#for burger
orth_model <- lm(restaurant_food_rating~burger, data = deduped_data)
deduped_data$restaurant_food_rating_adj_burger <- orth_model$residual

#for taco
orth_model <- lm(restaurant_food_rating~taco, data = deduped_data)
deduped_data$restaurant_food_rating_adj_taco <- orth_model$residual

#for sandwich
orth_model <- lm(restaurant_food_rating~taco, data = deduped_data)
deduped_data$restaurant_food_rating_adj_taco <- orth_model$residual



#preliminary linear regression 
prelim_model <- lm(Overall_rating~restaurant_food_rating + restaurant_service_rating + 
                     restaurant_value_rating, 
                   data = deduped_data)

#checking results for insights on significant variables
summary(prelim_model)

#second preliminary linear regression including price as an independednt variable
prelim_model2 <- lm(Overall_rating~restaurant_food_rating + restaurant_service_rating + 
                      restaurant_value_rating + Price_fig, 
                    data = deduped_data)
#results check
summary(prelim_model2)


############################################################  
# Price is not a determining factor!! Clearly seen below

#correlation between overall_rating and price
cor(deduped_data$Overall_rating, deduped_data$Price_fig)

#graphical correlation plot
plot(deduped_data$Overall_rating, deduped_data$Price_fig)
###########################################################


# Target Models: Testing for dish sentiment keyword variables in regression model

#adding noodle sentiments polarity to regression
model1 <- lm(Overall_rating~restaurant_food_rating + restaurant_service_rating + 
               restaurant_value_rating + noodle, 
             data = deduped_data)
summary(model1)

#adding pizza
model2 <- lm(Overall_rating~restaurant_food_rating + restaurant_service_rating + 
               restaurant_value_rating + pizza, 
             data = deduped_data)
summary(model2)

#adding roll
model3 <- lm(Overall_rating~restaurant_food_rating + restaurant_service_rating + 
               restaurant_value_rating + roll, 
             data = deduped_data)
summary(model3)

#adding burger
model4 <- lm(Overall_rating~restaurant_food_rating + restaurant_service_rating + 
               restaurant_value_rating + burger, 
             data = deduped_data)
summary(model4)

#adding taco
model5 <- lm(Overall_rating~restaurant_food_rating + restaurant_service_rating + 
               restaurant_value_rating + taco, 
             data = deduped_data)
summary(model5)

#adding pasta
model6 <- lm(Overall_rating~restaurant_food_rating + restaurant_service_rating + 
               restaurant_value_rating + pasta, 
             data = deduped_data)
summary(model6)

#adding steak
model7 <- lm(Overall_rating~restaurant_food_rating + restaurant_service_rating + 
               restaurant_value_rating + steak, 
             data = deduped_data)
summary(model7)

#adding pie
model8 <- lm(Overall_rating~restaurant_food_rating + restaurant_service_rating + 
               restaurant_value_rating + pie, 
             data = deduped_data)
summary(model8)

#from result of models 1 to 8,
# Seems like signature main course of cuisines have an significant effect


## What if I don't use food, I want to see the independent contribution ##
#add noodle
model_1 <- lm(Overall_rating~restaurant_service_rating + 
               restaurant_value_rating + noodle, 
             data = deduped_data)
summary(model_1)

#add pizza
model_2 <- lm(Overall_rating~restaurant_service_rating + 
               restaurant_value_rating + pizza, 
             data = deduped_data)
summary(model_2)

#add roll
model_3 <- lm(Overall_rating~restaurant_service_rating + 
               restaurant_value_rating + roll, 
             data = deduped_data)
summary(model_3)

#add burger
model_4 <- lm(Overall_rating~ restaurant_service_rating + 
               restaurant_value_rating + burger, 
             data = deduped_data)
summary(model_4)

#add taco
model_5 <- lm(Overall_rating~ restaurant_service_rating + 
               restaurant_value_rating + taco, 
             data = deduped_data)
summary(model_5)

#add pasta
model_6 <- lm(Overall_rating~ restaurant_service_rating + 
               restaurant_value_rating + pasta, 
             data = deduped_data)
summary(model_6)

# Answer: Sometimes
# All these dishes surely drive the rating. But, their contribution must be studied in conjunction with the other foods.
# In short, people do not seem to be interested in a particular food, but, instead in the meal.


# What if I include price? Any change to the model?
#add noodle
modelp_1 <- lm(Overall_rating~restaurant_food_rating + 
                 restaurant_service_rating + Price_fig + 
                restaurant_value_rating + noodle, 
              data = deduped_data)
summary(modelp_1)

#add pizza
modelp_2 <- lm(Overall_rating~restaurant_food_rating + 
                 restaurant_service_rating + Price_fig + 
                restaurant_value_rating + pizza, 
              data = deduped_data)
summary(modelp_2)

#add roll
modelp_3 <- lm(Overall_rating~restaurant_food_rating + 
                 restaurant_service_rating + Price_fig + 
                restaurant_value_rating + roll, 
              data = deduped_data)
summary(modelp_3)

#add burger
modelp_4 <- lm(Overall_rating~restaurant_food_rating +
                 restaurant_service_rating + 
                restaurant_value_rating + Price_fig + burger, 
              data = deduped_data)
summary(modelp_4)

#add taco
modelp_5 <- lm(Overall_rating~ restaurant_food_rating +
                 restaurant_service_rating  + 
                restaurant_value_rating+ Price_fig + taco, 
              data = deduped_data)
summary(modelp_5)

#add pasta
modelp_6 <- lm(Overall_rating~ restaurant_food_rating + 
                 restaurant_service_rating + Price_fig + 
                restaurant_value_rating + pasta, 
              data = deduped_data)
summary(modelp_6)

#add steak
modelp_6 <- lm(Overall_rating~ restaurant_food_rating + 
                 restaurant_service_rating + Price_fig + 
                 restaurant_value_rating + steak, 
               data = deduped_data)
summary(modelp_6)

#add pie
modelp_6 <- lm(Overall_rating~ restaurant_food_rating + 
                 restaurant_service_rating + Price_fig + 
                 restaurant_value_rating + pie, 
               data = deduped_data)
summary(modelp_6)

# Regressing Dish keywords only against overall ratings

#noodle only
model_st1 <- lm(Overall_rating~noodle,
                data = deduped_data)
summary(model_st1)

#pizza only
model_st2 <- lm(Overall_rating~pizza,
                data = deduped_data)
summary(model_st2)

#roll only
model_st3 <- lm(Overall_rating~roll,
                data = deduped_data)
summary(model_st3)

#burger only
model_st4 <- lm(Overall_rating~burger,
                data = deduped_data)
summary(model_st4)

#taco only
model_st5 <- lm(Overall_rating~taco,
                data = deduped_data)
summary(model_st5)

#sandwich only
model_st6 <- lm(Overall_rating~sandwich,
                data = deduped_data)
summary(model_st6)


