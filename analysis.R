# Create regression model matrix 
library(caret)

source('~/Documents/eScience/projects/delivery/dataprocess_model.R') # data from control groups
names(crt_df)

# create model matrix (ADD VARIABLES HERE)
model <- crt_df %>% 
  select(delay, left_20, left_m, u_price_avg, tmref_cat,time, user_exp, dist) # add covariates

summary(model)

# center or scale the data ? 
std_data <- preProcess(model[,-1], method = c("center", "scale"))
model <-  data.frame(predict(std_data, newdata = model))

# create time of day dummies if you need a reference then use 
# fullRank = T if False then you get all the categpries as dummy vars 
dmy <- dummyVars(" ~ .", data = model, fullRank = T)
fullR_dmy <- data.frame(predict(dmy, newdata = model))

# glm of delay  
# selct your model formula
names(fullR_dmy)

model_1 <- formula(delay ~ left_20 + u_price_avg + tmref_catlunch + tmref_catother + time + user_exp, dist)

simple <- lm(model_1, data = fullR_dmy)

summary(simple)
plot(simple)
# have some outliers 
########## Robust
library(MASS)
robust <- rlm(model_1, data = fullR_dmy)
summary(robust)
#############



