# Create regression model matrix 
library(caret)
# install.packages("Matching", dependencies=TRUE)
library(Matching)
library(tidyverse)
# detach("package::tidyverse", unload=TRUE)

source('~/Documents/eScience/projects/delivery/dataprocess_model.R') # data from control groups
names(crt_df)


# create model matrix (ADD VARIABLES HERE)
model <- crt_df %>% 
  dplyr::select(delay, prereq, prepare, ride, left_20, left_m, left2, left2_m,
                u_price_avg, tmref_cat,
                time, user_exp, dist, price, rider_income, paid, 
                ow_ratio, u_lunch_avg) %>%
  mutate(left_20 = as.factor(left_20), paid_ratio=paid/price)# add covariates

summary(model)

# center or scale the data ? 
std_data <- preProcess(model[,-1], method = c("center", "scale"))
model <-  data.frame(predict(std_data, newdata = model))

# create time of day dummies if you need a reference then use 
# fullRank = T if False then you get all the categpries as dummy vars 
dmy <- dummyVars(" ~ .", data = model, fullRank = T)
fullR_dmy <- data.frame(predict(dmy, newdata = model)) %>% na.omit()

# glm of delay  
# selct your model formula
names(fullR_dmy)

play <- sample_n(model,100)
pairs(dplyr::select(play, delay, prereq, left_m, left2_m, ride), cex=0.1)
model_1 <- formula(delay ~ left_20.1 + u_price_avg + tmref_catlunch + tmref_catother + 
                     time + user_exp + dist)

simple <- lm(model_1, data = fullR_dmy)

summary(simple)
plot(simple)
# have some outliers 
########## Robust
library(MASS)
robust <- rlm(model_1, data = fullR_dmy)
summary(robust)
############# Synthetic group 

# take random sample for testing 
r_sam <- fullR_dmy %>%
  sample_n(20000)

#rownames(r_sam) <- 1:nrow(r_sam)
summary(fullR_dmy)
#The covariates we want to match on
X = r_sam %>% select(-delay,-left_20.1)

#The outcome variable
Y= r_sam$delay

# Treatment
Tr = r_sam$left_20.1
#
#Let's call GenMatch() to find the optimal weight to give each
#covariate in 'X' so as we have achieved balance on the covariates in
#'BalanceMat'. This is only an example so we want GenMatch to be quick
#so the population size has been set to be only 16 via the 'pop.size'
#option. This is *WAY* too small for actual problems.
#For details see http://sekhon.berkeley.edu/papers/MatchingJSS.pdf.
#

genout <- GenMatch(Tr=Tr, X=X, estimand="ATE", M=1,
                   pop.size=16, max.generations=10, wait.generations=1)

genout


mgens <- Match(Y=Y, Tr= Tr, X = X, estimand="ATT",
               Weight.matrix = genout)
summary(mgens)

mb <- MatchBalance(Tr ~ u_price_avg + tmref_catlunch + tmref_catother + time + user_exp + dist,
                   match.out = mgens, nboots = 10, data = r_sam) 


