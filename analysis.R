# Create regression model matrix 
library(Matching)
library(caret)
library(tidyverse)

source('~/Documents/eScience/projects/delivery/dataprocess_model.R') # data from control groups
names(crt_df)

# create model matrix (ADD VARIABLES HERE)
model <- crt_df %>% 
  select(delay, left_20, left_m, u_price_avg, tmref_cat,time, user_exp, dist) %>%
  mutate(left_20 = as.factor(left_20))# add covariates

crt_df %>% select(delay) %>% head()

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

model_1 <- formula(delay ~ left_20.1 + u_price_avg + tmref_catlunch + tmref_catother + time + user_exp + dist)

simple <- lm(model_1, data = fullR_dmy)
summary(simple)
# have some outliers 
########## Robust
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
table(Tr)
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

genout <- GenMatch(Tr=Tr, X=X, estimand="ATE")

genout


mgens <- Match(Y=Y, Tr= Tr, X = X, estimand="ATT",
               Weight.matrix = genout)

summary(mgens)

mb <- MatchBalance(Tr ~ u_price_avg + tmref_catlunch + tmref_catother + time + user_exp + dist,
                   match.out = mgens, nboots = 10, data = r_sam) 

######Prop inverse probability 
tr_model <- formula(left_20.1 ~ u_price_avg + tmref_catlunch + tmref_catother + time + user_exp + dist)

glm1 <- glm(tr_model, family = binomial, data = fullR_dmy)
summary(glm1)

#IPW model
fullR_dmy$pihat.log <- glm1$fitted

#Calculate Weights
fullR_dmy$ipw.weights <- ifelse(fullR_dmy$left_20.1==1, 1/fullR_dmy$pihat.log,1/(1-fullR_dmy$pihat.log))

#ATE Outcome Analysis
full_ate <- lm (delay ~ left_20.1 + u_price_avg + tmref_catlunch + tmref_catother + time + user_exp + dist, data = fullR_dmy, weight = ipw.weights )
summary(full_ate)