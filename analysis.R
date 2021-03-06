# Create regression model matrix 
library(caret)
library(tidyverse)
# detach("package:tidyverse", unload=TRUE)
library(stargazer)
#source('~/Documents/eScience/projects/delivery/dataprocess_model.R') # data from control groups
source('/Users/kwongyu/Google Drive/dwb/git/delivery/dataprocess_model.R')

library(RCurl)

script <- RCurl::getURL("https://raw.githubusercontent.com/kwongyuw/delivery/master/dataprocess_model.R")
eval(parse(text = script))

names(crt_df)


# create model matrix (ADD VARIABLES HERE)
model <- crt_df %>% 
  dplyr::select(delay, prereq, prepare, ride, left_20, left_m, left2, left2_20, left2_m,
                u_price_avg, tmref_cat,
                time, user_exp, dist, price, rider_income, paid, 
                ow_ratio, u_lunch_avg) %>%
  mutate(left_20 = as.factor(left_20), left2_20 = as.factor(left2_20),
         complic = ifelse(time>0, ride/time,NA),
         paid_ratio=ifelse(price>=paid, paid/price, NA))# add covariates

crt_df %>% select(delay) %>% head()

summary(model)

# center or scale the data ? 
#std_data <- preProcess(model[,-1], method = c("center", "scale"))
#model <-  data.frame(predict(std_data, newdata = model))

# create time of day dummies if you need a reference then use 
# fullRank = T if False then you get all the categpries as dummy vars 
dmy <- dummyVars(" ~ .", data = model, fullRank = T)
fullR_dmy <- data.frame(predict(dmy, newdata = model)) %>% na.omit()

# glm of delay  
# selct your model formula
names(fullR_dmy)

play <- sample_n(model,1000)
pairs(dplyr::select(play, delay, prereq, prepare, ride, time), cex=0.1)
#most interesting one
model_1 <- formula(delay ~ prereq +prepare+price + tmref_catlunch + tmref_catother + 
                     user_exp + ow_ratio + rider_income*complic*dist)

simple <- lm(model_1, data = fullR_dmy)
summary(simple)

lm1 <- lm(delay ~ prereq +price + tmref_catlunch + tmref_catother, data=fullR_dmy)
lm2 <- lm(delay ~ prereq +prepare +price + tmref_catlunch + tmref_catother, data=fullR_dmy)
lm3 <- lm(delay ~ prereq +prepare +price + tmref_catlunch + tmref_catother + user_exp + ow_ratio + rider_income, data=fullR_dmy)
lm4 <- lm(delay ~ prereq +prepare +price + tmref_catlunch + tmref_catother + user_exp + ow_ratio + rider_income*complic*dist, data=fullR_dmy)
stargazer(lm1, lm2, lm3, lm4, type="text", report="cvt*", omit.stat=c("f", "ser", "rsq"))

g_o <- ggplot(crt_df, aes(x=o_lat, y=o_lon)) + geom_point(size=0.01, alpha=0.1)
g_o + geom_point(aes(x=d_lat, y=d_lon),size=0.01, alpha=0.2, color="red")

# have some outliers 
########## Robust
robust <- rlm(model_1, data = fullR_dmy)
summary(robust)
###
lm1 <- lm(delay ~ prereq +prepare + price + tmref_catlunch + tmref_catother + 
            user_exp + ow_ratio + rider_income + complic + dist, data=fullR_dmy)
lm2 <- lm(delay ~ left2_20.1 +prepare + price + tmref_catlunch + tmref_catother + 
            user_exp + ow_ratio + rider_income + complic + dist, data=fullR_dmy)
lm3 <- lm(delay ~ prereq +prepare + price + tmref_catlunch + tmref_catother + 
            user_exp + ow_ratio + rider_income + complic + dist, data=filter(fullR_dmy, left2_m>20))
stargazer(lm1, lm2, lm3, type="text", report="cvt*", omit.stat=c("f", "ser", "rsq"))


############# Synthetic group 
library(Matching)
# take random sample for testing 
r_sam <- fullR_dmy %>%
  sample_n(2000)

#rownames(r_sam) <- 1:nrow(r_sam)
summary(fullR_dmy)
#The covariates we want to match on
X = r_sam %>% dplyr::select(prepare, price, tmref_catlunch, tmref_catother, 
                            user_exp, ow_ratio, rider_income, complic, dist)

#The outcome variable
Y= r_sam$delay

# Treatment
Tr = r_sam$left2_20.1
table(Tr)
#

#Let's call GenMatch() to find the optimal weight to give each
#covariate in 'X' so as we have achieved balance on the covariates in
#'BalanceMat'. This is only an example so we want GenMatch to be quick
#so the population size has been set to be only 16 via the 'pop.size'
#option. This is *WAY* too small for actual problems.
#For details see http://sekhon.berkeley.edu/papers/MatchingJSS.pdf.
#

genout <- GenMatch(Tr=Tr, X=X, estimand="ATT", pop.size = 1000, max.generations=10, wait.generations=1)

#genout

mgens <- Match(Y=Y, Tr= Tr, X = X, estimand="ATT",
               Weight.matrix = genout)

summary(mgens)

mb <- MatchBalance(Tr ~ prepare + price + tmref_catlunch + tmref_catother + 
                   user_exp + ow_ratio + rider_income + complic + dist,
                   match.out = mgens, nboots = 10, data = r_sam) 

######Prop inverse probability 
tr_model <- formula(left2_20.1 ~ prepare + price + tmref_catlunch + tmref_catother + 
                      user_exp + ow_ratio + rider_income + complic + dist)

glm1 <- glm(tr_model, family = binomial, data = fullR_dmy)
summary(glm1)

#IPW model
fullR_dmy$pihat.log <- glm1$fitted

#Calculate Weights
fullR_dmy$ipw.weights <- ifelse(fullR_dmy$left2_20.1==1, 1/fullR_dmy$pihat.log,1/(1-fullR_dmy$pihat.log))

#ATE Outcome Analysis
lm1 <- lm (delay ~ left2_20.1 + prepare + price + tmref_catlunch + tmref_catother + 
             user_exp + ow_ratio + rider_income + complic + dist, 
           data = fullR_dmy)
full_ate <- lm (delay ~ left2_20.1 + prepare + price + tmref_catlunch + tmref_catother + 
                  user_exp + ow_ratio + rider_income + complic + dist, 
                data = fullR_dmy, weight = ipw.weights )
#summary(full_ate)
stargazer(lm1, full_ate, type="text", report="cvt*", omit.stat=c("f", "ser", "rsq"))


####PCA EXPLORATORY
names(model)
pca_dat <- crt_df %>%
  dplyr::select(prereq, prepare, ride, left2_m,
         u_price_avg,
         time, user_exp, dist, price, rider_income, paid, 
         ow_ratio) %>%
  sample_n(10000)

deliver_pca <- prcomp(pca_dat,
                    center = TRUE,
                    scale. = TRUE) 
summary(deliver_pca)

library(ggfortify)

autoplot(deliver_pca, data = pca_dat, loadings = T)

autoplot(kmeans(pca_dat, 5), data = pca_dat, frame = TRUE, size=0.1)

