#Kwong-Yu Wong
#Problem Set 1 
rm(list=ls())
library(R.matlab)           #necessary to read .mat file
library(EnvStats)
library(dplyr)
library(ggplot2)
library(lubridate)
library(caret)

#path to read data files
setwd("/Users/kwongyu/Google Drive/dwb/dwb_Data")
#df_DEst <- read.csv('data.csv', stringsAsFactors = FALSE) %>% #only focus on the best subset
df_DEst <- df_all %>%
  select(id:sup_id, place_tm, require_tm, finish_tm, rider_income, price, name, from_addr_xms:service, from_addr, tag1:tag4)
#take Sept 3rd as example, 437 sup ordered >=5 times account for ~70%(4067/6448), 1119 sups >=2 times ~90%(5967/6448)
#still need to 400+ restaurants (300+ even with only restaurant w/ info)
oneday <- filter(df_DEst, day(df_DEst$require_tm)==3, month(df_DEst$require_tm)==9)
#test <- oneday %>% add_count(sup_id, sort=TRUE)
test <- group_by(oneday,sup_id, name) %>% 
  summarize(n=n(), taste=first(taste), envir=first(envir), service=first(service), 
            avgexp_lo=first(avgexp_lo), avgexp_up=first(avgexp_up), avgexp=(avgexp_lo+avgexp_up)/2,
            tag1 = first(tag1), tag2=first(tag2), tag3=first(tag3), tag4=first(tag4), 
            tag_cnfast = (tag1=="中式快餐简餐"), tag_jp = (tag1==("日本菜")|tag1==("寿司")), tag_snack=(tag1=="甜点饮品"),
            actexp = mean(price+rider_income)/100, avgfee=mean(rider_income)/100, avgdist=mean(dist)) %>% 
  arrange(desc(n)) %>% ungroup()
test <- mutate(test, size=sum(test$n), shares=n/size)
#tag1 into all dummys
#test <- filter(test, !is.na(tag1)) # check the availability of tag1 scraped
#dmy <- dummyVars(test %>% select(tag1), data=test)
#test <- cbind(test, data.frame(predict(dmy, newdata = test)))
test <- filter(test, !is.na(avgexp_lo)) %>% filter(n>=5) #note: sum(test$share)!=1 aft filtering

#logit or OLS
# my choice of restaurant on  Xj: taste, envir, service, price lv, cuisine, expected delay, closeness(avgfee, close to other restaurants)
#                             Xi: spending habit, cuisine habit, purchased before

#purchased before
df_all <- arrange(df_all, place_tm) %>%
  group_by(user_id, sup_id, name) %>%
  arrange(user_id, sup_id, finish_tm) %>%
  mutate(ss_b4 = if_else(sup_id==lag(sup_id), 1, 0, missing=0), ss=cumsum(ss_b4)) %>%
  group_by(user_id) %>% arrange(user_id, place_tm) %>%
  #past average

#logit
test <- select(ungroup(df_all), id, user_id, sup_id, name, ss, ss_b4, user_exp, u_n_tt, place_tm, finish_tm, 
               price, rider_income,
               taste, envir, service, avgexp_lo, avgexp_up, tag1:tag4, u_price_avg, u_price_sd,
               from_tel, from_addr) %>%
        filter(u_n_tt>1) %>% mutate(chosen=1, avgexp = (avgexp_lo+avgexp_up)/2)
#summary(lm(shares ~ taste + envir + service + avgexp*tag_cnfast*tag_snack, data=test))
#assume last diff store is the only alternative considered
alt <- read.csv("alt_for.csv", stringsAsFactors = FALSE)
#===========4 HOURS to run raw========================
#alt <- test %>%
#        arrange(user_id, sup_id, finish_tm) %>%
#        mutate(chosen=0, asup_id=NA, aname=NA, ass=NA, ass_b4=NA,
#               afrom_tel=NA, afrom_addr=NA) #, asup_id=if_else(sup_id==lag(sup_id), aname=lag(name,ss+1)))
#for-looply matching each restaurant to last-ordered-diff restaurant
#tl = Sys.time()
#for (i in c(2:dim(alt)[1])) {
#  ref <- i - (alt$ss[i] + 1)
#  if (i %% 10000 == 0) {
#    cat(i)
#    cat(Sys.time() - tl)
#    write.csv(alt[1:i,],"alt_for.csv")
#  }
#  alt$asup_id[i] = alt$sup_id[ref]
#  alt$aname[i] = alt$name[ref]
#  alt$afrom_tel[i] = alt$from_tel[ref]
#  alt$afrom_addr[i] = alt$from_addr[ref]
#  alt$ass[i] = alt$ss[ref]
#  alt$ass_b4[i] = alt$ss_b4[ref]
#}
#===========4 HOURS to run raw========================

temp_sup <- select(test, sup_id, from_tel, from_addr, name, taste:tag4, avgexp) %>%
  unique()

alt <- left_join(alt, temp_sup, by=c("asup_id"="sup_id", "aname"="name", "afrom_tel"="from_tel", "afrom_addr"="from_addr"))
alt_tobind <- mutate(alt, sup_id = asup_id, name=aname, from_tel=afrom_tel, from_addr=afrom_addr,
                     taste=taste.y, envir=envir.y, service=service.y,
                     avgexp = avgexp.y, avgexp_lo=avgexp_lo.y, avgexp_up=avgexp_up.y, 
                     tag1=tag1.y, tag2=tag2.y, tag3=tag3.y, tag4=tag4.y,
                     ss=ass, ss_b4=ass_b4) %>%
              select(id:finish_tm, taste:tag4, avgexp, u_price_avg:chosen) %>%
              filter(!is.na(taste), !is.na(envir), !is.na(service), !is.na(avgexp))
alt_tobind$place_tm <- as.POSIXct(alt_tobind$place_tm)
alt_tobind$finish_tm <- as.POSIXct(alt_tobind$finish_tm)


df_log <- bind_rows(test,alt_tobind)  %>%
  filter(!is.na(taste), !is.na(envir), !is.na(service), !is.na(avgexp))
clean_choice <- group_by(df_log, id) %>%
  summarize(n=n()) %>% filter(n>1)
df_log <- filter(df_log, id %in% clean_choice$id) %>%
  arrange(id)
  
reg <- glm(chosen ~ taste + envir + service + avgexp*u_price_avg, data=df_log, family=binomial)
summary(reg)
df_log$fit <- reg$fitted.values
df_log$resid <- reg$residuals

sth <- sample(df_log$id,5)
#play <- filter(df_log, id %in% sth) %>% arrange(id)
#select(play, id, user_id, taste:service, resid, fit, chosen, name)
#play$utility <- reg$coef[1] + t((reg$coef[2:4]) %*% t(select(play, taste:service))) + 
#  (reg$coef[5]*play$avgexp + reg$coef[6]*play$u_price_avg) + reg$coef[7]*play$avgexp*play$u_price_avg +
#  play$resid
#select(play, id, taste:service, avgexp, resid, utility, fit, chosen, name)

df_log <- df_log %>%
  mutate(Eu = reg$coef[1] + t((reg$coef[2:4]) %*% t(select(df_log, taste:service))) + 
              (reg$coef[5]*df_log$avgexp + reg$coef[6]*df_log$u_price_avg) + reg$coef[7]*df_log$avgexp*df_log$u_price_avg)
  
#simulate logistic eps by order id s.t. 
#chosen: Eu + eps_sim > alt: Eu + eps_sim using 
df_log$eps_sim <- rlogis(dim(df_log)[1])
tl <- Sys.time()
while (dim(df_resim)[1] > 0) {
  df_resim <- df_log
  resim_check <- df_resim %>% group_by(id) %>% 
    summarize(chosen_u = max(chosen*(Eu + eps_sim)), 
              alt_u = max((1-chosen)*(Eu + eps_sim))) %>%
    filter(chosen_u<alt_u)
  if (!(length(resim_check$id) > 1)) {
    break
    df_log <- filter(df_log, !(id %in% resim_check$id))
    }
  df_resim <- filter(df_log, id %in% resim_check$id)
  df_log$eps_sim[which(df_log$id %in% resim_check$id)] <- rlogis(dim(df_resim)[1])
  print(length(resim_check$id))
}
print(Sys.time() - tl)

#Utility comparison using logistic (mean=0, sd=1) simulation
#(focus on util diff for now)
df_log <- mutate(df_log, u = Eu + eps_sim) %>%
  group_by(id) %>% 
  mutate(Eu_avg=mean(Eu), u_avg=mean(u), Echosen = max(Eu), Echosen = (Eu==Echosen),
         util_loss = sum(u*Echosen)-u) %>% 
  ungroup() %>% arrange(id)
# 1/3 customers change choice IF before realizing shock
#sum(df_log$Echosen * df_log$chosen)

filter(df_log, id %in% sth) %>%
  select(id, Eu, eps_sim, u, Echosen, chosen, util_loss,Eu_avg, u_avg) 
  
result <- filter(df_log, chosen==1)
#assuming stdd logistic uncertainty, 
#util loss ranges from -2.4(25p) to -0.1(75p)
summary(filter(result, util_loss!=0)$util_loss)
g_loss <- ggplot(filter(result, util_loss!=0), aes(x=util_loss)) + 
          stat_bin(binwidth = 0.1) + coord_cartesian(xlim=c(-10,6))

#monetary estimate
# log(p/(1-p)) = β0 + β1Χ -> fit=p which has not realized shock ε
# so should use β0 + β1X + ε = 2.00xx or -2.00xx (i.e. u defined above)
# assume MV = (1) f^u or (2) f*(e^u)
#  find f s.t. MV(f) >= Price 
# (try universal f -> user-specific f -> order-specific f)
# still problematic: e^2.0/e^-2.0 = 54, as large as 60times diff

#(TOTAL NON-SENSE) 
#result$f <- ((result$price/100)^(1/result$u))
#df_log <- left_join(df_log, select(result, id, f), by="id")
#df_log$p_sim <- df_log$f ^ df_log$u
#try fixing -> still weird once f switched btw >1 and <1
result$f2 <- (((result$price/100)/result$avgexp)^(result$u)) 
# shd be ^1/u not ^u, but f2 is around 1, so sensitive to power fcn-form indeed
df_log <- left_join(df_log, select(result, id, f2), by="id")
df_log$p_sim2 <- (df_log$f2 ^ (1/df_log$u))*df_log$avgexp
print(head(select(df_log, id, u, taste, envir, service, avgexp, price, f2, p_sim2),20))
print(format(head(df_log$p_sim,20),scientific=FALSE))

















# BLP is feasible. work out logit on sup_char & ind_char first
# (then affine transform to >= price+fee. move on to estimate cost of advanced order)

#=====================BLP====================
#GMM-required matrices
R <- cbind(test$taste, test$envir, test$service, test$avgexp) #data that need coef estimated on
# 283*4
#, test$tag1甜点饮品, test$tag1中式快餐简餐, test$tag1寿司
othX <- matrix(colSums(R[,-(4)]),nrow=dim(test)[1],ncol=3,byrow=TRUE) #:7
othX <- othX - R[,-(4)]
H <- cbind(R[,-(4)],othX) #IV
# 283*6
W0 <- diag(dim(H)[2])
J_gmm <- c()

J <- dim(R)[1]
M <- 1

#Assume same-day platform users as whole market
N<-test$size[1]
v <- matrix(0,nrow=N,ncol=M)
for (m in 1:M) {
  v[,m] <- rlnorm(N)
  #  v[,m] <- revd(N)
}
#loops set up
tol1 <- 10^(-6)                        #tolerance level for optimizing d
fr <- -0.2                          #grid search is used in R for 1-dimensional optimization
to <- 0.3

d <- matrix(5,nrow=J,ncol=M)        #at given theta, d for all goods in all mkts
d_new <- matrix(0,nrow=J,ncol=M)    #at given theta, updated d
s_hat <- matrix(0,nrow=J, ncol=M)   #estimated mkt share
ind_pr <- matrix(0,nrow=J, ncol=N)  #individual probability
dintheta <- list()                  #for all attempted theta(sig_a here), d, like grids
betalp_th <- matrix(0,nrow=dim(R)[2],ncol=1)  #for all sig_a, corresp. beta & alpha
count <- 0                          #counting theta attempted
ind_pr_all <- matrix(0,nrow=(J*M),ncol=N)

#Let the sig_a(theta2) party begins (i.e. main estimation fcn)
gmmFcn <- function(sig_a) {
  ptm <- proc.time()
  d <- matrix(5,nrow=J,ncol=M)
  d_new <- matrix(0,nrow=J,ncol=M)
  norm <- tol1 + 1
  while (norm > tol1) {
    d <- d_new
    for (m in 1:M) {
      for (j in 1:J) {
        #cat(j)
        ind_pr[j,] = exp(rep(d[j,m],N) - (sig_a*v[,m])*test$avgexp[j+(m-1)*J]) / 
          (1+rowSums(exp(matrix(d[,m],nrow=N,ncol=J,byrow=T) - (sig_a*v[,m]) %*% t(test$avgexp[((m-1)*J+1):(m*J)]))))
        #by vector manipulation, speed up to its ~38x... #ind_pr: JxN
        s_hat[j,m] <- mean(ind_pr[j,]) #mkt sh by summing over individuals  #s_hat: JxM
        #print(head(s_hat))
      }
      #print(ind_pr[1:3,1:3])
      ind_pr_all[((m-1)*3+1):((m-1)*3+J),] <<- ind_pr #ind_pr_all: J*MxN
      #print(ind_pr_all[1:3,1:3])
    }
    #print(head(d+log(test$shares) - log(s_hat)))
    d_new <- d + log(test$shares) - log(s_hat) #d: JxM
    #print(head(d_new))
    norm <- norm(d - d_new)
    print(norm)
    #print(proc.time() - ptm)
  }
  print(paste(as.character(c(proc.time() - ptm)[1]), "needed to finish delta estimate"))
  dintheta[[count+1]] <<- d 
  
  #GMM estimate: given this sig_a & hence d(sig_a), estimates beta & alpha
  betalp_th <<- cbind(betalp_th,c(0,0,0,0))
  betalp_th[,count+1] <<- solve(t(R)%*%H%*%W0%*%t(H)%*%R) %*% t(R)%*%H%*%W0%*%t(H) %*% c(d)
  J_gmm[count+1] <<- ((t(c(d) - R%*%betalp_th[,count+1])) %*% H%*%W0%*%t(H) %*% (c(d) - R%*%betalp_th[,count+1]))/(J*M)
  count <<- count + 1
  print(count)
  print(J_gmm[count])
  if (count==1) {print(proc.time() - ptm)}
  return(J_gmm[count])
}

#minimize the function
tl <- proc.time()
optgmm <- optimize(gmmFcn, c(fr,to))
print(proc.time() - tl)
betalp_th[,dim(betalp_th)[2]-1]
#store the estimates
sig_agmm <- optgmm$minimum
betalp_gmm <- betalp_th[,dim(betalp_th)[2]-1]
d_gmm <- dintheta[[length(dintheta)]]







