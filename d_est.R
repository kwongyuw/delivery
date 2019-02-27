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
  summarize(n=n(), taste=first(taste), envir=first(envir), service=first(service), avgexp_lo=first(avgexp_lo), avgexp_up=first(avgexp_up), 
            tag1 = first(tag1), tag2=first(tag2), tag3=first(tag3), tag4=first(tag4),
            actexp = mean(price+rider_income)/100, avgfee=mean(rider_income)/100) %>% 
  arrange(desc(n)) %>% ungroup()
test <- mutate(test, avgexp=(avgexp_lo+avgexp_up)/2, size=sum(test$n), shares=n/size)
test <- filter(test, !is.na(tag1)) # check the availability of tag1 scraped
dmy <- dummyVars(test %>% select(tag1), data=test)
test <- cbind(test, data.frame(predict(dmy, newdata = test)))
test <- filter(test, !is.na(avgexp_lo)) %>% filter(n>=5) #note: sum(test$share)!=1 aft filtering

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







