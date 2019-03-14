library(tidyverse)
library(clValid)
library(cluster)
library(factoextra)

source('/Users/kwongyu/Google Drive/dwb/git/delivery/dataprocess_model.R')

user <- group_by(crt_df, user_id) %>%
  arrange(user_id, finish_tm) %>%
  summarize(u_n_tt=n(), u_rinc_avg=mean(rider_income, na.rm=TRUE), u_rinc_sd=sd(rider_income),
            u_req_avg=mean(as.numeric(require_tm)), u_req_sd=sd(require_tm),
            u_price_avg=mean(price), u_price_sd=sd(price), u_span=max(as.numeric(finish_tm))-min(as.numeric(place_tm)))
user <- filter(user, u_n_tt<=10, u_rinc_avg<1100, u_price_avg<10000)

sum(is.na(user$u_rinc_avg))
user$u_rinc_sd <- NULL
user$u_req_sd <- NULL
user$u_price_sd <- NULL
user$u_span <- NULL

any(is.na(user))


set.seed(715)
small <- sample_n(user, 10000)
user_sc <- as.data.frame(scale(select(small, -user_id)))

dist_mat <- dist(user_sc, method="euclidean")
hclust_avg <- hclust(dist_mat, method='average')
plot(hclust_avg, labels=FALSE)
rect.hclust(hclust_avg , k = 10, border = 2:6)
cut_avg <- cutree(hclust_avg, k = 10) 
small_cl <- mutate(small, hie_cl = cut_avg)
count(small_cl,hie_cl)
#dunn(cluster=small_cl$hie_cl, Data=small_cl)
#k=4: 0.0013 vs k=10: 0.0003

kmean_cl <- kmeans(user_sc, 10)
#small_cl$kme_cl <- kmean_cl$cluster
#count(small_cl, kme_cl)
#table(small_cl$hie_cl, small_cl$kme_cl)
table(kmean_cl$cluster)
fviz_nbclust(user_sc, kmeans, method = "wss")
fviz_nbclust(user_sc, kmeans, method = "silhouette")
#gap_stat <- clusGap(user_sc, FUN = kmeans, nstart = 25,
#                    K.max = 10, B = 50)
#fviz_gap_stat(gap_stat)
kmean_cl <- kmeans(user_sc, 5)
fviz_cluster(kmean_cl, data=user_sc)
