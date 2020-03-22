# set packages and libraries
if(!require("psych")){install.packages("psych")}
if(!require("cluster")){install.packages("cluster")}
if(!require("fpc")){install.packages("")}
if(!require("sqldf")){install.packages("sqldf")}
if(!require("dplyr")){install.packages("dplyr")}
if(!require("tidyr")){install.packages("tidyr")}
library(MLmetrics)
library(dplyr)
library(tidyr)
library(proxy)


#------------------------------------------ read in data -----------------------------------------------------#

setwd("C:/Users/cxie/Desktop/Recommendation tools/Group assignment/Data-20200227")
getwd()

lfm_art <- read.delim("Artists.dat", header = TRUE, sep = "\t") %>% select(id, name)
lfm_tags <- read.delim("tags.dat", header = TRUE, sep = "\t")
lastfm <- read.delim("user_artists.dat", header = TRUE, sep = "\t")
user_tags <- read.delim("user_taggedartists.dat", header = TRUE, sep = "\t") %>% select(userID, artistID, tagID)


#------------------------------ create the matrix for collaborative filtering -------------------------------------#

#### Categorization of data with percentile ####

Percentile_00  = min(lastfm$weight)
Percentile_10  = quantile(lastfm$weight, 0.10)
Percentile_20  = quantile(lastfm$weight, 0.20) 
Percentile_30  = quantile(lastfm$weight, 0.30) 
Percentile_40  = quantile(lastfm$weight, 0.40)
Percentile_50  = quantile(lastfm$weight, 0.50) 
Percentile_60  = quantile(lastfm$weight, 0.60) 
Percentile_70  = quantile(lastfm$weight, 0.70) 
Percentile_80  = quantile(lastfm$weight, 0.80)
Percentile_90  = quantile(lastfm$weight, 0.90)
Percentile_100 = max(lastfm$weight) 

RB = rbind(Percentile_00, Percentile_10, Percentile_20, Percentile_30, Percentile_40, Percentile_50, Percentile_60, Percentile_70, Percentile_80, Percentile_90, Percentile_100)
dimnames(RB)[[2]] = "Value"
RB

lastfm$weight[lastfm$weight >= Percentile_00 & lastfm$weight <  Percentile_10]  = 1
lastfm$weight[lastfm$weight >= Percentile_10 & lastfm$weight <  Percentile_20]  = 2
lastfm$weight[lastfm$weight >= Percentile_20 & lastfm$weight <= Percentile_30] = 3
lastfm$weight[lastfm$weight >= Percentile_30 & lastfm$weight <= Percentile_40] = 4
lastfm$weight[lastfm$weight >= Percentile_40 & lastfm$weight <= Percentile_50] = 5
lastfm$weight[lastfm$weight >= Percentile_50 & lastfm$weight <  Percentile_60]  = 6
lastfm$weight[lastfm$weight >= Percentile_60 & lastfm$weight <  Percentile_70]  = 7
lastfm$weight[lastfm$weight >= Percentile_70 & lastfm$weight <= Percentile_80] = 8
lastfm$weight[lastfm$weight >= Percentile_80 & lastfm$weight <= Percentile_90] = 9
lastfm$weight[lastfm$weight >= Percentile_90 & lastfm$weight <= Percentile_100] = 10


# cleanup foreign characters in artist names: most will be converted to '?'
lfm_art$name <- iconv(lfm_art$name, from = "UTF-8", to = "ASCII//TRANSLIT")

# calc number of users listening to each artist
a_users <- arrange(summarise(group_by(lastfm, artistID), 
                             TotalUsers = length(unique(userID)) ), desc(TotalUsers) )

# truncate at top 1000
top_1000 <- a_users[1:1000,]

# find names of artists with most last.fm fans
most_fans <- subset(top_1000, artistID %in% lfm_art$id)

# re-arrange sort order to enable proper link to artist name
most_fans <- arrange(most_fans, artistID, TotalUsers)

# get names of artists
mf_names <- subset(lfm_art, id %in% most_fans$artistID)

most_fans$Name <- mf_names$name[mf_names$id %in% most_fans$artistID]

most_fans <- arrange(most_fans, desc(TotalUsers))

missing <- subset(top_1000, !(artistID %in% most_fans$artistID))

length(missing$artistID)

# remove all items not in top 1000 artist list
last_sm <- subset(lastfm, artistID %in% top_1000$artistID)

# remove all artist ID's missing from artists.dat file
last_sm <- subset(last_sm, !(artistID %in% missing$artistID))

# form new master list of valid artist ID's excluding the 182 missing ones
top_818 <- subset(top_1000, !(artistID %in% missing$artistID))

rm(top_1000)

length(unique(lfm_tags$tagID))

# count entries in file
nrow(user_tags)

# count distinct users
length(unique(user_tags$userID))

# count distinct artists
length(unique(user_tags$artistID))

summary(summarise(group_by(user_tags, userID),
                  TotalTags = length(userID == userID) )$TotalTags )

summary(summarise(group_by(user_tags, userID),
                  TotalUTags = length(unique(tagID)) )$TotalUTags )


# calc number of users listening to each artist
tag_counts <- arrange(summarise(group_by(user_tags, tagID), 
                                TotalUsers = length(unique(userID)) ), desc(TotalUsers) )

summary(tag_counts$TotalUsers)


par(mfrow=c(1,2))
hist(tag_counts$TotalUsers, col = "yellow", main = "Dist. of # of Genre Taggings", breaks = 50, xlab = "Number of Listeners")

boxplot(tag_counts$TotalUsers, col = "yellow", main = "Dist. of # of Genre Taggings", ylab = "Number of Listeners")


# truncate at top 200
tag_200 <- tag_counts[1:200,]

tag_200 <- arrange(tag_200, tagID)

# get tag names
tag_200$Names <- subset(lfm_tags, tagID %in% tag_200$tagID)$tagValue

# sort by number of users
tag_200 <- arrange(tag_200, desc(TotalUsers))


# truncate user-taggedartists to top 200 tagID's
u_toptags <- subset(user_tags, tagID %in% tag_200$tagID)

# count distinct artists
length(unique(u_toptags$artistID))

# truncate user-taggedartists to top 818 artists
u_toptags <- subset(u_toptags, artistID %in% top_818$artistID)

# count distinct artists
length(unique(u_toptags$artistID))


# calculate the number of times a genre tag has been applied to a given artist
u_tt <- summarise(group_by(u_toptags, artistID, tagID ),
                  Count = length(tagID) )

# count distinct artists
length(unique(u_tt$artistID))

# get a list of artists that haven't been tagged with one of top 200 tags
not_tagged <- subset(top_818, !(artistID %in% u_toptags$artistID))
not_tagged # all have relatively low user counts so OK to discard


# check to see whether artists have been tagged at all
not_tagged$artistID %in% user_tags$artistID

top_815 <- subset(top_818, artistID %in% u_toptags$artistID)
rm(top_818)

# count distinct artists
length(unique(top_815$artistID))

# remove all artist ID's missing from artists.dat file
last_sm <- subset(last_sm, artistID %in% top_815$artistID)

# count distinct users
length(unique(last_sm$userID))


#### CREATING USER-ARTIST MATRIX ###

# convert to wide format
l_mat <- spread(last_sm, artistID, weight)

# save UserIDs and remove 1st column from matrix
#user_ids <- as.vector(l_mat$userID)

rownames(l_mat) <- l_mat$userID

# create a matrix using cols 2 -> ncol of l_mat
lr_mat <- as.matrix(l_mat[,2:ncol(l_mat)])
#lr_mat[is.na(lr_mat)] <- 0

# calc number of ratings in matrix
nratings <- length(as.vector(lr_mat))
nratings

# calc density of matrix = 0.0337
sum(!is.na(as.vector(lr_mat)) ) / nratings


#------------------------------------ create the matrix for content-based --------------------------------------#

# convert to wide format
tmp_mat <- spread(u_tt, tagID, Count)

tmp_mat <- data.frame(tmp_mat)

# save artistIDs and remove 1st column from matrix
#ag_artistID <- as.vector(tmp_mat$artistID)
rownames(tmp_mat) <- tmp_mat$artistID

# create a matrix using cols 2 -> ncol of l_mat
ag_mat <- as.matrix(tmp_mat[,2:ncol(tmp_mat)])

ag_mat[is.na(ag_mat)] <- 0


rm(tmp_mat)


# calc number of ratings in matrix
ntags <- length(as.vector(ag_mat))
ntags


# calc density of matrix = 0.091
sum(!is.na(as.vector(ag_mat)) ) / ntags

# calc sparsity of matrix
1 - sum(!is.na(as.vector(ag_mat)) ) / ntags




#ag_mat (artist and tags) and lr_mat (users and artists) are tha tables


#---------------------------------------------------------------------------------------------------------------#
#---------------------------------- create the function for content-based --------------------------------------#
#---------------------------------------------------------------------------------------------------------------#

ContentBased <- function(product_data, test_data, N, NN, onlyNew=TRUE){
  
  # Similarity calculation (stolen from user-based CF)
  similarity_matrix <- as.matrix(simil(product_data, method="pearson"))
  
  print("Similarity calculation done")
  
  # Set Nearest neighbors (stolen from user-based CF)
  similarity_matrix_NN <- similarity_matrix
  
  for (k in 1:nrow(similarity_matrix_NN)){
    crit_val <- -sort(-similarity_matrix_NN[k,])[NN]
    similarity_matrix_NN[k,] <- ifelse(similarity_matrix_NN[k,] >= crit_val, similarity_matrix_NN[k,], 0)
  }
  
  similarity_matrix_NN[is.na(similarity_matrix_NN)] <- 0
  test_data2 <- test_data
  test_data2[is.na(test_data2)] <- 0
  
  print("Nearest neighbor selection done")
  
  ### Prediction (stolen from item-based CF) ###
  prediction <- matrix(, nrow=nrow(test_data), ncol=ncol(test_data), 
                       dimnames=list(rownames(test_data), colnames(test_data)))
  prediction2 <- matrix(, nrow=nrow(test_data), ncol(test_data), 
                        dimnames=list(rownames(test_data), colnames(test_data)))
  TopN <- matrix(, nrow=nrow(test_data), N, dimnames=list(rownames(test_data)))
  
  for (u in rownames(test_data)){
    # Numerator
    Num <-  test_data2[u, ] %*% similarity_matrix_NN
    
    # Denominator
    Denom <- colSums(similarity_matrix_NN, na.rm=TRUE)
    
    # Prediction
    prediction[u, ] <- Num/Denom
    
    if (onlyNew == TRUE){
      unseen <- names(test_data[u, is.na(test_data[u,])])
      prediction2[u, ] <- ifelse(colnames(prediction) %in% unseen, prediction[u, ], NA)
    }else{
      prediction2[u, ] <- prediction[u, ]
    }
    
    TopN[u, ] <- names(-sort(-prediction2[u, ])[1:N])
    
  }
  
  print("Prediction done")
  
  res <- list(prediction, TopN)
  names(res) <- c('prediction', 'topN')
  
  return(res)
}


######  compute content-based and evalute the prediction #######

CB <- ContentBased(ag_mat, lr_mat, 3, 15, onlyNew=T)

pred <- as.data.frame(CB$prediction)
TopN <- as.data.frame(CB$topN)


MAE(CB$prediction, lr_mat)
F1_Score(CB$prediction, lr_mat, 5)



#---------------------------------------------------------------------------------------------------------------#
#----------------------------------- User-based collaborative filtering ----------------------------------------#
#---------------------------------------------------------------------------------------------------------------#


### Step 1: Calculating similarities ###

lr_mat1 <- lr_mat

similarity_matrix <- as.matrix(simil(lr_mat1, method="pearson"))

### UBCF explained - Step 2: Retain nearest neighbors ###


# Keep nearest neighbors based on similarity threshold #
hist(similarity_matrix)

threshold = 0.5

similarity_matrix_threshold <- similarity_matrix
similarity_matrix_threshold[similarity_matrix_threshold < threshold] <- NA

# Keep N nearest neighbors #
NN=10

similarity_matrix_NN <- similarity_matrix

for (x in 1:nrow(similarity_matrix_NN)){
  crit_val <- -sort(-similarity_matrix_NN[x,])[NN]
  similarity_matrix_NN[x,] <- ifelse(similarity_matrix_NN[x,] >= crit_val, similarity_matrix_NN[x,], NA)
}

### UBCF explained - Step 3: Prediction ###

N = 3

prediction1 <- matrix(, nrow=nrow(lr_mat1), ncol(lr_mat1), dimnames=list(rownames(lr_mat1), colnames(lr_mat1)))
TopN1 <- matrix(, nrow=nrow(lr_mat1), N, dimnames=list(rownames(lr_mat1)))
#Preparation: Compute (Rni - mean(Ri))

for (x in rownames(lr_mat1)){
  
  ## Rni
  similarity_vector <- na.omit(similarity_matrix_NN[x, ])
  NN_norm <- lr_mat1[rownames(lr_mat1) %in% names(similarity_vector),]
  
  ## mean(Ri)
  CM <- colMeans(lr_mat1, na.rm=TRUE)
  
  ## (Rni - mean(Ri))
  for (l in 1:ncol(NN_norm)){
    NN_norm[,l] <- NN_norm[,l] - CM[l]
  }
  NN_norm[is.na(NN_norm)] <- 0
  
  # Numerator 
  ## sum[sim(u,n) * (Rni - mean(Ri))]
  Num = similarity_vector %*% NN_norm
  
  #Prediction
  ## mean(Ru + sum[sim(u,n) * (Rni - mean(Ri))]/sum(sim(u,n)))
  prediction1[x, ] =  mean(lr_mat1[x, ], na.rm=TRUE)  + (Num/sum(similarity_vector, na.rm=TRUE))
  TopN1[x, ] <- names(sort(-prediction1[x, ]))[1:N]
}


### Evaluation: Prediction Accuracy ###

# MAE
MAE(prediction1,lr_mat1)
# F1
F1_Score(prediction1,lr_mat1, 5)


#---------------------------------------------------------------------------------------------------------------#
#----------------------------------- Item-based collaborative filtering ----------------------------------------#
#---------------------------------------------------------------------------------------------------------------#


#### create item-based function
ItemBasedCF <- function(train_data, test_data, N, NN, onlyNew=TRUE){
  # Similarity
  
  similarity_matrix <- as.matrix(simil(t(train_data), method="pearson"))
  
  print("Similarity calculation done")
  # Nearest Neighbor
  similarity_matrix_NN <- similarity_matrix
  
  for (k in 1:ncol(similarity_matrix_NN)){
    crit_val <- -sort(-similarity_matrix_NN[,k])[NN]
    similarity_matrix_NN[,k] <- ifelse(similarity_matrix_NN[,k] >= crit_val, similarity_matrix_NN[,k], NA)
  }
  similarity_matrix_NN[is.na(similarity_matrix_NN)] <- 0
  
  train_data[is.na(train_data)] <- 0
  
  test_data2 <- test_data
  test_data2[is.na(test_data2)] <- 0
  
  print("Nearest neighbor selection done")
  
  ### Prediction ###
  prediction <- matrix(, nrow=nrow(test_data), ncol=ncol(test_data), 
                       dimnames=list(rownames(test_data), colnames(test_data)))
  prediction2 <- matrix(, nrow=nrow(test_data), ncol(test_data), 
                        dimnames=list(rownames(test_data), colnames(test_data)))
  TopN <- matrix(, nrow=nrow(test_data), N, dimnames=list(rownames(test_data)))
  
  for (u in rownames(test_data)){
    # Numerator
    Num <-  test_data2[u, ] %*% similarity_matrix_NN
    
    # Denominator
    Denom <- colSums(similarity_matrix_NN, na.rm=TRUE)
    
    # Prediction
    prediction[u, ] <- Num/Denom
    
    if (onlyNew == TRUE){
      unseen <- names(test_data[u, is.na(test_data[u,])])
      prediction2[u, ] <- ifelse(colnames(prediction) %in% unseen, prediction[u, ], NA)
    }else{
      prediction2[u, ] <- prediction[u, ]
    }
    
    TopN[u, ] <- names(-sort(-prediction2[u, ])[1:N])
    
  }
  
  print("Prediction done")
  
  res <- list(prediction, TopN)
  names(res) <- c('prediction', 'topN')
  
  return(res)
}


#### compute item-based with function: ItemBasedCF


## Split in train and test ##

LastFM_Matrix <- as(lr_mat,"matrix")
train <- LastFM_Matrix[1:1497,]
test <- LastFM_Matrix[1498:1871,]

### Recommend for all test users ###

ResultsIBCF <- ItemBasedCF(train, test, 3, NN=10, onlyNew=TRUE)

prediction <- as.data.frame(ResultsIBCF$prediction)

TopN <- as.data.frame(ResultsIBCF$topN)

### score a model ###

# Split train - Test
set.seed(2)
train_rows = sample(1:nrow(lr_mat), 0.7*nrow(lr_mat))

train <- lr_mat[train_rows,]
test <- lr_mat[-train_rows,]

### score item-based
#Item 10
st <- proc.time()
Item10 = ItemBasedCF(train, test, N=10, NN=10, onlyNew=TRUE)
(proc.time() - st)
#print("user = 2.893 / system = 0.757 / elapsed = 3.650")

#Item 15
st <- proc.time()
Item15 = ItemBasedCF(train, test, N=10, NN=15, onlyNew=TRUE)
(proc.time() - st)
#print("user = 2.878 / system = 0.738 / elapsed = 3.617")


### Prediction Accuracy ###

RMSE <- function(prediction, real){
  
  if (nrow(prediction) == nrow(real) & ncol(prediction) == ncol(real)){
    RMSE = sqrt( sum( (prediction - real)^2 , na.rm = TRUE ) / (nrow(prediction) * ncol(prediction)) )
    return(RMSE)
  }else{
    return("Dimension of prediction are not equal to dimension of real")
  }
}

# RMSE Item 10
RMSE(Item10$prediction, test)
# RMSE Item 15
RMSE(Item15$prediction, test) 

# MAE Item 10
MAE(Item10$prediction, test)
# MAE Item 15
MAE(Item15$prediction, test) 

# F1 for Item 10
F1_Score(Item10$prediction, test, 5)

# F1 for Item 15
F1_Score(Item15$prediction, test, 5)



#---------------------------------------------------------------------------------------------------------------#
#---------------------------------- Cluster-based collaborative filtering --------------------------------------#
#---------------------------------------------------------------------------------------------------------------#

### create cluster-based function

ClusterBasedCF <- function(data, N, centers, iter, onlyNew=TRUE){
  
  data2 <- data
  
  # fill with average product rating
  colmeans <- colMeans(data2, na.rm=TRUE)
  
  for (j in colnames(data2)){
    data2[, j] <- ifelse(is.na(data2[ ,j]), colmeans[j], data2[, j])
  }
  
  km <- kmeans(data2, centers=centers, iter.max=iter)
  
  head(km$cluster)
  head(km$centers)
  
  # Statistics of the groups
  tab <- table(km$cluster)
  
  # Assign users to groups
  RES <- cbind(data, as.data.frame(km$cluster))
  
  # Calculate average ratings for everi cluster
  aggregation <- aggregate(RES, list(RES$"km$cluster"), mean, na.rm=T)
  aggregation <- aggregation[,-1]
  
  # Make a prediction
  users <- as.data.frame(RES$"km$cluster")
  users <- cbind(users, rownames(RES))
  colnames(users) <- c("km$cluster", 'rn')
  
  
  prediction = merge(users, aggregation, by="km$cluster")
  rownames(prediction) <- prediction$rn
  
  prediction  <- prediction[order(rownames(prediction)), -1:-2]
  
  prediction2 <- matrix(, nrow=nrow(prediction), ncol(prediction), 
                        dimnames=list(rownames(prediction), colnames(prediction)))
  colnames(prediction2) <- colnames(prediction)
  rownames(prediction2) <- rownames(prediction)
  
  for (u in rownames(prediction)){
    if (onlyNew == TRUE){
      unseen <- names(data[u, is.na(data[u,])])
      
      prediction2[u, ] <- as.numeric(t(ifelse(colnames(prediction) %in% unseen, prediction[u, ], as.numeric(NA))))
    }else{
      prediction2[u, ] <- prediction[u, ]
    }
  }
  
  # TopN
  TopN <- t(apply(prediction, 1, function(x) names(head(sort(x, decreasing=TRUE), 5))))
  
  print("Prediction done")
  
  res <- list(prediction, TopN)
  names(res) <- c('prediction', 'topN')
  
  return(res)
}

#### compute cluster-based with function ClusterBasedCF ####

CB_CF <- as(lr_mat,"matrix")

ResultsCBCF <- ClusterBasedCF(CB_CF, 3, 200,iter=100, onlyNew=TRUE)

prediction_CB <- as.data.frame(ResultsCBCF$prediction)

TopN_CB <- as.data.frame(ResultsCBCF$topN)

#### creatz F1 evaluation function ####

F1_Score <- function(prediction, real, threshold=NA, TopN=NA){
  if (nrow(prediction) == nrow(real) & ncol(prediction) == ncol(real)){
    # Threshold #
    if (!is.na(threshold)){
      TP = sum(ifelse(prediction >= threshold & real >= threshold, 1, 0), na.rm=T)
      FP = sum(ifelse(prediction >= threshold & real < threshold, 1, 0), na.rm=T)
      FN = sum(ifelse(prediction < threshold & real >= threshold, 1, 0), na.rm=T)
      Recall = TP/(TP+FN)
      Precision = TP/(TP+FP)
      F1_Score = (2 * TP/(TP+FP) * TP/(TP+FN)) / (TP/(TP+FP) + TP/(TP+FN))
      Class_Thres = list(Recall, Precision, F1_Score)
      names(Class_Thres) = c("Recall", "Precision", "F1_Score")
      
    }
    if (!is.na(TopN)){
      TP = vector(, length = nrow(prediction))
      FP = vector(, length = nrow(prediction))
      FN = vector(, length = nrow(prediction))
      
      for (u in nrow(prediction)){
        threshold_pred = -sort(-prediction[u, ])[TopN]
        threshold_real = -sort(-real[u, ])[TopN]
        TP[u] = sum(ifelse(prediction[u, ] >= threshold_pred & real[u, ] >= threshold_real, 1, 0), na.rm=T)
        FP[u] = sum(ifelse(prediction[u, ] >= threshold_pred & real[u, ] < threshold_real, 1, 0), na.rm=T)
        FN[u] = sum(ifelse(prediction[u, ] < threshold_pred & real[u, ] >= threshold_real, 1, 0), na.rm=T)
      }
      TP = sum(TP[u])
      FP = sum(FP[u])
      FN = sum(FN[])
      Recall = TP/(TP+FN)
      Precision = TP/(TP+FP)
      F1_Score = (2 * TP/(TP+FP) * TP/(TP+FN)) / (TP/(TP+FP) + TP/(TP+FN))
      Class_TopN = list(Recall, Precision, F1_Score)
      names(Class_TopN) = c("Recall", "Precision", "F1_Score")
    }
    
    
    if (!is.na(threshold) & !is.na(TopN)){
      Class = list(Class_Thres, Class_TopN)
      names(Class) = c("Threshold", "TopN")
    }else if (!is.na(threshold) & is.na(TopN)) {
      Class = Class_Thres
    }else if (is.na(threshold) & !is.na(TopN)) {
      Class = Class_TopN
    }else{
      Class = "You have to specify the 'Threshold' or 'TopN' parameter!"
    }
    return(Class)  
  }else{
    return("Dimension of prediction are not equal to dimension of real")
  }
}

####  Evaluate with F1_Score function ####

set.seed(2)
train_rows <- sample(1:nrow(CB_CF), 0.5*nrow(CB_CF))
train <- CB_CF[train_rows,]
test <- CB_CF[-train_rows,]
test <- test[-nrow(test),]

pred_CBCF <- ClusterBasedCF(train, 3, 200,iter=100, onlyNew=TRUE)

prediction_CB <- as.data.frame(pred_CBCF$prediction)

TopN_CB <- as.data.frame(pred_CBCF$topN)

prediction = prediction_CB
real = test

# use own code
CB_F1 = F1_Score(prediction, real, threshold=5, TopN=5)


# use pacakge 
y_pred = prediction_CB
y_true = test
CB_F1 = F1_Score(y_pred, y_true, 5)

#---------------------------------------------------------------------------------------------------------------#
#----------------------------------- Hybrid(Content-based & Item-based) ----------------------------------------#
#---------------------------------------------------------------------------------------------------------------#

### Transform results of the Content Based and Item Based to lists  ###

CB_list <-  as.list(CB$prediction)
#test_CBF
ResultsIBCF_list <- as.list(ResultsIBCF$prediction)

####################
### Compute Mean ###
####################
hybrid <- rowMeans(cbind(as.numeric(CB_list), as.numeric(ResultsIBCF_list)), na.rm=T)


### Transform list back to matrix with correct number of dimensions ###
Hybrid_prediction <- matrix(hybrid, nrow=nrow(test), ncol=ncol(test))
rownames(Hybrid_prediction) <- rownames(test)
colnames(Hybrid_prediction) <- colnames(test)


### Evaluate ###
# RMSE
RMSE(Hybrid_prediction, test)

MAE(Hybrid_prediction, test)


#---------------------------------------------------------------------------------------------------------------#
#----------------------------------- Hybrid(Content-based & User-based) ----------------------------------------#
#---------------------------------------------------------------------------------------------------------------#

# Train a linear Regression

LastFM_Matrix <- as(lr_mat,"matrix")
train <- LastFM_Matrix[1:1497,]
test <- LastFM_Matrix[1498:1871,]
test1 <- test[1:187,]
test2 <- test[188:374,]

### flatten test dataset
test_list <- as.list(test1)
CB_list <-  as.list(CB$prediction)
UB_list <-  as.list(prediction1)

### Transform list and matrices to dataframe
test_df <- data.frame(matrix(unlist(test_list), byrow=T))
CB_df <- data.frame(matrix(unlist(CB_list), byrow=T))
UB_df <- data.frame(matrix(unlist(UB_list), byrow=T))


### Combine created dataframes
library(qpcR)
input <- qpcR:::cbind.na(test_df, CB_df, UB_df)
colnames(input) <- c('TARGET', 'CB', 'UB')

### Train the linear regression
fit <- lm(TARGET ~ CB + UB, data=input)
summary(fit)

### Score Models
CB_2 <- ContentBased(ag_mat, test2, 3, 10, onlyNew=F)

UB2 <- ItemBasedCF(train, test2, 3, 10, onlyNew=F)

### Matrix to list
test_list2 <- as.list(test2)
CB_list2 <- as.list(CB_2$prediction)
UB_list2 <- as.list(UB2$prediction)

### List to dataframe
test_df2 <- data.frame(matrix(unlist(test_list2), byrow=T))
CB_df2 <- data.frame(matrix(unlist(CB_list2), byrow=T))
UB_df2 <- data.frame(matrix(unlist(UB_list2), byrow=T))

### combine dataframes to have an input dataset for linear regression
input2 <- qpcR:::cbind.na(test_df2, CB_df2, UB_df2)
colnames(input2) <- c('TARGET', 'CB', 'UB')

### Predict using the model calculated on test2 dataset
Hybrid_lin_reg <- predict(fit, input2)

### Transform the list of results to matrix with the correct dimensions
Hybrid_lin_reg <- matrix(Hybrid_lin_reg, nrow=nrow(test2), ncol=ncol(test2))
rownames(Hybrid_lin_reg) <- rownames(test2)
colnames(Hybrid_lin_reg) <- colnames(test2)

# Evaluation
MAE(Hybrid_lin_reg, test1)
