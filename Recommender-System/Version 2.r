##############################
###Items_Based 
##############################
###############
#Read the data
###############
train = read.csv("train.csv")
##285 songs
#######
#Method: Cosine 
######
library(lsa)
rela_item = cosine(as.matrix(train[,-(1:2)]))
items.neighbours <- matrix(NA, nrow=ncol(rela_item),ncol=11,dimnames=list(colnames(rela_item)))

for(i in 1:nrow(items.neighbours)) 
{
  items.neighbours[i,] <- head(colnames(rela_item[ ,order(rela_item[i,],decreasing=TRUE)]),n=11)
}

items.neighbours=items.neighbours[,-1]
##Application. If a customer purchase/like a song "air", them we could recommend 10 similar songs
items.neighbours["air",]

##############################
###User_Based 
##############################
###############
song= train[,-(1:2)]
# 1.Scores function
getScore <- function(history, similarities)
{
  x <- sum(history*similarities)/sum(similarities)
  x
}
# 2. A placeholder matrix
holder <- matrix(NA, nrow=nrow(train),ncol=ncol(train)-1,dimnames=list((train$user),colnames(train[-1])))
holder = holder[,-1]
# 3. Find out the song they didnot purchase/like and list the simliarity. Then time the purchase history
#Note: the purchased song will be marked as  empty strin and don't recommend it.
get_score= function(song,holder,rela_item){
for(k in 1:nrow(song)){
  for (i in 1:ncol(song)){ 
      if (song[k,i] == 0){
        name = colnames(song)[i] 
        item = rela_item[,name]
        item = item[order(item,decreasing=TRUE)]
        namelist = attr(item,"names")[2:11]
        similarity = item [2:11]
        history = song[k,namelist]
        score = sum(similarity*history)/sum(similarity)
        holder[k,i] = score
      }else {
        holder[k,i] = ""
      }
    }
  }
return (holder)
}

holder = get_score(song,holder,rela_item)

# 4. List the top 10 for each user.
users.items <- matrix(NA, nrow=nrow(holder),ncol=10,dimnames = list(row.names(holder)))
for(i in 1:nrow(holder)) 
{
  users.items[i,] <- head(colnames(holder[ ,order(as.numeric(holder[i,]),decreasing=TRUE)]),n=10)
}

##Limitation of UserBased:
#1. It takes time to for loop all the data.  
#2. It cannot provide a result unless you run through all the data
#Advantage: It can provide a higher accuracy result. 

##Why we cannot use supervised learning like random forest to provide the recommendation list.
#Because we cannot run all the song with many sample, it will takes a huge time and history data.
#However, in this case, the collaborative filtering can calculate the scores soon only with some similar data. 
