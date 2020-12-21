setwd("~/AoC/2020/21")

#input <- readLines("./input21small.txt")
input <- readLines("./input21.txt")
ingredients<-c()
allergens <- c()

# extract allergens adn ingredients
for(row in input){
  ing <- unlist(strsplit(row, split = "(", fixed = T))[1]
  ing <- unlist(strsplit(ing, split =" "))
  all <- unlist(strsplit(row, split = "(contains ", fixed = T))[2]
  all <- unlist(strsplit(all, split = ", "))
  all[length(all)] <- substr(tail(all,1), start = 1, stop=nchar(tail(all,1))-1)
  ingredients <- unique(c(ingredients, ing))
  allergens <- unique(c(allergens, all))
  }

M <- data.frame(matrix(TRUE, nrow = 0, ncol = length(allergens)+1))
colnames(M)<-c("ingredient", allergens)

#run 2
r.all <- matrix(FALSE, nrow = length(input), ncol = length(allergens))
r.ing <- matrix(FALSE, nrow = length(input), ncol = length(ingredients))

for(i in 1:length(input)){
  row <- input[i]
  ing <- unlist(strsplit(row, split = "(", fixed = T))[1]
  ing <- unlist(strsplit(ing, split =" "))
  all <- unlist(strsplit(row, split = "(contains ", fixed = T))[2]
  all <- unlist(strsplit(all, split = ", "))
  all[length(all)] <- substr(tail(all,1), start = 1, stop=nchar(tail(all,1))-1)
  all.index <- which(allergens %in% all)
  ing.index <- which(ingredients %in% ing)
  r.all[i,all.index] <- TRUE
  r.ing[i,ing.index] <- TRUE
}
colnames(r.all)<-allergens
colnames(r.ing)<-ingredients

safe.ing<-rep(T, length(ingredients))

for(i in 1:ncol(r.all)){
  recipes.with.allergen <- r.all[,i]
  for(j in 1:ncol(r.ing)){
    test <- r.ing[recipes.with.allergen, j]
    if(all(test)){
      safe.ing[j]<-F
    }
  }
}

#result:
print(sum(r.ing[,safe.ing]))

# Create matrix of possible pairs ing/allergen
r.ing.a <- r.ing[, !safe.ing]
poss<-matrix(FALSE, nrow=length(ingredients[!safe.ing]), ncol = length(allergens))


for(i in 1:nrow(poss)){
  recipes.with.allergen <- which(r.all[,i]) 
  for(j in 1:ncol(poss)){
    recipes.with.ing <- which(r.ing.a[,j])
  if(all(recipes.with.allergen %in% recipes.with.ing)){
    poss[i,j] <- T
  }
}
}

# find pairs in possibilities matrix
pairs <- matrix(NA, nrow =8, ncol = 2)
for(k in 1:nrow(poss)){
  if(any(colSums(poss) == 1)){
  j <- which(colSums(poss) == 1)[1]
  i <- which(poss[,j])
  pairs[k,]<-c(i,j)
  poss[i,] <- F
  }else if(any(rowSums(poss) == 1)){
    i <- which(rowSums(poss) == 1)[1]
    j <- which(poss[i,])
    pairs[k,]<-c(i,j)
    poss[,j] <- F
  }
}

pairing<-cbind(allergens[pairs[,1]],ingredients[!safe.ing][pairs[,2]] )
print(paste(pairing[order(pairing[,1]),2], collapse = ","))
