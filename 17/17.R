setwd("~/AoC/2020/17")

input <- readLines("./input17.txt")
input <- matrix(unlist(strsplit(input, split="")), nrow = 8, byrow = T)
input <- input == "#"
d <- dim(input)
k <- 7
# init a 3d matrix
size <- c((d[1] + 2*k),(d[2] + 2*k),1 + 2 * k)
tenzor <- array(rep(F, prod(size)), size ) 
tenzor[(k+1):(k+8), (k+1):(k+8), k+1] <- input

sum.neighbours <- function(x,y,z, tenzor){
  s <- sum(tenzor[(x-1):(x+1), (y-1):(y+1), (z-1):(z+1)])
  if(tenzor[x,y,z]){
    s <- s - 1
  }
  return(s)
}

schrodinger <-function(x, alive) {
  if (alive) {
    if (x == 2 | x == 3) {
      return(T)
    } else{
      return(F)
    }
  } else{
    if (x == 3) {
      return(T)
    } else{
      return(F)
    }
  }
}

  

#cycling
for(cycle in 1:6) {
  tenzor.copy <- tenzor
  for (x in (k + 1 - cycle):(k + d[1] + cycle)) {
    for (y in (k + 1 - cycle):(k + d[2] + cycle)) {
      for (z in (k + 1 - cycle):(k + 1 + cycle)) {
        n <- sum.neighbours(x, y, z, tenzor)
        tenzor.copy[x, y, z] <- schrodinger(n, tenzor[x,y,z])
      }
    }
  }
  tenzor <- tenzor.copy
}
sum(tenzor)

###################################################################
# star 2
# recreate input and change some functions
# it coud be reworked so that star 1 is special case,
# but this show thought process better

input <- readLines("./input17.txt")
input <- matrix(unlist(strsplit(input, split="")), nrow = 8, byrow = T)
input <- input == "#"
d <- dim(input)
k <- 7

# init a 4d matrix
size <- c((d[1] + 2*k),(d[2] + 2*k),1 + 2 * k, 1 + 2*k)
tenzor <- array(rep(F, prod(size)), size ) 
tenzor[(k+1):(k+8), (k+1):(k+8), k+1, k+1] <- input

#sum.neighbours in 4D
sum.neighbours <- function(x,y,z,w, tenzor){
  s <- sum(tenzor[(x-1):(x+1), (y-1):(y+1), (z-1):(z+1), (w-1):(w+1)])
  if(tenzor[x,y,z,w]){
    s <- s - 1
  }
  return(s)
}


#cycling in 4D
for(cycle in 1:6) {
  tenzor.copy <- tenzor
  for (x in (k + 1 - cycle):(k + d[1] + cycle)) {
    for (y in (k + 1 - cycle):(k + d[2] + cycle)) {
      for (z in (k + 1 - cycle):(k + 1 + cycle)) {
        for (w in (k + 1 - cycle):(k + 1 + cycle)) {
        n <- sum.neighbours(x, y, z, w, tenzor)
        tenzor.copy[x, y, z, w] <- schrodinger(n, tenzor[x,y,z,w])
        }
      }
    }
  }
  tenzor <- tenzor.copy
}
sum(tenzor)