setwd("~/AoC/2020/08")

input <- read.table("./input08.txt", stringsAsFactors = F)

# star 1
pos <- 1
acc <- 0
indices <- c(1)
while (length(indices) == length(unique(indices))) {
  if (input[pos, 1] == "acc") {
    acc <- acc + input[pos, 2]
    pos <- pos + 1
  } else if (input[pos, 1] == "jmp") {
    pos <- pos + input[pos, 2]
  } else if (input[pos, 1] == "nop") {
    pos <- pos + 1
  } else{
    print("Error!")
    break
  }
  indices <- c(indices, pos)
}

print(acc)


# random :-)
# star 2
jeste.ne <- T
pokus <- 1
while (jeste.ne) {
  #randomly make one change
  r <- sample(which(input[, 1] == "jmp" | input[, 1] == "nop"), 1)
  rnd.input <- input
  if (rnd.input[r, 1] == "jmp") {
    rnd.input[r, 1] <- "nop"
  } else{
    rnd.input[r, 1] <- "jmp"
  }
  #was it the right change?
  pos <- 1
  acc <- 0
  indices <- c(1)
  while (length(indices) == length(unique(indices))) {
    if (rnd.input[pos, 1] == "acc") {
      acc <- acc + rnd.input[pos, 2]
      pos <- pos + 1
    } else if (rnd.input[pos, 1] == "jmp") {
      pos <- pos + rnd.input[pos, 2]
    } else if (rnd.input[pos, 1] == "nop") {
      pos <- pos + 1
    } else{
      print("Error!")
      break
    }
    indices <- c(indices, pos)
    if (pos == (dim(input)[1]) + 1) {
      jeste.ne <- F
      #if so, print result
      print(past("Akumulátor", acc))
      break
    }
  }
  print(pokus) # to see progress
  pokus <- pokus + 1
}
