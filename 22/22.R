setwd("~/AoC/2020/22")
input <- readLines("./input22.txt")
#input <- readLines("./input22small.txt")
#input <- readLines("./input22smallest.txt")

empty.line <- which(input == "")
deckA <- as.numeric(input[2:(empty.line-1)])
deckB <- as.numeric(input[(empty.line+2):(length(input))])


end <- min(length(deckA), length(deckB)) == 0

while(!end){
  a <- deckA[1]
  b <- deckB[1]
  if (a > b){
    deckA <- c(deckA[-1], a, b)
    deckB <- deckB[-1]
  }else{
    deckB <- c(deckB[-1], b, a)
    deckA <- deckA[-1]
  }
  end <- min(length(deckA), length(deckB)) == 0
}

print(sum(deckA*(length(deckA):1)))


# star 2 

round.game<-function(deckA, deckB){
  a <- deckA[1]
  b <- deckB[1]
  if((a <= length(deckA)-1) & (b <= length(deckB)-1)){
    win <- play(deckA[2:(1+a)], deckB[2:(1+b)])$win
    if(win == "A"){
      deckA <- c(deckA[-1], a, b)
      deckB <- deckB[-1]
    }else{
      deckB <- c(deckB[-1], b, a)
      deckA <- deckA[-1]
    }
  }else{
    if (a > b){
      deckA <- c(deckA[-1], a, b)
      deckB <- deckB[-1]
    }else{
      deckB <- c(deckB[-1], b, a)
      deckA <- deckA[-1]
    }
  }
  return(list("A" = deckA, "B" = deckB))
}


play <- function(deckA, deckB, end = FALSE) {
  prev <- c(paste(deckA, collapse = ";"), paste(deckB, collapse = ";"))
  while (!end) {
    dummy <- round.game(deckA, deckB)
    deckA.new <- dummy$A
    deckB.new <- dummy$B
    prev <-
      rbind(prev, c(
        paste(deckA.new, collapse = ";"),
        paste(deckB.new, collapse = ";")
      ))
    if (nrow(unique(prev)) != nrow(prev)) {
      print("Instant win player A")
      end = TRUE
      return(list(
        "A" = deckA.new,
        "B" = deckB.new,
        "win" = "A"
      ))
    }
    if (min(length(deckA.new), length(deckB.new)) == 0) {
      end = TRUE
      if (length(deckB.new) == 0) {
        win <- "A"
      } else{
        win <- "B"
      }
      return(list(
        "A" = deckA.new,
        "B" = deckB.new,
        "win" = win
      ))
    }
    
    deckA <- deckA.new
    deckB <- deckB.new
  }
}

deckA <- as.numeric(input[2:(empty.line-1)])
deckB <- as.numeric(input[(empty.line+2):(length(input))])

Z <- play(deckA, deckB)
Z
Z
print(sum(Z$A*(length(Z$A):1)))
