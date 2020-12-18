setwd("~/AoC/2020/18")

input <- readLines("./input18.txt")

#prepare some functions
eval.bracket <- function(bracket) {
  l <- (length(bracket) - 1) / 2
  result <- as.numeric(bracket[1])
  for (i in (1:l) * 2) {
    if (bracket[i] == "+") {
      result <- result + as.numeric(bracket[i + 1])
    } else{
      result <- result * as.numeric(bracket[i + 1])
    }
  }
  return(result)
}

find.bracket <- function(row) {
  l <- length(row)
  left <- NA
  right <- NA
  for (i in 1:l) {
    if (row[i] == "(") {
      left <- i
    } else if (row[i] == ")") {
      right <- i
      return(c(left, right))
    }
  }
  return(FALSE)
}

remove.bracket <- function(row, brackets) {
  bracket <- row[(brackets[1] + 1):(brackets[2] - 1)]
  value <- eval.bracket(bracket)
  row <- row[-((brackets[1] + 1):brackets[2])]
  row[brackets[1]] <- value
  return(row)
}

eval.row <- function(row) {
  while (T) {
    b <- find.bracket(row)
    if (any(b)) {
      row <- remove.bracket(row, b)
    }
    else{
      return(eval.bracket(row))
      break
    }
  }
}


#go star 1!
result <- c()
for (i in 1:length(input)) {
  row <- input[i]
  row <- unlist(strsplit(row, split = ""))
  row <- row[row != " "]
  result <- c(result, eval.row(row))
}

print(as.character(sum(result)))


### part 2
# rework functions


eval.bracket2 <- function(bracket) {
  times <- which(bracket == "+")
  if (length(times) > 0) {
    for (t in times) {
      t <- which(bracket == "+")[1] #ugly
      s <- as.numeric(bracket[t - 1]) + as.numeric(bracket[t + 1])
      bracket[t] <- s
      bracket <- bracket[c(-(t - 1), -(t + 1))]
      
    }
  }
  return(prod(as.numeric(bracket[seq(1, length(bracket), by = 2)])))
}

eval.row2 <- function(row) {
  while (T) {
    b <- find.bracket(row)
    if (any(b)) {
      row <- remove.bracket2(row, b)
    }
    else{
      return(eval.bracket2(row))
      break
    }
  }
}

remove.bracket2 <- function(row, brackets) {
  bracket <- row[(brackets[1] + 1):(brackets[2] - 1)]
  value <- eval.bracket2(bracket)
  row <- row[-((brackets[1] + 1):brackets[2])]
  row[brackets[1]] <- value
  return(row)
}

#go star 2!
result <- c()
for (i in 1:length(input)) {
  row <- input[i]
  row <- unlist(strsplit(row, split = ""))
  row <- row[row != " "]
  result <- c(result, eval.row2(row))
}

print(as.character(sum(result)))


