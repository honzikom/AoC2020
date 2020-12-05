setwd("~/AoC/2020/05")
input <-
  unlist(c(read.table("./input05.txt", stringsAsFactors = F)))


get.row <- function(x) {
  x <- substr(x, 1, 7)
  x <- unlist(strsplit(x, split = ""))
  seats <- 0:127
  for (y in x) {
    if (y == "F") {
      seats <- seats[0:(length(seats) / 2)]
    } else if (y == "B") {
      seats <- seats[(length(seats) / 2 + 1):length(seats)]
    }
  }
  return(seats)
}
get.col <- function(x) {
  x <- substr(x, 8, 10)
  x <- unlist(strsplit(x, split = ""))
  seats <- 0:7
  for (y in x) {
    if (y == "L") {
      seats <- seats[0:(length(seats) / 2)]
    } else if (y == "R") {
      seats <- seats[(length(seats) / 2 + 1):length(seats)]
    }
  }
  return(seats)
}

IDs <- c()
for (i in 1:length(input)) {
  IDs <- c(IDs, get.row(input[i]) * 8 + get.col(input[i]))
}

#star 1
print(max(IDs))

#star 2
print((min(IDs):max(IDs))[which(!(min(IDs):max(IDs) %in% IDs))])

