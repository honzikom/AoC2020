setwd("~/AoC/2020/02")

input <- read.table("./input02.txt", stringsAsFactors = F)

#star 1
counter <- 0
for (i in 1:1000) {
  range <- as.integer(unlist(strsplit(input[i, 1], split = "-")))
  letter <- substr(input[i, 2], 1, 1)
  x <-
    sum(unlist(strsplit(input[i, 3], split = "")) == letter) %in% (range[1]:range[2])
  if (x) {
    counter <- counter + 1
  }
}
print(counter)

#star 2
counter <- 0
for (i in 1:1000) {
  range <- as.integer(unlist(strsplit(input[i, 1], split = "-")))
  letter <- substr(input[i, 2], 1, 1)
  pass <- unlist(strsplit(input[i, 3], split = ""))
  check1 <- pass[range[1]] == letter
  check2 <- pass[range[2]] == letter
  
  if (xor(check1, check2)) {
    counter <- counter + 1
  }
}
print(counter)