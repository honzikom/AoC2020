setwd("~/AoC/2020/06")
input <- readLines("./input06.txt")

# star 1

answers <- c()
unique.ans <- c()
for (i in 1:(length(input))) {
  if (!(input[i] == "")) {
    answers <- paste(answers, input[i], sep = "")
  } else{
    answers <- unlist(strsplit(answers, split = ""))
    unique.ans <- c(unique.ans, length(unique(answers)))
    answers <- c()
  }
  if (i == length(input)) {
    answers <- unlist(strsplit(answers, split = ""))
    unique.ans <- c(unique.ans, length(unique(answers)))
    print(sum(unique.ans))
  }
}


# star 2
answers <- c()
all.ans <- c()
group.size <- 0
for (i in 1:(length(input))) {
  if (!(input[i] == "")) {
    answers <- paste(answers, input[i], sep = "")
    group.size <- group.size + 1
  } else{
    answers <- unlist(strsplit(answers, split = ""))
    all.ans <- c(all.ans, sum(table(answers) == group.size))
    answers <- c()
    group.size <- 0
  }
  if (i == length(input)) {
    answers <- unlist(strsplit(answers, split = ""))
    all.ans <- c(all.ans, sum(table(answers) == group.size))
    print(sum(all.ans))
  }
}