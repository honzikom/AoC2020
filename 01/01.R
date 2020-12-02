setwd("~/AoC/2020/01")

input <- read.table("./input01.txt")
input <- as.numeric(unlist(c(input)))
l <- length(input)

#star 1
for (i in 1:(l - 1)) {
  for (j in (i + 1):(l)) {
    if (input[i] + input[j] == 2020) {
      print(input[i] * input[j])
    }
  }
}

#star 2

for (i in (1:(l - 2))) {
  for (j in ((i + 1):(l - 1))) {
    for (k in ((j + 1):l)) {
      if ((input[i] + input[j] + input[k]) == 2020) {
        print(input[i] * input[j] * input[k])
      }
    }
  }
}
