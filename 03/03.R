setwd("~/AoC/2020/03")

input <- read.table(
  "./input03.txt",
  stringsAsFactors = F,
  sep = "\n",
  encoding = "UTF-8",
  comment.char = ""
)

input <- as.matrix(input)
input <- matrix(unlist(strsplit(input, split = "")),
                nrow = nrow(input),
                byrow = T)


count.trees <- function(map, init.pos, step, modulator) {
  trees <- c()
  while (pos[1] <= nrow(input)) {
    trees <- c (trees, input[pos[1], pos[2]])
    pos <- pos + step
    if (pos[2] > modulator) {
      pos[2] <- pos[2] %% modulator
    }
  }
  return(sum(trees == "#"))
}

pos <- c(1, 1)
modulator <- ncol(input)

# star 1
step <- c(1, 3)
count.trees(input, pos, step, modulator)

# star 2
pos <- c(1, 1)
modulator <- ncol(input)
steps <- matrix(c(1, 1, 3, 1, 5, 1, 7, 1, 1, 2), byrow = T, ncol = 2)
trees <- c()
for (i in 1:nrow(steps)) {
  trees <-
    c(trees, count.trees(input, pos, rev(steps[i, ]), modulator))
}
print(prod(trees))
