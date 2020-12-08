setwd("~/AoC/2020/07")
#input <- readLines("./input07_small.txt")
input <- readLines("./input07.txt")
rows <- matrix(unlist(strsplit(input, split = c("contain"))),
               ncol = 2, byrow = T)

#extract color names
names <- substr(rows[, 1], start = 1, stop = nchar(rows[, 1]) - 6)

#lets make a matrix!
cont.m <- matrix(0, nrow = length(names), ncol = length(names))
colnames(cont.m) <- names
rownames(cont.m) <- names

# and fill it!
for (i in 1:length(names)) {
  row.split <- unlist(strsplit(rows[i, 2], split = ","))
  for (bag in row.split) {
    if (bag == " no other bags.") {
      # do nothing
    } else{
      for (col in names) {
        x <- unlist(strsplit(bag, split = col))
        if (length(x) == 2) {
          cont.m[i, which(colnames(cont.m) == col)] <- as.numeric(x[1])
        }
      }
    }
  }
}

#star 1
#setup
shiny.gold <- rep(0, length(names))
shiny.gold[names == "shiny gold"] <- 1
res <- rep(F, length(names))
#go!
hile(any(shiny.gold != 0)) {
  shiny.gold <- (cont.m %*% shiny.gold)
  res <- res | (shiny.gold > 0)
}
print(sum(res))

#star 2
#setup
shiny.gold <- t(rep(0, length(names)))
shiny.gold[names == "shiny gold"] <- 1
res <- rep(0, length(names))
#go!
while (any(shiny.gold != 0)) {
  shiny.gold <- (shiny.gold %*% cont.m)
  res <- res + shiny.gold
}

print(sum(res))
