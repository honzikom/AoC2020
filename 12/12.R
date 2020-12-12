setwd("~/AoC/2020/12")
input <- read.table("./input12.txt", stringsAsFactors = F)
#input<-read.table("./input12small.txt", stringsAsFactors = F)

letter <- sapply(input, substr, start = 1, stop = 1)
number <-
  as.numeric(sapply(input, substr, start = 2, stop = nchar(input)))

#pos is current positions in x,y,angle
pos <- c(0, 0, 0)

for (i in 1:length(letter)) {
  if (letter[i] == "N") {
    pos <- pos + c(0, number[i], 0)
  }
  if (letter[i] == "S") {
    pos <- pos - c(0, number[i], 0)
  }
  if (letter[i] == "E") {
    pos <- pos + c(number[i], 0, 0)
  }
  if (letter[i] == "W") {
    pos <- pos - c(number[i], 0, 0)
  }
  if (letter[i] == "R") {
    pos <- pos - c(0, 0, number[i])
  }
  if (letter[i] == "L") {
    pos <- pos + c(0, 0, number[i])
  }
  if (letter[i] == "F") {
    pos <-
      pos + c(number[i] * cospi(pos[3] / 180),
              number[i] * sinpi(pos[3] / 180), 0)
  }
}
#manhattan
print(sum(abs(pos)[1:2]))

#star 2
ship <- c(0, 0)
wayp <- c(10, 1)
for (i in 1:length(letter)) {
  if (letter[i] == "N") {
    wayp <- wayp + c(0, number[i])
  }
  if (letter[i] == "S") {
    wayp <- wayp - c(0, number[i])
  }
  if (letter[i] == "E") {
    wayp <- wayp + c(number[i], 0)
  }
  if (letter[i] == "W") {
    wayp <- wayp - c(number[i], 0)
  }
  #rotation counterclockwise
  if (letter[i] == "L") {
    rot <- matrix(c(
      cospi(number[i] / 180),-sinpi(number[i] / 180),
      sinpi(number[i] / 180), cospi(number[i] / 180)),
    ncol = 2,
    byrow = T)
    wayp <- rot %*% (wayp - ship) + ship
  }
  # this is odd... because sin is odd!
  if (letter[i] == "R") {
    rot <- matrix(c(
      cospi(number[i] / 180), sinpi(number[i] / 180),
      -sinpi(number[i] / 180),cospi(number[i] / 180)
    ),
    ncol = 2,
    byrow = T)
    wayp <- rot %*% (wayp - ship) + ship
  }
  if (letter[i] == "F") {
    dir <- wayp - ship
    ship <- ship + number[i] * dir
    wayp <- wayp + number[i] * dir
  }
}
#manhattan
print(sum(abs(ship)))
