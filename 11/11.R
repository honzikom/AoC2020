setwd("~/AoC/2020/11")
#input <- read.table("./input11.txt", stringsAsFactors = F)
input <- read.table("./input11small.txt", stringsAsFactors = F)
dummy <- length(unlist(strsplit(input[1, ], split = "")))
plane <-
  data.frame(matrix(NA, nrow = dim(input)[1], ncol = length(unlist(
    strsplit(input[1, ], split = "")
  ))))
for (i in 1:nrow(plane)) {
  plane[i,] <- unlist(strsplit(input[i, ], split = ""))
}

occ.seats <- function(posx, posy, plane) {
  sizex <- dim(plane)[1]
  sizey <- dim(plane)[2]
  plane[posx, posy] <- "O"
  if (posx == 1) {
    if (posy == 1) {
      neigh <- plane[c(posx, posx + 1), c(posy, posy + 1)]
    } else if (posy == sizey) {
      neigh <- plane[c(posx, posx + 1), c(posy - 1, posy)]
    } else{
      neigh <- plane[c(posx, posx + 1), (posy - 1):(posy + 1)]
    }
  } else if (posx == sizex) {
    if (posy == 1) {
      neigh <- plane[c(posx - 1, posx), c(posy, posy + 1)]
    } else if (posy == sizey) {
      neigh <- plane[c(posx - 1, posx), c(posy - 1, posy)]
    } else{
      neigh <- plane[c(posx - 1, posx), (posy - 1):(posy + 1)]
    }
  } else{
    if (posy == 1) {
      neigh <- plane[(posx - 1):(posx + 1), c(posy, posy + 1)]
    } else if (posy == sizey) {
      neigh <- plane[(posx - 1):(posx + 1), c(posy - 1, posy)]
    } else{
      neigh <- plane[(posx - 1):(posx + 1), (posy - 1):(posy + 1)]
    }
  }
  return(sum(neigh == "X"))
}

update.seat <- function(posx, posy, plane) {
  # empty and no occupied around
  if (plane[posx, posy] == "L") {
    occ <- occ.seats(posx, posy, plane)
    if (occ == 0) {
      return("X")
    }
  }
  
  #occupied and 4 other occupied
  if (plane[posx, posy] == "X") {
    occ <- occ.seats(posx, posy, plane)
    if (occ >= 4) {
      return("L")
    }
  }
  return(plane[posx, posy])
}
test <- T
it <- 1
while (test) {
  print(it)
  updated.plane <- plane
  for (i in 1:nrow(plane)) {
    for (j in 1:ncol(plane)) {
      updated.plane[i, j] <- update.seat(i, j, plane)
    }
  }
  test <- !(all(updated.plane == plane))
  print(test)
  plane <- updated.plane
  it <- it + 1
}
print(sum(plane == "X"))

################################################################
input <- read.table("./input11.txt", stringsAsFactors = F)
#input <- read.table("./input11small.txt", stringsAsFactors = F)
dummy <- length(unlist(strsplit(input[1, ], split = "")))
plane <-
  data.frame(matrix(NA, nrow = dim(input)[1], ncol = length(unlist(
    strsplit(input[1, ], split = "")
  ))))
for (i in 1:nrow(plane)) {
  plane[i,] <- unlist(strsplit(input[i, ], split = ""))
}


see.in.dir <- function(posx, posy, dir, plane) {
  x <- posx + dir[1]
  y <- posy + dir[2]
  if (x == 0 | x == nrow(plane) + 1 | y == 0 | y == ncol(plane) + 1) {
    return("E")
  } else{
    while (x %in% 1:nrow(plane) &
           y %in% 1:ncol(plane) & isTRUE(plane[x, y] == ".")) {
      x <- x + dir[1]
      y <- y + dir[2]
    }
    if (x == 0 | x == nrow(plane) + 1 | y == 0 | y == ncol(plane) + 1) {
      return("E")
    } else{
      return(plane[x, y])
    }
  }
}


seen.seats <- function(posx, posy, directions, plane) {
  seats <- c()
  for (i in 1:nrow(directions)) {
    seats <- c(seats, see.in.dir(posx, posy, directions[i, ], plane))
  }
  return(seats)
}

update.seat2 <- function(posx, posy, directions, plane) {
  # empty and no occupied around
  if (plane[posx, posy] == "L") {
    occ <- sum(seen.seats(posx, posy, directions, plane) == "X", na.rm = T)
    if (occ == 0) {
      return("X")
    }
  }
  
  #occupied and 5 other occupied
  if (plane[posx, posy] == "X") {
    occ <- sum(seen.seats(posx, posy, directions, plane) == "X", na.rm = T)
    if (occ >= 5) {
      return("L")
    }
  }
  return(plane[posx, posy])
}

directions <-
  matrix(c(0, 1, 1, 1, 1, 0, 1, -1, 0, -1, -1, -1, -1, 0, -1, 1),
         ncol = 2,
         byrow = T)
test <- T
it <- 1
while (test) {
  print(it)
  updated.plane <- plane
  for (i in 1:nrow(plane)) {
    for (j in 1:ncol(plane)) {
      updated.plane[i, j] <- update.seat2(i, j, directions, plane)
    }
  }
  test <- !(all(updated.plane == plane))
  print(test)
  plane <- updated.plane
  it <- it + 1
}
print(sum(plane == "X"))
