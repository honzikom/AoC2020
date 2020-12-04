setwd("~/AoC/2020/04")
library("stringr")
input <- readLines("./input04.txt")

# extract rows
j <- 1
rows <- c(NA)
for (i in 1:length(input)) {
  if (!(input[i] == "")) {
    if (!is.na(rows[j])) {
      rows[j] <- paste(rows[j], input[i], sep = " ")
    } else{
      rows[j] <- input[i]
    }
  } else{
    j <- j + 1
    rows <- c(rows, NA)
  }
}

# create dataset
data <- as.data.frame(matrix(NA, nrow = length(rows), ncol = 8))
colnames(data) <-
  c("iyr", "ecl", "cid", "eyr", "pid", "hcl", "byr", "hgt")

for (i in 1:(dim(data)[1])) {
  splitted <- unlist(strsplit(rows[i], split = ":"))
  atts <- substr(splitted, nchar(splitted) - 2, nchar(splitted))
  atts <- atts[-length(atts)]
  for (j in 1:length(atts)) {
    if (j < length(atts)) {
      pattern <-
        paste(".*", atts[j], ":(.+) ", atts[j + 1], ".*", sep = "")
      found <- gsub(pattern, "\\1", rows[i])
      data[i, which(colnames(data) == atts[j])] <- found
    } else{
      data[i, which(colnames(data) == atts[j])] <- tail(splitted, 1)
    }
  }
}

# star 1
print(sum(complete.cases(data[, c(1, 2, 4, 5, 6, 7, 8)])))

# star 2

check.data <- matrix(F, nrow = nrow(data), ncol = ncol(data) - 1)
#byr
check.data[, 1] <- as.numeric(data$byr) %in% 1920:2002
#iyr
check.data[, 2] <- as.numeric(data$iyr) %in% 2010:2020
#eyr
check.data[, 3] <- as.numeric(data$eyr) %in% 2020:2030
#hgt
units <- substr(data$hgt, nchar(data$hgt) - 1, nchar(data$hgt))
hgt <- substr(data$hgt, 1, nchar(data$hgt) - 2)
for (i in 1:length(units)) {
  if (units[i] == "cm" & hgt[i] %in% 150:193) {
    check.data[i, 4] <- T
  }
  if (units[i] == "in" & hgt[i] %in% 59:76) {
    check.data[i, 4] <- T
  }
}
#hcl
for (i in 1:length(data$hcl)) {
  if (!is.na(data$hcl[i])) {
    if (substr(data$hcl[i], 1, 1) == "#") {
      color <- substr(data$hcl[i], 2, nchar(data$hcl[i]))
      if (nchar(color) == 6) {
        is.color <-
          all(unlist(str_split(color, "")) %in% c(0:9, letters[1:6]))
        if (is.color) {
          check.data[i, 5] <- T
        }
      }
    }
  }
}
#ecl
check.data[, 6] <- data$ecl %in% c("amb", "blu", "brn",
                                   "gry", "grn", "hzl", "oth")
#pid
for (i in 1:length(data$pid)) {
  if (!is.na(data$pid[i])) {
    if(nchar(data$pid[i]==9)){
      is.pass <-
        all(unlist(str_split(data$pid[i], "")) %in% as.character(c(0:9)))
      if(is.pass){
        check.data[i,7] <- T
      }
    }
  }}

#
sum(rowSums(check.data)==7)
