setwd("~/AoC/2020/19")

#input <- readLines("./input19small.txt")
input <- readLines("./input19.txt")

empty.line <- which(input == "")

rules <- input[1:(empty.line - 1)]
messages <- input[(empty.line + 1):length(input)]

# parse rules
parsed.rules <- c(0:(length(rules) - 1))
for (rule in rules) {
  x <- unlist(strsplit(rule, split = ":"))
  if (any(unlist(strsplit(x[2], split = "")) == "a")) {
    parsed.rules[1 + as.numeric(x[1])] <- "a"
  } else if (any(unlist(strsplit(x[2], split = "")) == "b")) {
    parsed.rules[1 + as.numeric(x[1])] <- "b"
  } else{
    parsed.rules[1 + as.numeric(x[1])] <- x[2]
  }
}

# update
# in: string of rules
# 1.) split into chars
# 2.) for each char
#      a) letter -> do nothing
#      b) number without | -> replace number with new rules
#      c) number with | -> duplicate inpute and replace with both options
# 3.) return vector of strings

update.rule <- function(rule) {
  rule <- unlist(strsplit(rule, split = " "))
  rule <- rule[rule != " " & rule !=""]
  result <- matrix(, nrow = 1, ncol = length(rule))
  for (i in 1:length(rule)) {
    # keep letter
    if (rule[i] == "a" | rule[i] == "b") {
      result[, i] <- rule[i]
    } else{
      # number
      x <- new.rules(rule[i])
      if (length(x) == 1) {
        result[, i] <- x
      } else{
        result <- rbind(result, result)
        result[1:(dim(result)[1] / 2), i] <- x[1]
        result[((dim(result)[1] / 2) + 1):(dim(result)[1]), i] <-
          x[2]
      }
    }
  }
  return(apply(result, 1, paste, collapse = " "))
}


new.rules <- function(r) {
  r <- as.numeric(r)
  x <- parsed.rules[r + 1]
  if (x == "a" | x == "b") {
    return(x)
  } else{
    x <- unlist(strsplit(x, split = " "))
    x <- x[x != " " & x!=""]
    if (any(x == "|")) {
      sep <- which(x == "|")
      x1 <- paste(x[1:(sep - 1)], collapse = " ")
      x2 <- paste(x[(sep + 1):length(x)], collapse = " ")
      return(c(x1, x2))
    } else{
      return(paste(x, collapse = " "))
    }
  }
}

update.all.rules <- function(vec.of.rules) {
  result <- c()
  for (rule in vec.of.rules) {
    result <- c(result, update.rule(rule))
  }
  return(result)
}

test.end <- function(vec.of.rules) {
  for (vec in vec.of.rules) {
    vec <- unlist(strsplit(vec, split = ""))
    vec <- vec[vec != " "]
    if (any(!(vec %in% c("a", "b")))) {
      return(F)
    }
  }
  return(T)
}

#start star 1
possibilities <- c("0")
round <- 0
while (!test.end(possibilities)) {
  round <- round + 1
  print(round)
  possibilities <- update.all.rules(possibilities)
  print(possibilities[1])
}
for (i in 1:length(possibilities)) {
  x <- unlist(strsplit(possibilities[i], split = ""))
  x <- x[x != " "]
  possibilities[i] <- paste(x, collapse = "")
}

#messeges satisfying rule 0:
print(sum(messages %in% possibilities))

write.csv(possibilities, "./poss")

poss <- rep(NA, length(possibilities))
for(i in 1:length(possibilities)){
  if(i %% 1000 == 0){print(i)}
  a <- unlist(strsplit(possibilities[i], split = " "))
  a[a == "18"] <- "a"
  a[a == "39"] <- "b"
  a <- paste(a, collapse = "")
  poss[i] <- a
}

print(sum(messages %in% poss))

new.to.check <- messages[!(messages %in% poss)]

rule8 <- c("8")
for(i in 1:11) {
  rule8 <- update.all.rules(rule8)
  print(rule8[1])
}
rule8[1]

rule11 <- c("11")
for(i in 1:10) {
  rule11 <- update.all.rules(rule11)
  print(rule11[1])
}

rule42 <- c("42")
for(i in 1:11) {
  rule42 <- update.all.rules(rule42)
  print(rule42[1])
}
rule42[1]

rule31 <- c("31")
for(i in 1:10) {
  rule31 <- update.all.rules(rule31)
  print(rule31[1])
}
rule31[1]
poss[1]

for(i in 1:length(rule8)){
  a <- unlist(strsplit(rule8[i], split = " "))
  a <- a[a != " "]
  rule8[i]<-paste(a, collapse = "")
}

for(i in 1:length(rule11)){
  a <- unlist(strsplit(rule11[i], split = " "))
  a <- a[a != " "]
  rule11[i]<-paste(a, collapse = "")
}

for(i in 1:length(rule42)){
  a <- unlist(strsplit(rule42[i], split = " "))
  a <- a[a != " "]
  rule42[i]<-paste(a, collapse = "")
}

for(i in 1:length(rule31)){
  a <- unlist(strsplit(rule31[i], split = " "))
  a <- a[a != " "]
  rule31[i]<-paste(a, collapse = "")
}

suc <- 0
#for (mess in new.to.check){
for (mess in messages){
    l <- nchar(mess)
    x = l/8
    submess <- rep(NA, x)
    if(x>=3){
      for(i in 1:x){
        submess[i]<-substr(mess, start = 8*(i-1)+1, stop = 8*i)
      }
      if(all(submess[1:2] %in% rule42) & submess[x] %in% rule31){
        if(x==3){
          suc <- suc + 1 
        }else{
          submess<-submess[-c(1,2,x)]
          if(all(submess %in% rule42)){
            suc <- suc + 1
          }else{
            r <- length(submess)
            test <- FALSE
            for(i in 1:floor(r/2)){
              if(all(submess[(r-i+1):r] %in% rule31)){
                if(all(submess[1:(r-i)] %in% rule42)){
                  test <- TRUE
                }
              }
            }
            if(test){suc <- suc + 1}
          }
        }
      }
    }
        
}


# not 283
# 