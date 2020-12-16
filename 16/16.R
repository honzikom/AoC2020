setwd("~/AoC/2020/16")
library(stringr)

input <- readLines("./input16.txt")

#parse input
empty.lines <- which(input =="")

fields <- input[1:(empty.lines[1]-1)]
strsplit(fields[1], split = c(":", "-", " or ", "-", " "))
fields <- matrix(as.numeric(unlist(str_extract_all(fields, "[0-9]+"))), 
                 ncol = 4, byrow = T)

my.ticket <- as.numeric(unlist(strsplit(input[empty.lines[2]-1], ",")))

nearby.tickets <- input[(empty.lines[2]+2):length(input)]
nearby.tickets <- matrix(as.numeric(unlist(strsplit(nearby.tickets, ","))),
                             byrow = T, nrow = length(input)-empty.lines[2]-1)

#validate ticket
can.be.valid <- function(ticket, fields){
  invalid <- c()
  for (value in ticket){
    value.test <- FALSE
    for( row in 1:20){
      if(value %in% c(fields[row,1]:fields[row,2],
                      fields[row,3]:fields[row,4]) ){
        value.test <- T
      }
    }
    if(value.test){ #value is possible in at least one field
      #do nothing
    }else{
      invalid <- c(invalid, value)
    }
  }
  if(length(invalid) == 0){ # no invalids
    return(list("valid" = TRUE, "values" = NA))
  }else{
    return(list("valid" = FALSE, "values" = invalid ))
  }
}

scanning.errors <- c()
for (i in 1:(dim(nearby.tickets)[1])){
    x <- can.be.valid(nearby.tickets[i,], fields)
    if(!x$valid){
      scanning.errors <- c(scanning.errors, x$values)
    }
}


print(sum(scanning.errors))

#####################################################################
#Part 2 work:

#Discard

discards <- c()
for (i in 1:(dim(nearby.tickets)[1])){
  x <- can.be.valid(nearby.tickets[i,], fields)
  if(!x$valid){
    discards<-c(discards, i)
  }
}

nearby.tickets <- nearby.tickets[-discards, ]

# It's random time!
print(factorial(20))
# ok, not today

#let' take a look:
plot(1, 1, type = "n", xlim = c(0,1000), ylim = c(0,20), ylab = "fields")
for(i in 1:20){
  segments(fields[i,1],i,fields[i,2],i)
  segments(fields[i,3],i,fields[i,4],i)
}


# function to cinvert range to vector of int
field2vec<-function(field){
  return(c(field[1]:field[2],field[3]:field[4]))
}


#in what field can be column in tickets
can.it.be.that.field <- function(it.col.index, field.index){
  field <- field2vec(fields[field.index,])
  values <- nearby.tickets[, it.col.index]
  return(all(values %in% field))
}

# matrix of what can be where
tab.of.possibilities <- matrix(NA, nrow = 20, ncol = 20)
for(i in 1:20){
  for(j in 1:20){
tab.of.possibilities[i,j] <- can.it.be.that.field(j,i)
}
}
#View(tab.of.possibilities)

#Looks great!
m <- tab.of.possibilities
result <-matrix(FALSE, nrow=20, ncol = 20)
for(i in 1:20){
  index.row <- which(rowSums(m) == 1)
  index.col <- which(m[index.row, ])
  m[,index.col] <- FALSE
  result[index.row, index.col] <- TRUE
}

#in result is mapping of fields to cols [row -> col]

#fields 1:6 [departures] are mapped to 
cols<-c()
for(i in 1:6){
cols <- c(cols, which(result[i,]))
}

#Here we go! This is the moment. Mom's spagetti
print(as.character(prod(my.ticket[cols])))