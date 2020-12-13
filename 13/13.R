setwd("~/AoC/2020/13")
library("gmp")

t0<-Sys.time() #To laugh in Milas's face!

input <- read.table("./input13.txt", stringsAsFactors = F)
#input <- read.table("./input13small.txt", stringsAsFactors = F)

time.stamp <- as.numeric(input[1,1])
buses <- unlist(strsplit(input[2,1], split = ","))
buses.no.x <- as.numeric(buses[buses != "x"])

wait.time <- time.stamp
waiting <- T
the.bus <- NA
while(waiting){
  if(any(wait.time %% buses.no.x == 0)){
    the.bus <- buses.no.x[which(wait.time %% buses.no.x == 0)]
    waiting <- F
  }else{
    wait.time <- wait.time + 1
  }
}
print((wait.time - time.stamp) * the.bus)


# Star 2
offset <- (0:(length(buses)-1))[buses != "x"]

# Chinese reminder theorem
N <- prod(buses.no.x)
y <- as.bigz(N/buses.no.x)

mod.inversion<-function(x, n){
  solution<-c()
  for(i in 1:length(x)){
    y<-(0:(n[i]-1))
    solution<-c(solution, y[which(((y*x[i]) %% n[i]) == 1)])
    }
  return(solution)
}

z<-mod.inversion(y,buses.no.x)
sol <- sum(as.bigz(-offset * y * z)) %% N

print(as.character(sol))
print(paste("Computed in:",round(Sys.time()-t0,6),"s."))
