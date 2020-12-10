setwd("~/AoC/2020/10")
input <- unlist(read.table("./input10.txt"))

#star 1
jolts<-sort(c(0,input, max(input)+3))
print(prod(table(diff(jolts))))

#star 2
# Popsal jsem 5 papírů, když jsem hledal rekurentní formuli
# a už jsem byl FAKT blízko!
# Po cestě jsem našel prvních pár členů... 
# pak jsou kouknul na input a hle, to stačí!
ways <- function(n){
  if(n == 0 | n == -1){
    return(1)
  }else if( n==1 ){
    return(2)
  }else if( n==2){
    return(4)
  }else if( n==3){
    return(7)
  }else if( n==4){
    return(15)
}else{
    print("Error, Darwin s tímhle nepočítal...")
  }
 }


d <- diff(jolts)
sub.ways <- c()
pos <- 1
n <- 0
while(pos <=length(d)){
  if(d[pos] == 1){
    n <- n+1
  }else{
    print(n-1)
    sub.ways <- c(sub.ways, ways(n-1))
    n <-0
  }
  pos <- pos+1
}
print(prod(sub.ways))

