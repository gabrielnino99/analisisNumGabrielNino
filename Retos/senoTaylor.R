rm(list=ls())
#Aproximacion de Taylor
x<-0
senoImp<-function(x){
  n<-10
  seno<-0
  dividendo<-0
  divisor<-0
  signo<-0
  for(i in 0:n){
    dividendo<-1
    for(j in 0:((2*i+1)-1)){
      dividendo<-dividendo*x
    }
    divisor<-1
    for(j in 1:(2*i+1)){
      divisor<-divisor*j
    }
    if(isTRUE(i%%2 == 0)){
      signo<-1
    }
    else{
      signo<--1
    }
    seno<-seno+(dividendo/divisor)*signo
  }
  print(seno)
}