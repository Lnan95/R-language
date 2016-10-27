dat<-read.table("C:\\Users\\Manager\\Desktop\\18.txt")


pocket.pla<-function(x,a=1)
{
  count=0
  y = as.matrix(x[,5])
  x = as.matrix(cbind(rep(1,length(y)),x[,-5]))
  w = c(0,0,0,0,0)

  while (count <= 50)
  {
    
    rank = sample(1:dim(x)[1],dim(x)[1])
    x = x[rank,]
    y = y[rank]
    for (i in c(1:dim(x)[1]))
    {
      s = x[i,]%*%w                                        
      if (sign(s) != y[i]) 
      {
        w2 = w+a*y[i]*x[i,]
        
        err_rate = sum(sign(x%*%w)==y)
        err_rate2 = sum(sign(x%*%w2)==y)
        if (err_rate2>err_rate) w = w2
      }
    }
    count = count + 1
    if (all(sign(x%*%w)==y)) return(list(w=w,times=count))
  }
  names(w) <- c("1","2","3","4","5")
  
  return(list(w=w,times=count))
}

y = as.matrix(dat[,5])
x = as.matrix(cbind(rep(1,length(y)),dat[,-5]))
a = pocket.pla(dat)
1 - sum(sign(x%*%a$w)==dat$V5)/length(y)


#18.2000times
err = 0

for (t in c(1:20))
{
  a = pocket.pla(dat[,-6])
  err = err+1 - sum(sign(x%*%a$w)==dat$V5)/length(y)
}


#19.pla
pla<-function(x)
{
  count=0
  y = as.matrix(x[,5])
  x = as.matrix(cbind(rep(1,length(y)),x[,-5]))
  w = c(0,0,0,0,0)
  halt = 1
  while (count < 80 & halt < 80)
  {
    for (i in c(1:dim(x)[1]))
    {
      s = x[i,]%*%w                                        
      if (sign(s) != y[i]) 
      {
        w = w+y[i]*x[i,]
        count = count+1
      }
    }
    halt = halt+1
  }
  names(w) <- c("1","2","3","4","5")
  
  return(list(w=w,times=count))
}
