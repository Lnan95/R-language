dat<-read.table("C:\\Users\\Manager\\Desktop\\15.txt")


pla<-function(x,a=1)
{
  count=0
  y = as.matrix(x[,5])
  x = as.matrix(cbind(rep(1,length(y)),x[,-5]))
  w = c(0,0,0,0,0)
  halt = 1
  while (halt == 1)
  {
    for (i in c(1:dim(x)[1]))
    {
      s = x[i,]%*%w                                        
      if (sign(s) != y[i]) 
      {
        w = w+a*y[i]*x[i,]
        count = count+1
      }
      
      if (all(sign(x %*% w)==y)) halt = 0
    }
  }
  names(w) <- c("1","2","3","4","5")
  
  return(list(w=w,times=count))
}


pla(dat)
#####output
#> pla(dat)
#$w
#1         2         3         4         5 
#-3.000000  3.084144 -1.583081  2.391305  4.528764 

#$times
#[1] 45

n = sample(1:dim(dat)[1L],dim(dat)[1L])
ddat<-dat[n,]
pla(ddat)

#> pla(ddat)
#$w
#1         2         3         4         5 
#-3.000000  2.499569 -0.605193  2.182297  3.833544 
#
#$times
#[1] 35