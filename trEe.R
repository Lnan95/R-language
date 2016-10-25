box<-function(dot,x,y)
{
  
  x>dot
  x<=dot
  l1=  label[x>dot]
  l2 = label[x<=dot]
  p1 = sum(l1 == "setosa")/length(l1)
  p2 = sum(l1 == "virginica")/length(l1)
  p3 = sum(l1 == "versicolor")/length(l1)
  
  p1_2 = sum(l2 == "setosa")/length(l2)
  p2_2 = sum(l2 == "virginica")/length(l2)
  p3_2 = sum(l2 == "versicolor")/length(l2)
  gini = p1*p2*p3+p1_2*p2_2*p3_2
  return(gini)
}



divide = function(train,label,w=100,box=box)
{
  train = train[order(train)]
  gini = sapply(xx,box,x=train,y=label)
  
  which_mini_gini = which.min(gini)
  bound = train[which_mini_gini]
  
  sep=train-bound
  sep_2 = min(sep[sep>0])
  bound = sep_2/2 + bound
  
  return(data.frame(bound=bound,min_gini=gini[which_mini_gini]))
}



tree <- function(x,label,fun=divide,is_random=FALSE,is_bagging=FALSE)
{
  p = dim(x)[2L]
  n = dim(x)[1L]
  
  if (is_random)
  {
    
    l=sample(x=c(1:n),size=round(n*2/3),replace = TRUE)
    x = x[l,]
    label = label[l]
    m = round(sqrt(p))
    feature <- sample(x = c(1:p),size=m)
    x = x[,feature]
    p = m
    
    div = as.data.frame(apply(x,2,fun,label=label))
    div_mini_gini = div[seq(2,to =p*2,by = 2)]
    div_bound = div[seq(1,to = p*2,by = 2)]
    whic = which.min(div_mini_gini)
    frame = data.frame(bound=div_bound[whic],which=feature[whic],row.names = NULL)
    
    return(frame)
  }
  
  if (is_bagging)
  {
    l=sample(x=c(1:n),size=round(n*2/3),replace = TRUE)
    x = x[l,]
    label = label[l]
  }
  
  div = as.data.frame(apply(x,2,fun,label=label))
  div_mini_gini = div[seq(2,to =p*2,by = 2)]
  div_bound = div[seq(1,to = p*2,by = 2)]
  whic = which.min(div_mini_gini)
  frame = data.frame(bound=div_bound[whic],which=whic,row.names = NULL)
  
  return(frame)
}

#first bagging tree

frame = data.frame(bound=0,which=0)
for (i in c(1:1000)) 
{
  new=tree(x,label,is_bagging = TRUE)
  names(new)<-c("bound","which")
  frame = rbind(frame,new)
}

frame = frame[-1,]
one = sum(frame$which==1)
two = sum(frame$which==2)
three = sum(frame$which==3)
four = sum(frame$which==4)


which.div = which.max(c(one,two,three,four))
mean(frame[frame$which==which.div,1])



#first random forest 

frame = data.frame(bound=0,which=0)
for (i in c(1:1000)) 
{
  new=tree(x,label,is_random = TRUE)
  names(new)<-c("bound","which")
  frame = rbind(frame,new)
}

frame = frame[-1,]
one = sum(frame$which==1)
two = sum(frame$which==2)
three = sum(frame$which==3)
four = sum(frame$which==4)


which.div = which.max(c(one,two,three,four))
mean(frame[frame$which==which.div,1])


#boosting#Î´×÷



#


  
