#computing the gini
box<-function(dot,x,y)
{
  l1=  y[x>dot]
  l2 = y[x<=dot]
  p1 = sum(l1 == "setosa")/length(l1)
  p2 = sum(l1 == "virginica")/length(l1)
  p3 = sum(l1 == "versicolor")/length(l1)

  p1_2 = sum(l2 == "setosa")/length(l2)
  p2_2 = sum(l2 == "virginica")/length(l2)
  p3_2 = sum(l2 == "versicolor")/length(l2)
  gini = p1*p2*p3+p1_2*p2_2*p3_2
  return(gini)
}

#find the optimal edge in one feature
divide = function(train,label,fun=box)
{
  label = label[order(train)]
  train = train[order(train)]
  xx = unique(train)
  gini = sapply(X = xx,fun,x=train,y=label)
  
  bound = xx[which.min(gini)]
  sep=train-bound
  sep_2 = min(sep[sep>0])
  bound = sep_2/2 + bound
  return(data.frame(bound=bound,min_gini=min(gini,na.rm = T),check.names = F))
}

#find the optimal edge in train
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
bound = mean(frame[frame$which==which.div,1])



#first random forest 

frame = data.frame(bound=0,which=0)
for (i in c(1:1000)) 
{
  new=tree(train,label,is_random = TRUE)
  names(new)<-c("bound","which")
  frame = rbind(frame,new)
}

frame = frame[-1,]
one = sum(frame$which==1)
two = sum(frame$which==2)
three = sum(frame$which==3)
four = sum(frame$which==4)


which.div = which.max(c(one,two,three,four))
bound = frame[frame$which==which.div,1]


#boosting#Î´×÷



  
