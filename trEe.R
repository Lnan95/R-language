tree = function(train,label,w=100) {
  mi = min(train)
  ma = max(train)
  box = seq(mi,ma,length.out = w)
  gini = as.numeric()
  
  for (i in box) {
    
    corr_1 = (train>mi) & (train<=i)
    cont_1 = as.character(label[corr_1])
    corr_2 = (train<=ma) & (train>i)
    cont_2 = as.character(label[corr_2])
    p1 = sum(cont_1 == "setosa")/length(cont_1)
    p2 = sum(cont_1 == "virginica")/length(cont_1)
    p3 = sum(cont_1 == "versicolor")/length(cont_1)
    
    p1_2 = sum(cont_2 == "setosa")/length(cont_2)
    p2_2 = sum(cont_2 == "virginica")/length(cont_2)
    p3_2 = sum(cont_2 == "versicolor")/length(cont_2)
    
    gini =c(gini, p1*p2*p3+p1_2*p2_2*p3_2)
  }
  which_mini_gini = which.min(gini)
  bound = box[which_mini_gini]
  
  if (gini[which_mini_gini]==0)
  {
    sep=train-bound
    sep_1 = max(sep[sep<0])
    sep_2 = min(sep[sep>0])
    bound = (sep_2-sep_1)/2 + bound
  }
  
  
  return(data.frame(bound=bound,min_gini=gini[which_mini_gini]))
}

#> tree(x$Petal.Length,label)
#bound min_gini
#1 2.503535        0
for (i in c(1:(w-which.min(gini)))) 
{
  if (sum((train>box[which.min(gini)]) & (train<box[which.min(gini)+i])>0))
  {
    break
  }
  bound = 1/2*i*(ma-mi)/w+box[which.min(gini)]
}
min_gini=min(gini[!is.na(gini)])
#> tree(x$Petal.Length,label)
#bound min_gini
#1 2.455035        0


tree_2 <- function(x,label,fun=tree)
{
  n = dim(x)[2L]
  div = as.data.frame(apply(x,2,tree,label=label))
  div_mini_gini = div[seq(2,to =n*2,by = 2)]
  div_bound = div[seq(1,to = n*2,by = 2)]
  whic = which.min(div_mini_gini)
  frame = data.frame(bound=div_bound[whic],which=whic,row.names = NULL)
  
  return(frame)
}




