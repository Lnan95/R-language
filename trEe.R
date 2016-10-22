tree = function(train,label) {
  mi = min(train)
  ma = max(train)
  box = seq(mi,ma,length.out = 50)
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
  bound = box[which.min(gini)]
  
  for (i in c(1:(50-which.min(gini)))) 
    {
    if (sum((train>box[which.min(gini)]) & (train<box[which.min(gini)+i])>0))
      {
      break
    }
    bound = 1/2*i*(ma-mi)/50+box[which.min(gini)]
  }
  min_gini=min(gini[!is.na(gini)])

  return(list(bound=bound,min_gini=min_gini,gini=gini,box=box))
}

tree(train$Petal.Length,label)