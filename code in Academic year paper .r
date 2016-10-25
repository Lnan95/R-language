test<-function(n=50,beta=c(1,0.8,0.6),m=1000,multicollinearity=TRUE){
  rss<-function(x){
    x<-x^2
    r<-apply(x,MARGIN = 2,FUN = sum)/dim(x)[1]
    r[7]<-sum(r)
    return(r)
  }   #计算系数残差差和的公式（xhat-x的操作不在该函数中）

  ###误差估计#设置初始参数
  beta <- beta
  coef.ridge <- numeric()
  coef.lse <- numeric()
  coef.step <- numeric()
  coef.lasso <-numeric()
  betanames<- c("(Intercept)","x[, 1]","x[, 2]","x[, 3]","x[, 4]","x[, 5]","x[, 6]")#逐步回归时用到
  df.step<-data.frame()
  for (j in 1:m) {
    y <- numeric()
    x <- numeric()
    for (i in 1:n) {
      #生成xij、epsilon
      eps <- rnorm(n = 1,mean = 0,sd = 1)
      x1<-rnorm(1,0,1)
      #multicollinearity为TRUE则为多重共线性下的模拟，反之为普通多元回归模拟
      if (multicollinearity) {
        x2<-x1*(0.8+rnorm(1,0,0.05))
      }
      else x2<-rnorm(1,0,1)
      x3<-rnorm(1,0,1)
      x4<-rnorm(1,0,1)
      x5<-rnorm(1,0,1)
      x6<-rnorm(1,0,1)
      yi<-sum(beta*c(x1,x2,x3))+eps
      y<-c(y,yi)
      x<-c(x,x1,x2,x3,x4,x5,x6)
    }
    #将x和y转为矩阵格式，以便进行回归操作
    y <- matrix(y)
    x <- matrix(x,ncol = 6,byrow = T)
    ###进行LASSO估计
    lm.la <- lars(x,y,type="lasso",intercept = F)
      #寻找cp最小时的系数解
    mincp <- as.numeric(which.min(lm.la$Cp))
    coef.la <- coef(lm.la)[mincp,]
      #将模拟的系数解整合成数据框
    coef.lasso <- as.numeric(c(coef.lasso,coef.la))
    df.lasso<-matrix(coef.lasso,ncol = 6,byrow = T)
    ###进行最小二乘估计
    lm.ls<-lm(y~x[,1]+x[,2]+x[,3]+x[,4]+x[,5]+x[,6])
    coef.ls<-coef(lm.ls)
    coef.lse<-c(coef.lse,coef.ls)
    df.ls<-matrix(as.numeric(coef.lse),ncol=7,byrow = T)#将模拟的系数解整合成数据框
    ###进行ridge估计
    lm.rid <- lm.ridge(y ~ x,lambda = seq(0,0.1,0.001))
    #选择GCV最小时的系数解
    which.rid <- dim(lm.rid$coef)[2]
    coef.rid<-lm.rid$coef[,which.rid]
    coef.ridge<-c(coef.ridge,coef.rid)
    #将模拟的系数解整合成数据框
    df.ridge <- data.frame(matrix(data = coef.ridge,ncol = 6))
    ###进行逐步回归估计
    lm.st=step(lm(y~x[,1]+x[,2]+x[,3]+x[,4]+x[,5]+x[,6]),trace=FALSE,direction = "backward")
    #使逐部回归系数格式统一,并整合成一个数据框
    coef.st<-coef(lm.st)
    betaname=betanames %in% names(coef.st)
    count = 0 #计数器
    one.coef.st = as.numeric() #整合时使用空参数
    for (k in betaname) {
      count = count+1
      coef = coef.st[betanames[count]]
      if (k == FALSE) {
        coef = 0 #若参数不存在，则创一新参数代替，取值为0
        names(coef)<-betanames[count]
      }
      one.coef.st <- c(one.coef.st,coef)
    }
    #利用数据框的合并整合模拟所得的参数
    coef.step <- data.frame(matrix(data = one.coef.st,nrow = 1))
    names(coef.step)<- c("(Intercept)","x[, 1]","x[, 2]","x[, 3]","x[, 4]","x[, 5]","x[, 6]")
    df.step<-rbind(df.step,coef.step)
  }
  ###正确率 错误率
  #LASSO
  correct.lasso <- sum(df.lasso[,1:3]!=0)/(m*3)
  incorrect.lasso <- sum(df.lasso[,4:6]!=0)/(m*3)
  #step
  correct.step <- sum(df.step[,2:4]!=0)/(m*3)
  incorrect.step <- sum(df.step[,5:7]!=0)/(m*3)
  correct.rate<<-data.frame(matrix(c(correct.lasso,incorrect.lasso,correct.step
,incorrect.step),ncol = 2),row.names = c("正确率","错误率"))
  colnames(correct.rate)<-c("lasso","step")
  ###beta最小残差合
  coef <- matrix(rep(c(1,0.8,0.6,0,0,0),m),ncol = 6,byrow = T)#真实系数形成的矩阵
  #lasso
  r.lasso <- df.lasso-coef
  rssbeta.lasso <- rss(r.lasso)
  #ls
  r.ls <- df.ls[,2:7]-coef
  rssbeta.ls <- rss(r.ls)#函数中编制的求残差函数
  #ridge
  r.ridge <- df.ridge-coef
  rssbeta.ridge <- rss(r.ridge)
  #step
  rssbeta.step <- rss(df.step[,2:7]-coef)
  df.rss<-data.frame(matrix(c(rssbeta.lasso,rssbeta.step,rssbeta.ls,rssbeta.ridge),ncol = 7
      ,byrow = T),row.names = c("lasso","逐步回归法","最小二乘法","岭回归"))
  colnames(df.rss)<-c(c("x1","x2","x3","x4","x5","x6","总和"))
  return(df.rss)
}


#测量部分

table<-numeric()
table.c<-numeric()
for (i in c(50,200,500,1000,10000)){
  result<-test(n = i,m = 1000,multicollinearity=FALSE)
  table<-rbind(table,result)#绘制系数残差平方和的表
  table.c<-rbind(table.c,correct.rate)#正确率与错误率表
}