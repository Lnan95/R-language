simulate<-function(n,m,beta=c(2,1,0.8,0.6),multicollinearity)
{
  #使用最小二乘法、岭回归、逐步回归和lasso对多重共线性和一般情况下的多元方程回归模拟
  #
  #input:
  #n为样本量
  #m为重复的次数
  #beta为线性方程f(x)的真实β
  #若输入multicollinearity，则进行多重共线性情况的模拟，其值为相关系数_
  #若参数multicollinearity缺失，进行一般多元情况的模拟
  #output:
  #输出list格式的最小二乘法、岭回归、逐步回归法和lasso算法m次估计的参数平均值
  #以及逐步回归法和lasso算法在变量筛选中的正确率
  
  
  #生成（m*n × 6）的正态分布随机数x，和m*n个误差项，进行矩阵计算求得y值
  #如果参数multicollinearity被赋值，则原先的正态随机数矩阵x改为
  #Xi1和Xi2相关系数约为multicollinearity的随机数矩阵X
  if (missing(multicollinearity)) 
  {
    x    <- matrix(rnorm(n*m*6,0,1),ncol = 6)
    beta <- c(2,1,0.8,0.4,0,0)
    eps  <- rnorm(n*m,0,1)
    y    <- x%*%beta+eps
    
  } else {
            x      <- matrix(rnorm(n*m*6,0,1),ncol = 6)
            eps_b1 <- rnorm(n*m,0,0.05)
            x[,1]  <- multicollinearity*x[,2] +eps_b1 #x1 = α*x2 + eps
            eps    <- rnorm(n*m,0,1)
            y      <- x%*%beta + eps
         }
  
  #输出1次最小二乘法得到的系数估计值
  fit_lm <- function(index,x,y,n)
  {
    lm_fit  <- lm(y[(index+1):(index+n)]~x[(index+1):(index+n),])
    coef_lm <- as.numeric(lm_fit$coefficients)[2:7]
    return(coef_lm)
  }
  #输出1次岭回归得到的系数估计值
  fit_rid <- function(index,x,y,n)
  {
    lm_rid    <- lm.ridge(y[(index+1):(index+n)] ~ x[(index+1):(index+n),]
                          ,lambda = seq(0,0.2,0.01))
    which_GCV <- which.min(lm_rid$GCV)
    coef_rid  <- as.numeric(lm_rid$coef[,which_GCV])
    return(coef_rid)
  }
  #输出1次lasso算法得到的系数估计值
  fit_lasso <- function(index,x,y,n)
  {
    lm_la   <- lars(x[(index+1):(index+n),],y[(index+1):(index+n)],type="lasso",intercept = F)
    mincp   <- as.numeric(which.min(lm_la$Cp))
    coef_la <- coef(lm_la)[mincp,]
    return(coef_la)
  }
  #输出1次逐步回归法得到的系数估计值
  fit_step <- function(index,x,y,n,betanames)
  {
    fit_step  <- step(lm(y[(index+1):(index+n)]~.,data = data.frame(x[(index+1):(index+n),])),
                      trace=FALSE,direction = "backward")
    coef_step <- coef(fit_step)
    position  <- as.numeric(betanames %in% names(coef_step))
    coef_step <- as.numeric(coef_step) * position
    return(coef_step)
  }
  
  #利用等差数组index在pply族函数中隐循环运行上面四个函数，提高效率
  #betanames用于对逐步回归法筛选后的变量进行引索的参数
  index <- 0:(m-1)*n
  betanames <- c("(Intercept)","X1","X2","X3","X4","X5","X6")
  
  coef_lm    <- sapply(index,fit_lm,x=x,y=y,n=n)
  coef_rid   <- sapply(index,fit_rid,x=x,y=y,n=n)
  coef_lasso <- sapply(index,fit_lasso,x=x,y=y,n=n)
  coef_step  <- sapply(index,fit_step,x=x,y=y,n=n,betanames=betanames)[-1,]
  
  mean_lm_coef_hat    <- apply(coef_lm,1,mean)
  mean_rid_coef_hat   <- apply(coef_rid,1,mean)
  mean_lasso_coef_hat <- apply(coef_lasso,1,mean)
  mean_step_coef_hat  <- apply(coef_step,1,mean)
  
  #分别计算lasso算法和逐步回归法在变量筛选上的正确率和错误率
  correct_lasso <- sum(coef_lasso[c(5,6),]==0)/(2*m)
  correct_step  <- sum(coef_step[c(5,6),]==0)/(2*m)
  wrong_lasso   <- sum(coef_lasso[-c(5,6),]==0)/(4*m)
  wrong_step    <- sum(coef_step[-c(5,6),]==0)/(4*m)
  
  return(list(lm=mean_lm_coef_hat,rid=mean_rid_coef_hat,lasso=mean_lasso_coef_hat,
              step=mean_step_coef_hat,correct_lasso=correct_lasso,correct_step=correct_step,
              wrong_lasso = wrong_lasso,wrong_step = wrong_step))
}