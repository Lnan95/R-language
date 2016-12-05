#数车工具
library(stringr)
library(tseries)
counting <- function()
{
  #在scan的输入流中输入回车车流量+1，违章数不变，输入1，车流量和违章数各+1
  #输入q退出程序，并输出车流量、违章数以及计数时段
  result <- numeric(2)
  start  <- substr(Sys.time(),1,19)
  t.data <- data.frame()
  n = 0#无意义的初始量
  while(n != 'q')
  {
    n <- scan(n = 1,quiet = T,what = character())
    if (length(n)==0) 
    {
      time <- substr(Sys.time(),1,19)
      result <- data.frame('车流量'=1,'是否违章'=0,'时间'=time)
      t.data = rbind(t.data,result)
      n<-character(1)
    }
    else if (n=='1')
    {
      time <- substr(Sys.time(),1,19)
      result <- data.frame('车流量'=1,'是否违章'=1,'时间'=time)
      t.data = rbind(t.data,result)
    }
  }
  
  .end <- substr(Sys.time(),1,19)
  cat('\n\n车辆违章情况:','\n\n时段 :',str_c(c(start,.end),collapse=' ~ '),
      '\n车流量 =',sum(t.data[,1]),'\n违章数 =',sum(t.data[,2]),sep = '','\n\n')
  return(t.data)
}

#计算p值
P_value <- function(x,cdf=pnorm,paramet= c(0,1),side=0){
  #pnorm为正太分布的cdf，side=-1左侧检验，0双边，1右侧
  #正态分布时paramet=c(mu,signa)
  n <-length(paramet)
  p <-cdf(x,paramet[1],paramet[2])
  if(side <0) p
  else if(side >0) 1-p
  else
    if(p<1/2) 2*p
  else  2*(1-p)
}


#数据整理
library(data.table)
#第一段时间
dat   = read.csv('C:\\Users\\Manager\\Desktop\\1.csv')
.time = as.POSIXlt(dat[,3])
dat1  = cbind(dat[,-3],as.data.frame(.time))
factor.time = cut(dat1[,3],as.POSIXlt(c('2016-12-02 09:00:00','2016-12-02 09:20:00',
                                        '2016-12-02 09:40:00','2016-12-02 10:00:00')))
dat1  = cbind(dat1,as.data.frame(factor.time))

#第二段时间
dat   = read.csv('C:\\Users\\Manager\\Desktop\\2.csv')
.time = as.POSIXlt(dat[,3])
dat2  = cbind(dat[,-3],as.data.frame(.time))
factor.time = cut(dat2[,3],as.POSIXlt(c('2016-12-02 10:00:00','2016-12-02 10:20:00',
                                        '2016-12-02 10:40:00','2016-12-02 11:00:00')))
dat2  = cbind(dat2,as.data.frame(factor.time))

#第三段时间
dat   =read.csv('C:\\Users\\Administrator\\Desktop\\3.csv')
.time =as.POSIXlt(dat[,3])
dat3  =cbind(dat[,-3],as.data.frame(.time))
factor.time = cut(dat3[,3],as.POSIXlt(c('2016-12-02 14:00:00','2016-12-02 14:20:00',
                                        '2016-12-02 14:40:00','2016-12-02 15:00:00')))
dat3  = cbind(dat3,as.data.frame(factor.time))
#第四段时间
dat   = read.csv('C:\\Users\\Administrator\\Desktop\\4.csv')
.time = as.POSIXlt(dat[,3])
dat4  = cbind(dat[,-3],as.data.frame(.time))
factor.time = cut(dat4[,3],as.POSIXlt(c('2016-12-02 17:00:00','2016-12-02 17:20:00',
                                        '2016-12-02 17:40:00','2016-12-02 18:50:05')))
dat4  = cbind(dat4,as.data.frame(factor.time))

#总数据
dat = rbind(dat1,dat2,dat3,dat4)
dat = data.table(dat)
dat = dat[,.(sum(车流量),sum(是否违章)),by=factor.time]
colnames(dat) <- c('时间起点','车流量','违章数量')
dat




#实验内容

#1.中心位置的符号检验
#计算违章率
options(digits=3) #设置有效小数个数
p = dat[,违章数量/车流量,by=时间起点]
colnames(p)[2] = '违章率'
#计算z统计量和p值
S.plus = sum(p[,2]>0.05)
C = 1/2;n = dim(dat)[1L]
z_stat=(S.plus-n/2+C)/sqrt(n/4)
P_value(z_stat)

#2.随机游程检验
#数据再处理
#使用的数据为4份原数据中的“是否违章”
dat = c(dat1[,2],dat2[,2],dat3[,2],dat4[,2])
x=factor(c(rep(0,100),rep(1,20)))
#使用tseries包的随机游程检验工具
tseries::runs.test(x)

#3.Cox-Staut趋势检验
#对比表
n = 12;c = n/2
p.change = p[1:6,2]-p[1:6+c,2]
#S+，S-和K值
S.plus  = sum(p.change>0);S.minus = sum(p.change<0)
K=c(S.plus,S.minus)[which.min(c(S.plus,S.minus))]
#构建Z统计量,并计算P值
.n = S.plus + S.minus
z_stat = (K+sign(K-.n)*0.5-.n/2)/sqrt(.n/4)
P_value(z_stat,side = -1)#左侧检验

#4.配对样本的符号检验
p1=p[c(1:3,7:9),2];p2=p[!c(1:3,7:9),2]
cbind(p1,p2)
#求Z统计量,并计算p值
S.plus = sum(p2>p1)
C = 1/2;n = dim(p1)[1L]
z_stat=(S.plus-n/2+C)/sqrt(n/4)
P_value(z_stat)

#绘图