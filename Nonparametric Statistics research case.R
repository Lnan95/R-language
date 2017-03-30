#��������
library(stringr)
library(tseries)
counting <- function()
{
  #��scan��������������س�������+1��Υ�������䣬����1����������Υ������+1
  #����q�˳����򣬲������������Υ�����Լ�����ʱ��
  result <- numeric(2)
  start  <- substr(Sys.time(),1,19)
  t.data <- data.frame()
  n = 0#������ĳ�ʼ��
  while(n != 'q')
  {
    n <- scan(n = 1,quiet = T,what = character())
    if (length(n)==0) 
    {
      time <- substr(Sys.time(),1,19)
      result <- data.frame('������'=1,'�Ƿ�Υ��'=0,'ʱ��'=time)
      t.data = rbind(t.data,result)
      n<-character(1)
    }
    else if (n=='1')
    {
      time <- substr(Sys.time(),1,19)
      result <- data.frame('������'=1,'�Ƿ�Υ��'=1,'ʱ��'=time)
      t.data = rbind(t.data,result)
    }
  }
  
  .end <- substr(Sys.time(),1,19)
  cat('\n\n����Υ�����:','\n\nʱ�� :',str_c(c(start,.end),collapse=' ~ '),
      '\n������ =',sum(t.data[,1]),'\nΥ���� =',sum(t.data[,2]),sep = '','\n\n')
  return(t.data)
}

#����pֵ
P_value <- function(x,cdf=pnorm,paramet= c(0,1),side=0){
  #pnormΪ��̫�ֲ���cdf��side=-1�����飬0˫�ߣ�1�Ҳ�
  #��̬�ֲ�ʱparamet=c(mu,signa)
  n <-length(paramet)
  p <-cdf(x,paramet[1],paramet[2])
  if(side <0) p
  else if(side >0) 1-p
  else
    if(p<1/2) 2*p
  else  2*(1-p)
}


#��������
library(data.table)
#��һ��ʱ��
dat   = read.csv('C:\\Users\\Manager\\Desktop\\1.csv')
.time = as.POSIXlt(dat[,3])
dat1  = cbind(dat[,-3],as.data.frame(.time))
factor.time = cut(dat1[,3],as.POSIXlt(c('2016-12-02 09:00:00','2016-12-02 09:20:00',
                                        '2016-12-02 09:40:00','2016-12-02 10:00:00')))
dat1  = cbind(dat1,as.data.frame(factor.time))

#�ڶ���ʱ��
dat   = read.csv('C:\\Users\\Manager\\Desktop\\2.csv')
.time = as.POSIXlt(dat[,3])
dat2  = cbind(dat[,-3],as.data.frame(.time))
factor.time = cut(dat2[,3],as.POSIXlt(c('2016-12-02 10:00:00','2016-12-02 10:20:00',
                                        '2016-12-02 10:40:00','2016-12-02 11:00:00')))
dat2  = cbind(dat2,as.data.frame(factor.time))

#������ʱ��
dat   =read.csv('C:\\Users\\Administrator\\Desktop\\3.csv')
.time =as.POSIXlt(dat[,3])
dat3  =cbind(dat[,-3],as.data.frame(.time))
factor.time = cut(dat3[,3],as.POSIXlt(c('2016-12-02 14:00:00','2016-12-02 14:20:00',
                                        '2016-12-02 14:40:00','2016-12-02 15:00:00')))
dat3  = cbind(dat3,as.data.frame(factor.time))
#���Ķ�ʱ��
dat   = read.csv('C:\\Users\\Administrator\\Desktop\\4.csv')
.time = as.POSIXlt(dat[,3])
dat4  = cbind(dat[,-3],as.data.frame(.time))
factor.time = cut(dat4[,3],as.POSIXlt(c('2016-12-02 17:00:00','2016-12-02 17:20:00',
                                        '2016-12-02 17:40:00','2016-12-02 18:50:05')))
dat4  = cbind(dat4,as.data.frame(factor.time))

#������
dat = rbind(dat1,dat2,dat3,dat4)
dat = data.table(dat)
dat = dat[,.(sum(������),sum(�Ƿ�Υ��)),by=factor.time]
colnames(dat) <- c('ʱ�����','������','Υ������')
dat




#ʵ������

#1.����λ�õķ��ż���
#����Υ����
options(digits=3) #������ЧС������
p = dat[,Υ������/������,by=ʱ�����]
colnames(p)[2] = 'Υ����'
#����zͳ������pֵ
S.plus = sum(p[,2]>0.05)
C = 1/2;n = dim(dat)[1L]
z_stat=(S.plus-n/2+C)/sqrt(n/4)
P_value(z_stat)

#2.����γ̼���
#�����ٴ���
#ʹ�õ�����Ϊ4��ԭ�����еġ��Ƿ�Υ�¡�
dat = c(dat1[,2],dat2[,2],dat3[,2],dat4[,2])
x=factor(c(rep(0,100),rep(1,20)))
#ʹ��tseries��������γ̼��鹤��
tseries::runs.test(x)

#3.Cox-Staut���Ƽ���
#�Աȱ�
n = 12;c = n/2
p.change = p[1:6,2]-p[1:6+c,2]
#S+��S-��Kֵ
S.plus  = sum(p.change>0);S.minus = sum(p.change<0)
K=c(S.plus,S.minus)[which.min(c(S.plus,S.minus))]
#����Zͳ����,������Pֵ
.n = S.plus + S.minus
z_stat = (K+sign(K-.n)*0.5-.n/2)/sqrt(.n/4)
P_value(z_stat,side = -1)#������

#4.��������ķ��ż���
p1=p[c(1:3,7:9),2];p2=p[!c(1:3,7:9),2]
cbind(p1,p2)
#��Zͳ����,������pֵ
S.plus = sum(p2>p1)
C = 1/2;n = dim(p1)[1L]
z_stat=(S.plus-n/2+C)/sqrt(n/4)
P_value(z_stat)

#��ͼ