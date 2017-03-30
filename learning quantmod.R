library(quantmod)
library(xts)
library(TTR)
library(PerformanceAnalytics)
setwd("C://Users//Manager//Desktop//data")
##��ù�Ʊ����
sszz <- getSymbols("000001.SS",
                   from = "2003-01-01",
                   to = "2017-01-06",
                   src = "yahoo",
                   auto.assign = FALSE)#�ֶ���ֵ

  #����һ�������飩��ֱ��ץȡ�������ļ������000001.sz��xts�ļ��У����޷���getsymbol��ֵ
  getSymbols('000001.sz',src='yahoo')
  getSymbols(c("^SSEC","CHL"))
  #��ʹ�ô˷�ʽ����ù�Ʊ��Ϣֱ�Ӹ���WK������
  setSymbolLookup(WK=list(name='000002.sz',src='yahoo'))
  getSymbols("WK")#Goldman OHLC from yahoo
  getSymbols("^SSEC")#�����֤��ָ����ֵ��SSEC��

##handling data,��ΪOHLC��ʽ
  #Op,Hi,Lo,Cl,Vo,Ad,��ȡ���ݵĿ��̼���߼���ͼ����̼������ѵ������̼�
  Op(WK)
  #is.OHLC, has.OHLC, has.Op,has.Cl,has.Hi,has.Lo,has.Ad, and has.Vo
  is.OHLC(WK)
  #seriesHi��seriesLo     where and what was the high(low) point
  seriesHi(WK[c(1:200),])
  #daily percent change open to close
  #one period open to open change
  #the percent change from high to close 
  OpCl(WK) 
  OpOp(WK) 
  HiCl(WK[c(1:100)]) 
  #Lag: What was the previous value in the series
  #Next: What is the next value in the series
  #Delt: Compute the change (delta) from two prices
  Lag(Cl(WK[c(1:100),]))#��һ��ΪNA
  Delt(Op(WK),Cl(WK),k=1:3)#1�졢2���3��Ŀ��̼ۺ����̼۵Ĳ�
  #xts�Ĵ���
  #help('[.xts') provides more detail as to the specifics for those interested in learning more
  WK['2007'] #returns all Goldman's 2007 OHLC
  WK['2008'] #now just 2008
  WK['2008-01'] #now just January of 2008
  WK['2007-06::2008-01-12'] #Jun of 07 through Jan 12 of 08
  WK['::'] # everything in WK
  WK['2008::'] # everything in WK, from 2008 onward
  non.contiguous <- c('2007-01','2007-02','2007-12')
  WK[non.contiguous] 
  #ʱ���ʽCCYY-MM-DD HH:MM:SS
  #'::'���������
  #wk/wk[,1] return non-conformable arrays
  wk=WK[c(1:100),]
  wk/drop(wk[,1])#����,��
  wk/wk[,1,drop=TRUE]
  #��ǰ���ʱ�����е���ȡ
  last(WK) #returns the last obs.
  last(WK,8) #returns the last 8 obs.
  first(wk)
  last(WK, '3 weeks')
  last(WK, '-3 weeks') # all except the last 3 weeks
  last(WK, '3 months')
  last(first(WK, '2 weeks'), '3 days') 
  
  #���ݵ�ʱ��ת����day to month
  to.weekly(WK)
  periodicity(wk)
  unclass(periodicity(wk))
  to.weekly(wk)
  to.monthly(wk)
  periodicity(to.monthly(wk))
  ndays(wk); nweeks(wk); nyears(wk)
  # Let's try some non-OHLC to start
  getFX("USD/EUR")#��Ҫ��ǽ
  "USDEUR"
  periodicity(USDEUR)
  to.weekly(USDEUR)
  periodicity(to.weekly(USDEUR)) 
  #Apply by Period
  endpoints(wk,on="months") #Ѱ��ÿ�½�����index
  # find the maximum closing price each week
  apply.weekly(wk,FUN=function(x) { max(Cl(x)) } )
  # the same thing - only more general
  period.apply(wk,endpoints(wk,on='weeks'),
                   FUN=function(x) { max(Cl(x)) } )
  # same thing - only 50x faster!
  as.numeric(period.max(Cl(wk),endpoints(wk,on='weeks'))) 
  #period.min, period.sum, and period.prod
  period.min(Cl(wk),INDEX = endpoints(wk,on='weeks'))#ÿ��Cl�����ֵ
  period.prod(Cl(wk),INDEX = endpoints(wk,on='weeks'))#ÿ�����̵��۳�
  #Period Returns :fast and reliable way to calculate returns over calendar periods
  
  # Quick returns - quantmod style
  dailyReturn(wk) # returns by day
  weeklyReturn(wk) # returns by week
  monthlyReturn(wk) # returns by month, indexed by yearmon
  # daily,weekly,monthly,quarterly, and yearly
  allReturns(wk) # note the plural 
  
##visualize financial data with standard financial charting tools.
  #��������ͼ
  chartSeries(WK[c(1:100),],name="���")
  chartSeries(WK, up.col='red', dn.col='green', TA="addVo(); addMACD(); addSMA(n=10)")
  chartSeries(SSEC,theme="white")#��ɫ����
  #��������ͼ
  candleChart(WK[c(1:100),],theme="white",up.col='red', dn.col='green')
  #��������
  barChart(AAPL[c(1:100),])
  #��ͼ
  lineChart(AAPL[c(1:100),])
  #reChart����x���Ϊmonth��ʽ
  candleChart(WK,theme='white', type='candles') 
  reChart(major.ticks='months',subset='first 16 weeks')
  
  #����ָ��,�����������ļ�����
  addMACD()
  addBBands() 
  # The TA argument to chartSeries is one way to specify the
  #indicator calls to be applied to the chart.
  #NULL mean don't draw any.
  #addTA allows you to add basic indicators
  # to your charts - even if they aren't part of quantmod. 
  chartSeries(wk, TA=NULL,theme = 'white') 
  addTA(OpCl(wk),col='blue', type='h')
  # Using newTA it is possible to create your own
  # generic TA function --- let's call it addOpCl
  addOpCl <- newTA(OpCl,col='green',type='h')
  addOpCl()


  
  #���о�
  setSymbolLookup(SPY='yahoo', VXN=list(name='^VIX',src='yahoo'))
  mm <- specifyModel(Next(OpCl(SPY)) ~ OpCl(SPY) + Cl(VIX))
  modelData(mm)
  

  
  setSymbolLookup(WK=list(name="C:\\Users\\Manager\\Desktop\\data\\000001",src='csv'))
  
  
  
  
##��R����ʵ�ּ򵥵Ľ��׻ز�
  
  # �Ȼ�ȡS&P500�Ľ������ݣ�Ȼ����������̼ۣ��ɺ���Cl()��ȡ��������5�վ���ֵ��
  getSymbols('^GSPC')
  close <- Cl(GSPC)
  mv5 <- SMA(close, 5)
  # �����ǵ����̼۴���5�վ��ߣ������������У�ȡ1�����������֣�ȡ0����-1�Ǵ������գ������á���
  sig <- ifelse(close < mv5, 1, 0)#-1Ϊ����
  # ʹ��Lag()���ź������򡰹�ȥ���Ƴ�һ�죬������������źţ�Ӧ�õ����졣
  sig <- Lag(sig) #���������򡰹�ȥ���ӳ�һ��
  # ������������
  # discrete��������ɢ��ʽ���㵱�������ʣ���(Close-Close(-1))/Close(-1)
  # continous������������ʽ���㵱�������ʣ���ln(Close/Close(-1))
  roc <- ROC(type='discrete',close) 
  ret <- roc * sig
  # ������������ͼ
  charts.PerformanceSummary(ret)
  # ������İ���ǻ������棬�൱�ڶ�cumprod(1+ret)�Ļ�ͼ��
  # �ڶ����������棬�൱�ڶ�retԭʼ�������ݵĻ�ͼ��
  # ����������µ�ͼ���ֳơ�ˮ��ͼ���������µ��ɷֶ�����������������Ƿ�������״�����о��ֲ���ʩ��
  
  #findDrawdowns:���ػس�����ʼʱ�䣬ʱ�������س���ֵ������sortDrawdowns���������س���
  findDrawdowns(ret[-c(1:5)])  #ȥna      
  sortDrawdowns(findDrawdowns(ret[-c(1:5)]))
  #maxDrawdown:��������ʱ�����е����س�
  t(round(maxDrawdown((ret[-c(1:5)])),4))
  #table.Drawdowns:�������س���ͳ��������
  table.Drawdowns(ret[-c(1:5)])
  
##quantmod�趨ģ����ʽ
  #specifyModel���������趨ͳ������ģ�͵�ģ����ʽ
  q.model <- specifyModel(Next(OpCl(CHL)) ~ Lag(OpHi(CHL),0:3) + Hi(SSEC))
  #�鿴ģ������
  head(modelData(q.model))
  #buildData()��������ֱ������ģ�Ͷ�Ӧ�����ݼ���
  #�������൱����specifyModel+modelData�����ǿ���һ���������Ĺ��̡�
  bD=buildData(Next(OpCl(CHL)) ~ Lag(OpHi(CHL),0:3) + Hi(SSEC))
  #����ģ�Ͳ���
  ## �򵥵�����ģ��
  bM <- buildModel(q.model,method='lm',training.per=c('2013-08-01','2013-09-30'))#'lm'
  ## ��ʾ���
  bM
  summary(bM)
  #ģ�ͽ����ȡ
  getModelData(bM, na.rm = TRUE)
  #�ز�
  tradeModel(bM)
  
  
  
  
  
  
  
  
  library(compiler)
  TstFun <- function()
  {
    for( i in 1:10)
    {  
      x1 <- (rnorm(1000000,100,30))
    }
  }
  # ʹ��cmpfun()��������TstFun()����Ϊ�ֽ��뺯��
  # ͬʱ���ֽ��뺯�����ص��ڴ����Ա����
  CTstFun <- cmpfun(TstFun)
  # ��ͨ��ʽ���ú���
  system.time(TstFun())
  #user  system elapsed 
  #4.85    0.08    5.10 
  
  # �����ֽ��뺯��
  # ����ʱ�������Լ���
  system.time(CTstFun())  
  #user  system elapsed 
  #4.56    0.02    4.75
  
  # disassemble()���������ص��ڴ��е��ֽ��뺯��ж�ص�
  f  <- function(x) print(x)
  cf <- cmpfun(f)
  # ж�ص�����
  disassemble(CTstFun)
  # ж��֮���ǿ���ʹ��
  system.time(CTstFun())  
  