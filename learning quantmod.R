library(quantmod)
library(xts)
library(TTR)
library(PerformanceAnalytics)
setwd("C://Users//Manager//Desktop//data")
##获得股票数据
sszz <- getSymbols("000001.SS",
                   from = "2003-01-01",
                   to = "2017-01-06",
                   src = "yahoo",
                   auto.assign = FALSE)#手动赋值

  #方法一（不建议），直接抓取，但是文件会存在000001.sz的xts文件中，且无法用getsymbol赋值
  getSymbols('000001.sz',src='yahoo')
  getSymbols(c("^SSEC","CHL"))
  #可使用此方式将获得股票信息直接附在WK变量上
  setSymbolLookup(WK=list(name='000002.sz',src='yahoo'))
  getSymbols("WK")#Goldman OHLC from yahoo
  getSymbols("^SSEC")#获得上证综指，赋值到SSEC中

##handling data,多为OHLC格式
  #Op,Hi,Lo,Cl,Vo,Ad,提取数据的开盘价最高价最低价收盘价量和已调整收盘价
  Op(WK)
  #is.OHLC, has.OHLC, has.Op,has.Cl,has.Hi,has.Lo,has.Ad, and has.Vo
  is.OHLC(WK)
  #seriesHi、seriesLo     where and what was the high(low) point
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
  Lag(Cl(WK[c(1:100),]))#第一天为NA
  Delt(Op(WK),Cl(WK),k=1:3)#1天、2天和3天的开盘价和收盘价的差
  #xts的处理
  #help('[.xts') provides more detail as to the specifics for those interested in learning more
  WK['2007'] #returns all Goldman's 2007 OHLC
  WK['2008'] #now just 2008
  WK['2008-01'] #now just January of 2008
  WK['2007-06::2008-01-12'] #Jun of 07 through Jan 12 of 08
  WK['::'] # everything in WK
  WK['2008::'] # everything in WK, from 2008 onward
  non.contiguous <- c('2007-01','2007-02','2007-12')
  WK[non.contiguous] 
  #时间格式CCYY-MM-DD HH:MM:SS
  #'::'区域操作符
  #wk/wk[,1] return non-conformable arrays
  wk=WK[c(1:100),]
  wk/drop(wk[,1])#可以,或
  wk/wk[,1,drop=TRUE]
  #最前最后时间序列的提取
  last(WK) #returns the last obs.
  last(WK,8) #returns the last 8 obs.
  first(wk)
  last(WK, '3 weeks')
  last(WK, '-3 weeks') # all except the last 3 weeks
  last(WK, '3 months')
  last(first(WK, '2 weeks'), '3 days') 
  
  #数据的时间转化，day to month
  to.weekly(WK)
  periodicity(wk)
  unclass(periodicity(wk))
  to.weekly(wk)
  to.monthly(wk)
  periodicity(to.monthly(wk))
  ndays(wk); nweeks(wk); nyears(wk)
  # Let's try some non-OHLC to start
  getFX("USD/EUR")#需要翻墙
  "USDEUR"
  periodicity(USDEUR)
  to.weekly(USDEUR)
  periodicity(to.weekly(USDEUR)) 
  #Apply by Period
  endpoints(wk,on="months") #寻找每月结束的index
  # find the maximum closing price each week
  apply.weekly(wk,FUN=function(x) { max(Cl(x)) } )
  # the same thing - only more general
  period.apply(wk,endpoints(wk,on='weeks'),
                   FUN=function(x) { max(Cl(x)) } )
  # same thing - only 50x faster!
  as.numeric(period.max(Cl(wk),endpoints(wk,on='weeks'))) 
  #period.min, period.sum, and period.prod
  period.min(Cl(wk),INDEX = endpoints(wk,on='weeks'))#每周Cl的最低值
  period.prod(Cl(wk),INDEX = endpoints(wk,on='weeks'))#每周收盘的累乘
  #Period Returns :fast and reliable way to calculate returns over calendar periods
  
  # Quick returns - quantmod style
  dailyReturn(wk) # returns by day
  weeklyReturn(wk) # returns by week
  monthlyReturn(wk) # returns by month, indexed by yearmon
  # daily,weekly,monthly,quarterly, and yearly
  allReturns(wk) # note the plural 
  
##visualize financial data with standard financial charting tools.
  #绘制蜡烛图
  chartSeries(WK[c(1:100),],name="万科")
  chartSeries(WK, up.col='red', dn.col='green', TA="addVo(); addMACD(); addSMA(n=10)")
  chartSeries(SSEC,theme="white")#白色背景
  #绘制蜡烛图
  candleChart(WK[c(1:100),],theme="white",up.col='red', dn.col='green')
  #简易蜡烛
  barChart(AAPL[c(1:100),])
  #线图
  lineChart(AAPL[c(1:100),])
  #reChart，将x轴改为month格式
  candleChart(WK,theme='white', type='candles') 
  reChart(major.ticks='months',subset='first 16 weeks')
  
  #增加指标,其余在量化文件夹中
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


  
  #待研究
  setSymbolLookup(SPY='yahoo', VXN=list(name='^VIX',src='yahoo'))
  mm <- specifyModel(Next(OpCl(SPY)) ~ OpCl(SPY) + Cl(VIX))
  modelData(mm)
  

  
  setSymbolLookup(WK=list(name="C:\\Users\\Manager\\Desktop\\data\\000001",src='csv'))
  
  
  
  
##用R语言实现简单的交易回测
  
  # 先获取S&P500的交易数据，然后根据其收盘价（由函数Cl()抽取）计算其5日均线值：
  getSymbols('^GSPC')
  close <- Cl(GSPC)
  mv5 <- SMA(close, 5)
  # 策略是当收盘价大于5日均线，代表可以入市，取1，否则代表清仓，取0。（-1是代表卖空，不适用。）
  sig <- ifelse(close < mv5, 1, 0)#-1为做空
  # 使用Lag()将信号序列向“过去”推迟一天，代表将昨天的信号，应用到今天。
  sig <- Lag(sig) #将该序列向“过去”延迟一天
  # 计算收益序列
  # discrete代表用离散方式计算当天收益率，即(Close-Close(-1))/Close(-1)
  # continous代表用连续方式计算当天收益率，即ln(Close/Close(-1))
  roc <- ROC(type='discrete',close) 
  ret <- roc * sig
  # 画出策略收益图
  charts.PerformanceSummary(ret)
  # 最上面的板块是积累收益，相当于对cumprod(1+ret)的绘图；
  # 第二个是日收益，相当于对ret原始收益数据的绘图；
  # 最下面的是下跌图（又称“水下图”），将下跌成分独立绘出，有助于我们分析亏损状况和研究弥补措施。
  
  #findDrawdowns:返回回撤的起始时间，时间间隔，回撤数值，常与sortDrawdowns连用找最大回撤。
  findDrawdowns(ret[-c(1:5)])  #去na      
  sortDrawdowns(findDrawdowns(ret[-c(1:5)]))
  #maxDrawdown:返回收益时间序列的最大回撤
  t(round(maxDrawdown((ret[-c(1:5)])),4))
  #table.Drawdowns:返回最差回撤的统计量表格。
  table.Drawdowns(ret[-c(1:5)])
  
##quantmod设定模型形式
  #specifyModel函数可以设定统计量化模型的模型形式
  q.model <- specifyModel(Next(OpCl(CHL)) ~ Lag(OpHi(CHL),0:3) + Hi(SSEC))
  #查看模型数据
  head(modelData(q.model))
  #buildData()函数可以直接生成模型对应的数据集，
  #其作用相当于是specifyModel+modelData。我们可以一步完成上面的过程。
  bD=buildData(Next(OpCl(CHL)) ~ Lag(OpHi(CHL),0:3) + Hi(SSEC))
  #估计模型参数
  ## 简单的线型模型
  bM <- buildModel(q.model,method='lm',training.per=c('2013-08-01','2013-09-30'))#'lm'
  ## 显示结果
  bM
  summary(bM)
  #模型结果提取
  getModelData(bM, na.rm = TRUE)
  #回测
  tradeModel(bM)
  
  
  
  
  
  
  
  
  library(compiler)
  TstFun <- function()
  {
    for( i in 1:10)
    {  
      x1 <- (rnorm(1000000,100,30))
    }
  }
  # 使用cmpfun()函数编译TstFun()函数为字节码函数
  # 同时将字节码函数加载到内存中以便调用
  CTstFun <- cmpfun(TstFun)
  # 普通方式调用函数
  system.time(TstFun())
  #user  system elapsed 
  #4.85    0.08    5.10 
  
  # 调用字节码函数
  # 运行时间有明显减少
  system.time(CTstFun())  
  #user  system elapsed 
  #4.56    0.02    4.75
  
  # disassemble()函数将加载到内存中的字节码函数卸载掉
  f  <- function(x) print(x)
  cf <- cmpfun(f)
  # 卸载掉函数
  disassemble(CTstFun)
  # 卸载之后还是可以使用
  system.time(CTstFun())  
  