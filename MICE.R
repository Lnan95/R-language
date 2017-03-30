#缺失值是一个实际数据处理中常见的问题。其缺失机制大致可以分为完全随机缺失（MCAR），非随机缺失（MNAR）
#缺失的例子说明：假设一个数据集有3个变量 X1 , X2 , Y ,假设 X1 , X2 是完全变量, Y存在缺失值，那么
#当 Y 以0.5概率缺失，为MCAR
#当 X1<0 或者当 Y<0 , Y 发生缺失，为MNAR

#对于完全随机缺失，直接剔除一般不会带来偏误，而对于非完全随机缺失，由于缺失值出现的位置可能和其他变量有关联，
#带来的后果则相对复杂得多。
#常见处理方法
##删除法
#1.直接删除有缺失值的样本
#2.删除存在大面积缺失值的变量。或完全变量分析：如果研究的问题只涉及到全部变量中的一部分变量，
#这部分变量是完整的，那么可以只分析这几个完整变量之间的关系

##填补法
#单变量填补
#1.简单随机填补：对于每一个缺失值，从已有的该变量数据中随机抽样作为填补值，填补进缺失位置。
#仅仅考虑到了缺失变量本身，而并没有考虑到相关变量的信息。因此，信息量的利用少。

#2.均值/中位数/分位数填补：用存在缺失值的变量的已有值的均值/中位数/分位数，作为填补值。这种方法显然会导致方差偏小。

#3.回归填补：将缺失变量作为因变量，相关变量（其他变量）作为自变量，进行回归拟合，用预测值作为填补值。
#用于作为自变量的变量最好是具有完全数据（无缺失）。

#4.热平台和冷平台：热平台法又称匹配插补法，思路是在完全数据样本中，找到一个和具有缺失值的样本相似的完全数据样本，
#用完全数据样本值作为填充值，其过程有点类似于K阶近邻的思想。冷平台法又称条件均值插补法，思路是先将总体分层（聚类），
#采用样本所在层（类）的完全数据的均值来替代缺失值。

##多变量填补
#1.回归插补法：对缺失变量和完全数据变量拟合多元回归模型来预测缺失值。是多重填补法的一种应用。
#多重填补法（Multiple Imputation Missing Data）的具体技术方法众多且相对复杂，限于篇幅此处不一一展开。

#mice包介绍
#mice即是基于多重填补法构造的。基本思想是对于一个具有缺失值的变量，
#用其他变量的数据对这个变量进行拟合，再用拟合的预测值对这个变量的缺失值进行填补。
set.seed(2016)
data <- airquality
data[sample(nrow(data),7),3] <- NA
data[sample(nrow(data),7),4] <- NA
data <- data[-c(5,6)]
#采用R自带的airquality数据，其第一和第二列数据已经有很多缺失值，
#现在我们再人为地在第三个第四列中加入随机的7个缺失值。由于最后两列是月份和日期，
#不适合作为自变量，所以在缺失值填充中先剔除掉。
init = mice(data, maxit=0)#最大迭代次数
meth = init$method
predM = init$predictorMatrix
#如果不想要使全部变量都参与拟合，比如排除掉Ozone变量，加入以下代码：
#predM[, c("Ozone")]=0
#当然，即使你排除了拟合变量，并不意味着简单的将其排除，他只是不作为拟合变量，但仍然会作为被拟合变量，进行缺失值填充。

#如果你想要跳过某个变量,如Temp，不对其填补，加入以下代码：
meth[c("Temp")]=""
#在这种情况下，虽然这个变量不会被作为被拟合变量进行填充，但仍然会作为拟合变量用于拟合其他变量的缺失值。

#对于每个变量的拟合，可以指定所用的拟合方法：
meth[c("Ozone")]="norm"
meth[c("Solar.R")]="logreg"
meth[c("Wind")]="polyreg"
#=后面的双引号内的即为方法的名字，=前面的双引号中的为所需指定的变量。
#norm代表贝叶斯线性回归，logreg代表logit回归拟合，polyreg代表多项式拟合。
#对于数值型数据，默认使用随机回归添补法(pmm)；对二元因子数据，
#默认使用Logistic回归添补法(logreg)；对多元因子数据，默认使用分类回归添补法(polyreg)。
#设定完成后执行填充：
imputed = mice(data, method=meth, predictorMatrix=predM, m=5)
#这里我们选择让所有变量都进入拟合，每个变量的方法用默认方法（即不运行前面的设定代码），
#注意变量拟合时采用方法需要根据不同变量的情况选择，否则可能导致效果不佳或者算法无法计算。
#这需要事先对数据情况有一定了解。另外作为一个例子，本例仅仅作为代码演示，并不代表正确的参数选择方法。

#输出填充结果到imputed：
imputed <- complete(imputed)#1.提取拟合后的变量。
#检查是否存在缺失值
sapply(imputed, function(x) sum(is.na(x)))
#到这里就已经可以解决大部分缺失值问题了，如果想继续深入了解mice包的用法，请看下一节。
#对比
data_compare = cbind(airquality[,c(3:4)],imputed[,c(3,4)])
data_compare = data_compare[!complete.cases(data[,c(3,4)]),]
colMeans(data_compare[,c(1,2)]-data_compare[,-c(1,2)])
#      Wind       Temp 
#-0.2692308 -1.6923077 


##进阶版本
#对分类变量的填充效果一般不是非常好，不建议对分类变量（categorical variables ）做填充，本例中只对连续变量进行填充。
#假设数据是MCAR的，一般数据量的【5%】的样本存在缺失值是相对安全的比例。

#计算数据的缺失率
miss <- function(x){sum(is.na(x))/length(x)*100}
apply(data,2,miss)#每列的缺失率（%）
apply(data,1,miss)#每行的缺失率（%）
#从变量角度看，可以看到Qzone的缺失率高达25%，因此我们可能需要剔除这个变量。
#对于样本，由于只有4个变量，缺失一个就有高达25%的缺失率，权衡一下样本量，我们可以选择剔除缺失高达50%和以上的样本。

#使用mice判断缺失情况
#对于上述缺失率等缺失情况的计算，在mice包中还有更简便的函数：
md.pattern(data)#最左边一列是样本数，右边的0-1矩阵的1代表列对应的变量没有缺失，0代表有缺失。
                #最右边一列是对应模式累计缺失值个数，最下面一列是变量累积存在的缺失模式个数。
                #假如 第三行的数据410111，表示有4个样本满足这行的缺失模式，1011表示这行的缺失模式是缺失了Solar.R变量，
                #最右边的1表示这种缺失模式缺失了1个变量数据。
## Number of observations per patterns for all pairs of variables
md.pairs(data)
#The pattern rr represents the number of observations where both pairs of values are observed.
#The pattern rm represents the exact opposite, these are the number of observations where both 
#variables are missing values. The pattern mr shows the number of observations
#where the first variable's value (e.g. the row variable) is observed and second (or column) variable is missing. 
#The pattern mm is just the opposite.


#可以用VIM包获得缺失值的可视化表示（内容一样）
library(VIM)
aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, labels=names(data), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))
marginplot(data[c(1,2)])
#marginplot(data[c(1,2)])一次只表示2个变量的缺失情况，这里表示了第一和第二列变量。
#左边的红色箱线图表示有Ozone缺失的样本的Solar.R的分布，蓝色的箱线图表示的是剩下的数据点的分布。下方的2个箱线图的含义类似。
##在MCAR的假设下，蓝色和红色的箱线图应该非常接近。！！！完全随机缺失

##distributions of missing variable by another specified variable
pbox(data, pos=2)#用col2作比较
#The pbox function above will plot the marginal distribution of a variable within levels or categories of 
#another variable. Here we obtain a plot of the distribution of the variable x2 by y1 and y4 . 
#Pos=2 or position 2 in the data file refers to the fact that x2 is in the second column of the data file. 

#填充缺失值
#这次我们详细介绍mice函数的用法
tempData <- mice(data,m=5,maxit=50,meth='pmm')
summary(tempData)
#mice()函数用于生成填充矩阵tempData，其中的参数有：
#data，需要填充的数据集
#m，多重填补法的填补矩阵数。默认为5
#method，填补用的方法，pmm代表预测均值匹配(predictive mean matching),用 methods(mice) 可以看到有哪些可用的方法
#maxit，迭代次数，默认50次

#如果想要查看填充的是那些值可以用以下代码
tempData$imp$Temp#最左边的一列表示的是被填充样本序号，之后的5列是多重填补法生成的5个填补矩阵对这个变量产生的填充数。

#当然也可以如前一节一样对每个变量指定各自的拟合方法。
#最后生成完全数据集：
completedData <- complete(tempData,1)#1表示用tempData$imp中的第一个矩阵来填充，如果想要用其他矩阵则可以改成2，3，4等等。

#再次测验（maxit增加）
imputed = completedData
data_compare = cbind(airquality[,c(3:4)],imputed[,c(3,4)])
data_compare = data_compare[!complete.cases(data[,c(3,4)]),]
colMeans(data_compare[,c(1,2)]-data_compare[,-c(1,2)])
#      Wind       Temp 
#0.45384615 0.07692308 

# 查看初始数据和插补数据的分布情况
#我们利用一些有用的图对初始数据和插补后的数据分布做对比 
library(lattice)
xyplot(tempData,Ozone ~ Wind+Temp+Solar.R,pch=18,cex=1)
# 我们希望看到的是洋红点呈现出的形状（插补值）跟蓝色点（观测值）呈现出的形状是匹配的。
#从图中可以看到，插补的值的确是“近似于实际值”。 

# 另一个有用的图是密度图： 
densityplot(tempData)
#洋红线是每个插补数据集的数据密度曲线，蓝色是观测值数据的密度曲线。再次根据我们之前的假定，我们希望这些分布是相似的。 
##如何对制定的数据用指定的矩阵拟合？

# 另一个有用的可视化是由stripplot()函数得到的包含个别点的变量分布图。 
stripplot(tempData, pch = 20, cex = 1.2)

#离群值
ggplot(data = NULL, mapping = aes(x = "", y = airquality$Ozone)) + geom_boxplot(outlier.colour
                                                                     = 'red', outlier.shape = 20, width =2)
#图中可知，有一部分数据落在上四分位数的1.5倍四分位距之上，即异常值，下面通过编程，将异常值找出来
QL <- quantile(airquality$Ozone, probs = 0.25,na.rm = T)
QU <- quantile(airquality$Ozone, probs = 0.75,na.rm = T)
QU_QL <- QU-QL
airquality[which(airquality$Ozone > QU + 1.5*QU_QL),]
#异常值可用离异常点最近的点替换，或用上四分位数的1.5倍四分位距或下四分位数的1.5倍四分位距替换


## 合并
#假设我们下一步的分析是对数据拟合一个线性模型。你或许会问应该选择哪个插补数据集。
#mice包可以轻易的对每个数据集分别拟合一个模型，再把结果合并到一起。 
modelFit1 <- with(tempData,lm(Temp~ Ozone+Solar.R+Wind))
summary(pool(modelFit1))
#modelFit1变量包含所有插补数据集的拟合结果，pool()函数将结果合并到一起。显然，仅从Qzone变量来看的话，是统计显著的。
#请注意，这里除了lm()模型给出的结果外还包含其它列：fim指的是各个变量缺失信息的比例，lambda指的是每个变量对缺失数据的贡献大小。

#记住，我们之前对mice函数初始化了一个特定的seed，因此所得的结果多少依赖于我们最初的选择。
#为了减少这种影响，我们可以通过更改mice()函数默认m=5的参数来插补更多的数据集。如下所示： 
tempData2 <- mice(data,m=50,seed=245435)
modelFit2 <- with(tempData2,lm(Temp~ Ozone+Solar.R+Wind))
summary(pool(modelFit2))
#                 Pr(>|t|)
#Ozone          8.645631e-10
#在考虑初始化随机种子后，我们得到的结论跟之前基本是一致的——仅从Qzone变量来看是统计显著的。 