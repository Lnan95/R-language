#ȱʧֵ��һ��ʵ�����ݴ����г��������⡣��ȱʧ���ƴ��¿��Է�Ϊ��ȫ���ȱʧ��MCAR���������ȱʧ��MNAR��
#ȱʧ������˵��������һ�����ݼ���3������ X1 , X2 , Y ,���� X1 , X2 ����ȫ����, Y����ȱʧֵ����ô
#�� Y ��0.5����ȱʧ��ΪMCAR
#�� X1<0 ���ߵ� Y<0 , Y ����ȱʧ��ΪMNAR

#������ȫ���ȱʧ��ֱ���޳�һ�㲻�����ƫ�󣬶����ڷ���ȫ���ȱʧ������ȱʧֵ���ֵ�λ�ÿ��ܺ����������й�����
#�����ĺ������Ը��ӵöࡣ
#������������
##ɾ����
#1.ֱ��ɾ����ȱʧֵ������
#2.ɾ�����ڴ����ȱʧֵ�ı���������ȫ��������������о�������ֻ�漰��ȫ�������е�һ���ֱ�����
#�ⲿ�ֱ����������ģ���ô����ֻ�����⼸����������֮��Ĺ�ϵ

##���
#�������
#1.������������ÿһ��ȱʧֵ�������еĸñ������������������Ϊ�ֵ�����ȱʧλ�á�
#�������ǵ���ȱʧ��������������û�п��ǵ���ر�������Ϣ����ˣ���Ϣ���������١�

#2.��ֵ/��λ��/��λ������ô���ȱʧֵ�ı���������ֵ�ľ�ֵ/��λ��/��λ������Ϊ�ֵ�����ַ�����Ȼ�ᵼ�·���ƫС��

#3.�ع������ȱʧ������Ϊ���������ر�����������������Ϊ�Ա��������лع���ϣ���Ԥ��ֵ��Ϊ�ֵ��
#������Ϊ�Ա����ı�������Ǿ�����ȫ���ݣ���ȱʧ����

#4.��ƽ̨����ƽ̨����ƽ̨���ֳ�ƥ��岹����˼·������ȫ���������У��ҵ�һ���;���ȱʧֵ���������Ƶ���ȫ����������
#����ȫ��������ֵ��Ϊ���ֵ��������е�������K�׽��ڵ�˼�롣��ƽ̨���ֳ�������ֵ�岹����˼·���Ƚ�����ֲ㣨���ࣩ��
#�����������ڲ㣨�ࣩ����ȫ���ݵľ�ֵ�����ȱʧֵ��

##������
#1.�ع�岹������ȱʧ��������ȫ���ݱ�����϶�Ԫ�ع�ģ����Ԥ��ȱʧֵ���Ƕ��������һ��Ӧ�á�
#���������Multiple Imputation Missing Data���ľ��弼�������ڶ�����Ը��ӣ�����ƪ���˴���һһչ����

#mice������
#mice���ǻ��ڶ����������ġ�����˼���Ƕ���һ������ȱʧֵ�ı�����
#���������������ݶ��������������ϣ�������ϵ�Ԥ��ֵ�����������ȱʧֵ�������
set.seed(2016)
data <- airquality
data[sample(nrow(data),7),3] <- NA
data[sample(nrow(data),7),4] <- NA
data <- data[-c(5,6)]
#����R�Դ���airquality���ݣ����һ�͵ڶ��������Ѿ��кܶ�ȱʧֵ��
#������������Ϊ���ڵ������������м��������7��ȱʧֵ����������������·ݺ����ڣ�
#���ʺ���Ϊ�Ա�����������ȱʧֵ��������޳�����
init = mice(data, maxit=0)#����������
meth = init$method
predM = init$predictorMatrix
#�������Ҫʹȫ��������������ϣ������ų���Ozone�������������´��룺
#predM[, c("Ozone")]=0
#��Ȼ����ʹ���ų�����ϱ�����������ζ�ż򵥵Ľ����ų�����ֻ�ǲ���Ϊ��ϱ���������Ȼ����Ϊ����ϱ���������ȱʧֵ��䡣

#�������Ҫ����ĳ������,��Temp������������������´��룺
meth[c("Temp")]=""
#����������£���Ȼ����������ᱻ��Ϊ����ϱ���������䣬����Ȼ����Ϊ��ϱ��������������������ȱʧֵ��

#����ÿ����������ϣ�����ָ�����õ���Ϸ�����
meth[c("Ozone")]="norm"
meth[c("Solar.R")]="logreg"
meth[c("Wind")]="polyreg"
#=�����˫�����ڵļ�Ϊ���������֣�=ǰ���˫�����е�Ϊ����ָ���ı�����
#norm������Ҷ˹���Իع飬logreg����logit�ع���ϣ�polyreg��������ʽ��ϡ�
#������ֵ�����ݣ�Ĭ��ʹ������ع�������(pmm)���Զ�Ԫ�������ݣ�
#Ĭ��ʹ��Logistic�ع�������(logreg)���Զ�Ԫ�������ݣ�Ĭ��ʹ�÷���ع�������(polyreg)��
#�趨��ɺ�ִ����䣺
imputed = mice(data, method=meth, predictorMatrix=predM, m=5)
#��������ѡ�������б�����������ϣ�ÿ�������ķ�����Ĭ�Ϸ�������������ǰ����趨���룩��
#ע��������ʱ���÷�����Ҫ���ݲ�ͬ���������ѡ�񣬷�����ܵ���Ч�����ѻ����㷨�޷����㡣
#����Ҫ���ȶ����������һ���˽⡣������Ϊһ�����ӣ�����������Ϊ������ʾ������������ȷ�Ĳ���ѡ�񷽷���

#����������imputed��
imputed <- complete(imputed)#1.��ȡ��Ϻ�ı�����
#����Ƿ����ȱʧֵ
sapply(imputed, function(x) sum(is.na(x)))
#��������Ѿ����Խ���󲿷�ȱʧֵ�����ˣ��������������˽�mice�����÷����뿴��һ�ڡ�
#�Ա�
data_compare = cbind(airquality[,c(3:4)],imputed[,c(3,4)])
data_compare = data_compare[!complete.cases(data[,c(3,4)]),]
colMeans(data_compare[,c(1,2)]-data_compare[,-c(1,2)])
#      Wind       Temp 
#-0.2692308 -1.6923077 


##���װ汾
#�Է�����������Ч��һ�㲻�Ƿǳ��ã�������Է��������categorical variables ������䣬������ֻ����������������䡣
#����������MCAR�ģ�һ���������ġ�5%������������ȱʧֵ����԰�ȫ�ı�����

#�������ݵ�ȱʧ��
miss <- function(x){sum(is.na(x))/length(x)*100}
apply(data,2,miss)#ÿ�е�ȱʧ�ʣ�%��
apply(data,1,miss)#ÿ�е�ȱʧ�ʣ�%��
#�ӱ����Ƕȿ������Կ���Qzone��ȱʧ�ʸߴ�25%��������ǿ�����Ҫ�޳����������
#��������������ֻ��4��������ȱʧһ�����иߴ�25%��ȱʧ�ʣ�Ȩ��һ�������������ǿ���ѡ���޳�ȱʧ�ߴ�50%�����ϵ�������

#ʹ��mice�ж�ȱʧ���
#��������ȱʧ�ʵ�ȱʧ����ļ��㣬��mice���л��и����ĺ�����
md.pattern(data)#�����һ�������������ұߵ�0-1�����1�����ж�Ӧ�ı���û��ȱʧ��0������ȱʧ��
                #���ұ�һ���Ƕ�Ӧģʽ�ۼ�ȱʧֵ������������һ���Ǳ����ۻ����ڵ�ȱʧģʽ������
                #���� �����е�����410111����ʾ��4�������������е�ȱʧģʽ��1011��ʾ���е�ȱʧģʽ��ȱʧ��Solar.R������
                #���ұߵ�1��ʾ����ȱʧģʽȱʧ��1���������ݡ�
## Number of observations per patterns for all pairs of variables
md.pairs(data)
#The pattern rr represents the number of observations where both pairs of values are observed.
#The pattern rm represents the exact opposite, these are the number of observations where both 
#variables are missing values. The pattern mr shows the number of observations
#where the first variable's value (e.g. the row variable) is observed and second (or column) variable is missing. 
#The pattern mm is just the opposite.


#������VIM�����ȱʧֵ�Ŀ��ӻ���ʾ������һ����
library(VIM)
aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, labels=names(data), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))
marginplot(data[c(1,2)])
#marginplot(data[c(1,2)])һ��ֻ��ʾ2��������ȱʧ����������ʾ�˵�һ�͵ڶ��б�����
#��ߵĺ�ɫ����ͼ��ʾ��Ozoneȱʧ��������Solar.R�ķֲ�����ɫ������ͼ��ʾ����ʣ�µ����ݵ�ķֲ����·���2������ͼ�ĺ������ơ�
##��MCAR�ļ����£���ɫ�ͺ�ɫ������ͼӦ�÷ǳ��ӽ�����������ȫ���ȱʧ

##distributions of missing variable by another specified variable
pbox(data, pos=2)#��col2���Ƚ�
#The pbox function above will plot the marginal distribution of a variable within levels or categories of 
#another variable. Here we obtain a plot of the distribution of the variable x2 by y1 and y4 . 
#Pos=2 or position 2 in the data file refers to the fact that x2 is in the second column of the data file. 

#���ȱʧֵ
#���������ϸ����mice�������÷�
tempData <- mice(data,m=5,maxit=50,meth='pmm')
summary(tempData)
#mice()������������������tempData�����еĲ����У�
#data����Ҫ�������ݼ�
#m��������������������Ĭ��Ϊ5
#method����õķ�����pmm����Ԥ���ֵƥ��(predictive mean matching),�� methods(mice) ���Կ�������Щ���õķ���
#maxit������������Ĭ��50��

#�����Ҫ�鿴��������Щֵ���������´���
tempData$imp$Temp#����ߵ�һ�б�ʾ���Ǳ����������ţ�֮���5���Ƕ���������ɵ�5����������������������������

#��ȻҲ������ǰһ��һ����ÿ������ָ�����Ե���Ϸ�����
#���������ȫ���ݼ���
completedData <- complete(tempData,1)#1��ʾ��tempData$imp�еĵ�һ����������䣬�����Ҫ��������������Ըĳ�2��3��4�ȵȡ�

#�ٴβ��飨maxit���ӣ�
imputed = completedData
data_compare = cbind(airquality[,c(3:4)],imputed[,c(3,4)])
data_compare = data_compare[!complete.cases(data[,c(3,4)]),]
colMeans(data_compare[,c(1,2)]-data_compare[,-c(1,2)])
#      Wind       Temp 
#0.45384615 0.07692308 

# �鿴��ʼ���ݺͲ岹���ݵķֲ����
#��������һЩ���õ�ͼ�Գ�ʼ���ݺͲ岹������ݷֲ����Ա� 
library(lattice)
xyplot(tempData,Ozone ~ Wind+Temp+Solar.R,pch=18,cex=1)
# ����ϣ����������������ֳ�����״���岹ֵ������ɫ�㣨�۲�ֵ�����ֳ�����״��ƥ��ġ�
#��ͼ�п��Կ������岹��ֵ��ȷ�ǡ�������ʵ��ֵ���� 

# ��һ�����õ�ͼ���ܶ�ͼ�� 
densityplot(tempData)
#�������ÿ���岹���ݼ��������ܶ����ߣ���ɫ�ǹ۲�ֵ���ݵ��ܶ����ߡ��ٴθ�������֮ǰ�ļٶ�������ϣ����Щ�ֲ������Ƶġ� 
##��ζ��ƶ���������ָ���ľ�����ϣ�

# ��һ�����õĿ��ӻ�����stripplot()�����õ��İ��������ı����ֲ�ͼ�� 
stripplot(tempData, pch = 20, cex = 1.2)

#��Ⱥֵ
ggplot(data = NULL, mapping = aes(x = "", y = airquality$Ozone)) + geom_boxplot(outlier.colour
                                                                     = 'red', outlier.shape = 20, width =2)
#ͼ�п�֪����һ���������������ķ�λ����1.5���ķ�λ��֮�ϣ����쳣ֵ������ͨ����̣����쳣ֵ�ҳ���
QL <- quantile(airquality$Ozone, probs = 0.25,na.rm = T)
QU <- quantile(airquality$Ozone, probs = 0.75,na.rm = T)
QU_QL <- QU-QL
airquality[which(airquality$Ozone > QU + 1.5*QU_QL),]
#�쳣ֵ�������쳣������ĵ��滻���������ķ�λ����1.5���ķ�λ������ķ�λ����1.5���ķ�λ���滻


## �ϲ�
#����������һ���ķ����Ƕ��������һ������ģ�͡����������Ӧ��ѡ���ĸ��岹���ݼ���
#mice���������׵Ķ�ÿ�����ݼ��ֱ����һ��ģ�ͣ��ٰѽ���ϲ���һ�� 
modelFit1 <- with(tempData,lm(Temp~ Ozone+Solar.R+Wind))
summary(pool(modelFit1))
#modelFit1�����������в岹���ݼ�����Ͻ����pool()����������ϲ���һ����Ȼ������Qzone���������Ļ�����ͳ�������ġ�
#��ע�⣬�������lm()ģ�͸����Ľ���⻹���������У�fimָ���Ǹ�������ȱʧ��Ϣ�ı�����lambdaָ����ÿ��������ȱʧ���ݵĹ��״�С��

#��ס������֮ǰ��mice������ʼ����һ���ض���seed��������õĽ���������������������ѡ��
#Ϊ�˼�������Ӱ�죬���ǿ���ͨ������mice()����Ĭ��m=5�Ĳ������岹��������ݼ���������ʾ�� 
tempData2 <- mice(data,m=50,seed=245435)
modelFit2 <- with(tempData2,lm(Temp~ Ozone+Solar.R+Wind))
summary(pool(modelFit2))
#                 Pr(>|t|)
#Ozone          8.645631e-10
#�ڿ��ǳ�ʼ��������Ӻ����ǵõ��Ľ��۸�֮ǰ������һ�µġ�������Qzone����������ͳ�������ġ� 