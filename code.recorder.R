code.recorder<-function(number_of_rows,language,note='',time)
{
  if (missing(time)) time <- Sys.Date()
  if (is.numeric(time)) 
  {
    warning("������'2016-04-21'���͵�ʱ����Ϣ")
    return()
  }
  x   <- data.frame(time=as.Date(time),language=toupper(language),
                    number_of_rows=number_of_rows,note=note,stringsAsFactors = F)
  lib <- read.csv('code_recorder.csv',stringsAsFactors=F)
  lib <- rbind(x,lib)
  lib <- lib[order(as.Date(lib$time),decreasing = T),]
  write.csv(lib,'code_recorder.csv',row.names = FALSE)
  cat('�����Ѹ���')
}

read.recorder   <- function() return(read.csv('code_recorder.csv',stringsAsFactors=F))
delete.recorder <- function(update=FALSE)
{
  if (!update) 
  {
    lib <- read.csv('code_recorder.csv',stringsAsFactors=F)
    print(lib)
    cat('Ҫɾ����һ�У�')
    n   <- scan(n = 1,quiet = T)
    if (n<=0) return('��������ȷ��ֵ')
    print(lib[n,])
    cat('ȷ��������1')
    m   <- scan(n = 1,quiet = T)
    if (m==1) lib <- lib[-n,]
    write.csv(lib,'code_recorder.csv',row.names = FALSE)
    cat('ɾ���ɹ�')
  } else {
    lib <- read.csv('code_recorder.csv',stringsAsFactors=F)
    print(lib)
    cat('�޸���һ�����ݣ�')
    m   <- scan(n = 1,quiet = T)
    print(lib[m,])
    cat('Ҫ�޸���һ�����ݣ�')
    n   <- scan(n = 1,quiet = T)
    print(as.character(lib[m,n]))
    cat('���޸�',lib[m,n],'��?������1')
    r   <- scan(n = 1,quiet = T)
    if (r==1) 
    {
      cat('�����µ���ֵ:')
      if (n!=3) new <- scan(n = 1,quiet = T,what = character())
      else new <- scan(n = 1,quiet = T)
      lib[n,m] <- new
      write.csv(lib,'code_recorder.csv',row.names = FALSE)
      cat('���³ɹ�')
    }
  }
}

update.recorder <- function(time)
{
  if (missing(time)) time = Sys.Date()
  lib = read.csv('code_recorder.csv',stringsAsFactors=F)
  x   = lib[as.Date(lib$time)==as.Date(time),]
  if (dim(x)[1L] > 1)
  {
    print(x)
    cat('��������')
    row<- scan(n = 1,quiet = T)
    if (row<1) return('����������')
    print(x[row,])
    cat('����1ȷ������')
    r  <- scan(n = 1,quiet = T)
    if (r!=1) return('������ȡ��')
    else {
      cat('������䶯����ֵ')
      n = scan(n = 1,quiet = T)
      lib[as.Date(lib$time)==as.Date(time),][row,3] = lib[as.Date(lib$time)==as.Date(time),][row,3]+n
      write.csv(lib,'code_recorder.csv',row.names = FALSE)
      return('���³ɹ�')
    }
  } else {
    print(x)
    cat('����1ȷ������')
    r  <- scan(n = 1,quiet = T)
    if (r!=1) return('������ȡ��')
    else {
      cat('������䶯����ֵ')
      n = scan(n = 1,quiet = T)
      lib[as.Date(lib$time)==as.Date(time),3] <- lib[as.Date(lib$time)==as.Date(time),3]+n
      write.csv(lib,'code_recorder.csv',row.names = FALSE)
      return('���³ɹ�')
    }
  }
}


x=data.frame(time=as.Date('2016-11-24'),language='R',number_of_rows=185,note='ggplot,echarts',stringsAsFactors = F)
y=data.frame(time=as.Date('2016-11-13'),language='MATLAB',number_of_rows=475,note='classify tree',stringsAsFactors = F)
x=rbind(x,y)
#write.csv(x,'code_recorder.csv',row.names = FALSE)