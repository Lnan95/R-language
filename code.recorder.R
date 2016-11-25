code.recorder<-function(number_of_rows,language,time)
{
  if (missing(time)) time <- Sys.Date()
  if (is.numeric(time)) 
  {
    warning("请输入'2016-04-21'类型的时间信息")
    return()
  }
  x   <- data.frame(time=as.Date(time),language=toupper(language),
               number_of_rows=number_of_rows,stringsAsFactors = F)
  lib <- read.csv('code_recorder.csv',stringsAsFactors=F)
  lib <- rbind(x,lib)
  lib <- lib[order(as.Date(lib$time),decreasing = T),]
  write.csv(lib,'code_recorder.csv',row.names = FALSE)
  cat('数据已更新')
}

read.recorder   <- function() return(read.csv('code_recorder.csv',stringsAsFactors=F))
delete.recorder <- function(update=FALSE)
{
  if (!update) 
  {
    lib <- read.csv('code_recorder.csv',stringsAsFactors=F)
    print(lib)
    cat('要删除哪一行？')
    n   <- scan(n = 1,quiet = T)
    if (n<=0) return('请输入正确数值')
    print(lib[n,])
    cat('确认请输入1')
    m   <- scan(n = 1,quiet = T)
    if (m==1) lib <- lib[-n,]
    write.csv(lib,'code_recorder.csv',row.names = FALSE)
  } else {
            lib <- read.csv('code_recorder.csv',stringsAsFactors=F)
            print(lib)
            cat('修改哪一行数据？')
            m   <- scan(n = 1,quiet = T)
            print(lib[m,])
            cat('要修改哪一列数据？')
            n   <- scan(n = 1,quiet = T)
            print(as.character(lib[m,n]))
            cat('是修改',lib[m,n],'吗?是输入1')
            r   <- scan(n = 1,quiet = T)
            if (r==1) 
              {
                cat('输入新的数值:')
                if (n!=3) new <- scan(n = 1,quiet = T,what = character())
                else new <- scan(n = 1,quiet = T)
                lib[n,m] <- new
                write.csv(lib,'code_recorder.csv',row.names = FALSE)
                cat('更新成功')
              }
         }
}

undate.recorder <- function(time)
{
  if (missing(time)) time = Sys.Date()
  lib = read.csv('code_recorder.csv',stringsAsFactors=F)
  x   = lib[as.Date(lib$time)==as.Date(time),]
  if (dim(x)[1L] > 1) 
  {
    print(x)
    cat('请输入行')
    row<- scan(n = 1,quiet = T)
    if (row<1) return('行输入有误！')
    print(x[row,])
    cat('输入1确定更新')
    r  <- scan(n = 1,quiet = T)
    if (r!=1) return('操作已取消')
    else {
            cat('请输入变动的数值')
            n = scan(n = 1,quiet = T)
            lib[as.Date(lib$time)==as.Date(time),3] = lib[as.Date(lib$time)==as.Date(time),3]+n
            write.csv(lib,'code_recorder.csv',row.names = FALSE)
            return('更新成功')
          }
  } else {
            print(x)
            cat('输入1确定更新')
            r  <- scan(n = 1,quiet = T)
            if (r!=1) return('操作已取消')
            else {
                    cat('请输入变动的数值')
                    n = scan(n = 1,quiet = T)
                    lib[as.Date(lib$time)==as.Date(time),3] <- x[,3]+n
                    write.csv(lib,'code_recorder.csv',row.names = FALSE)
                    return('更新成功')
                  }
         }
  
}


x=data.frame(time=as.Date('2016-11-24'),language='R',number_of_rows=185,stringsAsFactors = F)
y=data.frame(time=as.Date('2016-11-13'),language='MATLAB',number_of_rows=475,stringsAsFactors = F)
x=rbind(x,y)
write.csv(x,'code_recorder.csv',row.names = FALSE)
