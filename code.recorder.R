##================================================================##
###  It's a simple R code to record your programming learning.   ###
###  If you may want to						                               ###
###  1) record the number of code you have programmed.	         ###
###  2) visualize your Programming training process. 			       ###
###	 							                                               ###
###  Here's a solution :)	                                       ###
##================================================================##

##================================================================##
###  some explanation of the function below:                     ###
###  you can						                                         ###
###  1) keep a record by code.recorder() .	                     ###
###  2) read your record by read.record(). 			                 ###
###	 3) delete or edit your record by delete.record()					   ###
###  and the last function my_try(),which is edit by the function  ###
###  try().it is used to prevent wrong type of data.             ###
##================================================================##

#Encoding:CP936
#author:Linsinan
#orgnization:ZUEL
#time:2016-12-31
#R version 3.2.5 
#Platform: i386-w64-mingw32/i386 (32-bit)
#Running under: Windows >= 8 (build 9200)
require(tidyverse)

#记录数据
code.recorder <- function(number_of_rows, language, note='', time = Sys.Date())
{
  #输入参数：1.代码行数，int；2.语言，文本格式；3.代码内容，文本格式，4.时间，默认参数为当日。
  #数据输入后，数据会录入储存文件。
  if (is.numeric(time)) 
  {
    warning("请输入'2016-04-21'类型的时间信息")
    return()
  }
  if (!is.numeric(number_of_rows)) return("输入类型有误。")
  my_try({a=as.Date(time)})
  new_record <- data.frame(time=as.Date(time),language=toupper(language),
                    number_of_rows=number_of_rows,note=note)
  old_record <- read.record()
  new_record %>% rbind(old_record) %>% arrange(desc(time)) %>% 
    write.csv('code_recorder.csv', row.names = FALSE)
  cat('数据已更新')
}

#数据读取
read.record <- function(Asfactor = T) 
{
  if (!any(Asfactor %in% c(T,F,1,0)))
  {
    Asfactor <- TRUE
    cat('stringsAsFactors : TRUE\n')
  }
  return(my_try2({read.csv('code_recorder.csv', stringsAsFactors = Asfactor)}))
}


#数据调整，update=F即可修改单独一个数据
edit.record <- function(update=T,Justhead=15)
{
  #数据调整函数，默认是删除数据；如果需要更新数据，update=T，按行引索修复。
  #如果记录量较多可以使用Justhead调整显示的数据。
  if (!update) 
  {
    record <- read.record(F)
    if (!nrow(record)) return("并没有找到数据")
    if (Justhead) print(head(record, Justhead))
    else print(record)
    cat('要删除哪一行？')
    n <- scan(n = 1,quiet = T)
    if (n <= 0) return('请输入正确数值') #输入行为-1时错误
    print(record[n,])
    cat('确认请输入1')
    m   <- scan(n = 1, quiet = T)
    if (m == 1) 
    {
      record <- record[-n,]
      record <- as.data.frame(record)
      my_try({b=mutate(record,time = as.Date(time),number_of_rows = as.numeric(number_of_rows))})
      write.csv(record,'code_recorder.csv', row.names = FALSE)
      cat('删除成功')
    } else cat('操作已取消。')
  } else 
  {
    record <- read.record(F)
    if (!nrow(record)) return("并没有找到数据")
    if (Justhead) print(head(record, Justhead))
    else print(record)
    cat('修改哪一行数据？')
    m <- scan(n = 1, quiet = T)
    if (m > nrow(record)) return("行数输入有误。")
    print(record[m,])
    cat('要修改哪一列数据？')
    n <- scan(n = 1, quiet = T)
    if (n > 4) return("列数输入有误。")
    print(as.character(record[m,n]))
    cat('是修改',record[m,n],'吗?是输入1')
    r <- scan(n = 1, quiet = T)
    if (r == 1) 
    {
      cat('输入新的数值:')
      new <- scan(n = 1,quiet = T,what = character())
      if (n == 3) if (suppressWarnings(is.na(as.numeric(new)))) return("请输入数值型数据！")
      record[m,n] <- new
      write.csv(record,'code_recorder.csv',row.names = FALSE)
      cat('更新成功')
    }
    else return("输入错误")
  }
}


#训练情况查看
plot.record <- function(type=1,mytheme = my_theme) {
  plot.new()
  record <- read.record() %>% mutate(time = as.Date(time)) %>% arrange(time)
  if (type==1){
    record %>% group_by(language) %>% summarise(number_of_rows = sum(number_of_rows)) %>% 
    ggplot() +  geom_bar(mapping = aes(x= language,y=number_of_rows),stat = "identity") + mytheme() +
    ylab("Number of rows") + ggtitle('Total number of rows in each language I have coded ')
  }
  else if (type==2){
    languages <- unique(record$language)
    first_day <- with(record,first(time))
    final_day <- with(record,last(time))
    condit <- record %>% filter(time == first_day) %>% 
      with(languages %in% unique(language))
    condit2 <- record %>% filter(time == final_day) %>% 
      with(languages %in% unique(language))
    add_point <- tibble(time = rep(c(first_day,final_day),c(sum(!condit),sum(!condit2))),
                       language = languages[c((1:length(languages))[!condit],
                                              (1:length(languages))[!condit2])],
                       number_of_rows = 0, note = '')
    add_point %>% rbind(record) %>% 
      arrange(time) %>% 
      group_by(language) %>% 
      mutate(number_of_rows = cumsum(number_of_rows)) %>% 
      ggplot() + geom_line(mapping = aes(x=time,y=number_of_rows,color = language)) + mytheme() +
      ylab("Number of rows")
  }
}



#异常情况处理
my_try <- function (expr) 
{
  record <- suppressWarnings(
  tryCatch(expr, error = function(e) {
    cat("出错了:",conditionMessage(e))
    stop("\n请确认输入的格式:\n1.时间格式为\"2016-12-13。\"\n2.代码数格式为数字。\n")
  }))
  return(record)
}

my_try2 <- function(expr)
{
  old_record <- suppressWarnings(
    tryCatch(expr,error = function(e) old_record <- data.frame())
  )
  return(old_record)
}

my_theme <- function(base_size = 12, base_family = "sans"){
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      axis.text = element_text(size = 12),
      axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
      axis.title = element_text(size = 14),
      panel.grid.major = element_line(color = "grey"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "aliceblue"),
      strip.background = element_rect(fill = "lightgrey", color = "grey", size = 1),
      strip.text = element_text(face = "bold", size = 12, color = "black"),
      legend.position = "bottom",
      legend.justification = "top", 
      legend.box = "horizontal",
      legend.box.background = element_rect(colour = "grey50"),
      legend.background = element_blank(),
      panel.border = element_rect(color = "grey", fill = NA, size = 0.5)
    )
}



# parse_datetime("20101010")以后可以加入parse_date("2010-10-01")  

#guess_parser("2010-10-01")
#guess_parser("15:01")
#guess_parser(c("TRUE", "FALSE"))
#guess_parser(c("1", "5", "9"))
#guess_parser(c("12,352,561"))
#str(parse_guess("2010-10-10"))

