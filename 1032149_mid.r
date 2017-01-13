library(markdown)
library(data.table)

#一. 請隨機產生 10000 組正整數儲存成 vector 格式，並輸出成 random10k.csv (5%)
x <- c(sample(10000))
write.csv(x,"C:/Users/user1/Desktop/random10k.csv")

#二. 請使用 for 迴圈列出 15 個[費布納西(Fibonacci)數列](https://en.wikipedia.org/wiki/Fibonacci_number) (10%)
fibonacci <- function(x){
  if ( x==0 || x==1 ){
    return (1)
  }else{
    return (fibonacci(x-1)+fibonacci(x-2))
  }
}
sapply(c(1:15), fibonacci)

#三. 請將 sample_data.txt 輸入進 R 內，並完成以下計算 (55%)
sample_data <- fread(file.choose(),header = FALSE, 
                        na.strings = c('-9991', '-9996', 
                                       '-9997', '-9998', '-9999'))
colnames <-  c( 'yyyymmddhh', 'PS01', 'TX01', 'RH01','WD01', 'WD02', 'PP01', 'SS01')
setnames(sample_data, colnames)

#(a) 將 ```yyyymmddhh``` 轉成 POSIXct 時間戳記格式，
#並新增為一個欄(variable)，命名為 timestamp。並將此 sample data 輸出為sample_data_parsed.csv (以逗號分隔，具有欄位名稱)
sample_data[, timestamp:=as.POSIXct(strptime(yyyymmddhh, '%Y%m%d%H'))]
write.csv(sample_data,'C:/Users/user1/Desktop/sample_data_parsed.csv')

#(b) 請計算 2014 年至 2015 年這個測站的每月平均氣溫、每月平均濕度、每月累積降水，並用表格呈現。表格範例如下：

sample_data[, year := data.table::year(timestamp)]
sample_data[, month := data.table::month(timestamp)]
##每月平均氣溫
aggrMonth_TX <- aggregate(TX01 ~ month, data = sample_data, FUN = mean, na.rm = TRUE)
##每月平均濕度
aggrMonth_RH <- aggregate(RH01 ~ month, data = sample_data, FUN = mean, na.rm = TRUE)
##每月累積降水
aggrMonth_PP <- aggregate(PP01 ~ month, data = sample_data, FUN = mean, na.rm = TRUE)

#(c) 請計算 2014 年和 2015 年最冷月分別是在哪個月份？(提示：先計算月均溫)
aggrMonth_2014 <- aggregate(PP01 ~ year$2014, data = sample_data)

