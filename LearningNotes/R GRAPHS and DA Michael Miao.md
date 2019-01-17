R语言 条形图
由 xiaoxiaogang 创建，陈 最后一次修改 2016-12-12
条形图表示矩形条中的数据，条的长度与变量的值成比例。 R语言使用函数barplot()创建条形图。 R语言可以在条形图中绘制垂直和水平条。 在条形图中，每个条可以给予不同的颜色。
语法

在R语言中创建条形图的基本语法是 -
barplot(H, xlab, ylab, main, names.arg, col)

以下是所使用的参数的描述 - 
H是包含在条形图中使用的数值的向量或矩阵。
xlab是x轴的标签。
ylab是y轴的标签。
main是条形图的标题。
names.arg是在每个条下出现的名称的向量。
col用于向图中的条形提供颜色。




!!!! save graphs
# Save the file.
dev.off()


boxplot(x, data, notch, varwidth, names, main)
以下是所使用的参数的描述 - 
x是向量或公式。
数据是数据帧。
notch是逻辑值。 设置为TRUE以绘制凹口。
varwidth是一个逻辑值。 设置为true以绘制与样本大小成比例的框的宽度。
names是将打印在每个箱线图下的组标签。
main用于给图表标题。


hist(v,main,xlab,xlim,ylim,breaks,col,border)
以下是所使用的参数的描述 - 
v是包含直方图中使用的数值的向量。
main表示图表的标题。
col用于设置条的颜色。
border用于设置每个条的边框颜色。
xlab用于给出x轴的描述。
xlim用于指定x轴上的值的范围。
ylim用于指定y轴上的值的范围。
break用于提及每个条的宽度。

# Create the data for the chart.
v <- c(7,12,28,3,41


plot(v,type,col,xlab,ylab)
以下是所使用的参数的描述 - 
v是包含数值的向量。
类型采用值“p”仅绘制点，“l”仅绘制线和“o”绘制点和线。
xlab是x轴的标签。
ylab是y轴的标签。
main是图表的标题。
col用于给点和线的颜色。
# Give the chart file a name.
png(file = "line_chart_label_colored.jpg")

# Plot the bar chart.
plot(v,type = "o", col = "red", xlab = "Month", ylab = "Rain fall",
   main = "Rain fall chart")

# Save the file.
dev.off()


# Create data for the graph.
x <-  c(21, 62, 10,53)
labels <-  c("London","New York","Singapore","Mumbai")

piepercent<- round(100*x/sum(x), 1)

# Give the chart file a name.
png(file = "city_percentage_legends.jpg")

# Plot the chart.
pie(x, labels = piepercent, main = "City pie chart",col = rainbow(length(x)))
legend("topright", c("London","New York","Singapore","Mumbai"), cex = 0.8,
   fill = rainbow(length(x)))

# Save the file.
dev.off()





-----------------------------------------------------------------


R语言 CSV文件


# Get and print current working directory.
print(getwd())

# Set current working directory.
setwd("/web/com")

# Get and print current working directory.
print(getwd())

!!!!!!!!!
# Get the person detail having max salary.
retval <- subset(data, salary == max(salary))
print(retval)
subset很关键！

install.packages("xlsx")




url <- "http://www.geos.ed.ac.uk/~weather/jcmb_ws/"

# Gather the html links present in the webpage.
links <- getHTMLLinks(url)

# Identify only the links which point to the JCMB 2015 files. 
filenames <- links[str_detect(links, "JCMB_2015")]

# Store the file names as a list.
filenames_list <- as.list(filenames)

# Create a function to download the files by passing the URL and filename list.
downloadcsv <- function (mainurl,filename) {
   filedetails <- str_c(mainurl,filename)
   download.file(filedetails,filename)
}

# Now apply the l_ply function and save the files into the current R working directory.
l_ply(filenames,downloadcsv,mainurl = "http://www.geos.ed.ac.uk/~weather/jcmb_ws/")






·-----------------------------------------------------------------------------------------------------------------------


Mean平均值

通过求出数据集的和再除以求和数的总量得到平均值
函数mean()用于在R语言中计算平均值。
语法

用于计算R中的平均值的基本语法是 -
mean(x, trim = 0, na.rm = FALSE, ...)
以下是所使用的参数的描述 - 
x是输入向量。
trim用于从排序向量的两端丢弃一些观察结果。
na.rm用于从输入向量中删除缺失值。


应用修剪选项

当提供trim参数时，向量中的值被排序，然后从计算平均值中减去所需的观察值。
当trim = 0.3时，来自每端的3个值将从计算中减去以找到均值。
在这种情况下，排序的向量是（-21，-5,2,3,4.2,7,8,12,18,54），并且从用于计算平均值的向量中移除的值是（-21，-5,2） 从左边和（12,18,54）从右边。
# Create a vector.
x <- c(12,7,3,4.2,18,2,54,-21,8,-5)

# Find Mean.
result.mean <-  mean(x,trim = 0.3)
print(result.mean)
当我们执行上面的代码，它产生以下结果 -
[1] 5.55


# Create the function.
getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Create the vector with numbers.
v <- c(2,1,2,3,1,2,3,4,1,5,5,3,2,3)

# Calculate the mode using the user function.
result <- getmode(v)
print(result)

# Create the vector with characters.
charv <- c("o","it","the","it","it")

# Calculate the mode using the user function.
result <- getmode(charv)
print(result)


x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)

# Apply the lm() function.
relation <- lm(y~x)

print(relation)


# The predictor vector.
x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)

# The resposne vector.
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)

# Apply the lm() function.
relation <- lm(y~x)

# Find weight of a person with height 170.
a <- data.frame(x = 170)
result <-  predict(relation,a)
print(result)
当我们执行上面的代码，它产生以下结果 -
       1 








 y = 1/(1+e^-(a+b1x1+b2x2+b3x3+...))
以下是所使用的参数的描述 - 
y是响应变量。
x是预测变量。
a和b是作为数字常数的系数。
用于创建回归模型的函数是glm()函数。
语法

逻辑回归中glm()函数的基本语法是 -
glm(formula,data,family)



R语言 标准分布
由 xiaoxiaogang 创建，youj 最后一次修改 2016-12-12
在来自独立源的数据的随机集合中，通常观察到数据的分布是正常的。 这意味着，在绘制水平轴上的变量值和垂直轴上的值的计数的图形时，我们得到钟形曲线。 曲线的中心表示数据集的平均值。 在图中，50％的值位于平均值的左侧，另外50％位于图表的右侧。 这在统计学中被称为正态分布。
R语言有四个内置函数来产生正态分布。 它们描述如下。
dnorm(x, mean, sd)
pnorm(x, mean, sd)
qnorm(p, mean, sd)
rnorm(n, mean, sd)
以下是在上述功能中使用的参数的描述 - 
x是数字的向量。
p是概率的向量。
n是观察的数量（样本大小）。
mean是样本数据的平均值。 它的默认值为零。
sd是标准偏差。 它的默认值为1。
dnorm（）

该函数给出给定平均值和标准偏差在每个点的概率分布的高度。
# Create a sequence of numbers between -10 and 10 incrementing by 0.1.
x <- seq(-10, 10, by = .1)

# Choose the mean as 2.5 and standard deviation as 0.5.
y <- dnorm(x, mean = 2.5, sd = 0.5)

# Give the chart file a name.
png(file = "dnorm.png")

plot(x,y)

# Save the file.
dev.off()




R语言 二项分布
由 xiaoxiaogang 创建，youj 最后一次修改 2016-12-12
二项分布模型处理在一系列实验中仅发现两个可能结果的事件的成功概率。 例如，掷硬币总是给出头或尾。 在二项分布期间估计在10次重复抛掷硬币中精确找到3个头的概率。
R语言有四个内置函数来生成二项分布。 它们描述如下。
dbinom(x, size, prob)
pbinom(x, size, prob)
qbinom(p, size, prob)
rbinom(n, size, prob)


# Get the dataset.
input <- mtcars

# Create the regression model.
result <- aov(mpg~hp*am,data = input)
print(summary(result))
当我们执行上面的代码，它产生以下结果 -
            Df Sum Sq Mean Sq F value   Pr(>F)    
hp           1  678.4   678.4  77.391 1.50e-09 ***
am           1  202.2   202.2  23.072 4.75e-05 ***
hp:am        1    0.0     0.0   0.001    0.981    
Residuals   28  245.4     8.8                     
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


时间序列分析中ts()函数的基本语法是 -
timeseries.object.name <-  ts(data, start, end, frequency)
以下是所使用的参数的描述 - 
data是包含在时间序列中使用的值的向量或矩阵。
start以时间序列指定第一次观察的开始时间。
end指定时间序列中最后一次观测的结束时间。
frequency指定每单位时间的观测数。
除了参数“data”，所有其他参数是可选的。
例

考虑从2012年1月开始的一个地方的年降雨量细节。我们创建一个R时间序列对象为期12个月并绘制它。
# Get the data points in form of a R vector.
rainfall <- c(799,1174.8,865.1,1334.6,635.4,918.5,685.5,998.6,784.2,985,882.8,1071)

# Convert it to a time series object.
rainfall.timeseries <- ts(rainfall,start = c(2012,1),frequency = 12)

# Print the timeseries data.
print(rainfall.timeseries)

# Give the chart file a name.
png(file = "rainfall.png")

# Plot a graph of the time series.
plot(rainfall.timeseries)

# Save the file.
dev.off()


决策树的使用的例子是 - 预测电子邮件是垃圾邮件或非垃圾邮件，预测肿瘤癌变，或者基于这些因素预测贷款的信用风险。通常，使用观测数据（也称为训练数据）来创建模型。然后使用一组验证数据来验证和改进模型。 R具有用于创建和可视化决策树的包。对于新的预测变量集合，我们使用此模型来确定R包“party”用于创建决策树。
安装R语言包

在R语言控制台中使用以下命令安装软件包。您还必须安装相关软件包（如果有）。
install.packages("party")

# Load the party package. It will automatically load other dependent packages.
library(party)

# Create the input data frame.
input.dat <- readingSkills[c(1:105),]

# Give the chart file a name.
png(file = "decision_tree.png")

# Create the tree.
  output.tree <- ctree(
  nativeSpeaker ~ age + shoeSize + score, 
  data = input.dat)

# Plot the tree.
plot(output.tree)

# Save the file.
dev.off()


R语言unif（4）的输出是什么？
它生成0和1之间的4个随机数。


unlist()是什么？
它将列表转换为向量。

给予R语言表达式，从使用pbinom的硬币51个硬币中得到26个或更少的头。



"%%"和"%/%"之间有什么区别？
"%%"给出第一向量与第二向量的除法的余数，而"%/%"给出第一向量与第二向量的除法的商。


列出包"MASS"中可用的数据集
data(package ="MASS")

列出所有可用软件包中可用的数据集。
data(package = .packages(all.available = TRUE))


# And lapply returns list and sapply returns a vector. Marko Smiljanic, Dabbles in R for few years now :-) I assume that you already know what *apply function do :-). Main difference between lapply and sapply is that sapply will try to simplify as much as it can the output of lapply.


read_csv also offers many additional arguments for making adjustments to your data as you read it in:

# specify the column class using col_types
read_csv("mydata.csv", col_types = list(col_double(), 
                                        col_character(), 
                                        col_character()))
##   variable 1 variable 2 variable 3
## 1         10       beer       TRUE
## 2         25       wine       TRUE
## 3          8     cheese      FALSE

# we can also specify column classes with a string
# in this example d = double, _ skips column, c = character
read_csv("mydata.csv", col_types = "d_c")
##   variable 1 variable 3
## 1         10       TRUE
## 2         25       TRUE
## 3          8      FALSE

# set column names
read_csv("mydata.csv", col_names = c("Var 1", "Var 2", "Var 3"), skip = 1)
##   Var 1  Var 2 Var 3
## 1    10   beer  TRUE
## 2    25   wine  TRUE
## 3     8 cheese FALSE

# set the maximum number of lines to read in
read_csv("mydata.csv", n_max = 2)
##   variable 1 variable 2 variable 3
## 1         10       beer       TRUE
## 2         25       wine       TRUE


df <- data.frame(var1 = c(10, 25, 8), 
                 var2 = c("beer", "wine", "cheese"), 
                 var3 = c(TRUE, TRUE, FALSE),
                 row.names = c("billy", "bob", "thornton"))

df
##          var1   var2  var3
## billy      10   beer  TRUE
## bob        25   wine  TRUE
## thornton    8 cheese FALSE


# save() can be used to save multiple objects in you global environment,
# in this case I save two objects to a .RData file
x <- stats::runif(20)
y <- list(a = 1, b = TRUE, c = "oops")
save(x, y, file = "xy.RData")

# save.image() is just a short-cut for ‘save my current workspace’,
# i.e. all objects in your global environment
save.image()

# write rds file readr
readr::write_rds(x, "x.rds")

# save a single object to file
saveRDS(x, "x.rds")

# restore it under a different name
x2 <- readRDS("x.rds")
identical(x, x2)
[1] TRUE


# number of variables
ncol(mtcars)
## [1] 11

# number of rows
nrow(mtcars)
## [1] 32

# number of rows and variables
dim(mtcars)
## [1] 32 11

# look at the last 10 rows
tail(mtcars, 10)

sapply(mtcars, class)

# fivenum() function provides min, 25%, 50% (median), 75%, and max
fivenum(mtcars$mpg)
## [1] 10.40 15.35 19.20 22.80 33.90

# default quantile() percentiles are 0%, 25%, 50%, 75%, and 100% 
# provides same output as fivenum()
quantile(mtcars$mpg)
##     0%    25%    50%    75%   100% 
## 10.400 15.425 19.200 22.800 33.900

# we can customize quantile() for specific percentiles
quantile(mtcars$mpg, probs = seq(from = 0, to = 1, by = .1))
##    0%   10%   20%   30%   40%   50%   60%   70%   80%   90%  100% 
## 10.40 14.34 15.20 15.98 17.92 19.20 21.00 21.47 24.08 30.09 33.90

# we can quickly compute the difference between the 1st and 3rd quantile
IQR(mtcars$mpg)
## [1] 7.375

library(moments)

skewness(mtcars$mpg)
## [1] 0.6404399
kurtosis(mtcars$mpg)
## [1] 2.799467

tapply 对df更好























