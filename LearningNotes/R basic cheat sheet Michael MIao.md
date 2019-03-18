

unlist()是什么？
它将列表转换为向量。



`unlist()`


```{r}
unlist()



```

如何从R语言工作区中删除向量？
rm(x)

列出包"MASS"中可用的数据集
data(package ="MASS")

列出所有可用软件包中可用的数据集。

M = matrix( c('a','a','b','c','b','a'), nrow = 2, ncol = 3, byrow = TRUE)


Matrices 矩阵

矩阵是二维矩形数据集。 它可以使用矩阵函数的向量输入创建。
# Create a matrix.
M = matrix( c('a','a','b','c','b','a'), nrow = 2, ncol = 3, byrow = TRUE)
print(M)
当我们执行上面的代码，它产生以下结果
     [,1] [,2] [,3]
[1,] "a"  "a"  "b" 
[2,] "c"  "b"  "a"


Arrays 数组

虽然矩阵被限制为二维，但阵列可以具有任何数量的维度。 数组函数使用一个dim属性创建所需的维数。 在下面的例子中，我们创建了一个包含两个元素的数组，每个元素为3x3个矩阵。
# Create an array.
a <- array(c('green','yellow'),dim = c(3,3,2))
print(a)



Factors 因子
# Create a vector.
apple_colors <- c('green','green','yellow','red','red','red','green')

# Create a factor object.
factor_apple <- factor(apple_colors)

# Print the factor.
print(factor_apple)
print(nlevels(factor_apple))


Data Frames 数据帧

数据帧是表格数据对象。 与数据帧中的矩阵不同，每列可以包含不同的数据模式。 第一列可以是数字，而第二列可以是字符，第三列可以是逻辑的。 **它是等长度的向量的列表。**

# Create the data frame.
BMI <- 	data.frame(
   gender = c("Male", "Male","Female"), 
   height = c(152, 171.5, 165), 
   weight = c(81,93, 78),
   Age = c(42,38,26)
)
print(BMI)


--------------------------------------------------

cat()函数将多个项目组合成连续打印输出


以点(.)开头的变量被隐藏，它们可以使用ls()函数的“all.names = TRUE”参数列出。
print(ls(all.name = TRUE))
当我们执行上面的代码，它产生以下结果 -
[1] ".cars"        ".Random.seed" ".var_name"    ".varname"     ".varname2"   
[6] "my var"       "my_new_var"   "my_var"       "var.1"        "var.2"        
[11]"var.3"        "var.name"     "var_name2."   "var_x"  



删除变量

可以使用rm()函数删除变量。 下面我们删除变量var.3。 打印时，抛出变量错误的值。
rm(var.3)
print(var.3)

一起删除

rm(list = ls())
print(ls())





&&	称为逻辑AND运算符。 取两个向量的第一个元素，并且只有两个都为TRUE时才给出TRUE。	
v <- c(3,0,TRUE,2+2i)
t <- c(1,3,TRUE,2+3i)
print(v&&t)
它产生以下结果 -
TRUE
||	称为逻辑OR运算符。 取两个向量的第一个元素，如果其中一个为TRUE，则给出TRUE。	
v <- c(0,0,TRUE,2+2i)
t <- c(0,3,TRUE,2+3i)
print(v||t)
它产生以下结果 -
FALSE



%in%	此运算符用于标识元素是否属于向量。

1	repeat循环
多次执行一系列语句，并简化管理循环变量的代码。
2	while循环
在给定条件为真时，重复语句或语句组。 它在执行循环体之前测试条件。
3	for循环
像while语句，不同之处在于它测试在循环体的端部的条件。


v <- LETTERS[1:4]
for ( i in v) {
   print(i)
}

# Combine above three vectors into one data frame.
addresses <- cbind(city,state,zipcode)



library(MASS)
merged.Pima <- merge(x = Pima.te, y = Pima.tr,
   by.x = c("bp", "bmi"),
   by.y = c("bp", "bmi")
)
print(merged.Pima)
nrow(merged.Pima)


melt()拆分数据

现在我们拆分数据进行重组，将除类型和年份以外的所有列转换为多行展示。
molten.ships <- melt(ships, id = c("type","year"))
print(molten.ships)
当我们执行上面的代码，它产生以下结果 -
      type year  variable  value
1      A   60    period      60
2      A   60    period      75
3      A   65    period      60
4      A   65    period      75
............
............
9      B   60    period      60
10     B   60    period      75
11     B   65    period      60
12     B   65    period      75
13     B   70    period      60
...........
...........
41     A   60    service    127
42     A   60    service     63
43     A   65    service   1095
...........


cast()重构数据

我们可以将被拆分的数据转换为一种新形式，使用cast()函数创建每年每种类型的船的总和。
recasted.ship <- cast(molten.ships, type+year~variable,sum)
print(recasted.ship)

！！！！！！  cast function


# Create a sequence of numbers from 32 to 44.
print(seq(32,44))

# Find mean of numbers from 25 to 82.
print(mean(25:82))

# Find sum of numbers frm 41 to 68.
print(sum(41:68))


# Create a function with arguments.
new.function <- function(a,b,c) {
   result <- a * b + c
   print(result)
}

# Call the function by position of arguments.
new.function(5,3,11)

# Call the function by names of the arguments.
new.function(a = 11, b = 5, c = 3)
当我们执行上面的代码，它产生以下结果 -
[1] 26
[1] 58



# Atomic vector of type character.
print("abc");

# Atomic vector of type double.
print(12.5)

# Atomic vector of type integer.
print(63L)

# Atomic vector of type logical.
print(TRUE)

# Atomic vector of type complex.
print(2+3i)

# Atomic vector of type raw.
print(charToRaw('hello'))



向量元素排序

向量中的元素可以使用sort()函数排序。
v <- c(3,8,4,5,0,11, -9, 304)

# Sort the elements of the vector.
sort.result <- sort(v)
print(sort.result)
h

list可以报考好多种类！！
# Create a list containing strings, numbers, vectors and a logical values.
list_data <- list("Red", "Green", c(21,32,11), TRUE, 51.23, 119.1)
print(list_data)
当我们执行上面的代码，它产生以下结果 -
[[1]]
[1] "Red"

[[2]]
[1] "Green"

[[3]]
[1] 21 32 11

[[4]]
[1] TRUE

[[5]]
[1] 51.23

[[6]]
[1] 119.1

merged.list <- c(list1,list2)


二、数组array：多维的同一类型集合（字符型、数值型、逻辑型、复数型），R可以很容易地生成和处理数组，特别是矩阵matrix是一个二维数组。


1.可以通过定义dim(维度)将向量变成matrix。

a=c(1,3,4,5,6,7,8,9,3)
> dim(a)=c(3,3)
> a
     [,1] [,2] [,3]
[1,]    1    5    8
[2,]    3    6    9
[3,]    4    7    3

或者：

> a=array(a,dim=c(3,3))
> a
     [,1] [,2] [,3]
[1,]    1    5    8
[2,]    3    6    9
[3,]    4    7    3

或者：

> a=matrix(a,nrow=3,ncol=3);a
     [,1] [,2] [,3]
[1,]    1    5    8
[2,]    3    6    9
[3,]    4    7    3

> is.vector(a)
[1] FALSE
> is.matrix(a)
[1] TRUE
> is.array(a)
[1] TRUE

> is.list(a)
[1] FALSE

可以发现，a已经通过定义维度将其变成了一个矩阵（matrix）和数组（array)，下面将讲matrix其实是一个二维的array。

2.下标引用
> a=c(1:24)
> dim(a)=c(2,3,4)
> a[2,1,2]
[1] 8
> a[1,2:3,2:3]
     [,1] [,2]
[1,]    9   15
[2,]   11   17
> a[1, , ]
     [,1] [,2] [,3] [,4]
[1,]    1    7   13   19
[2,]    3    9   15   21
[3,]    5   11   17   23
--------------------- 
作者：有腹肌的小蝌蚪_ 
来源：CSDN 
原文：https://blog.csdn.net/yezonggang/article/details/51103460 
版权声明：本文为博主原创文章，转载请附上博文链接！





！！将列表转换为向量

列表可以转换为向量，使得向量的元素可以用于进一步的操作。 可以在将列表转换为向量之后应用对向量的所有算术运算。 要做这个转换，我们使用	unlist()	函数。 它将列表作为输入并生成向量。
# Create lists.
list1 <- list(1:5)
print(list1)

list2 <-list(10:14)
print(list2)

# Convert the lists to vectors.
v1 <- unlist(list1)
v2 <- unlist(list2)

print(v1)
print(v2)

# Now add the vectors
result <- v1+v2
print(result)



----------------------------------

matrix(data, nrow, ncol, byrow, dimnames)


# Elements are arranged sequentially by row.
M <- matrix(c(3:14), nrow = 4, byrow = TRUE)
print(M)

# Elements are arranged sequentially by column.
N <- matrix(c(3:14), nrow = 4, byrow = FALSE)
print(N)

# Define the column and row names.
rownames = c("row1", "row2", "row3", "row4")
colnames = c("col1", "col2", "col3")

P <- matrix(c(3:14), nrow = 4, byrow = TRUE, dimnames = list(rownames, colnames))
print(P)
当我们执行上面的代码，它产生以下结果 -
     [,1] [,2] [,3]
[1,]    3    4    5
[2,]    6    7    8
[3,]    9   10   11
[4,]   12   13   14
     [,1] [,2] [,3]
[1,]    3    7   11
[2,]    4    8   12
[3,]    5    9   13
[4,]    6   10   14
     col1 col2 col3
row1    3    4    5
row2    6    7    8
row3    9   10   11
row4   12   13   14


# Access the element at 2nd column and 4th row.
print(P[4,2])

# Add the matrices.
result <- matrix1 + matrix2
cat("Result of addition","
")
print(result)

命名列和行

我们可以使用dimnames参数给数组中的行，列和矩阵命名。
# Create two vectors of different lengths.
vector1 <- c(5,9,3)
vector2 <- c(10,11,12,13,14,15)
column.names <- c("COL1","COL2","COL3")
row.names <- c("ROW1","ROW2","ROW3")
matrix.names <- c("Matrix1","Matrix2")

# Take these vectors as input to the array.
result <- array(c(vector1,vector2),dim = c(3,3,2),dimnames = list(row.names,column.names,
   matrix.names))
print(result)


# Print the 2nd Matrix.
print(result[,,2])
 最后2 表示大位置

 # Use apply to calculate the sum of the rows across all the matrices.
result <- apply(new.array, c(1), sum)
print(result)

生成因子级别

我们可以使用gl()函数生成因子级别。 它需要两个整数作为输入，指示每个级别有多少级别和多少次。
语法

gl(n, k, labels)

通过使用str()函数可以看到数据帧的结构。


# Create the data frame.
emp.data <- data.frame(
   emp_id = c (1:5),
   emp_name = c("Rick","Dan","Michelle","Ryan","Gary"),
   salary = c(623.3,515.2,611.0,729.0,843.25),
   
   start_date = as.Date(c("2012-01-01", "2013-09-23", "2014-11-15", "2014-05-11",
      "2015-03-27")),
   stringsAsFactors = FALSE
)
# Extract first two rows.
result <- emp.data[1:2,]
print(result)
当我们执行上面的代码，它产生以下结果 -
  emp_id    emp_name   salary    start_date
1      1     Rick      623.3     2012-01-01
2      2     Dan       515.2     2013-09-23





# CONVERT FACTOR TO NUMERIC
as.numeric(as.character())



#  学习下rshiny

r-shiny examples


# command + delete 删除整个行


# for
for (i in x){
 print(x[i])
}


# matplot for matrix

# drop=false 
可以avoid 变成vector for matrix

# 选择funciton 太酷了
`file.choose()`

#merge df
`merge(statas, mydf, by.x=",by.y=")`

# drop a column 
`df$column_wanted_drop = NULL`


# ggplot
data - geometriec - geometrics - statisits - facets- coordinates - theme

# overriding

# geom_density
`geom_density(aes(fill=genre),position="stack")`

# add smooth 
geom_smooth(fill=NA)	

#geom_jitter()
see the data better

# alpha=0.5 负责透明度


#facet
`facet_grid(Genre~.)`

`facet_grid(Genre~year)` 方格子

# coordinate
```{r}
xlim(500,900)+ylim(1,10)
# wont work well for all time.
# because cut off the graph beyond limit

```

# zoom 
```
coor_cartesian(ylim=c(0,50),xlim=c())

```

#theme
可以改变坐标轴文字大小和 title大小颜色等
legeng.justificaition=c(1,1) 右上角
family=改字体

#strwrap(
    "Stopping distance of cars (ft) vs. speed (mph) from Ezekiel (1930)",
    width = 30), collapse = "\n")
    
    
# paste 
(nth <- paste0(1:12, c("st", "nd", "rd", rep("th", 9))))
 [1] "1st"  "2nd"  "3rd"  "4th"  "5th"  "6th"  "7th"  "8th"  "9th"  "10th"
[11] "11th" "12th"


# df %>%
  select(select_vars(names(df), starts_with('b', ignore.case = TRUE)))

#Alternatively
select_vars(names(df), matches('^[Bb]'))



# With R 3.3.0, we can use strrep from base R

strrep("my_string",2)
#[1] "my_stringmy_string"
We can also pass a vector of values in times

strrep("my_string",1:3)
#[1] "my_string"                   "my_stringmy_string"         
#[3] "my_stringmy_stringmy_string" 



