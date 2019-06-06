# 代表所有的

```{r}
tree1 <- tree(Premie~., data = training)  


```

#split data 


- https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function


#把图片放进一行 plot in one row

```{r}

par(mfrow=c(1,2))
```

# Tree and prune 
```{r}
library(tree)
tree.carseats <- tree(Sales ~ ., data = Carseats.train)
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty = 0)


yhat <- predict(tree.carseats, newdata = Carseats.test)
mean((yhat - Carseats.test$Sales)^2)

#We may conclude that the Test MSE is about 4.15.

#Use cross-validation in order to determine the optimal level of tree complexity. Does pruning the tree improve the test error rate ?

cv.carseats <- cv.tree(tree.carseats)
plot(cv.carseats$size, cv.carseats$dev, type = "b")
tree.min <- which.min(cv.carseats$dev)
points(tree.min, cv.carseats$dev[tree.min], col = "red", cex = 2, pch = 20)


PRUNEtree <- prune.tree(tree1, best = 8)
plot(PRUNEtree)
text(PRUNEtree, cex = .85, pretty = 0)
```

# Tree prune EXAMPLE
– https://rpubs.com/ryankelly/dtrees
- https://rpubs.com/ppaquay/65564

# How to Get the Frequency Table of a Categorical Variable as a Data Frame in R

https://chemicalstatistician.wordpress.com/2015/02/03/how-to-get-the-frequency-table-of-a-categorical-variable-as-a-data-frame-in-r/

#nrow

# order 
b<-baseball[order(baseball$team),]
stra_sample <- getdata(b,str_team)


# DA 面试



https://intellipaat.com/interview-question/r-interview-questions/ R 面试大全

# Mysql Tutorial

https://www.tutorialspoint.com/mysql/


# missing value imputation
https://datascienceplus.com/missing-value-treatment/

anyNA()


# Using print() in R advatage
https://www.stat.auckland.ac.nz/~paul/ItDT/HTML/node70.html

# ucb 数据分析 r code全套


http://uc-r.github.io/missing_values

# returns path for the current working directory
	getwd()                  

# set the working directory to a specified directory
	setwd("path/of/directory")   
	

# basically removes everything in the working environment -- use with caution!
	rm(list = ls()) 

# change a specific option (i.e. number of digits to print on output)
	options(digits=3)   
	
	
## Note that the largest number of digits that can be displayed is 22. Requesting any larger number of digits will result in an error message.

pi
	## [1] 3.141592654

options(digits = 22)
pi
	## [1] 3.141592653589793115998 

for loop 怎么写

# `for` loop to add corresponding elements in each vector
for (i in seq_along(x)) {
        z[i] <- x[i] + y[i]
        print(z)
}



# 习惯

 Good
average <- mean(feet / 12 + inches, na.rm = TRUE)

# Bad
average<-mean(feet/12+inches,na.rm=TRUE)
There’s a small exception to this rule: :, :: and ::: don’t need spaces around them.

# Good
x <- 1:10
base::get

# Bad
x <- 1 : 10
base :: get

# ways of spliting the data.
Typically, we are not lacking in the size of our data here, so a 70-30 split is often sufficient. The two most common ways of splitting data include simple random sampling and stratified sampling.


Stratified sampling
However, if we want to explicitly control our sampling so that our training and test sets have similar 
y
 distributions, we can use stratified sampling. This is more common with classification problems where the reponse variable may be imbalanced (90% of observations with response “Yes” and 10% with response “No”). Ho
 
 
# or as Leo Breiman said “live with your data before you plunge into modeling.”


# full rank one-hot encode - recommended for generalized linear models and
# neural networks
full_rank  <- dummyVars( ~ ., data = df, fullRank = TRUE)
train_oh   <- predict(full_rank, train_1)
test_oh    <- predict(full_rank, test_1)

# less than full rank --> dummy encoding
dummy    <- dummyVars( ~ ., data = df, fullRank = FALSE)
train_oh <- predict(dummy, train_1)
test_oh  <- predict(dummy, test_1)

To normalize, we have two options:

Option 1: normalize with a log transformation. This will transform most right skewed distributions to be approximately normal.




# log transformation
train_log_y <- log(train_1$Sale_Price)
test_log_y  <- log(test_1$Sale_Price)
Option 2: use a Box Cox transformation. A Box Cox transformation is more flexible and will find the transformation from a family of power transforms that will transform the variabe as close as possible to a normal distribution. Important note: be sure to compute the lambda on the training set and apply that same lambda to both the training and test set to minimize data leakage.

##### Box Cox transformation
lambda  <- forecast::BoxCox.lambda(train_1$Sale_Price)
train_bc_y <- forecast::BoxCox(train_1$Sale_Price, lambda)
test_bc_y  <- forecast::BoxCox(test_1$Sale_Price, lambda)



sapply(house[, important], function(x) sum(is.na(x)))


a <- c(8, 9, 10)
b <- c(9, 10)
c <- 10

seq_along(a)
# [1] 1 2 3
seq_along(b)
# [1] 1 2
seq_along(c)
# [1] 1

seq(a)
# [1] 1 2 3
seq(b)
# [1] 1 2
seq(c)
# [1]  1  2  3  4  5  6  7  8  9 10