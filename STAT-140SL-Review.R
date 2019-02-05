STAT-140SL-Review
Date:February 2019
=========================================================

Get the data read as quickly as possible
One schould also check the file, the nice thing about summary in base R is the count of NA.

options(width=110)
library(readxl)
data <- read_excel("~/Desktop/STAT140SLWI2019/caseAnalysis.xls")
names(data)
## [1] "Observation Number"                  "Quarter"                            
## [3] "Employee Id"                         "Sex (Male=1)"                       
## [5] "Race"                                "Age"                                
## [7] "Hospital Visit This Quarter (1=Yes)" "Salary"                             
## [9] "Health Score"
head(data)
## # A tibble: 6 x 9
##   `Observation Num… Quarter `Employee Id` `Sex (Male=1)`  Race   Age `Hospital Visit T… Salary `Health Score`
##               <dbl>   <dbl>         <dbl>          <dbl> <dbl> <dbl>              <dbl>  <dbl>          <dbl>
## 1                 1       1             1              0     3  27.3                  0 36907.           3.70
## 2                 2       2             1              0     3  27.8                  0 37907.           4.98
## 3                 3       3             1              0     3  28.1                  0 38907.           4.01
## 4                 4       4             1              0     3  28.3                  0 39907.           2.34
## 5                 5       5             1              0     3  28.6                  0 40907.           2.11
## 6                 6       6             1              0     3  28.8                  0 41907.           1.46
summary(data)
##  Observation Number    Quarter        Employee Id    Sex (Male=1)         Race            Age        
##  Min.   :    1      Min.   : 1.000   Min.   :   1   Min.   :0.0000   Min.   :1.000   Min.   :  7.00  
##  1st Qu.: 4776      1st Qu.: 5.000   1st Qu.: 498   1st Qu.:0.0000   1st Qu.:1.000   1st Qu.: 26.33  
##  Median : 9552      Median : 8.000   Median : 996   Median :1.0000   Median :1.000   Median : 28.57  
##  Mean   : 9552      Mean   : 7.343   Mean   : 998   Mean   :0.5063   Mean   :1.597   Mean   : 30.59  
##  3rd Qu.:14328      3rd Qu.:10.000   3rd Qu.:1498   3rd Qu.:1.0000   3rd Qu.:2.000   3rd Qu.: 32.45  
##  Max.   :19103      Max.   :12.000   Max.   :2000   Max.   :1.0000   Max.   :3.000   Max.   :172.00  
##                                                     NA's   :71       NA's   :2123                    
##  Hospital Visit This Quarter (1=Yes)     Salary       Health Score    
##  Min.   :0.0000                      Min.   :28351   Min.   : 0.6266  
##  1st Qu.:0.0000                      1st Qu.:44551   1st Qu.: 2.3567  
##  Median :0.0000                      Median :48196   Median : 3.1196  
##  Mean   :0.1114                      Mean   :48298   Mean   : 3.5881  
##  3rd Qu.:0.0000                      3rd Qu.:51958   3rd Qu.: 4.1280  
##  Max.   :1.0000                      Max.   :68826   Max.   :10.0000  
## 
Alternative – using dplyr.

library(dplyr)
summarise_all(data, funs(mean), na.rm=TRUE)
## # A tibble: 1 x 9
##   `Observation Num… Quarter `Employee Id` `Sex (Male=1)`  Race   Age `Hospital Visit T… Salary `Health Score`
##               <dbl>   <dbl>         <dbl>          <dbl> <dbl> <dbl>              <dbl>  <dbl>          <dbl>
## 1              9552    7.34          998.          0.506  1.60  30.6              0.111 48298.           3.59
summarise_all(data, funs(min), na.rm=TRUE)
## # A tibble: 1 x 9
##   `Observation Num… Quarter `Employee Id` `Sex (Male=1)`  Race   Age `Hospital Visit T… Salary `Health Score`
##               <dbl>   <dbl>         <dbl>          <dbl> <dbl> <dbl>              <dbl>  <dbl>          <dbl>
## 1                 1       1             1              0     1     7                  0 28351.          0.627
summarise_all(data, funs(max), na.rm=TRUE)
## # A tibble: 1 x 9
##   `Observation Num… Quarter `Employee Id` `Sex (Male=1)`  Race   Age `Hospital Visit T… Salary `Health Score`
##               <dbl>   <dbl>         <dbl>          <dbl> <dbl> <dbl>              <dbl>  <dbl>          <dbl>
## 1             19103      12          2000              1     3   172                  1 68826.             10
rename(data, "Sex" = "Sex (Male=1)")
## # A tibble: 19,103 x 9
##    `Observation Numb… Quarter `Employee Id`   Sex  Race   Age `Hospital Visit This Qua… Salary `Health Score`
##                 <dbl>   <dbl>         <dbl> <dbl> <dbl> <dbl>                     <dbl>  <dbl>          <dbl>
##  1                  1       1             1     0     3  27.3                         0 36907.           3.70
##  2                  2       2             1     0     3  27.8                         0 37907.           4.98
##  3                  3       3             1     0     3  28.1                         0 38907.           4.01
##  4                  4       4             1     0     3  28.3                         0 39907.           2.34
##  5                  5       5             1     0     3  28.6                         0 40907.           2.11
##  6                  6       6             1     0     3  28.8                         0 41907.           1.46
##  7                  7       7             1     0     3  29.1                         0 42907.           4.73
##  8                  8       8             1     0     3  29.3                         0 43907.           2.34
##  9                  9       9             1     0     3  29.6                         0 44907.           2.76
## 10                 10      10             1     0     3  29.8                         0 45907.           2.83
## # ... with 19,093 more rows
We know from the interview material that the max health score should be 6 so there is already a problem. Human age maxes out around 120 and it is not legal to work children under the age of 16. Whether to set the age 172 to missing or to some upper value is not clear.

data <- data %>%
     mutate(`Health Score` = replace(`Health Score`, `Health Score` > 6, NA),
            Age = replace(Age, (Age > 100 | Age < 16), NA))
summarise_all(data, funs(mean), na.rm=TRUE)
## # A tibble: 1 x 9
##   `Observation Num… Quarter `Employee Id` `Sex (Male=1)`  Race   Age `Hospital Visit T… Salary `Health Score`
##               <dbl>   <dbl>         <dbl>          <dbl> <dbl> <dbl>              <dbl>  <dbl>          <dbl>
## 1              9552    7.34          998.          0.506  1.60  30.5              0.111 48298.           3.14
summarise_all(data, funs(min), na.rm=TRUE)
## # A tibble: 1 x 9
##   `Observation Num… Quarter `Employee Id` `Sex (Male=1)`  Race   Age `Hospital Visit T… Salary `Health Score`
##               <dbl>   <dbl>         <dbl>          <dbl> <dbl> <dbl>              <dbl>  <dbl>          <dbl>
## 1                 1       1             1              0     1    16                  0 28351.          0.627
summarise_all(data, funs(max), na.rm=TRUE)
## # A tibble: 1 x 9
##   `Observation Num… Quarter `Employee Id` `Sex (Male=1)`  Race   Age `Hospital Visit T… Salary `Health Score`
##               <dbl>   <dbl>         <dbl>          <dbl> <dbl> <dbl>              <dbl>  <dbl>          <dbl>
## 1             19103      12          2000              1     3    72                  1 68826.           6.00
Questions
1) What are the demographic characteristics of employees?
We need to recognize that the data structure involves “repeated measures”. One question is “which quarter do we choose”? Typically, demographic characteristics are time invariant OR change at regular intervals (e.g. annually). Since we have been given no guidance, I would do what is easiest.

Create tables describing the employee’s first quarter only:
prop.table(table(data$Race[data$Quarter==1])) ## see the proportion of the race
## 
##         1         2         3 
## 0.5598007 0.2774086 0.1627907
prop.table(table(data$`Sex (Male=1)`[data$Quarter==1]))  ## see the proportion of the sex
## 
##         0         1 
## 0.5146628 0.4853372
mean(data$Age[data$Quarter==1])
## [1] 28.83686
We can see nearly 56% percent of emaployees are White, followed by 28% black and 16% Asian. In terms of gender, they are relatively same. 51% are female and 49% male. The mean age in the first quarter is about 28.8

check whether Demographics change over time
Race proportion is stable.

tapply(data$Race==1, data$Quarter, mean, na.rm=TRUE)
##         1         2         3         4         5         6         7         8         9        10        11 
## 0.5598007 0.5508906 0.5552268 0.5580495 0.5543624 0.5566097 0.5553528 0.5558209 0.5570588 0.5575581 0.5565167 
##        12 
## 0.5562572
These numbers concern me.

tapply(data$Race==1, data$Quarter, length)
##    1    2    3    4    5    6    7    8    9   10   11   12 
##  684  891 1139 1448 1671 1775 1850 1885 1914 1934 1950 1962
I am not comfortable with the fact that I have more data on employees in Quarter 12. The company was probably growing over this time period and by examining only quarter 1, I may have made a mistake. Decided to keep only those employees with complete time information (present in quarters 1-12)

junk <- as.data.frame(tapply(data$Quarter,data$`Employee Id`, length))
junk$`Employee Id` <- row.names(junk) 
names(junk)[1] <- "Quarters"
head(junk)
##   Quarters Employee Id
## 1       12           1
## 2       12           2
## 3        7           3
## 4        5           4
## 5       10           5
## 6       10           6
Q12 <- junk[junk$Quarters==12,]
Q12$`Employee Id` <- as.numeric(Q12$`Employee Id`)
data2 <- data[data$`Employee Id` %in% Q12$`Employee Id`,]
data2 <- na.omit(data2)
names(data2)[4] <- "Sex"
Now check:

tapply(data2$Race==1, data2$Quarter, length)
##   1   2   3   4   5   6   7   8   9  10  11  12 
## 562 561 561 563 568 559 564 564 563 562 569 554
round(tapply(data2$Race==1, data2$Quarter, mean, na.rm=TRUE), 3)
##     1     2     3     4     5     6     7     8     9    10    11    12 
## 0.553 0.553 0.558 0.556 0.569 0.555 0.566 0.559 0.558 0.555 0.552 0.549
None of these employees changed their gender over time.

tapply(data2$Sex, data2$Quarter,mean,na.rm=TRUE) 
##         1         2         3         4         5         6         7         8         9        10        11 
## 0.4893238 0.4884135 0.4884135 0.4849023 0.4876761 0.4847943 0.4911348 0.4858156 0.4866785 0.4839858 0.4885764 
##        12 
## 0.4873646
Just shows people getting older

tapply(data2$Age, data2$Quarter,mean,na.rm=TRUE) 
##        1        2        3        4        5        6        7        8        9       10       11       12 
## 28.66086 29.21661 29.55712 29.72155 29.97438 30.23942 30.64404 30.70442 31.23215 31.08489 31.54769 31.56843
See changes in Health score over time (as measured by Quarter)

data2Fix <- data2[data2$`Health Score` < 10 & data2$Age > 17 & data2$Age < 120 ,] 
tapply(data2Fix$`Health Score`, data2Fix$Quarter, mean, na.rm=TRUE) 
##        1        2        3        4        5        6        7        8        9       10       11       12 
## 2.957047 3.109327 3.077057 3.089555 3.121173 3.149519 3.184976 3.140245 3.207308 3.111688 3.217390 3.265607
We could examine the same table for the employees who were not present in all 12 quarters. They are different.

data3 <- data[!(data$`Employee Id` %in% data2$`Employee Id`),]
data3 <- na.omit(data3)
names(data3)[4] <- "Sex"
tapply(data3$Race==1, data3$Quarter, length)
##    2    3    4    5    6    7    8    9   10   11   12 
##  176  391  653  834  923  985 1001 1016 1044 1059 1046
round(tapply(data3$Race==1, data3$Quarter, mean, na.rm=TRUE), 3)
##     2     3     4     5     6     7     8     9    10    11    12 
## 0.517 0.552 0.551 0.548 0.553 0.551 0.551 0.554 0.553 0.557 0.552
tapply(data3$Sex, data3$Quarter, mean, na.rm=TRUE) 
##         2         3         4         5         6         7         8         9        10        11        12 
## 0.4772727 0.5191816 0.5191424 0.5167866 0.5200433 0.5319797 0.5244755 0.5167323 0.5268199 0.5089707 0.5181644
tapply(data3$Age, data3$Quarter, mean, na.rm=TRUE) 
##        2        3        4        5        6        7        8        9       10       11       12 
## 26.12747 27.24745 27.89524 28.62229 29.44622 30.22487 30.77780 31.24798 31.76019 32.29423 32.57947
data3Fix <- data3[data3$`Health Score` < 10 & data3$Age > 17 & data3$Age < 120 ,] 
tapply(data3Fix$`Health Score`, data3Fix$Quarter, mean, na.rm=TRUE) 
##        2        3        4        5        6        7        8        9       10       11       12 
## 2.920459 3.016294 3.075503 3.053807 3.105204 3.198287 3.130124 3.160713 3.167407 3.244065 3.317271
Over time, the average age of employees is increasing and the health score increases as well.
2) What characteristics is the health score associated with?
create plots
The obvious one is age, we can examine the employees who have been there all 12 quarters

library(ggplot2)
ggplot(data2Fix, aes(x=trunc(Age), y=`Health Score`)) + geom_point(alpha=0.2) + theme_classic()


library(ggplot2)
ggplot(data2Fix, aes(x=trunc(Age), y=`Health Score`, colour=factor(Quarter))) + geom_point(alpha=0.2)  + geom_smooth(span = 0.2, se = FALSE) + theme_classic()


ggplot(data2Fix, aes(x=trunc(Age), y=`Health Score`)) + geom_point(alpha=0.2)  + geom_smooth(span = 0.2) + facet_wrap(~Quarter) + theme_classic()
 Curious about the employees who were more recently hired

ggplot(data3Fix, aes(x=trunc(Age), y=`Health Score`)) + geom_point(alpha=0.2)  + geom_smooth(span = 0.2) + facet_wrap(~Quarter) + theme_classic()


data2Fix$All12 <- 1
data3Fix$All12 <- 0
dataFix <- bind_rows(data2Fix, data3Fix)
dataFix$I1 <- interaction(dataFix$`Hospital Visit This Quarter (1=Yes)`, dataFix$All12)
ggplot(dataFix, aes(x=I1, y=`Health Score`)) + geom_boxplot(fill = c("red", "red","green","green")) +
    labs(x="Hospital Visit This Quarter (1=Yes) & Employee Type (1 =12 quarters)") +
    theme_classic()


Break it up by quarter

ggplot(dataFix, aes(x=I1, y=`Health Score`))  + 
    geom_boxplot() + 
    labs(x="Hospital Visit This Quarter (1=Yes) & Employee Type (1 =12 quarters)") +
    facet_wrap(~ Quarter) + theme_classic()


dataFix2 <- dataFix %>% group_by(.dots=c("Quarter","I1")) %>% summarise(.,avg=mean(`Health Score`, na.rm=TRUE))
ggplot(dataFix2, aes(x=Quarter, y=avg, group=I1, 
                  color=factor(I1))) + 
                  geom_line()  + scale_color_discrete(name = "Hospital Visit This Quarter (1=Yes)") +  theme_classic() + labs(title="Health Score by Quarter and Hospital Visit", x ="Quarter", y = "mean Health Score") + expand_limits(y=c(0,6))


dataFix3 <- dataFix %>% group_by(.dots=c("Quarter","I1", "Sex")) %>%
                                   summarise(.,avg=mean(`Health Score`, na.rm=TRUE))
dataFix3$visitgender <- unclass(factor(paste0(dataFix3$I1,dataFix3$Sex)))
ggplot(dataFix3, aes(x=Quarter, y=avg, group=factor(visitgender), colour=factor(visitgender))) + geom_line() + scale_color_manual(name="Gender-Visit", labels = c("M-No Visit-New", "F-No Visit-New", "M-No Visit-12Q", "F-No Visit-12Q", "M-Visit-New", "F-Visit-New", "M-Visit-12Q", "F-Visit-12Q"), values=c("black", "green","brown","orange","blue","magenta","grey80","red")) + expand_limits(y=c(0,6)) +
theme_classic()


dataFix3$visitgender <- unclass(factor(paste0(dataFix3$I1,dataFix3$Sex)))
ggplot(dataFix3, aes(x=Quarter, y=avg, group=factor(visitgender), colour=factor(visitgender))) + geom_line() + scale_color_manual(name="Gender-Visit", labels = c("M-No Visit-New", "F-No Visit-New", "M-No Visit-12Q", "F-No Visit-12Q", "M-Visit-New", "F-Visit-New", "M-Visit-12Q", "F-Visit-12Q"), values=c("black", "green","brown","orange","blue","magenta","grey80","red")) +
theme_classic()


from these plots, we can see that following factors influence the health score: Gender, Visit, Age
3) Based on the data provided, how would you evaluate InsurAHealth’s claim that employees are getting sicker?

Suggestion: First, outline how you would evaluate the claim. Then, time-permitting, implement the steps you suggested.
The health score is a thorny issue, health scores can be high and no hospitalization is involved, but when there is a hospitalization, health score has a high minimum value.

It is not clear to me that it is a good measure of “sickness”, instead we can try modeling hospitalization using the score and other information:

set.seed(111)
part1 <-dataFix[sample(1:nrow(dataFix),5289),]
part2 <-dataFix[!(dataFix$`Observation Number` %in% part1$`Observation Number`),]
model1 <- glm(`Hospital Visit This Quarter (1=Yes)` ~.,family=binomial(link='logit'),data=part1[, c(2,4,5,6,7,8,9)])
summary(model1)
## 
## Call:
## glm(formula = `Hospital Visit This Quarter (1=Yes)` ~ ., family = binomial(link = "logit"), 
##     data = part1[, c(2, 4, 5, 6, 7, 8, 9)])
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.0155  -0.5095  -0.3993  -0.3259   2.5974  
## 
## Coefficients:
##                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)    -3.274e+00  6.140e-01  -5.332 9.73e-08 ***
## Quarter         6.259e-02  1.819e-02   3.441  0.00058 ***
## Sex             1.905e-02  1.113e-01   0.171  0.86408    
## Race           -3.850e-02  6.583e-02  -0.585  0.55864    
## Age            -2.208e-02  7.506e-03  -2.941  0.00327 ** 
## Salary         -9.434e-06  1.221e-05  -0.772  0.43989    
## `Health Score`  5.529e-01  4.115e-02  13.436  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 3628.5  on 5288  degrees of freedom
## Residual deviance: 3423.9  on 5282  degrees of freedom
## AIC: 3437.9
## 
## Number of Fisher Scoring iterations: 5
Some complex interplay between health score and age is occurring.