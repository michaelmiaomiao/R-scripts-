########################################################################################
# new survey (544 questions and 514 after deleting "#prom" ones)
# report is a final version of the results of all questions in screening process
########################################################################################

# introduction ------------------------------------------------------------
# 可以得到多个csv的表格: 主要由all career combination 出来
# 如果需要出一个单独的excel，需要放到python里用pandas里的pd.to_excel
# 这样得到的是单一一个人的答案，需要：
# 转置成survey的形式，每一行代表一个人

# variable name需要和finalquestion_excel里保持一致
# 初始化: 由work flow里进行输入rawsurvey（format或者sheet名字会变）
# rm(list=ls())
# file <- "Summer Test Response20180724.xlsx"
# # file <- "Staff Test Response.xlsx"
# file_finalquestiontotal <- "./survey/Summer UES Self-Discovery Inventory20180722.xlsx"
# set up working directory

setwd("~/Documents/usa_ues")
options(tz="America/Los_Angeles")
library("stringr",warn.conflicts = FALSE)
library("data.table",warn.conflicts = FALSE)
library("magrittr",warn.conflicts = FALSE)
library("ggplot2",warn.conflicts = FALSE)
library("stargazer",warn.conflicts = FALSE)
library("grid",warn.conflicts = FALSE)
library(gridExtra,warn.conflicts = FALSE)
library(reshape2,warn.conflicts = FALSE)
library("ggfortify",warn.conflicts = FALSE)
library(readxl,warn.conflicts = FALSE)
library("lubridate",warn.conflicts = FALSE)
library("date",warn.conflicts = FALSE)
# ---------------change variable names and create empty data table----------------
# load survey questions: 1. Record all variable names 2. if the file is saved as xlsx, then this line can be used.

# load original questionaire sheet by sheet
# remember to delete all the lines/variables indicating the pomps
# finalquestion_total <- read_xlsx("./survey/Final Question.xlsx", sheet = "Questions") %>%
finalquestion_total <- read_xlsx(file_finalquestiontotal, sheet = "Final Web Questions") %>%
  as.data.table %>%
  # 排序
  .[order(`Variable Name`),]
finalquestion_total <- as.data.frame(finalquestion_total)
finalquestion_total <- finalquestion_total[-which(is.na(str_match(finalquestion_total$`Variable Name`, "#prom"))==FALSE),] %>%
  as.data.table
# lists is the options for open questions in the survey, e.g. specialist
lists <- read_xlsx(file_finalquestiontotal,sheet = "Lists") %>%
  as.data.table

# View(finalquestion_total)
# get all variable names and rename them for later use
varname <- str_replace_all(finalquestion_total[,`Variable Name`],"-","_")
varname <- str_replace_all(varname," ","")
finalquestion_total$`Variable Name` <- varname
finalquestion_total$reorder <- c(0) # for reorder, everytime a new statistics is created in report, reorder is updated accordingly

#################################################################
# new data frame to simmulate the results (dataset: survey)     #
#################################################################
# create empty data table for simulation
# first use matrix to assign row numbers and column numbers
set.seed(10001)

# Simmulations (Fake value)
# the number of answers (in order to simulate students' choices)
# some answers are reported in the Lists Sheet
NumAns <- rowSums(!is.na(finalquestion_total[,which(is.na(str_match(colnames(finalquestion_total),"Ans [1-7]"))==FALSE),with = FALSE]))
# upload raw data from the answers of the interviewees

# change sheet names
# rawsurvey <- read_xlsx(file,sheet = "Form Responses 1")
# rawsurvey <- read_xlsx(file,sheet = sheetname)


# rawsurvey <- read.csv(file)
# rawsurvey[1,] <- str_replace_all(rawsurvey[1,],"-","_")
# colnames(rawsurvey) <- str_replace_all(rawsurvey[1,]," ","")
colnames(rawsurvey) <- str_replace_all(colnames(rawsurvey)," ","")


# rawsurvey <- rawsurvey[-1,]%>%
#   as.data.table
rawsurvey <- as.data.table(rawsurvey)

rawsurvey <- rawsurvey[,"trackingnumber":=1:nrow(rawsurvey)] %>%
  as.data.frame # adding tracking number

rawsurvey[,3:ncol(rawsurvey)] <- apply(rawsurvey[,3:ncol(rawsurvey)],2,function(x){str_replace_all(x,"[a-z]+","")})
rawsurvey[,3:ncol(rawsurvey)] <- apply(rawsurvey[,3:ncol(rawsurvey)],2,function(x){str_replace_all(x,"[A-Z]+","")})
rawsurvey[,3:ncol(rawsurvey)] <- apply(rawsurvey[,3:ncol(rawsurvey)],2,function(x){str_replace_all(x,"-","")})

rawsurvey[,3:ncol(rawsurvey)] <- apply(rawsurvey[,3:ncol(rawsurvey)],2,function(x){
  options(warn = -1)
  as.numeric(x)
})

# rawsurvey[,3:ncol(rawsurvey)] <- apply(rawsurvey[,3:ncol(rawsurvey)],2,function(x){as.numeric(x)})
rawsurvey <- rawsurvey[!is.na(rawsurvey[,2]),]


set.seed(10001) # 设定seed
survey <- do.call(cbind,lapply(NumAns, function(x){
  if (x!=0 & x!=1){
    # floor(rep(0,nrow(rawsurvey)))} #真实情况
    floor(runif(nrow(rawsurvey),1,x+1))}
  else{
    # floor(rep(0,nrow(rawsurvey))) #真实情况
    floor(runif(nrow(rawsurvey),1,6))
    # NA #不能取成NA，因为这样的话就没有办法从lists里选数，也不能用str_c的测试
    # 但是真实数据到来的时候，这一步是可以省略的
  }
})) %>%
  as.data.table%>%
  `colnames<-`(c(varname))%>%
  as.data.frame
survey$trackingnumber <- 1:nrow(survey)

rawloc <- as.numeric(na.omit(match(colnames(survey),colnames(rawsurvey))))
survey[,which(is.na(match(colnames(survey),colnames(rawsurvey)))==FALSE)] <- rawsurvey[,rawloc]
# check
identical(colnames(survey)[which(is.na(match(colnames(survey),colnames(rawsurvey)))==FALSE)],colnames(rawsurvey)[rawloc])

survey <- as.data.table(survey)


# adjust for reverse iterms
# rev is a indicator to mark which variable is reverse item
# use str_count to identify the variables which need to be adjusted

rev <- varname[which(str_count(finalquestion_total$Reverse)>0)]

# have changed it to negative here. and in the following part, we can use 8-score to indicate the real number
Answersloc <- which(is.na(str_match(colnames(finalquestion_total),"Ans[1-7]"))==FALSE)
# 
# 
reverseadj <- do.call(c,lapply(1:length(rev),function(x){
  choice <- finalquestion_total[which(finalquestion_total$`Variable Name`==rev[x]),Answersloc,with=FALSE][1,]
  sum(!is.na(choice))
}))
# 
# 
# reverseadj <- matrix(rep(reverseadj,each=nrow(rawsurvey)),byrow = FALSE,nrow = nrow(rawsurvey))+1
# reverseadj <- reverseadj-survey[,rev,with=FALSE] # create new lines

# survey[,which(str_count(finalquestion_total$Reverse)>0)] <- reverseadj
# for reorder: add a new indentification row

survey <- survey %>%
  as.data.frame %>%
  # rbind(.,rep(7,ncol(.))) %>%
  as.data.table


# 标准化/统一化 -----------------------------------------------------------------

Answersloc <- which(is.na(str_match(colnames(finalquestion_total),"Ans[1-7]"))==FALSE)
div <- do.call(c,lapply(varname,function(x){
  choice <- finalquestion_total[which(finalquestion_total$`Variable Name`==x),Answersloc,with=FALSE][1,]
  sum(!is.na(choice))
}))

survey <- survey %>%
  as.data.frame

# check
revloc <- which(str_count(finalquestion_total$Reverse)>0)
# div[revloc] <- 1
# check line的ADHD项
div[which(is.na(str_match(varname,"P_Disa_ADHD_Hyper"))==FALSE)] <- ifelse(div[which(is.na(str_match(varname,"P_Disa_ADHD_Hyper"))==FALSE)]-2>0,3,0)
div[which(is.na(str_match(varname,"P_Disa_ADHD_Inatt"))==FALSE)] <- ifelse(div[which(is.na(str_match(varname,"P_Disa_ADHD_Inatt"))==FALSE)]-2>0,3,0)

# For student, ADHD needs to be reversed
# Step 1: change the scale from [1,5] to [0,0,1,2,3]
div[which(is.na(str_match(varname,"S_Disa_ADHD_Inatt"))==FALSE)] <- ifelse(div[which(is.na(str_match(varname,"S_Disa_ADHD_Inatt"))==FALSE)]-2>0,3,0)

survey <- rbind(survey,1,div) # this line should be 100% after changing the scale
div_by <- matrix(rep(div,each=nrow(survey)),byrow = FALSE,nrow = nrow(survey))

# 调整标准化
survey[,which(div==3 | div==5|div==7)] <- survey[,which(div==3|div==5|div==7)]/div_by[,which(div==3|div==5|div==7)]*7

survey <- as.data.table(survey)


#################################################################
# statistical results                                           #
#    report                                                     #
#################################################################
# Initialize data frame: and all the statistics results is contained in this data frame
# reportvar <- str_replace_all(finalquestion_total$`Q#`,"-","_")
# Initialize report as 0 
# report$trackingnumber <- data.frame(1:nrow(survey))
report <- matrix(0,nrow(survey),length(varname)) %>%
  as.data.table %>%
  `colnames<-` (varname)
report[,"trackingnumber":=1:nrow(survey)]
print(identical(colnames(report)[1:(ncol(report)-1)],varname)) # check if these two lists are the same

########################################################################################
#    Background                                                                        #
#  already reordered for the firsttime
########################################################################################


# Define functions --------------------------------------------------------

# identify the location of students' choice
Answersloc <- which(is.na(str_match(colnames(finalquestion_total),"Ans[1-7]"))==FALSE)
# replace variable name with tracking numbers to avoid making update manually
# Define functions to automatically updated survey report (the most important part)
report_des <- function(x,text = "Subject"){
  options(warn = -1) # disable warnings
  # change all the data tables into data frames 
  survey <- as.data.frame(survey)
  report <- as.data.frame(report)
  finalquestion_total <- as.data.frame(finalquestion_total)
  lists <- as.data.frame(lists)
  # choice is a vector for selection
  choice <- finalquestion_total[which(finalquestion_total$`Variable Name`==varname[x]),Answersloc][1,]
  {if(sum(!is.na(choice))==0){
    report[,x] <<- str_c(finalquestion_total$Question[x],"-- ",survey[,x])
    finalquestion_total$reorder[x] <<- 1
  }
    if(sum(!is.na(choice))==1){
      report[,x] <<- str_c(finalquestion_total$Question[x], "-- ", 
                           lists[,which(colnames(lists)==text)][survey[,which(colnames(survey)==varname[x])[1]]])
      finalquestion_total$reorder[x] <<- -1
    }
    if(sum(!is.na(choice))!=0 & sum(!is.na(choice))!=1){
      report[,x] <<- factor(survey[,which(colnames(survey)==varname[x])[1]],
                            levels = 1:NumAns[x], 
                            labels = str_c(finalquestion_total$Question[x],"-- ",choice[1,1:NumAns[x]]))
      finalquestion_total$reorder[x] <<- 1
    }}
}

report_sum <- function(x=1,text){
  survey <- as.data.frame(survey)
  report <- as.data.frame(report)
  finalquestion_total <- as.data.frame(finalquestion_total)
  
  loc <- do.call(c,lapply(text,function(i){
    loc <<- which(is.na(str_match(varname,i))==FALSE)
  }))
  finalquestion_total$reorder[loc] <<- 1
  
  report[,loc[1]] <<-
    rowSums(survey[,loc],na.rm = TRUE)
  report[,loc[2:length(loc)]] <<- NA
  report[,loc[1]]
}


report_mean <- function(x=1,text){
  survey <- as.data.frame(survey)
  report <- as.data.frame(report)
  finalquestion_total <- as.data.frame(finalquestion_total)
  # text = c("S_Care_Voc_IArti","S_Care_Voca_Artis")
  loc <- do.call(c,lapply(text,function(i){
    i <- as.character(i)
    loc <<- which(is.na(str_match(varname,i))==FALSE)
  }))
  
  finalquestion_total$reorder[loc] <<- 1
  
  report[,loc[1]] <<-
    rowMeans(survey[,loc],na.rm = TRUE)
  report[,loc[2:length(loc)]] <<- NA
  report[,loc[1]]
}




report_meang <- function(x=1,text){
  survey <- as.data.frame(survey)
  report <- as.data.frame(report)
  finalquestion_total <- as.data.frame(finalquestion_total)
  
  loc <- do.call(c,lapply(text,function(i){
    loc <<- which(is.na(str_match(varname,i))==FALSE)
  }))
  finalquestion_total$reorder[loc] <<- 1
  
  report[,loc[1]] <<-
    rowMeans(survey[,loc],na.rm = TRUE)/NumAns[x]
  report[,loc[2:length(loc)]] <<- NA
  report[,loc[1]]
}

# Curve and standarization
curved <- function(var){
  survey <- as.data.frame(survey)
  # var <- c("S_Lear_Meta_Meta_9","S_Lear_Meta_Meta_8")
  varloc <<- do.call(c,lapply(var,function(x){
    x <- as.character(x)
    which(is.na(str_match(colnames(survey),x))==FALSE)
  }))
  survey[,varloc] <<- matrix(rep(attr(scale(survey[,varloc]),"scaled:center"),each = nrow(survey)),byrow = FALSE,nrow=nrow(survey)) + 
    as.data.frame(scale(survey[,varloc])) %>%
    round(.,2)
}


# define the function for presenting several plots in one page (for analysis)
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


# change value according to the original survey
#################################################################
#   ADHD         
#   gender info 
#################################################################
# all values needs to be changed in range [0,3]
# ADHD needs to be reversed




# survey[,varname[which(is.na(str_match(varname,"P_Disa_ADHD_Hyper"))==FALSE)]] <- 
#   survey[,varname[which(is.na(str_match(varname,"P_Disa_ADHD_Hyper"))==FALSE)],with=FALSE]-2
# survey[,varname[which(is.na(str_match(varname,"P_Disa_ADHD_Inatt"))==FALSE)]] <- 
#   survey[,varname[which(is.na(str_match(varname,"P_Disa_ADHD_Inatt"))==FALSE)],with=FALSE]-2
# 
# survey <- as.data.frame(survey)
# for (i in 1:ncol(survey[,which(is.na(str_match(varname,"P_Disa_ADHD_Hyper"))==FALSE)])){
#   survey[,which(is.na(str_match(varname,"P_Disa_ADHD_Hyper"))==FALSE)][,i][
#     which(survey[,which(is.na(str_match(varname,"P_Disa_ADHD_Hyper"))==FALSE)][,i]<0)] <- 0       
# }
# 
# for (i in 1:ncol(survey[,which(is.na(str_match(varname,"P_Disa_ADHD_Inatt"))==FALSE)])){
#   survey[,which(is.na(str_match(varname,"P_Disa_ADHD_Inatt"))==FALSE)][,i][
#     which(survey[,which(is.na(str_match(varname,"P_Disa_ADHD_Inatt"))==FALSE)][,i]<0)] <- 0       
# }
# survey <- as.data.table(survey)
# 
# 
# # For student, ADHD needs to be reversed
# # Step 1: change the scale from [1,5] to [0,0,1,2,3]
# survey <- as.data.frame(survey)
# survey[,which(is.na(str_match(varname,"S_Disa_ADHD_Inatt"))==FALSE)] <- 6-survey[,which(is.na(str_match(varname,"S_Disa_ADHD_Inatt"))==FALSE)]
# 
# survey[,varname[which(is.na(str_match(varname,"S_Disa_ADHD_Inatt"))==FALSE)]] <-
#   survey[,varname[which(is.na(str_match(varname,"S_Disa_ADHD_Inatt"))==FALSE)]]-2
# 
# survey <- as.data.frame(survey) # change it to data frame since there is no explicit commands
# # in data table do do the exactly same thing
# for (i in 1:ncol(survey[,which(is.na(str_match(varname,"S_Disa_ADHD_Inatt"))==FALSE)])){
#   survey[,which(is.na(str_match(varname,"S_Disa_ADHD_Inatt"))==FALSE)][,i][
#     which(survey[,which(is.na(str_match(varname,"S_Disa_ADHD_Inatt"))==FALSE)][,i]<0)] <- 0
# }
# survey <- as.data.table(survey) # after change the answer scales, transfer it back to data table
# 
# 


