########################################################################################
# new survey (544 questions)
# report is a final version of the results of all questions in screening process
########################################################################################

# variable name需要和finalquestion_excel里保持一致

rm(list=ls())
file <- "Staff Test Response.xlsx"
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
# load survey questions: 1. Record all variable names 2. 
# if the file is saved as xlsx, then this line can be used.

# load original questionaire sheet by sheet
# remember to delete all the lines/variables indicating the pomps
# finalquestion_total <- read_xlsx("./survey/Final Question.xlsx", sheet = "Questions") %>%
finalquestion_total <- read_xlsx("./survey/Summer UES Self-Discovery Inventor20180709.xlsx", sheet = "Final Web Questions") %>%
  as.data.table %>%
  # 排序
  .[order(`Variable Name`),]
finalquestion_total <- as.data.frame(finalquestion_total)
finalquestion_total <- finalquestion_total[-which(is.na(str_match(finalquestion_total$`Variable Name`, "#prom"))==FALSE),] %>%
  as.data.table
# lists is the options for open questions in the survey, e.g. specialist
lists <- read_xlsx("./survey/Summer UES Self-Discovery Inventor20180709.xlsx",sheet = "Lists") %>%
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
# rawsurvey <- read_xlsx("Staff Test Response.xlsx",sheet = "Form Responses 1")
rawsurvey <- read_xlsx(file,sheet = "Form Responses 1")
rawsurvey[1,] <- str_replace_all(rawsurvey[1,],"-","_")
colnames(rawsurvey) <- rawsurvey[1,]
rawsurvey <- rawsurvey[-1,]%>%
  as.data.table

rawsurvey <- rawsurvey[,"trackingnumber":=1:nrow(rawsurvey)] %>%
  as.data.frame # adding tracking number
rawsurvey[,3:ncol(rawsurvey)] <- apply(rawsurvey[,3:ncol(rawsurvey)],2,function(x){str_replace_all(x,"[a-z]+","")})
rawsurvey[,3:ncol(rawsurvey)] <- apply(rawsurvey[,3:ncol(rawsurvey)],2,function(x){str_replace_all(x,"[A-Z]+","")})
rawsurvey[,3:ncol(rawsurvey)] <- apply(rawsurvey[,3:ncol(rawsurvey)],2,function(x){str_replace_all(x,"-","")})
rawsurvey[,3:ncol(rawsurvey)] <- apply(rawsurvey[,3:ncol(rawsurvey)],2,function(x){as.numeric(x)})
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
reverseadj <- 8-survey[,rev,with=FALSE] # create new lines
survey[,which(str_count(finalquestion_total$Reverse)>0)] <- reverseadj
# for reorder: add a new indentification row
survey <- survey %>%
  as.data.frame %>%
  # rbind(.,rep(7,ncol(.))) %>%
  as.data.table

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
identical(colnames(report)[1:(ncol(report)-1)],varname) # check if these two lists are the same
########################################################################################
#    Background                                                                        #
#  already reordered for the firsttime
########################################################################################
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
  
  Num <- which(is.na(str_match(varname,text))==FALSE)
  finalquestion_total$reorder[Num] <<- 1
  
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
  
  Num <- which(is.na(str_match(varname,text))==FALSE)
  finalquestion_total$reorder[Num] <<- 1
  
  loc <- do.call(c,lapply(text,function(i){
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
  
  Num <- which(is.na(str_match(varname,text))==FALSE)
  finalquestion_total$reorder[Num] <<- 1
  
  loc <- do.call(c,lapply(text,function(i){
    loc <<- which(is.na(str_match(varname,i))==FALSE)
  }))
  finalquestion_total$reorder[loc] <<- 1
  
  report[,loc[1]] <<-
    rowMeans(survey[,loc],na.rm = TRUE)/NumAns[x]
  report[,loc[2:length(loc)]] <<- NA
  report[,loc[1]]
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
survey[,varname[which(is.na(str_match(varname,"P_Disa_ADHD_Hyper"))==FALSE)]] <- 
  survey[,varname[which(is.na(str_match(varname,"P_Disa_ADHD_Hyper"))==FALSE)],with=FALSE]-2
survey[,varname[which(is.na(str_match(varname,"P_Disa_ADHD_Inatt"))==FALSE)]] <- 
  survey[,varname[which(is.na(str_match(varname,"P_Disa_ADHD_Inatt"))==FALSE)],with=FALSE]-2

survey <- as.data.frame(survey)
for (i in 1:ncol(survey[,which(is.na(str_match(varname,"P_Disa_ADHD_Hyper"))==FALSE)])){
  survey[,which(is.na(str_match(varname,"P_Disa_ADHD_Hyper"))==FALSE)][,i][
    which(survey[,which(is.na(str_match(varname,"P_Disa_ADHD_Hyper"))==FALSE)][,i]<0)] <- 0       
}

for (i in 1:ncol(survey[,which(is.na(str_match(varname,"P_Disa_ADHD_Inatt"))==FALSE)])){
  survey[,which(is.na(str_match(varname,"P_Disa_ADHD_Inatt"))==FALSE)][,i][
    which(survey[,which(is.na(str_match(varname,"P_Disa_ADHD_Inatt"))==FALSE)][,i]<0)] <- 0       
}
survey <- as.data.table(survey)

# For student, ADHD needs to be reversed
# Step 1: change the scale from [1,5] to [0,0,1,2,3]
survey[,varname[which(is.na(str_match(varname,"S_Disa_ADHD_Inatt"))==FALSE)]] <- 
  survey[,varname[which(is.na(str_match(varname,"S_Disa_ADHD_Inatt"))==FALSE)],with=FALSE]-2

survey <- as.data.frame(survey) # change it to data frame since there is no explicit commands
# in data table do do the exactly same thing
for (i in 1:ncol(survey[,which(is.na(str_match(varname,"S_Disa_ADHD_Inatt"))==FALSE)])){
  survey[,which(is.na(str_match(varname,"S_Disa_ADHD_Inatt"))==FALSE)][,i][
    which(survey[,which(is.na(str_match(varname,"S_Disa_ADHD_Inatt"))==FALSE)][,i]<0)] <- 0       
}
survey[,which(is.na(str_match(varname,"S_Disa_ADHD_Inatt"))==FALSE)] <- 3-survey[,which(is.na(str_match(varname,"S_Disa_ADHD_Inatt"))==FALSE)]
survey <- as.data.table(survey) # after change the answer scales, transfer it back to data table






# SAgreeableness
ReportFormula <- report_mean(text = "S_Pers_Big5_Agree") %>%
  report_mean(text = "S_Pers_Big5_Agree")%>%
  as.data.frame%>%
  `colnames<-`(c("SAgreeableness"))
# SConscientiousness
ReportFormula$SConscientiousness <- report_mean(text = "S_Pers_Big5_Consc")%>%
  report_mean(text = "S_Pers_Big5_Consc")
# SExtraversion
ReportFormula$SExtraversion <- report_mean(text = "S_Pers_Big5_Extra") %>%
  report_mean(text = "S_Pers_Big5_Extra")
# SEmotional Stability
ReportFormula$`SEmotional Stability` <- report_mean(text = "S_Pers_Big5_Emoti") %>%
  report_mean(text = "S_Pers_Big5_Emoti")
# SOpenness
ReportFormula$SOpenness <- report_mean(text = "S_Pers_Big5_Openn")%>%
  report_mean(text = "S_Pers_Big5_Openn")
# SVocConfidenceArt
ReportFormula$SVocConfidenceArt <- report_mean(text = "S_Care_Voc_CArti") %>%
  report_mean(text = "S_Care_Voc_CArti")
# SVocConfidenceCon
ReportFormula$SVocConfidenceCon <- report_mean(text = "S_Care_Voc_CConv")%>%
  report_mean(text = "S_Care_Voc_CConv")
# SVocConfidenceEnt
ReportFormula$SVocConfidenceEnt <- report_mean(text = "S_Care_Voc_CEnte")%>%
  report_mean(text = "S_Care_Voc_CEnte")
# SVocConfidenceInv
ReportFormula$SVocConfidenceInv <- report_mean(text = "S_Care_Voc_CInve")%>%
  report_mean(text = "S_Care_Voc_CInve")
# SVocConfidenceRea
ReportFormula$SVocConfidenceRea <- report_mean(text = "S_Care_Voc_CReal")%>%
  report_mean(text = "S_Care_Voc_CReal")
# SVocConfidenceSoc
ReportFormula$SVocConfidenceSoc <- report_mean(text = "S_Care_Voc_CSoci")%>%
  report_mean(text = "S_Care_Voc_CSoci")

# SVocInterestArt
ReportFormula$SVocInterestArt <- report_mean(text = c("S_Care_Voc_IArti","S_Care_Voca_Artis"))%>%
  report_mean(text = c("S_Care_Voc_IArti","S_Care_Voca_Artis"))
# SVocInterestCon
ReportFormula$SVocInterestCon <- report_mean(text = c("S_Care_Voc_IConv","S_Care_Voca_Conve"))%>%
  report_mean(text = c("S_Care_Voc_IConv","S_Care_Voca_Conve"))
# SVocInterestEnt
ReportFormula$SVocInterestEnt <- report_mean(text = c("S_Care_Voc_IEnte","S_Care_Voca_Enter"))%>%
  report_mean(text = c("S_Care_Voc_IEnte","S_Care_Voca_Enter"))
# SVocInterestInv
ReportFormula$SVocInterestInv <- report_mean(text = c("S_Care_Voc_IInve","S_Care_Voca_Inves"))%>%
  report_mean(text = c("S_Care_Voc_IInve","S_Care_Voca_Inves"))
# SVocInterestRea
ReportFormula$SVocInterestRea <- report_mean(text = c("S_Care_Voc_IReal","S_Care_Voca_Reali"))%>%
  report_mean(text = c("S_Care_Voc_IReal","S_Care_Voca_Reali"))
# SVocInterestSoc
ReportFormula$SVocInterestSoc <- report_mean(text = c("S_Care_Voc_ISoci","S_Care_Voca_Socia"))%>%
  report_mean(text = c("S_Care_Voc_ISoci","S_Care_Voca_Socia"))

# SParenting
ReportFormula$SParenting <- report_mean(text = c("S_Back_Pare_Abili1", "S_Back_Pare_Consi1" , "S_Back_Pare_Empat1","S_Back_Pare_Engag1", "S_Back_Pare_Expec1" , "S_Back_Pare_Expec2" , "S_Back_Pare_Liste1" , "S_Back_Pare_Patie1" , "S_Back_Pare_Under1"))%>%
  report_mean(text = c("S_Back_Pare_Abili1", "S_Back_Pare_Consi1" , "S_Back_Pare_Empat1","S_Back_Pare_Engag1", "S_Back_Pare_Expec1" , "S_Back_Pare_Expec2" , "S_Back_Pare_Liste1" , "S_Back_Pare_Patie1" , "S_Back_Pare_Under1"))
# PParenting
ReportFormula$PParenting <- report_mean(text = c("P_Back_Pare_Abili1" , "P_Back_Pare_Consi1" , "P_Back_Pare_Empat1" , "P_Back_Pare_Empat2" , "P_Back_Pare_Engag1" , "P_Back_Pare_Liste1" , "P_Back_Pare_Patie1" , "P_Back_Pare_Under1" , "P_Back_Pare_Under2" , "P_Back_Pare_zEnga1"))%>%
  report_mean(text = c("P_Back_Pare_Abili1" , "P_Back_Pare_Consi1" , "P_Back_Pare_Empat1" , "P_Back_Pare_Empat2" , "P_Back_Pare_Engag1" , "P_Back_Pare_Liste1" , "P_Back_Pare_Patie1" , "P_Back_Pare_Under1" , "P_Back_Pare_Under2" , "P_Back_Pare_zEnga1"))
# PAgreeableness
ReportFormula$PAgreeableness <- report_mean(text = "P_Pers_Big5_Agree")%>%
  report_mean(text = "P_Pers_Big5_Agree")
# PConPcientiousness
ReportFormula$PConPcientiousness <- report_mean(text ="P_Pers_Big5_Consc")%>%
  report_mean(text ="P_Pers_Big5_Consc")
# PExtraversion
ReportFormula$PExtraversion <- report_mean(text = "P_Pers_Big5_Extra")%>%
  report_mean(text = "P_Pers_Big5_Extra")
# PEmotional Stability
ReportFormula$`PEmotional Stability` <- report_mean(text = "P_Pers_Big5_Emoti")%>%
  report_mean(text = "P_Pers_Big5_Emoti")
# POpenness
ReportFormula$POpenness <- report_mean(text = "P_Pers_Big5_Openn")%>%
  report_mean(text = "P_Pers_Big5_Openn")
# PVocational ConfidenceArt
ReportFormula$`PVocational ConfidenceArt` <- report_mean(text = "P_Care_Voca_Artis")%>%
  report_mean(text = "P_Care_Voca_Artis")
# PVocational ConfidenceCon
ReportFormula$`PVocational ConfidenceCon` <- report_mean(text = "P_Care_Voca_Conve")%>%
  report_mean(text = "P_Care_Voca_Conve")
# PVocational ConfidenceEnt
ReportFormula$`PVocational ConfidenceEnt` <- report_mean(text = "P_Care_Voca_Enter")%>%
  report_mean(text = "P_Care_Voca_Enter")
# PVocational ConfidenceInv
ReportFormula$`PVocational ConfidenceInv` <- report_mean(text = "P_Care_Voca_Inves")%>%
  report_mean(text = "P_Care_Voca_Inves")
# PVocational ConfidenceRea
ReportFormula$`PVocational ConfidenceRea` <- report_mean(text = "P_Care_Voca_Reali")%>%
  report_mean(text = "P_Care_Voca_Reali")
# PVocational ConfidenceSoc
ReportFormula$`PVocational ConfidenceSoc` <- report_mean(text = "P_Care_Voca_Socia")%>%
  report_mean(text = "P_Care_Voca_Socia")

# Critical Thinking 
ReportFormula$`Critical Thinking` <- report_mean(text = c("S_Lear_Crit_Criti","S_Lear_Crit_Refle"))%>%
  report_mean(text = c("S_Lear_Crit_Criti","S_Lear_Crit_Refle"))
# Reflective Thinking
ReportFormula$`Reflective Thinking` <- report_mean(text = "S_Lear_Meta_Meta")%>%
  report_mean(text = "S_Lear_Meta_Meta")
# Creativity
ReportFormula$Creativity <- report_mean(text = c("S_Crea_Self_Creat1" , "S_Crea_Self_Evide1" , "S_Crea_Self_Evide2" , "S_Crea_Self_Evide3" , "S_Crea_Self_Perso1"))%>%
  report_mean(text = c("S_Crea_Self_Creat1" , "S_Crea_Self_Evide1" , "S_Crea_Self_Evide2" , "S_Crea_Self_Evide3" , "S_Crea_Self_Perso1"))
# Ability to focus
ReportFormula$`Ability to focus` <- report_sum(text = c("S_Disa_ADHD_Inatt"))%>%
  report_sum(text = c("S_Disa_ADHD_Inatt"))
ReportFormula$`Ability to focus` <- 27-ReportFormula$`Ability to focus`

# Organization
ReportFormula$Organization <- report_mean(text = "S_Lear_Orga_Organ")%>%
  report_mean(text = "S_Lear_Orga_Organ")
# Time Management
ReportFormula$`Time Management` <- report_mean(text = c("S_Lear_Orga_L_Tim","S_Lear_Orga_S_Tim"))%>%
  report_mean(text = c("S_Lear_Orga_L_Tim","S_Lear_Orga_S_Tim"))

# Grit 
ReportFormula$Grit <- report_mean(text = "S_Pers_GRIT_GRIT")%>%
  report_mean(text = "S_Pers_GRIT_GRIT")
# Resilience
ReportFormula$Resilience <- report_mean(text = "S_Pers_Resi_Resil")%>%
  report_mean(text = "S_Pers_Resi_Resil")
# Growth Mindset
ReportFormula$`Growth Mindset` <- report_mean(text = c("S_Know_Mind_Growt","S_Know_Mind_Fixed"))%>%
  report_mean(text = c("S_Know_Mind_Growt","S_Know_Mind_Fixed"))
# Motivations
ReportFormula$Motivations <- report_mean(text = c("S_Moti_Acad_Extri","S_Moti_Acad_Intri","S_Moti_Acad_N_Amo1")) %>%
  report_mean(text = c("S_Moti_Acad_Extri","S_Moti_Acad_Intri","S_Moti_Acad_N_Amo1")) 
# Self Confidence
ReportFormula$`Self Confidence` <- report_mean(text = "S_Care_Voc_C")%>%
  report_mean(text = "S_Care_Voc_C")

# Work Ethic
ReportFormula$`Work Ethic` <- report_mean(text = c ("S_Pers_Big5_Consc1" , "S_Pers_Big5_Consc2" , "S_Pers_Big5_Consc3" , "S_Pers_Big5_Consc4" , "S_Know_cult_Filia5" , "S_Know_cult_Non_c1" , "S_Comm_Coll_Parti1" , "S_Comm_Coll_Parti3"))%>%
  report_mean(text = c ("S_Pers_Big5_Consc1" , "S_Pers_Big5_Consc2" , "S_Pers_Big5_Consc3" , "S_Pers_Big5_Consc4" , "S_Know_cult_Filia5" , "S_Know_cult_Non_c1" , "S_Comm_Coll_Parti1" , "S_Comm_Coll_Parti3"))
# Positive Attitude
ReportFormula$`PositiveAttitude`<- report_mean(text = c("S_Pers_Resi_Resil1","S_Pers_Resi_Resil2","S_Pers_Resi_Resil3","S_Pers_Resi_Resil4","S_Pers_Resi_Resil9"))%>%
  report_mean(text = c("S_Pers_Resi_Resil1","S_Pers_Resi_Resil2","S_Pers_Resi_Resil3","S_Pers_Resi_Resil4","S_Pers_Resi_Resil9"))

# Communication
ReportFormula$Communication <- report_mean(text = c("S_Comm_Coll_Engag1","S_Comm_Coll_Manag1","S_Comm_Coll_Manag2"))%>%
  report_mean(text = c("S_Comm_Coll_Engag1","S_Comm_Coll_Manag1","S_Comm_Coll_Manag2"))
# Empathy
ReportFormula$Empathy <- report_mean(text = c("S_Comm_Coll_Brain1","S_Comm_Coll_Colle1","S_Comm_Coll_Colle2","S_Comm_Coll_Colle3","S_Comm_Coll_Using1","S_Comm_Comm_Useo1"))%>%
  report_mean(text = c("S_Comm_Coll_Brain1","S_Comm_Coll_Colle1","S_Comm_Coll_Colle2","S_Comm_Coll_Colle3","S_Comm_Coll_Using1","S_Comm_Comm_Useo1"))
# Relationship Management
ReportFormula$`Relationship Management` <- report_mean(text = c("S_Comm_Comm_Activ1" , "S_Comm_Comm_Activ2" , "S_Comm_Comm_Commu1" , "S_Comm_Comm_Commu2" , "S_Comm_Comm_Useo1" , "S_Comm_Comm_Useo2", "S_Comm_Comm_Useo3" , "S_Comm_Comm_Useo4"))%>%
  report_mean(text = c("S_Comm_Comm_Activ1" , "S_Comm_Comm_Activ2" , "S_Comm_Comm_Commu1" , "S_Comm_Comm_Commu2" , "S_Comm_Comm_Useo1" , "S_Comm_Comm_Useo2", "S_Comm_Comm_Useo3" , "S_Comm_Comm_Useo4"))
# Flexibility&Adaptability
ReportFormula$`Flexibility&Adaptability` <- report_mean(text = c("S_Pers_Big5_Openn2" , "S_Pers_Big5_Openn1", "S_Pers_Big5_Openn3","S_Pers_Big5_Openn4"))%>%
  report_mean(text = c("S_Pers_Big5_Openn2" , "S_Pers_Big5_Openn1", "S_Pers_Big5_Openn3","S_Pers_Big5_Openn4"))
# Team Work
ReportFormula$`Team Work` <- report_mean(text = c("S_Comm_Coll_Brain1" , "S_Comm_Coll_Colle1" , "S_Comm_Coll_Colle2" , "S_Comm_Coll_Colle3" , "S_Comm_Coll_Engag1" , "S_Comm_Coll_Manag1" , "S_Comm_Coll_Manag2" , "S_Comm_Coll_Parti1" , "S_Comm_Coll_Parti2" , "S_Comm_Coll_Parti3" , "S_Comm_Coll_Using1"))%>%
  report_mean(text = c("S_Comm_Coll_Brain1" , "S_Comm_Coll_Colle1" , "S_Comm_Coll_Colle2" , "S_Comm_Coll_Colle3" , "S_Comm_Coll_Engag1" , "S_Comm_Coll_Manag1" , "S_Comm_Coll_Manag2" , "S_Comm_Coll_Parti1" , "S_Comm_Coll_Parti2" , "S_Comm_Coll_Parti3" , "S_Comm_Coll_Using1"))
# Coping with Pressure
ReportFormula$`Coping with Pressure` <- report_mean(text = c("S_Pers_Resi_Resil4" ,  "S_Pers_Resi_Resil5" ,  "S_Pers_Resi_Resil6" ,  "S_Pers_Resi_Resil7" ,  "S_Pers_Resi_Resil8"))%>%
  report_mean(text = c("S_Pers_Resi_Resil4" ,  "S_Pers_Resi_Resil5" ,  "S_Pers_Resi_Resil6" ,  "S_Pers_Resi_Resil7" ,  "S_Pers_Resi_Resil8"))
# Negotiation
ReportFormula$`Negotiation` <- report_mean(text = c("S_Comm_Coll_Parti2","S_Comm_Comm_Activ1", "S_Comm_Comm_Activ2" , "S_Comm_Comm_Commu1" , "S_Comm_Comm_Commu2" , "S_Comm_Comm_Useo3"))%>%
  report_mean(text = c("S_Comm_Coll_Parti2","S_Comm_Comm_Activ1", "S_Comm_Comm_Activ2" , "S_Comm_Comm_Commu1" , "S_Comm_Comm_Commu2" , "S_Comm_Comm_Useo3"))
# Conflict Resolution
ReportFormula$`Conflict Resolution` <- report_mean(text = c("S_Comm_Coll_Manag1" , "S_Comm_Coll_Manag2" ,"S_Care_Voc_CSoci1" , "S_Care_Voc_CSoci2" , "S_Care_Voc_CSoci3" , "S_Care_Voc_CSoci4" , "S_Care_Voc_CSoci5"))%>%
  report_mean(text = c("S_Comm_Coll_Manag1" , "S_Comm_Coll_Manag2" ,"S_Care_Voc_CSoci1" , "S_Care_Voc_CSoci2" , "S_Care_Voc_CSoci3" , "S_Care_Voc_CSoci4" , "S_Care_Voc_CSoci5"))



ReportFormula[1,]
ReportFormula$Artconfidence <- ifelse(ReportFormula$SVocConfidenceArt>4,1,-1)
ReportFormula$Artinterest <- ifelse(ReportFormula$SVocInterestArt>4,1,-1)

ReportFormula$Reaconfidence <- ifelse(ReportFormula$SVocConfidenceRea>4,1,-1)
ReportFormula$Reainterest <- ifelse(ReportFormula$SVocInterestRea>4,1,-1)

ReportFormula$Socconfidence <- ifelse(ReportFormula$SVocConfidenceSoc>4,1,-1)
ReportFormula$Socinterest <- ifelse(ReportFormula$SVocInterestSoc>4,1,-1)

ReportFormula$Conconfidence <- ifelse(ReportFormula$SVocConfidenceCon>4,1,-1)
ReportFormula$Coninterest <- ifelse(ReportFormula$SVocInterestCon>4,1,-1)

ReportFormula$Entconfidence <- ifelse(ReportFormula$SVocConfidenceEnt>4,1,-1)
ReportFormula$Entinterest <- ifelse(ReportFormula$SVocInterestEnt>4,1,-1)

ReportFormula$Invconfidence <- ifelse(ReportFormula$SVocConfidenceInv>4,1,-1)
ReportFormula$Invinterest <- ifelse(ReportFormula$SVocInterestInv>4,1,-1)


# identifying the criteria ----------------------------------------------------
# R
Rc <- mean(c(ReportFormula$SConscientiousness[which(is.na(str_match(rawsurvey[,2],"yuchi"))==FALSE)],
       ReportFormula$SConscientiousness[which(is.na(str_match(rawsurvey[,2],"alex"))==FALSE)]
       ))/7*100
Re <- mean(c(
       ReportFormula$SExtraversion[which(is.na(str_match(rawsurvey[,2],"alex"))==FALSE)],
       ReportFormula$SExtraversion[which(is.na(str_match(rawsurvey[,2],"alex"))==FALSE)]
       ))/7*100
# I
Io <- mean(c(ReportFormula$SOpenness[which(is.na(str_match(rawsurvey[,2],"yuchi"))==FALSE)],
       ReportFormula$SOpenness[which(is.na(str_match(rawsurvey[,2],"alex"))==FALSE)]
       ))/7*100
In <- mean(c(
       ReportFormula$`SEmotional Stability`[which(is.na(str_match(rawsurvey[,2],"yuchi"))==FALSE)],
       ReportFormula$`SEmotional Stability`[which(is.na(str_match(rawsurvey[,2],"alex"))==FALSE)]
       ))/7*100
# A
Ao <- mean(c(ReportFormula$SOpenness[which(is.na(str_match(rawsurvey[,2],"yuchi"))==FALSE)],
       ReportFormula$SOpenness[which(is.na(str_match(rawsurvey[,2],"evelyn"))==FALSE)],
       ReportFormula$SOpenness[which(is.na(str_match(rawsurvey[,2],"kylie"))==FALSE)]
       ))/7*100
# S
So <- mean(c(ReportFormula$SOpenness[which(is.na(str_match(rawsurvey[,2],"linli"))==FALSE)],
       ReportFormula$SOpenness[which(is.na(str_match(rawsurvey[,2],"miyoki"))==FALSE)],
       ReportFormula$SOpenness[which(is.na(str_match(rawsurvey[,2],"lorena"))==FALSE)]
       ))/7*100
Se <- mean(c(
       ReportFormula$SExtraversion[which(is.na(str_match(rawsurvey[,2],"linli"))==FALSE)],
       ReportFormula$SExtraversion[which(is.na(str_match(rawsurvey[,2],"miyoki"))==FALSE)],
       ReportFormula$SExtraversion[which(is.na(str_match(rawsurvey[,2],"lorena"))==FALSE)]
       ))/7*100
Sa <- mean(c(
       ReportFormula$SAgreeableness[which(is.na(str_match(rawsurvey[,2],"linli"))==FALSE)],
       ReportFormula$SAgreeableness[which(is.na(str_match(rawsurvey[,2],"miyoki"))==FALSE)],
       ReportFormula$SAgreeableness[which(is.na(str_match(rawsurvey[,2],"lorena"))==FALSE)]
       ))/7*100
# E
Ec <- mean(c(ReportFormula$SConscientiousness[which(is.na(str_match(rawsurvey[,2],"chris"))==FALSE)],
       ReportFormula$SConscientiousness[which(is.na(str_match(rawsurvey[,2],"yuchi"))==FALSE)],
       ReportFormula$SConscientiousness[which(is.na(str_match(rawsurvey[,2],"katherine"))==FALSE)]
       ))/7*100
Ee <- mean(c(
       ReportFormula$SExtraversion[which(is.na(str_match(rawsurvey[,2],"chris"))==FALSE)],
       ReportFormula$SExtraversion[which(is.na(str_match(rawsurvey[,2],"yuchi"))==FALSE)],
       ReportFormula$SExtraversion[which(is.na(str_match(rawsurvey[,2],"katherine"))==FALSE)]
       ))/7*100
En <- mean(c(
       ReportFormula$`SEmotional Stability`[which(is.na(str_match(rawsurvey[,2],"chris"))==FALSE)],
       ReportFormula$`SEmotional Stability`[which(is.na(str_match(rawsurvey[,2],"yuchi"))==FALSE)],
       ReportFormula$`SEmotional Stability`[which(is.na(str_match(rawsurvey[,2],"katherine"))==FALSE)]
       ))/7*100
# C
Cc <- mean(c(ReportFormula$SConscientiousness[which(is.na(str_match(rawsurvey[,2],"nian"))==FALSE)],
       ReportFormula$SConscientiousness[which(is.na(str_match(rawsurvey[,2],"tyler"))==FALSE)],
       ReportFormula$SConscientiousness[which(is.na(str_match(rawsurvey[,2],"gidien"))==FALSE)],
       ReportFormula$SConscientiousness[which(is.na(str_match(rawsurvey[,2],"oliver"))==FALSE)],
       ReportFormula$SConscientiousness[which(is.na(str_match(rawsurvey[,2],"mark"))==FALSE)]
       ))/7*100
Cn <- mean(c(
       ReportFormula$`SEmotional Stability`[which(is.na(str_match(rawsurvey[,2],"nian"))==FALSE)],
       ReportFormula$`SEmotional Stability`[which(is.na(str_match(rawsurvey[,2],"tyler"))==FALSE)],
       ReportFormula$`SEmotional Stability`[which(is.na(str_match(rawsurvey[,2],"gidien"))==FALSE)],
       ReportFormula$`SEmotional Stability`[which(is.na(str_match(rawsurvey[,2],"oliver"))==FALSE)],
       ReportFormula$`SEmotional Stability`[which(is.na(str_match(rawsurvey[,2],"mark"))==FALSE)]
       ))/7*100
criteria <- c(Rc,Re,Io,In,Ao,So,Se,Sa,Ec,Ee,En,Cc,Cn)
names(criteria) <- c("Rc","Re","Io","In","Ao",'So','Se','Sa','Ec','Ee','En','Cc','Cn')
criteria

mean(ReportFormula$SOpenness)/7*100
mean(ReportFormula$SConscientiousness)/7*100
mean(ReportFormula$SExtraversion)/7*100
mean(ReportFormula$SAgreeableness)/7*100
mean(ReportFormula$`SEmotional Stability`)/7*100
# 如果需要用staff的平均得分来作为参考，那么前面导入的文件名字需要更换

# making plots ------------------------------------------------------------

########################################################################################
# the last part of this report: output
########################################################################################
# export the results to csv file
# write.csv(report,"./workingdata/report_fakevalue.csv",row.names = FALSE)

# quit(save = "no")


# Personality Report: e.g. for one person
# change the index number to expand them into several parts

lapply(1:nrow(survey),function(i){
# lapply(9,function(i){
  
re <- rbind(
  `Emotional Stability` <- ReportFormula$`SEmotional Stability`,
  Agreeableness <- ReportFormula$SAgreeableness,
  Extraversion <- ReportFormula$SExtraversion,
  Conscientiousness <- ReportFormula$SConscientiousness,
  Openness <- ReportFormula$SOpenness)
# i <- 1
a <- c(order(re[1,])[i],order(re[2,])[i],order(re[3,])[i],order(re[4,])[i],order(re[5,])[i])
b <- ncol(re)
re <- cbind(re[,i])

rownames(re) <- c("Openness","Conscientiousness","Extraversion",
                  "Agreeableness", "Neurotism")
rownames(re) <- c("Neurotism","Agreeableness","Extraversion","Conscientiousness","Openness")
colnames(re) <- "score"

theme_set(theme_bw())  
re <- as.data.frame(re)
# Data Prep
re$name <- rownames(re)
# re$score[which(re$score<4)] <- -re$score[which(re$score<4)]
set.seed(i)
re$score <- c(qnorm((a/b-0.001)[1],50,10),((re$score /7)*100)[2:5]) #+rnorm(5)
re$score

# re$score <- pnorm(a/b)*100
# re$score <- (a/b)*100

# this criteria needs to be changed accordingly
re$per_type <- ifelse(re$score < c(50,55,53,54,55), "below average", "above average")  # above / below avg flag
                      
re$name <- factor(re$name, levels = re$name)  # convert to factor to retain sorted order in plot.

# Diverging Barcharts
options(decimal = 2)
ggplot(re, aes(x=name, y=score, label=score)) + 
  geom_bar(stat='identity', aes(fill=per_type), width=.5)  +
  scale_fill_manual(name="Big Five",
                    labels = c("Above Average", "Below Average"),
                    values = c("above average"="#014d64", "below average"="#6794a7")) +
  labs(x="",
       subtitle = rawsurvey[,2][i]) + 
  coord_flip()+
  ylim(c(0,100))+
  geom_text(aes(label=round(score,1)))
# ggsave(str_c("./figures/personality/",rawsurvey[,2][i],"_personality",".png"))
ggsave(do.call(str_c,lapply(c("./figures/personality/",rawsurvey[,2][i],"_personality_",i,".png"),function(x){
  str_replace_na(x,"")
})))
})

# 7 Success Character ---------------------------------------------------------
lapply(1:nrow(survey),function(i){
  suc <- rbind(
    ReportFormula$PositiveAttitude,
    ReportFormula$`Work Ethic`,
    ReportFormula$`Self Confidence`,
    ReportFormula$Motivations,
    ReportFormula$`Growth Mindset`,
    ReportFormula$Resilience,
    ReportFormula$Grit
  )
  # i = 9 # chris
  a <- c(order(suc[1,])[i],order(suc[2,])[i],order(suc[3,])[i],order(suc[4,])[i],order(suc[5,])[i],order(suc[6,])[i],order(suc[7,])[i])
  b <- ncol(suc)
  suc <- cbind(suc[,i])
  
  rownames(suc) <- c("Positive Attitude","Work Ethic","Self Confidence","Motivations","Growth Mindset","Resilience","Grit")
  colnames(suc) <- "score"
  
  theme_set(theme_bw())  
  suc <- as.data.frame(suc)
  # Data Prep
  suc$name <- rownames(suc)
  # re$score[which(re$score<4)] <- -re$score[which(re$score<4)]
  set.seed(1000+i)
  suc$score <- (suc$score /7)*100 # + rnorm(7)
  # 随机扰动
  # suc$score <- qnorm((suc$score /7))*100
  # re$score <- (a/b)*100
  
  suc$per_type <- ifelse(suc$score < c(53,55,53,54,65,53,53), "below average", "above average")  # above / below avg flag
  
  suc$name <- factor(suc$name, levels = suc$name)  # convert to factor to retain sorted order in plot.
  
  # Diverging Barcharts
  options(decimal = 2)
  ggplot(suc, aes(x=name, y=score, label=score)) + 
    geom_bar(stat='identity', aes(fill=per_type), width=.5)  +
    scale_fill_manual(name="7 Key Success Characters",
                      labels = c("Above Average", "Below Average"),
                      values = c("above average"="#014d64", "below average"="#6794a7")) +
    labs(x="",
         subtitle = rawsurvey[,2][i]) + 
    coord_flip()+
    ylim(c(0,100))+
    geom_text(aes(label=round(score,1)))
  ggsave(do.call(str_c,lapply(c("./figures/7SuccessCharacters/",rawsurvey[,2][i],"_7SuccessCharacters_",i,".png"),function(x){
    str_replace_na(x,"")
  })))
})

# 6 Key Success Competencies ---------------------------------------------------------
lapply(1:nrow(survey),function(i){
  suc <- rbind(
    ReportFormula$`Time Management`,
    ReportFormula$Organization,
    ReportFormula$`Ability to focus`,
    ReportFormula$Creativity,
    ReportFormula$`Reflective Thinking`,
    ReportFormula$`Critical Thinking`
  )
  # i = 9 # chris
  a <- c(order(suc[1,])[i],order(suc[2,])[i],order(suc[3,])[i],order(suc[4,])[i],order(suc[5,])[i],order(suc[6,])[i])
  b <- ncol(suc)
  suc <- cbind(suc[,i])
  
  rownames(suc) <- c("Time Management","Organization","Ability to focus","Creativity","Reflective Thinking","Critical Thinking")
  colnames(suc) <- "score"
  
  theme_set(theme_bw())  
  suc <- as.data.frame(suc)
  # Data Prep
  suc$name <- rownames(suc)
  # re$score[which(re$score<4)] <- -re$score[which(re$score<4)]
  set.seed(1000+i)
  suc$score <- (suc$score /7)*100 # + rnorm(7)
  # 随机扰动
  # suc$score <- qnorm((suc$score /7))*100
  # re$score <- (a/b)*100
  
  suc$per_type <- ifelse(suc$score < c(53,55,53,54,65,53), "below average", "above average")  # above / below avg flag
  
  suc$name <- factor(suc$name, levels = suc$name)  # convert to factor to retain sorted order in plot.
  
  # Diverging Barcharts
  options(decimal = 2)
  ggplot(suc, aes(x=name, y=score, label=score)) + 
    geom_bar(stat='identity', aes(fill=per_type), width=.5)  +
    scale_fill_manual(name="7 Key Success Characters",
                      labels = c("Above Average", "Below Average"),
                      values = c("above average"="#014d64", "below average"="#6794a7")) +
    labs(x="",
         subtitle = rawsurvey[,2][i]) + 
    coord_flip()+
    ylim(c(0,100))+
    geom_text(aes(label=round(score,1)))
  ggsave(do.call(str_c,lapply(c("./figures/6KeySuccessCompetencies/",rawsurvey[,2][i],"_6KeySuccessCompetencies_",i,".png"),function(x){
    str_replace_na(x,"")
  })))
  })

# 8 Key EQ Skills ---------------------------------------------------------
lapply(1:nrow(survey),function(i){
  suc <- rbind(
    ReportFormula$`Conflict Resolution`,
    ReportFormula$Negotiation,
    ReportFormula$`Coping with Pressure`,
    ReportFormula$`Team Work`,
    ReportFormula$`Flexibility&Adaptability`,
    ReportFormula$`Relationship Management`,
    ReportFormula$Empathy,
    ReportFormula$Communication
    
  )
  # i = 9 # chris
  a <- c(order(suc[1,])[i],order(suc[2,])[i],order(suc[3,])[i],order(suc[4,])[i],order(suc[5,])[i],order(suc[6,])[i],order(suc[7,])[i],order(suc[8,])[i])
  b <- ncol(suc)
  suc <- cbind(suc[,i])
  
  rownames(suc) <- c("Conflict Resolution","Negotiation","Coping with Pressure","Team Work","Flexibility & Adaptability","Relationship Management","Empathy","Communication")
  colnames(suc) <- "score"
  
  theme_set(theme_bw())  
  suc <- as.data.frame(suc)
  # Data Prep
  suc$name <- rownames(suc)
  # re$score[which(re$score<4)] <- -re$score[which(re$score<4)]
  set.seed(1000+i)
  suc$score <- (suc$score /7)*100 # + rnorm(7)
  # 随机扰动
  # suc$score <- qnorm((suc$score /7))*100
  # re$score <- (a/b)*100
  
  suc$per_type <- ifelse(suc$score < c(53,55,53,54,65,53,53,53), "below average", "above average")  # above / below avg flag
  
  suc$name <- factor(suc$name, levels = suc$name)  # convert to factor to retain sorted order in plot.
  
  # Diverging Barcharts
  options(decimal = 2)
  ggplot(suc, aes(x=name, y=score, label=score)) + 
    geom_bar(stat='identity', aes(fill=per_type), width=.5)  +
    scale_fill_manual(name="7 Key Success Characters",
                      labels = c("Above Average", "Below Average"),
                      values = c("above average"="#014d64", "below average"="#6794a7")) +
    labs(x="",
         subtitle = rawsurvey[,2][i]) + 
    coord_flip()+
    ylim(c(0,100))+
    geom_text(aes(label=round(score,1)))
  ggsave(do.call(str_c,lapply(c("./figures/8KeyEQSkills/",rawsurvey[,2][i],"_8KeyEQSkills_",i,".png"),function(x){
    str_replace_na(x,"")
  })))
  })


# 内部分析 --------------------------------------------------------------------
# Distribution: e.g. confidence in art

g <- ggplot(ReportFormula,aes(x=SVocConfidenceArt))+
  geom_bar()+
  geom_vline(aes(xintercept=ReportFormula$SVocConfidenceArt[2]),col = "red")
g

g <- ggplot(ReportFormula,aes(x=SAgreeableness))+
  geom_bar()+
  geom_vline(aes(xintercept=ReportFormula$SAgreeableness[2]),col = "red")
g


dev.off()
# two dimensions
theme_set(theme_classic())
g1 <- ggplot(ReportFormula,aes(x=Artconfidence,y=Artinterest))+
  geom_jitter()+
  # labs(title="Career Selection")+
  geom_vline(aes(xintercept=0),col="red")+
  geom_hline(aes(yintercept=0),col="red")+
  labs(x = "Art Confidence",
       y = "Art Interest")+
  geom_point(aes(x=Artconfidence[2],y=Artinterest[2]),col="red",size = 5)
g1

g2 <- ggplot(ReportFormula,aes(x=Reaconfidence,y=Reainterest))+
  geom_jitter()+
  geom_vline(aes(xintercept=0),col="red")+
  geom_hline(aes(yintercept=0),col="red")+
  geom_point(aes(x=Reaconfidence[2],y=Reainterest[2]),col="red",size = 5)
g2


g3 <- ggplot(ReportFormula,aes(x=Socconfidence,y=Socinterest))+
  geom_jitter()+
  geom_vline(aes(xintercept=0),col="red")+
  geom_hline(aes(yintercept=0),col="red")+
  geom_point(aes(x=Socconfidence[2],y=Socinterest[2]),col="red",size = 5)
g3

g4 <- ggplot(ReportFormula,aes(x=Conconfidence,y=Coninterest))+
  geom_jitter()+
  geom_vline(aes(xintercept=0),col="red")+
  geom_hline(aes(yintercept=0),col="red")+
  geom_point(aes(x=Conconfidence[2],y=Coninterest[2]),col="red",size = 5)
g4


g5 <- ggplot(ReportFormula,aes(x=Entconfidence,y=Entinterest))+
  geom_jitter()+
  geom_vline(aes(xintercept=0),col="red")+
  geom_hline(aes(yintercept=0),col="red")+
  geom_point(aes(x=Entconfidence[2],y=Entinterest[2]),col="red",size = 5)
g5


g6 <- ggplot(ReportFormula,aes(x=Invconfidence,y=Invinterest))+
  geom_jitter()+
  geom_vline(aes(xintercept=0),col="red")+
  geom_hline(aes(yintercept=0),col="red")+
  geom_point(aes(x=Invconfidence[2],y=Invinterest[2]),col="red",size = 5)
g6


gg <- multiplot(g1,g2,g3,g4,g5,g6,cols=2)
# export the result (test version)
ggsave("./figures/report1.png",plot=multiplot(g1,g2,g3,g4,g5,g6,cols=2))




# Skill list --------------------------------------------------------------
# sort by job scores 


# Career Report: for analysis -----------------------------------------------------------
# six aspects

g1 <- ggplot(ReportFormula,aes(x=SVocConfidenceArt,y=SVocInterestArt))+
  geom_jitter()+
  geom_vline(aes(xintercept=4),col="red")+
  geom_hline(aes(yintercept=4),col="red")+
  geom_point(aes(x=SVocConfidenceArt[2],y=SVocInterestArt[2]),col="red",size = 5)
g2 <- ggplot(ReportFormula,aes(x=SVocConfidenceCon,y=SVocInterestCon))+
  geom_jitter()+
  geom_vline(aes(xintercept=4),col="red")+
  geom_hline(aes(yintercept=4),col="red")+
  geom_point(aes(x=SVocConfidenceCon[2],y=SVocInterestCon[2]),col="red",size = 5)
g3 <- ggplot(ReportFormula,aes(x=SVocConfidenceEnt,y=SVocInterestEnt))+
  geom_jitter()+
  geom_vline(aes(xintercept=4),col="red")+
  geom_hline(aes(yintercept=4),col="red")+
  geom_point(aes(x=SVocConfidenceEnt[2],y=SVocInterestEnt[2]),col="red",size = 5)
g4 <- ggplot(ReportFormula,aes(x=SVocConfidenceInv,y=SVocInterestInv))+
  geom_jitter()+
  geom_vline(aes(xintercept=4),col="red")+
  geom_hline(aes(yintercept=4),col="red")+
  geom_point(aes(x=SVocConfidenceInv[2],y=SVocInterestInv[2]),col="red",size = 5)
g5 <- ggplot(ReportFormula,aes(x=SVocConfidenceRea,y=SVocInterestRea))+
  geom_jitter()+
  geom_vline(aes(xintercept=4),col="red")+
  geom_hline(aes(yintercept=4),col="red")+
  geom_point(aes(x=SVocConfidenceRea[2],y=SVocInterestRea[2]),col="red",size = 5)
g6 <- ggplot(ReportFormula,aes(x=SVocConfidenceSoc,y=SVocInterestSoc))+
  geom_jitter()+
  geom_vline(aes(xintercept=4),col="red")+
  geom_hline(aes(yintercept=4),col="red")+
  geom_point(aes(x=SVocConfidenceSoc[2],y=SVocInterestSoc[2]),col="red",size = 5)

multiplot(g1,g2,g3,g4,g5,g6,cols=2)
# export the result (test version)
ggsave("./figures/report_career2.png",plot=multiplot(g1,g2,g3,g4,g5,g6,cols=2))

# check ranges of these results
range(ReportFormula$SVocConfidenceArt)

# **********************career挑选的指标：可以进行更换，更换后，下面对应的名字也需要更换
career <- cbind(-1,ReportFormula$SVocInterestArt,ReportFormula$SVocInterestCon,
                ReportFormula$SVocInterestEnt,ReportFormula$SVocInterestInv,
                ReportFormula$SVocInterestRea,ReportFormula$SVocInterestSoc,
                ReportFormula$SVocConfidenceArt,ReportFormula$SVocConfidenceCon,
                ReportFormula$SVocConfidenceEnt,ReportFormula$SVocConfidenceInv,
                ReportFormula$SVocConfidenceRea,ReportFormula$SVocConfidenceSoc)

# reorder the results
ReportFormula$career <- apply(career,1,function(x){
  names(x) <- c("","A","C","E","I","R","S","Acon","Ccon","Econ","Icon","Rcon","Scon")
  reorder <- order(x[c("A","C","E","I","R","S")],decreasing = TRUE)
  x <- c(x[1],x[c("A","C","E","I","R","S")][reorder],x[c("Acon","Ccon","Econ","Icon","Rcon","Scon")][reorder])
  career_list <- c("",names(x)[2:7])
  str_sub(str_c(career_list[1*as.integer(x[2]>4&x[8]>4)+1],
                career_list[2*as.integer(x[3]>4&x[9]>4)+1],
                career_list[3*as.integer(x[4]>4&x[10]>4)+1],
                career_list[4*as.integer(x[5]>4&x[11]>4)+1],
                career_list[5*as.integer(x[6]>4&x[12]>4)+1],
                career_list[6*as.integer(x[7]>4&x[13]>4)+1]),1,3)
})


# 放宽confidence条件
ReportFormula$career[str_count(ReportFormula$career)<=2] <- apply(career[str_count(ReportFormula$career)<=2,],1,function(x){
  names(x) <- c("","A","C","E","I","R","S","Acon","Ccon","Econ","Icon","Rcon","Scon")
  reorder <- order(x[c("A","C","E","I","R","S")],decreasing = TRUE)
  x <- c(x[1],x[c("A","C","E","I","R","S")][reorder],x[c("Acon","Ccon","Econ","Icon","Rcon","Scon")][reorder])
  career_list <- c("",names(x)[2:7])
  str_sub(str_c(career_list[1*as.integer(x[2]>=4&x[8]>=3.5)+1],
                career_list[2*as.integer(x[3]>=4&x[9]>=3.5)+1],
                career_list[3*as.integer(x[4]>=4&x[10]>=3.5)+1],
                career_list[4*as.integer(x[5]>=4&x[11]>=3.5)+1],
                career_list[5*as.integer(x[6]>=4&x[12]>=3.5)+1],
                career_list[6*as.integer(x[7]>=4&x[13]>=3.5)+1]),1,3)
})

# 需要继续放宽条件来看！！！！！！！！！！！！
# 放宽条件. 继续放宽条件
ReportFormula$career[str_count(ReportFormula$career)<=2] <- apply(career[str_count(ReportFormula$career)<=2,],1,function(x){
  names(x) <- c("","A","C","E","I","R","S","Acon","Ccon","Econ","Icon","Rcon","Scon")
  reorder <- order(x[c("A","C","E","I","R","S")],decreasing = TRUE)
  x <- c(x[1],x[c("A","C","E","I","R","S")][reorder],x[c("Acon","Ccon","Econ","Icon","Rcon","Scon")][reorder])
  career_list <- c("",names(x)[2:7])
  str_sub(str_c(career_list[1*as.integer(x[2]>=3.5&x[8]>=3.5)+1],
                career_list[2*as.integer(x[3]>=3.5&x[9]>=3.5)+1],
                career_list[3*as.integer(x[4]>=3.5&x[10]>=3.5)+1],
                career_list[4*as.integer(x[5]>=3.5&x[11]>=3.5)+1],
                career_list[5*as.integer(x[6]>=3.5&x[12]>=3.5)+1],
                career_list[6*as.integer(x[7]>=3.5&x[13]>=3.5)+1]),1,3)
})


# 用big five来调整如果全部一样的：但是需要重新设置criteria, criteria来源于
career_bigfive <- cbind(-1,
  ReportFormula$SOpenness,
  ReportFormula$SConscientiousness,
  ReportFormula$SExtraversion,
  ReportFormula$SAgreeableness,
  ReportFormula$`SEmotional Stability`)

ReportFormula$career[str_count(ReportFormula$career)<=2] <- apply(career_bigfive[str_count(ReportFormula$career)<=2,],1,function(x){
  x <- x/7*100
  career_list <- c("","R","I","A",'S','E','C')

  str_sub(str_c(career_list[1*as.integer(x[3]/7*100>=78.57&x[4]/7*100>=42.86)+1],
                career_list[2*as.integer(x[2]/7*100>=70&x[6]/7*100>=45)+1],
                career_list[3*as.integer(x[1]/7*100>=75)+1],
                career_list[4*as.integer(x[2]/7*100>=70.24&x[4]/7*100>=78&x[5]/7*100>=73)+1],
                career_list[5*as.integer(x[3]/7*100>=75&x[4]/7*100>=75&x[6]/7*100>=75)+1],
                career_list[6*as.integer(x[3]/7*100>=70.54&x[6]/7*100>=45)+1]),1,3)
})

# check if everyone has been given a type
sum(is.na(str_count(ReportFormula$career)==3))==0

# 确定工作type
# matching careers from career list ---------------------------------------
career_data <- read.csv("./workingdata/report/skill_list/career_index_6categories.csv") %>%
  .[,2:4] %>%
  as.data.table %>%
  .[order(description,importance.score,decreasing = TRUE)]
career_data$description <- as.character(career_data$description)
career_data$job.title <- as.character(career_data$job.title)

# Sorting career index ----------------------------------------------------
# for the type EI
# 最后排出来的顺序是工作，最后得到的结果是工作
# create a score indicating the total score of one student's type(sum up by group)
# 把所有职业只留下学生呈现出来的类型

# test lines for recommendation possibilities
career_data$job.title[is.na(str_match(str_sub(career_data$description,1,1),paste0("^",str_sub(ReportFormula$career[1],1,1))))==FALSE][1:15]
career_data$importance.score[is.na(str_match(str_sub(career_data$description,1,1),paste0("^",str_sub(ReportFormula$career[1],1,1))))==FALSE][1:15]
career_data$description[is.na(str_match(str_sub(career_data$description,1,1),paste0("^",str_sub(ReportFormula$career[1],1,1))))==FALSE][1:15]

career_data[which(is.na(str_match(career_data$description,paste0("^",str_sub(ReportFormula$career[1],1,1))))==FALSE)]


# sorting career_data, sum importance score up and select the top 15 careers for each person
career_list_top15 <- sapply(ReportFormula$career,function(i){
  if(str_count(i)>0){
    career_data <- as.data.frame(career_data)
    career_data_sub <- do.call(rbind,lapply(str_split(i,"")[[1]],function(x){
      # 开头字母匹配
      career_data <- as.data.frame(career_data)
      career_data[which(is.na(str_match(career_data$description,paste0("^",x)))==FALSE),] %>%
        as.data.table
    })) %>%
      .[,sum(importance.score),c("job.title")] %>%
      `colnames<-`(c("job","score")) %>%
      .[order(score,decreasing = TRUE)]%>%
      .[,job]%>%
      .[1:15]}
  else{
    # NA
    rep(NA,15)
  }
})


# Add the percentage score of each career type recommended ----------------

percentage_score <- sapply(ReportFormula$career,function(i){
  if(str_count(i)>0){
    career_data <- as.data.frame(career_data)
    career_data_sub <- do.call(rbind,lapply(str_split(i,"")[[1]],function(x){
      # 开头匹配
      career_data <- as.data.frame(career_data)
      career_data[which(is.na(str_match(career_data$description,paste0("^",x)))==FALSE),] %>%
        as.data.table
    })) %>%
      .[,mean(importance.score),c("job.title")] %>%
      `colnames<-`(c("job","score")) %>%
      .[order(score,decreasing = TRUE)]%>%
      # .[,job]%>%
      .[,score]%>%
      .[1:15]}
  else{
    # NA
    rep(NA,15)
  }
})
colnames(percentage_score)<-(paste0("student",1:nrow(rawsurvey)))
rownames(percentage_score)<-(paste0("job",1:15))

options(decimal = 2)
write.csv(percentage_score,"./workingdata/report/skill_list/percentage_score.csv",dec = 2)

# career list_top 15 jobs without separate scores -------------------------
colnames(career_list_top15) <- c()
career_list_top15 <- as.data.frame(t(career_list_top15)) # every person is a row
colnames(career_list_top15) <- paste0("job",1:15)
career_list_top15 <- cbind(ReportFormula$career,career_list_top15)
colnames(career_list_top15) <- c("Job Type",paste0("Job",1:15))
career_list_top15 <- t(career_list_top15)
colnames(career_list_top15) <- paste0("Student",1:nrow(rawsurvey))
rownames(career_list_top15) <- c("Job Type",paste0("Job",1:15))
career_list_top15 <- rbind(rawsurvey[,2],career_list_top15)
career_list_top15[2,] <- str_sub(career_list_top15[2,],1,2)
write.csv(career_list_top15,"./workingdata/report/skill_list/career_top15.csv")


# ReportFormula$career is the matched job type of one student

career_list_top15 <- rbind(career_list_top15,do.call(cbind,lapply(ReportFormula$career,function(i){
  if(str_count(i)>0){
    var <- rep("",7)
    content <- sapply(str_split(i,"")[[1]],function(x){
      step1 <- career_data$description[which(is.na(str_match(career_data$description,paste0("^",x)))==FALSE)]
      step2 <- step1[which(duplicated(step1)==FALSE)]
      step2})
    
    var[2:(length(content)+1)] <- content
    var
  }
  else{
    rep("",7)
    # NA
  }
}) ))

write.csv(career_list_top15,"./workingdata/report/skill_list/career_top15.csv")


# 找对应工作的importance score: 暂时不需要了
# career_data_wide <- reshape(career_data,direction = "wide",idvar="job.title",timevar="description") %>%
#   as.data.table
# 
# importance_score <- do.call(rbind,lapply(1:nrow(survey),function(i){
#   loc <- match(career_list_top15[2:16,1],career_data_wide$job.title)
#   content <- career_data_wide[loc,2:7]
#   
# })) %>%
#   `colnames<-`(c("S","R","I","E","C","A"))
# write.csv(importance_score,"./workingdata/report/skill_list/importance_score.csv")
# 
# 



# 两个icon, 把big five的得分计算出来
# work activities and work context -----------------------------------------------------------------

# work activities ---------------------------------------------------------
# 字母type: ReportFormula$career，用这个type来做索引
# 对应work type
workactivities <- read_xlsx("./rawdata/report/Report Descriptions.xlsx",sheet = "recommended work activity") %>%
  as.data.frame

career_list_top15 <- rbind(career_list_top15,do.call(cbind,lapply(ReportFormula$career,function(i){
  if(str_count(i)>0){
    loc <- sapply(str_split(i,"")[[1]],function(x){
      which(is.na(str_match(workactivities[,1],x))==FALSE)
    })
    # needs to be 5 iterms
    c(workactivities[loc[[1]][1:5],2],workactivities[loc[[2]][1:5],2])
  }
  else{
    rep("",5)
  }
}) ))
write.csv(career_list_top15,"./workingdata/report/skill_list/career_top15.csv")


# work context in general -------------------------------------------------
# 对应于work type
career_bigfive <- career_bigfive[,-1]
workcontext <- read_xlsx("./rawdata/report/Report Descriptions.xlsx",sheet = "recommended work context") %>%
  as.data.frame

career_list_top15 <- rbind(career_list_top15,do.call(cbind,apply(career_bigfive,1,function(i){
  if(str_count(i)>0){
    loc <- sapply(str_split(i,"")[[1]],function(x){
      which(is.na(str_match(workactivities[,1],x))==FALSE)
    })
    # needs to be 5 iterms
    c(workactivities[loc[[1]][1:5],2],workactivities[loc[[2]][1:5],2])
  }
  else{
    rep("",5)
  }
}) ))
write.csv(career_list_top15,"./workingdata/report/skill_list/career_top15.csv")


do.call(cbind,apply(career_bigfive,1,function(i){
    typelist <- c("O","C","E","A","N")
    # 先根据分数来确定是哪个
    type <- str_sub(str_c(typelist[1*as.integer(x[3]/7*100>=78.57&x[4]/7*100>=42.86)+1],
                          typelist[2*as.integer(x[2]/7*100>=70&x[6]/7*100>=45)+1],
                          typelist[3*as.integer(x[1]/7*100>=75)+1],
                          typelist[4*as.integer(x[2]/7*100>=70.24&x[4]/7*100>=78&x[5]/7*100>=73)+1],
                          typelist[5*as.integer(x[3]/7*100>=75&x[4]/7*100>=75&x[6]/7*100>=75)+1])
    loc <- sapply(str_split(type,"")[[1]],function(x){
      which(is.na(str_match(workactivities[,1],x))==FALSE)
    # needs to be 5 iterms
    c(workactivities[loc[[1]][1:5],2],workactivities[loc[[2]][1:5],2])

  })
  })




#career结束 ---------------------------

# career list 出一个industry的list，
# top industry
# big five准备出图





