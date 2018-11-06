# Summary for String Mapping


 1. This is the R markdown file to match two sets of strings.
 2. In the sample code, we input two .CSV files each contains a list of questions. 
 3. The code helps to match the questions (strings) using match function. 
 4. This is only an approximate matching at rate about 80%, and the unmatched ones would be replaced by N/A.
 5. The order of your input files maters, because the algorithm behind is to replace one list of variables by another, therefore the length of variables in each file decides which to replace which. 
 6. There might be some redundancy because when mapping several exactly same strings, the code only reacts to first pairs and leave the next ones same as the first pair.
7. The output file would be a combined .CSV result of the two input files, while the strings used for mapping(matching) only appears in one column. (e.g file1 contains variables "id","questions"; file2 contains "variable_name","questions", when we try to match by "questions", the result would contain "id","variable_name", and "questions" for one time.)
8. New parameters included for input file addresses.
2. New parameters for variable names in each input file. 
3. New parameter for the new column created in the output file afrer finishing the mapping. 
4. New parameter for output file name. 
4. By using the parameters, we prevent from changeing the code each time we run it and make it more general for any two files we want to match based on certain variable. 
5. To run the code using parameter, click knit with parameters, and then enter the values. 
6. A resulted output file and knited file would be created in current working directory after each run. 
8. Manualy work needed at the ened. 
8. And a sample result is attached.  
9. Sample input files are also included.


---------------------------

### To use command line for running R scripts. 

1. The "String Mapping.Rmd" file could be ran in the terminal.
2. Open terminal and type: cd git/R-scripts/Mapping (to enter the working directory where the script is saved at.)
3. Call the R environmnet by typing: r. 
4. Type: getwd(), to check if the working directory is correct. 
5. Call library by typing: library(knitr).
6. Use knit function to knit run the code by typing: knit("~/git/R-scripts/Mapping\String Mapping.Rmd")
7. A markdown file would be creadted along with an output file in current working directory.
8. Type: quit(save = "no") to leave the R environment.
9. sample typing in terminal are attached below to guide: 

--------------------


```
Michael-bc11:~ michael$ cd git/R-scripts/Mapping
Michael-bc11:Mapping michael$ r

R version 3.5.1 (2018-07-02) -- "Feather Spray"
Copyright (C) 2018 The R Foundation for Statistical Computing
Platform: x86_64-apple-darwin15.6.0 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

```

```
[Previously saved workspace restored]

> getwd()
[1] "/Users/michael/git/R-scripts/Mapping"
> library(knitr)
> knit("/Users/michael/git/R-scripts/Mapping/String\ Mapping.Rmd")


processing file: /Users/michael/git/R-scripts/Mapping/String Mapping.Rmd
  |......                                                           |   9%
  ordinary text without R code

  |............                                                     |  18%
label: unnamed-chunk-1
  |..................                                               |  27%
  ordinary text without R code

  |........................                                         |  36%
label: unnamed-chunk-2
  |..............................                                   |  45%
  ordinary text without R code

  |...................................                              |  55%
label: unnamed-chunk-3
  |.........................................                        |  64%
  ordinary text without R code

  |...............................................                  |  73%
label: unnamed-chunk-4
  |.....................................................            |  82%
  ordinary text without R code

  |...........................................................      |  91%
label: unnamed-chunk-5
  |.................................................................| 100%
  ordinary text without R code


output file: String Mapping.md

[1] "String Mapping.md"

```


