
# Summary for Web-scraping.

1. This is the R markdown file for web_scraping.
2. It requires the Google Chrome Browser and Selector Gadget.
3. The instruction for using Selector Gadget is in the code file.
4. It has limits that it cannot scrape the websites simultaneously from multiple addresses. 
5. The output file is in the form of .CSV.
6. A sample result is attached for reference, named as "output_file_name.csv".


- Reference for using Selector Gadget in R: [Selector Gadget in R](https://cran.r-project.org/web/packages/rvest/vignettes/selectorgadget.html)
- Reference for installing and using Selector Gadget in Google Chrome: [Selector Gadget in Chrome](https://selectorgadget.com)

##Example:
### We scrap the undergraduate majors at UCLA from its admission website. 

- Open the R markdown file and run the required packages.

#![Packages](http://imgsrc.baidu.com/forum/w%3D580/sign=3e9cae27399b033b2c88fcd225cf3620/4e5a8058d109b3de7634574bc1bf6c81800a4c1a.jpg) 



- Put the website address as showned in the R markdown.

#![Address](http://imgsrc.baidu.com/forum/w%3D580/sign=f182d3e8bffb43161a1f7a7210a54642/57d6c7628535e5dd2bfe16fc7bc6a7efcf1b62d3.jpg) 

- Open Google Chrome, and click Selecor Gadget on the upper right corner. 



- Then, select the text expected to be scraped. Use command if you want to select several in one time.
- There will be phrased created as showned on the lower right corner.

#![GOOGLE](http://imgsrc.baidu.com/forum/w%3D580/sign=f015bfc8bbfd5266a72b3c1c9b199799/9207712762d0f703e05126ff05fa513d2797c5c2.jpg) 
- Copy all the phrases and paste them onto html_notes("phrases paste here") in R markdown file.

#![SELECTOR](http://imgsrc.baidu.com/forum/w%3D580/sign=c015d72e2b9759ee4a5060c382fa434e/2ec53d2eb9389b50872d53718835e5dde6116ed3.jpg) 
- Run the code and you get raw results from web; clean and process the raw results.
- Export the results in the form of a .csv file into the location you expect.
- Here is how the final result would be look like: 

#![Result](http://imgsrc.baidu.com/forum/w%3D580/sign=7c14c50560224f4a5799731b39f69044/9d21f7dde71190efb82921fcc31b9d16fcfa60d3.jpg) 
- Here is what the file looks like in local.

#![resultoverview](http://imgsrc.baidu.com/forum/w%3D580/sign=210407d834dbb6fd255be52e3925aba6/b1f89813632762d087dd179cadec08fa503dc6c2.jpg) 

--------------------
 - ### New updates from 9.7:
 
1. Parameters are included for the following place:
      - website address for the web you may want to scrape down;
      - web_nodes which controls specific sections you want;
      - variable names for the output file;
      - output address and output file name.
2. There is no need for changing the original code then for each run. 
3. Recommended to kniw with parameters where there will be a window for you to enter your parameter values. 
4. Refer [Run R markdown in Command Line](https://github.com/USA-UES/R-scripts/blob/master/Mapping/ReadME.md) for running in terminal.


