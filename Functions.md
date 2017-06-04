---
title: "Functions"
author: "D&D"
date: "May 25, 2017"
output:  html_document
---

<p>
The data we used was collected by survey and input through a google form, </br>
because of the fact that it was entered as strings by several people we had to reenter ,</br>
some of it as well as define some of the columns, as either string or numeric in MS Excel. </br>
once loaded we filtered some errors created by MS Excel using ```filter()```.</br> 
and named the data:
</p>

```r
library(shiny)
library(shinythemes)
library(markdown)
library(knitr)
library(curl)
library(ggplot2)
library(dplyr)
library(leaflet)
library(reshape2)


#####
# loading and naming the files into data.frames
# setting the working directory is not neccesary, unless you want to check the data outside of shiny.
 # setwd("C:\\Users\\Dror Bogin\\Desktop\\University\\Geogeraphy\\seminar_diana\\app_test")

socio <- read.csv("SOCIO.csv", header = TRUE, stringsAsFactors = TRUE)
physical <- read.csv("PHYSICAL.csv", header = TRUE, stringsAsFactors = TRUE)
physical <- select(physical,1:8)
pay <- read.csv("PAY_EILAT.csv", header = TRUE, stringsAsFactors = TRUE)
latlon <- read.csv("LATLON.csv", header = TRUE, stringsAsFactors = TRUE)

names(socio) <- c("ID", "num_res", "subten", "sex_1","age_1", "edu_1", "work_1","sex_2","age_2", "edu_2", "work_2", "kids_num","kids_age", "income", "notes")
names(physical) <- c("ID","glaz", "shutter", "hou_size","hea", "mon_hea", "dur_hea", "hou_type")
names(pay) <- c("ID","AM_FIRST_PAY","START_FIRST_PAY","END_FIRST_PAY","AM_SEC_PAY","START_SEC_PAY","END_SEC_PAY","AM_THIRD_PAY","START_THIRD_PAY","END_THIRD_PAY")
names(latlon) <- c("ID", "Address", "LAT","LON")
```
<p>
We then calculated the elecricty consumption per household by the ammount they payed, </br>
this was done using Dplyr's mutate function while considering each possible date for</br>
electric bill as the fixed payment changes between months while the variable payment is</br>
allways an added 17%, here is and example for one of the consumption in KW columns created. </br>
a total of three such columns were created:
</p>

```r
pay <- mutate(pay, kw1 = if_else(pay$START_FIRST_PAY == "2016-07-01", (((pay$AM_FIRST_PAY - 26.89)-((pay$AM_FIRST_PAY*100/117)*0.17))/0.4558),if_else(pay$START_FIRST_PAY == "2016-09-01", (((pay$AM_FIRST_PAY - 25.07)-((pay$AM_FIRST_PAY*100/117)*0.17))/0.4558),if_else(pay$START_FIRST_PAY == "2016-11-01", (((pay$AM_FIRST_PAY - 26.43)-((pay$AM_FIRST_PAY*100/117)*0.17))/0.4558),if_else(pay$START_FIRST_PAY == "2017-01-01", (((pay$AM_FIRST_PAY - 31.81)-((pay$AM_FIRST_PAY*100/117)*0.17))/0.4726),0)))))
```
<p>
Next we wanted to know whih months each household used their electric water heater. </br>
for that we created a data frame with month names, and made it as long as the physical table (which could have gotten longer). </br>
in order to populate that data frame we created another one into which </br>
we used ```colsplit()``` on the mon_hea column which held the months heated in number format. </br>

</p>
```r
input <- physical
months <- month.name

mondata <- data.frame(matrix(nrow = length(input$ID), ncol = 13))
colnames(mondata) <- c("ID",months)
mondata$ID <- seq(1,length(input$ID))
ID <- 1:length(input$ID)
input1 <- data.frame(ID)
input1[,2:13] <- colsplit(input$mon_hea,",", names = c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12"))

```
We then populated the mondata data frame using an ```ifelse()``` statement checking the value of </br>
the corresponding cell in the input1 data frame. </br>
the statement checks for the existence of the selected month in each of the cells within that ID row. </br>
it does so by comparing it to a <a href ="http://www.regular-expressions.info/">*REGEX*</a> expression using the ```base::grepl()``` function, </br>
to check if it contains that specific month. </br>
we have run 12 variations on that statement, 1 for each month. </br>
</br>
example of the statement:
</p>
```r
mondata[,2] <- ifelse(grepl("^1$",input1$X1,ignore.case = TRUE)|grepl("^1$",input1$X2,ignore.case = TRUE)|grepl("^1$",input1$X3,ignore.case = TRUE)|grepl("^1$",input1$X4,ignore.case = TRUE)|grepl("^1$",input1$X5,ignore.case = TRUE)|grepl("^1$",input1$X6,ignore.case = TRUE)|grepl("^1$",input1$X7,ignore.case = TRUE)|grepl("^1$",input1$X8,ignore.case = TRUE)|grepl("^1$",input1$X9,ignore.case = TRUE)|grepl("^1$",input1$X10,ignore.case = TRUE)|grepl("^1$",input1$X11,ignore.case = TRUE)|grepl("^1$",input1$X12,ignore.case = TRUE),1,0)

```

<p>
once we had all of the data in the original tables ready for use we ordered them, </br>
and joined them one by one into one data frame:
</p>
```r
pay <- pay[order(pay$ID),]
socio <- socio[order(socio$ID),]
physical <- physical[order(physical$ID),]
latlon <- latlon[order(latlon$ID),]
eilat <- left_join(pay,socio, by = c("ID" = "ID"))
eilat <- left_join(eilat, physical, by = c("ID" = "ID"))
eilat <- left_join(eilat, mondata, by = c("ID" = "ID"))
eilat <- left_join(eilat, latlon, by = c("ID" = "ID"))
```

<p>
we then created 5 more columns using dplyr's ```mutate()``` function, </br>
first an ordered factor for the income level, </br>
then 3 other columns calculating the electricity consumption (in kw) per capita, </br>
and last a non ordered factor for house type.
</p>

```r

eilat <- mutate(eilat, incomelevel = if_else(income == "0-5000", "low", if_else(income == "5000-10000", "mid-low", if_else(income == "10000-15000","mid" ,if_else(income == "15000-20000","mid-high", if_else(income == "20000+", "high", "NA"))))))
eilat$incomelevel <- factor(eilat$incomelevel, levels = c(NA,"low", "mid-low", "mid", "mid-high","high"), ordered = TRUE)

eilat <- mutate(eilat, ConsumpPerCap1 = kw1/num_res)
eilat <- mutate(eilat, ConsumpPerCap2 = kw2/num_res)
eilat <- mutate(eilat, ConsumpPerCap3 = kw3/num_res)
eilat$hou_type <- factor(eilat$hou_type, labels = c("private","apartement"))

```

<p>
after that we used ```filter()``` again to get clean and subseted data frames </br> we used thos in the interactive graphs in the application. </br>
</p>

```r
lev <- levels(eilat$START_FIRST_PAY)[2:6]
new_dataclean <- filter(eilat, eilat$START_FIRST_PAY %in% lev )
eilatjanfeb <- filter(new_dataclean,new_dataclean$START_FIRST_PAY == "2017-01-01")
eilatnovdec <- filter(new_dataclean,new_dataclean$START_FIRST_PAY == "2016-11-01")
eilatsepoc <- filter(new_dataclean,new_dataclean$START_FIRST_PAY == "2016-09-01")
eilatjulaug <- filter(new_dataclean,new_dataclean$START_FIRST_PAY == "2016-07-01")
eilatmayjun <- filter(new_dataclean,new_dataclean$START_FIRST_PAY == "2016-05-01")
lev2 <- paste(levels(eilat$incomelevel[!is.na(eilat$incomelevel)])," ", table(eilat$incomelevel[!is.na(eilat$incomelevel)]) )

cleandates <- rev(as.character(lev))[1:5]
cleandates <- as.Date(cleandates)
```
 <p>
 and right before creating the <b>ShinyApp</b> we created several linear models, </br>
 those were used to understand the way some variables effect electricity consumption.</br>
 some are basic linear models while use more than one variable. </br>
 those presented were selected because they show behaviour similliar to what </br>
 was found in the literature review. </br>
</p>

```r
lm1 <- lm(data = eilatjanfeb, formula = num_res~kw1, na.action = na.omit)
summary(lm1)
lm2 <- lm(data = eilatjanfeb, formula = num_res~ConsumpPerCap1, na.action = na.omit)
summary(lm2)
lm3 <- lm(data = eilatjanfeb, formula = kw1~hou_size*incomelevel, na.action = na.omit)
summary(lm3)
lm4 <- lm(data = eilatjanfeb, formula = kw1~hou_size*incomelevel*age_1, na.action = na.omit)
summary(lm4)
lm5 <- lm(data = eilatjanfeb, formula = kw1~hou_size*incomelevel*age_1*hou_type, na.action = na.omit)
summary(lm5)
lm6 <- lm(data = eilatjanfeb, formula = kw1~hou_size, na.action = na.omit)
summary(lm6)
lm7 <- lm(data = eilatjanfeb, formula = ConsumpPerCap1~hou_size, na.action = na.omit)
summary(lm7)
```
<p>
the rest of the script was used to create the <b>ShinyApp</b> and is included in slide 7. </br>
it includes the plots you will see in slides 3-5, the map in slide 5, </br>
and the connection to the <b>Rmarkdown</b> files which were used to create the text. </br>
</p>
</br>










































