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
# #####
# calculating the electricty usage for each bill by using the payment made,
# we use mutate 3 times because the maximum number of electricty bills
# given by any person questioned were 3

pay <- mutate(pay, kw1 = if_else(pay$START_FIRST_PAY == "2016-07-01", (((pay$AM_FIRST_PAY - 26.89)-((pay$AM_FIRST_PAY*100/117)*0.17))/0.4558),if_else(pay$START_FIRST_PAY == "2016-09-01", (((pay$AM_FIRST_PAY - 25.07)-((pay$AM_FIRST_PAY*100/117)*0.17))/0.4558),if_else(pay$START_FIRST_PAY == "2016-11-01", (((pay$AM_FIRST_PAY - 26.43)-((pay$AM_FIRST_PAY*100/117)*0.17))/0.4558),if_else(pay$START_FIRST_PAY == "2017-01-01", (((pay$AM_FIRST_PAY - 31.81)-((pay$AM_FIRST_PAY*100/117)*0.17))/0.4726),0)))))
pay <- mutate(pay, kw2 = if_else(pay$START_SEC_PAY == "2016-07-01", (((pay$AM_SEC_PAY - 26.89)-((pay$AM_SEC_PAY*100/117)*0.17))/0.4558),if_else(pay$START_SEC_PAY == "2016-09-01", (((pay$AM_SEC_PAY - 25.07)-((pay$AM_SEC_PAY*100/117)*0.17))/0.4558),if_else(pay$START_SEC_PAY == "2016-11-01", (((pay$AM_SEC_PAY - 26.43)-((pay$AM_SEC_PAY*100/117)*0.17))/0.4558),if_else(pay$START_SEC_PAY == "2017-01-01", (((pay$AM_SEC_PAY - 31.81)-((pay$AM_SEC_PAY*100/117)*0.17))/0.4726),0)))))
pay <- mutate(pay, kw3 = if_else(pay$START_THIRD_PAY == "2016-03-01", (((pay$AM_THIRD_PAY - 26.89)-((pay$AM_THIRD_PAY*100/117)*0.17))/0.4558),if_else(pay$START_THIRD_PAY == "2016-09-01", (((pay$AM_THIRD_PAY - 25.07)-((pay$AM_THIRD_PAY*100/117)*0.17))/0.4558),if_else(pay$START_THIRD_PAY == "2016-11-01", (((pay$AM_THIRD_PAY - 26.43)-((pay$AM_THIRD_PAY*100/117)*0.17))/0.4558),if_else(pay$START_THIRD_PAY == "2017-01-01", (((pay$AM_THIRD_PAY - 31.81)-((pay$AM_THIRD_PAY*100/117)*0.17))/0.4726),if_else(pay$START_THIRD_PAY == "2016-05-01", (((pay$AM_THIRD_PAY - 25.52)-((pay$AM_THIRD_PAY*100/117)*0.17))/0.4558),0))))))

# creating an index table to use fill with the months each household heated their water

input <- physical
months <- month.name

mondata <- data.frame(matrix(nrow = length(input$ID), ncol = 13))
colnames(mondata) <- c("ID",months)
mondata$ID <- seq(1,length(input$ID))
ID <- 1:length(input$ID)
input1 <- data.frame(ID)
input1[,2:13] <- colsplit(input$mon_hea,",", names = c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12"))


# filling the index table by testing the months heated dates with REGEX exppressions

mondata[,2] <- ifelse(grepl("^1$",input1$X1,ignore.case = TRUE)|grepl("^1$",input1$X2,ignore.case = TRUE)|grepl("^1$",input1$X3,ignore.case = TRUE)|grepl("^1$",input1$X4,ignore.case = TRUE)|grepl("^1$",input1$X5,ignore.case = TRUE)|grepl("^1$",input1$X6,ignore.case = TRUE)|grepl("^1$",input1$X7,ignore.case = TRUE)|grepl("^1$",input1$X8,ignore.case = TRUE)|grepl("^1$",input1$X9,ignore.case = TRUE)|grepl("^1$",input1$X10,ignore.case = TRUE)|grepl("^1$",input1$X11,ignore.case = TRUE)|grepl("^1$",input1$X12,ignore.case = TRUE),1,0)
mondata[,3] <- ifelse(grepl("^2$",input1$X1,ignore.case = TRUE)|grepl("^2$",input1$X2,ignore.case = TRUE)|grepl("^2$",input1$X3,ignore.case = TRUE)|grepl("^2$",input1$X4,ignore.case = TRUE)|grepl("^2$",input1$X5,ignore.case = TRUE)|grepl("^2$",input1$X6,ignore.case = TRUE)|grepl("^2$",input1$X7,ignore.case = TRUE)|grepl("^2$",input1$X8,ignore.case = TRUE)|grepl("^2$",input1$X9,ignore.case = TRUE)|grepl("^2$",input1$X10,ignore.case = TRUE)|grepl("^2$",input1$X11,ignore.case = TRUE)|grepl("^2$",input1$X12,ignore.case = TRUE),1,0)
mondata[,4] <- ifelse(grepl("^3$",input1$X1,ignore.case = TRUE)|grepl("^3$",input1$X2,ignore.case = TRUE)|grepl("^3$",input1$X3,ignore.case = TRUE)|grepl("^3$",input1$X4,ignore.case = TRUE)|grepl("^3$",input1$X5,ignore.case = TRUE)|grepl("^3$",input1$X6,ignore.case = TRUE)|grepl("^3$",input1$X7,ignore.case = TRUE)|grepl("^3$",input1$X8,ignore.case = TRUE)|grepl("^3$",input1$X9,ignore.case = TRUE)|grepl("^3$",input1$X10,ignore.case = TRUE)|grepl("^3$",input1$X11,ignore.case = TRUE)|grepl("^3$",input1$X12,ignore.case = TRUE),1,0)
mondata[,5] <- ifelse(grepl("^4$",input1$X1,ignore.case = TRUE)|grepl("^4$",input1$X2,ignore.case = TRUE)|grepl("^4$",input1$X3,ignore.case = TRUE)|grepl("^4$",input1$X4,ignore.case = TRUE)|grepl("^4$",input1$X5,ignore.case = TRUE)|grepl("^4$",input1$X6,ignore.case = TRUE)|grepl("^4$",input1$X7,ignore.case = TRUE)|grepl("^4$",input1$X8,ignore.case = TRUE)|grepl("^4$",input1$X9,ignore.case = TRUE)|grepl("^4$",input1$X10,ignore.case = TRUE)|grepl("^4$",input1$X11,ignore.case = TRUE)|grepl("^4$",input1$X12,ignore.case = TRUE),1,0)
mondata[,6] <- ifelse(grepl("^5$",input1$X1,ignore.case = TRUE)|grepl("^5$",input1$X2,ignore.case = TRUE)|grepl("^5$",input1$X3,ignore.case = TRUE)|grepl("^5$",input1$X4,ignore.case = TRUE)|grepl("^5$",input1$X5,ignore.case = TRUE)|grepl("^5$",input1$X6,ignore.case = TRUE)|grepl("^5$",input1$X7,ignore.case = TRUE)|grepl("^5$",input1$X8,ignore.case = TRUE)|grepl("^5$",input1$X9,ignore.case = TRUE)|grepl("^5$",input1$X10,ignore.case = TRUE)|grepl("^5$",input1$X11,ignore.case = TRUE)|grepl("^5$",input1$X12,ignore.case = TRUE),1,0)
mondata[,7] <- ifelse(grepl("^6$",input1$X1,ignore.case = TRUE)|grepl("^6$",input1$X2,ignore.case = TRUE)|grepl("^6$",input1$X3,ignore.case = TRUE)|grepl("^6$",input1$X4,ignore.case = TRUE)|grepl("^6$",input1$X5,ignore.case = TRUE)|grepl("^6$",input1$X6,ignore.case = TRUE)|grepl("^6$",input1$X7,ignore.case = TRUE)|grepl("^6$",input1$X8,ignore.case = TRUE)|grepl("^6$",input1$X9,ignore.case = TRUE)|grepl("^6$",input1$X10,ignore.case = TRUE)|grepl("^6$",input1$X11,ignore.case = TRUE)|grepl("^6$",input1$X12,ignore.case = TRUE),1,0)
mondata[,8] <- ifelse(grepl("^7$",input1$X1,ignore.case = TRUE)|grepl("^7$",input1$X2,ignore.case = TRUE)|grepl("^7$",input1$X3,ignore.case = TRUE)|grepl("^7$",input1$X4,ignore.case = TRUE)|grepl("^7$",input1$X5,ignore.case = TRUE)|grepl("^7$",input1$X6,ignore.case = TRUE)|grepl("^7$",input1$X7,ignore.case = TRUE)|grepl("^7$",input1$X8,ignore.case = TRUE)|grepl("^7$",input1$X9,ignore.case = TRUE)|grepl("^7$",input1$X10,ignore.case = TRUE)|grepl("^7$",input1$X11,ignore.case = TRUE)|grepl("^7$",input1$X12,ignore.case = TRUE),1,0)
mondata[,9] <- ifelse(grepl("^8$",input1$X1,ignore.case = TRUE)|grepl("^8$",input1$X2,ignore.case = TRUE)|grepl("^8$",input1$X3,ignore.case = TRUE)|grepl("^8$",input1$X4,ignore.case = TRUE)|grepl("^8$",input1$X5,ignore.case = TRUE)|grepl("^8$",input1$X6,ignore.case = TRUE)|grepl("^8$",input1$X7,ignore.case = TRUE)|grepl("^8$",input1$X8,ignore.case = TRUE)|grepl("^8$",input1$X9,ignore.case = TRUE)|grepl("^8$",input1$X10,ignore.case = TRUE)|grepl("^8$",input1$X11,ignore.case = TRUE)|grepl("^8$",input1$X12,ignore.case = TRUE),1,0)
mondata[,10] <- ifelse(grepl("^9$",input1$X1,ignore.case = TRUE)|grepl("^9$",input1$X2,ignore.case = TRUE)|grepl("^9$",input1$X3,ignore.case = TRUE)|grepl("^9$",input1$X4,ignore.case = TRUE)|grepl("^9$",input1$X5,ignore.case = TRUE)|grepl("^9$",input1$X6,ignore.case = TRUE)|grepl("^9$",input1$X7,ignore.case = TRUE)|grepl("^9$",input1$X8,ignore.case = TRUE)|grepl("^9$",input1$X9,ignore.case = TRUE)|grepl("^9$",input1$X10,ignore.case = TRUE)|grepl("^9$",input1$X11,ignore.case = TRUE)|grepl("^9$",input1$X12,ignore.case = TRUE),1,0)
mondata[,11] <- ifelse(grepl("^10$",input1$X1,ignore.case = TRUE)|grepl("^10$",input1$X2,ignore.case = TRUE)|grepl("^10$",input1$X3,ignore.case = TRUE)|grepl("^10$",input1$X4,ignore.case = TRUE)|grepl("^10$",input1$X5,ignore.case = TRUE)|grepl("^10$",input1$X6,ignore.case = TRUE)|grepl("^10$",input1$X7,ignore.case = TRUE)|grepl("^10$",input1$X8,ignore.case = TRUE)|grepl("^10$",input1$X9,ignore.case = TRUE)|grepl("^10$",input1$X10,ignore.case = TRUE)|grepl("^10$",input1$X11,ignore.case = TRUE)|grepl("^10$",input1$X12,ignore.case = TRUE),1,0)
mondata[,12] <- ifelse(grepl("^11$",input1$X1,ignore.case = TRUE)|grepl("^11$",input1$X2,ignore.case = TRUE)|grepl("^11$",input1$X3,ignore.case = TRUE)|grepl("^11$",input1$X4,ignore.case = TRUE)|grepl("^11$",input1$X5,ignore.case = TRUE)|grepl("^11$",input1$X6,ignore.case = TRUE)|grepl("^11$",input1$X7,ignore.case = TRUE)|grepl("^11$",input1$X8,ignore.case = TRUE)|grepl("^11$",input1$X9,ignore.case = TRUE)|grepl("^11$",input1$X10,ignore.case = TRUE)|grepl("^11$",input1$X11,ignore.case = TRUE)|grepl("^11$",input1$X12,ignore.case = TRUE),1,0)
mondata[,13] <- ifelse(grepl("^12$",input1$X1,ignore.case = TRUE)|grepl("^12$",input1$X2,ignore.case = TRUE)|grepl("^12$",input1$X3,ignore.case = TRUE)|grepl("^12$",input1$X4,ignore.case = TRUE)|grepl("^12$",input1$X5,ignore.case = TRUE)|grepl("^12$",input1$X6,ignore.case = TRUE)|grepl("^12$",input1$X7,ignore.case = TRUE)|grepl("^12$",input1$X8,ignore.case = TRUE)|grepl("^12$",input1$X9,ignore.case = TRUE)|grepl("^12$",input1$X10,ignore.case = TRUE)|grepl("^12$",input1$X11,ignore.case = TRUE)|grepl("^12$",input1$X12,ignore.case = TRUE),1,0)

# naming the columns for easy use further on


pay <- pay[order(pay$ID),]
socio <- socio[order(socio$ID),]
physical <- physical[order(physical$ID),]
latlon <- latlon[order(latlon$ID),]
eilat <- left_join(pay,socio, by = c("ID" = "ID"))
eilat <- left_join(eilat, physical, by = c("ID" = "ID"))
eilat <- left_join(eilat, mondata, by = c("ID" = "ID"))
eilat <- left_join(eilat, latlon, by = c("ID" = "ID"))

# classifying the income level into an ordered factor
eilat <- mutate(eilat, incomelevel = if_else(income == "0-5000", "low", if_else(income == "5000-10000", "mid-low", if_else(income == "10000-15000","mid" ,if_else(income == "15000-20000","mid-high", if_else(income == "20000+", "high", "NA"))))))
eilat$incomelevel <- factor(eilat$incomelevel, levels = c(NA,"low", "mid-low", "mid", "mid-high","high"), ordered = TRUE)

eilat <- mutate(eilat, ConsumpPerCap1 = kw1/num_res)
eilat <- mutate(eilat, ConsumpPerCap2 = kw2/num_res)
eilat <- mutate(eilat, ConsumpPerCap3 = kw3/num_res)
eilat$hou_type <- factor(eilat$hou_type, labels = c("private","apartement"))

# preparing filtered tables for use in the app,
# in the plots slide you can switch between the different dates

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



############## U.I ##############

# most of the presentation is in the Rmarkdown and .txt files so the u.i. seems a bit dull and empty.

ui= tagList(
  shinythemes::themeSelector(),
  navbarPage("Data Management Project D&D",
               tabPanel("Introduction - Slide 1", 
                        fluidRow(
                          column(8, includeMarkdown("intro.md"))
                        )
                        ),
             tabPanel("Methodology - Slide 2",
                      h2("Metodology"),
                      fluidRow(),
                      includeMarkdown("Functions.md")
                      
             ),
             tabPanel("Basic Statistics - Slide 3", 
                      h2("Basic Statistics"),
                      fluidRow(),
                      includeMarkdown("basic_stats.md"),
                      h3("Plots"),
                      fluidRow(column(8,verbatimTextOutput('text3.5')),
                      column(8,plotOutput('plot3'))),
                      fluidRow(column(6,verbatimTextOutput('text5')),column(6,verbatimTextOutput('text6'))),
                      fluidRow(column(6,plotOutput('plot5')),column(6,plotOutput('plot6'))),
                      fluidRow(),
                      fluidRow()
                      
                                   ),
             tabPanel("Analysis - Slide 4",
                      h2("Advanced Analysis"),
                      fluidRow(),
                      includeMarkdown("Analysis.md"),
                      fluidRow(column(6,verbatimTextOutput('text4')),column(6,verbatimTextOutput('text11'))),
                      fluidRow(column(6,plotOutput('plot4')),column(6,plotOutput('plot11'))),
                      fluidRow(column(6,verbatimTextOutput('text7')),column(6,verbatimTextOutput('text10'))),
                      fluidRow(column(6,plotOutput('plot7')),column(6,plotOutput('plot10'))),
                      fluidRow(column(6,verbatimTextOutput('text8')),column(6,verbatimTextOutput('text9'))),
                      fluidRow(column(6,plotOutput('plot8')),column(6,plotOutput('plot9'))),
                      fluidRow(column(3),column(4,verbatimTextOutput('lm1')),column(4)),
                      fluidRow(column(3),column(4,verbatimTextOutput('lm2')),column(4)),
                      fluidRow(column(3),column(4,verbatimTextOutput('lm3')),column(4))

    
             ),
tabPanel("Interactive Plots - Slide 5",  h2("Interactive Plots + leaflet map"),
         h4("you can change the variables on the selection panel and see them plot out in ggplot or graphics regular plot"),
         sidebarPanel(
           selectInput('date', 'Dates', choices = cleandates),
           selectInput('xcol', 'X Variable', names(new_dataclean)),
           selectInput('ycol', 'Y Variable', names(new_dataclean),
                       selected=names(new_dataclean)[[2]]),
           
           tabPanel("Text1", verbatimTextOutput('text1'))
           ,
           tabPanel("Text2", verbatimTextOutput('text2')),
           tabPanel("Text3",verbatimTextOutput('text3') )
         ),
         mainPanel(tabsetPanel("plot Types",tabPanel("GGplot",
           plotOutput('plot2')),
           tabPanel("Graphics adaptive", 
                    plotOutput('plot1')
                    )
         )
         ),
         fluidRow(leafletOutput('map1'))
         ),
          tabPanel("Summary - Slide 6", 
         titlePanel("So, in summary..."), 
         fluidRow(
           column(8, includeText("summary.txt"))
         )
),
        tabPanel("Our script - Slide 7",
                 titlePanel("Our Entire Script"),
                 
                 fluidRow(column(1),column(12,includeMarkdown("projectscript.Rmd"))))
      
                 )
           )



############### SERVER #############

server = function(input, output, session) {
  
  # making the data react to user selection.
  # we use the date of the first payment since that one has the most obsevations
  
  df <- reactive({
    if (input$date == "2017-01-01") {
      df <- eilatjanfeb
    } else if(input$date == "2016-11-01") {
      df <- eilatnovdec
    } else if(input$date == "2016-09-01"){
      df <- eilatsepoc
    } else if(input$date == "2016-07-01"){
      df <- eilatjulaug
    } else if(input$date == "2016-05-01"){
      df <- eilatmayjun
     } else{
      df <- new_dataclean
    }
    df
  })
  

  selectedData <- reactive({
    df()[,c(input$xcol[!is.na(input$xcol)], input$ycol[!is.na(input$ycol)])]
  })

  # plotting using regular graphics plot for the variyng plot type it gives us 
  plot1 <- reactive({
  plot(selectedData())
  })

  output$plot1 <- renderPlot({
  palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = rainbow(7),
         pch = 20, cex = 3)
  })
  #plotting function using ggplot2
   output$plot2 <- renderPlot({
     ggplot(selectedData(),aes(x=selectedData()[,input$xcol],y=selectedData()[,input$ycol], colour = selectedData()[,input$xcol]),height = 400,width = 600)+geom_point()+ xlab(input$xcol)+ ylab(input$ycol)+labs(colour=input$xcol)
  })
   
   output$plot3 <- renderPlot({
     ggplot()+
     geom_bar(aes(x = eilat$incomelevel, fill = eilat$incomelevel)) +
     labs(fill='Legend', labels = lev2, x = "Income level", y = "Count") 
   })
   
   output$plot4 <- renderPlot({
        ggplot(lm6$model, aes_string(x = names(lm6$model)[2], y = names(lm6$model)[1])) + 
       stat_smooth(method = "loess", col = "red") + geom_point() + labs(y = "Consumption", x = "House size")
   }) 
  
   output$plot5<-renderPlot({
    
     df <- as.data.frame(table(eilat$hou_type))
     ggplot()+
       geom_bar(aes(x=eilat$hou_type, fill = eilat$incomelevel)) +
       labs(fill = 'Legend', x = "House Type", y = "Count") + geom_text(data = df, aes(x = Var1, y = Freq-10, label = Freq))
   })
   
   output$plot6<-renderPlot({
     a <- as.data.frame(c(eilat$edu_1,eilat$edu_2))
     a <- filter(a, !is.na(a))
     t <- as.data.frame(table(a))
     names(a) <- "education"
     t$a <- factor(t$a, labels = c("Highschool Graduate", "Academic Degree"))
     ggplot(data = t, aes(x = a))+geom_bar(aes(y = Freq, fill = t$a),stat="identity") + 
       labs(fill = 'Legend', x = "Education Type", y = "Count") + 
       geom_text(data = t, aes(x = a, y = Freq-16, label = Freq))
   })
   
   output$plot7 <- renderPlot({
  ggplot(lm1$model, aes_string(x = names(lm1$model)[1], y = names(lm1$model)[2])) +   
       stat_smooth(method = "loess", col = "red") +
       geom_point() + labs(y = "Consumption", x = "Number of Residents")
   })

   output$plot8 <- renderPlot({
     ggplot(data = eilatjanfeb[!is.na(eilatjanfeb$incomelevel),],aes(incomelevel,kw1)) + geom_boxplot(aes(group = incomelevel, color = incomelevel)) + labs(x = "Income Level", y = "Elec. consumption", color = "Legend")
   })
 
   output$plot9 <- renderPlot({
     ggplot(data = eilat,aes(hou_type,hou_size)) + geom_boxplot(aes(group = hou_type, fill = hou_type)) + labs(x = "House Type", y = "House Size", fill = "Legend")
   })
     
   output$plot10 <- renderPlot({
     ggplot(lm2$model, aes_string(x = names(lm2$model)[1], y = names(lm2$model)[2])) +   
       stat_smooth(method = "loess", col = "blue") +
       geom_point() + labs(y = "Consumption per Capita", x = "Number of Residents")
   })
   
   output$plot11 <- renderPlot({
     ggplot(lm7$model, aes_string(x = names(lm7$model)[2], y = names(lm7$model)[1])) + 
       stat_smooth(method = "loess", col = "blue") + geom_point() + labs(y = "Consumption per capita", x = "House size")
   }) 
   
   
  output$text1 <- renderText(paste("number of observations: ", nrow(df())))
  output$text2 <- renderText(paste("R Squared for X ~ Y: ",summary(lm(df()[,input$xcol]~df()[,input$ycol]))$r.squared))
  output$text3 <- renderText(paste("*Note: if R^2 is NA switch between X and Y" ))
  output$text3.5 <- renderText(paste("Distribution of income level" ))
  output$text4 <- renderText(paste("Elecricity consumption over house size" ))
  output$text5 <- renderText(paste("House type" ))
  output$text6 <- renderText(paste("Education types" ))
  output$text7 <- renderText(paste("Consumption over number of residents"))
  output$text8 <- renderText(paste("Income Level"))
  output$text9 <- renderText(paste("Houses type compared to house size"))
  output$text10 <- renderText(paste("Consumption per capita over number of residents"))
  output$text11 <- renderText(paste("Elecricity consumption per capita over house size"))
  output$lm1 <- renderPrint(summary(lm3))
  output$lm2 <- renderPrint(summary(lm4))
  output$lm3 <- renderPrint(summary(lm5))
  output$txtsum1 <- renderText({ paste0('This app lets you interactively view the varying elements which may effet the way and amount of electricity consumption')})

  
  points <- filter(eilat,!is.na(eilat$LAT))
  points$LAT <- jitter(points$LAT, factor = 4)
  points$LON <- jitter(points$LON, factor = 4)
  pop <- paste("ID:", points$ID,"</br>","Number of Residents: ", points$num_res,"</br>", "House Size (M^2): ",points$hou_size,"</br>" ,"House type: ",points$hou_type,"</br>","Income Level:", points$incomelevel )
  
  
  map = leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng = points$LON, lat = points$LAT, color = "Red" , popup = pop) 
  
    output$map1 = renderLeaflet(map)
  
output$table1 <- renderTable({
  df <- as.data.frame(table(eilat[,c(34,47)]))
  names(df) <- c("House Type", "Frequnecy")
})
  
  
}


shinyApp(ui,server)

