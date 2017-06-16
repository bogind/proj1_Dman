---
title: "Untitled"
author: "D&D"
date: "June 16, 2017"
output: html_document
---

<p>
The aim of this research was to conclude which factors affect household electricity consumption. In slide number 4 you can see the factors which we have found to have the most impact on electricity consumption. 
</p>
<p>
For the first 4 plots, we used the LOWESS (Locally Weighted Scatterplot Smoothing) method. This approach is mostly visual and it can help to see relationships between variables. The disadvantage of that method is that it can't confirm significance.  
</p>
<p>
Plot 4 confirmed that electricity consumption grows in larger houses. This relationship is the only one that got R square 0.47 in linear analysis. From the plot 9, we can conclude that detached houses are bigger then apartments and due to that we expect that households that live in detached houses will consume more electricity. Plot number 8 showed that households with a higher income level consume more electricity. Here too we assume that households with a higher income reside in detached houses.
</p>
<p>
Electricity consumption tends to rise when a household has more residents, but when we consider electricity consumption per capita we can see it is more efficient to live with four people than alone. 
</p>
<p>
The Multivariate regression showed each time high adjusted R-square but we can see from the summary of the linear models that in most cases the p-value is not good enough and there is no good result from the regression. 
</p>
<p>
Although the plots show good visual influence on electricity the significance of the data is not high. We assume that if there were more households that participated in the study we could see better results and use more methods of statistical analysis. For further research, we recommend to gather more data for the analysis.  
</P>

<p>
<h3>The packages we used:</h3>
<h4>hover over each to see wich functions we used</h4>

<div title = 'count = 4:"if_else", "left_join", "mutate", "select", "filter"'><b>dplyr</div>
<div title = 'count = 11:"aes", "aes_string", "geom_bar", "geom_boxplot", "geom_point", "geom_text", "ggplot", "labs", "stat_smooth", "xlab","ylab"'>ggplot2</div>
<div title = 'count = 5:"addCircleMarkers", "addTiles", "leaflet", "leafletOutput", "renderLeaflet"'>leaflet</div>
<div title = 'count = 5:"addCircleMarkers", "addTiles", "leaflet", "leafletOutput", "renderLeaflet"'>leaflet</div>
<div title = 'count = 1:"colsplit"'>reshape2</div>
<div title = 'count = 22:"column", "fluidRow", "h2", "h3", "h4",
"includeMarkdown", "mainPanel",  "navbarPage", "plotOutput", "reactive",
"renderPlot", "renderPrint", "renderTable", "renderText", "selectInput",
"shinyApp", "sidebarPanel", "tabPanel", "tabsetPanel", "tagList",
"titlePanel", "verbatimTextOutput"'>shiny</div>
<div title = 'count = 1:"themeSelector"'>shinythemes</div>
<div title = 'count = 1:"list.functions.in.file", that was used to make this list'>NCmisn</div></b></br>
* However most of the functions we used are from the<div title = 'base, graphics, grDevices,stats & utils.'><b> R core</div></b> packages.
</p>
</br></br>
<h3>Bibliography:</h3>
<h4>each entry contains a hyperlink to its DOI</h4>
<p>
<a href = https://doi.org/10.1017/CBO9781107415324> 1. Ipcc. (2014). Summary for Policymakers. Climate Change 2014: Synthesis Report. Contribution of Working Groups I, II and III to the Fifth Assessment Report of the Intergovernmental Panel on Climate Change. </a></br>
<a href = https://doi.org/10.1016/j.enbuild.2012.01.037> 2. McLoughlin, F., Duffy, A., & Conlon, M. (2012). Characterising domestic electricity consumption patterns by dwelling and occupant socio-economic variables: An Irish case study. Energy and Buildings, 48(July 2009), 240–248.</a> </br>
<a href = https://doi.org/10.1196/annals.1439.003> 3. MEA. (2005). Ecosystems and human well-being. Ecosystems (Vol. 5).</a> </br>
<a href = https://doi.org/10.1016/j.enbuild.2006.11.001> 4. Santamouris, M., Kapsis, K., Korres, D., Livada, I., Pavlou, C., & Assimakopoulos, M. N. (2007). On the relation between the energy and social characteristics of the residential sector. Energy and Buildings, 39(8), 893–905. </a></br>
<a href = https://doi.org/10.1016/j.enbuild.2007.09.001> 5. Yohanis, Y. G., Mondol, J. D., Wright, A., & Norton, B. (2008). Real-life energy use in the UK: How occupancy and dwelling characteristics affect domestic electricity use. Energy and Buildings, 40(6), 1053–1059.</a> </br>
</p>
</br></br></br>
