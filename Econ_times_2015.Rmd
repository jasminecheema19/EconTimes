---
title: "Econ_times_2015"
author: "Jasmine Cheema"
date: "July 1, 2016"
output: html_document
---


```{r}
#load the libraries to be used
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)

#Read both the excel files containing HDI and CPI
majorHDI <- read.csv("Major-HDI.csv")
majorCPI <- read.csv("Major-CPI.csv")

#Creat a new data set which joins both HDI and CPI data
major <- full_join(majorHDI, majorCPI, by ="Country")

#Remove all rown which contain NA
major <- na.omit(major)

#Read the file with regions included 
region <- read.csv("region.csv")

#join major and region file to complete the dataset for graph
major <- inner_join(major, region, by= "Country")

#scatter plot
p1 <- ggplot(major, aes(x=CPI, y=HDI, color= Region))

p2 <- p1 + geom_smooth(aes(group = 1), method = "loess", se = FALSE, formula=y ~ log(x), color="red")
p3 <- p2 + geom_point()


#make open circles
p3 <- p2 + geom_point(shape=1, size=4) 

#to darken the circles
p3 <- p2 + geom_point(shape=1, size=4.5) +geom_point(shape=1, size=4.25)+geom_point(shape=1, size=4)

#countries to be labeled 

pointstoLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore")

#geom_text makes the labels crowded 
#We'll use ggrepel's geom_text_repel to repel the labels
library(ggrepel)
p4 <- p3 + geom_text_repel(aes(label= Country), color= "gray20", data= subset(major, Country %in% pointstoLabel), force=10)

#change the order and names of the region
major$Region <- factor(major$Region, levels= c("EU W. Europe",
                                               "Americas",
                                               "Asia Pacific",
                                               "East EU Cemt Asia",
                                               "MENA",
                                               "SSA"),
                       labels = c("OECD",
                                  "Americas",
                                  "Asia &\nOceania",
                                  "Central &\nEastern Europe",
                                  "Middle East &\nnorth Africa",
                                  "Sub-Saharan\nAfrica"))

p4$data <- major

#Labelling and scaling the axes and title
library(grid)

p5 <- p4 + scale_x_continuous(name= "Corruption Perception Index, 2014, (100=Least corrupt)", 
                              limits = c(10, 100),
                              breaks = seq(10,100, by=10))+
       scale_y_continuous(name= "Human Development Index, 2014, (1=Best)", 
                     limits = c(0.2, 1),
                     breaks = seq(0.2, 1.0, by = 0.1))+ ggtitle("Corruption and Human development")
  

#using economist theme
library(grid) # for the 'unit' function
library(ggthemes)
p6 <- p5 +
  theme_economist_white() + # start with a minimal theme and add what we need
  theme(text = element_text(color = "gray20"),
        legend.position = c("top"), # position the legend in the upper left 
        legend.direction = "horizontal",
        legend.justification = 0.1, # anchor point for legend.position.
        legend.text = element_text(size = 11, color = "gray10"),
        axis.text = element_text(face = "italic"),
        axis.title.x = element_text(vjust = -1), # move title away from axis
        axis.title.y = element_text(vjust = 2), # move away for axis
        axis.ticks.y = element_blank(), # element_blank() is how we remove elements
        axis.line = element_line(color = "gray40", size = 0.5),
        axis.line.y = element_blank(),
        panel.grid.major = element_line(color = "gray50", size = 0.5),
        panel.grid.major.x = element_blank()
  )

```

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```



```{r }
summary(major)
```

## Including Plot

You can also embed plots, for example:

```{r, echo=FALSE}
plot(p6)
```
