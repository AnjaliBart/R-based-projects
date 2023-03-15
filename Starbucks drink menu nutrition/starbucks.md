---
title: "R Notebook"
output:
  html_document:
    df_print: paged
---
```{r}
#Starbuck drinks menu analysis

library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
library(treemap)
library(RColorBrewer)
library(forcats)
library(treemapify)
library(colorspace)

getwd()

setwd('E:/GDAC/CaseStudy/StarbucksDataset/archive (1)')

```

```{r}

## STEP 1: Collect data

drink_nutri <- read_excel('starbucks_drinkMenu_expanded.xlsx')

drink_sum <- read_excel('starbucks_drinkMenu_expanded.xlsx', sheet= 'pivot')

View(drink_sum)

View(drink_nutri)
colnames(drink_nutri)
str(drink_nutri)

n_distinct(drink_nutri)

n_distinct(drink_nutri$Beverage_category)    # 9 Beverage categories

n_distinct(drink_nutri$Beverage)             # 33 sub categories


```
```{r}

#Visualization

# 1 sub category per category - treemap

table(drink_nutri$Beverage)


 #using geom_treemap
drink_nutri %>%
   group_by(Beverage_category) %>%
   summarise(num_cat= n()) %>%
 ggplot(aes(area = num_cat, fill = num_cat, label= Beverage_category)) +
   geom_treemap()+
  geom_treemap_text(color="White", size=10) +
  labs(fill= "Count")+
  scale_fill_gradient2(low = "#72e40c",
                       mid = "#a08212",
                      high = "#4b1a04")
 
 
  #2  Average calories per category - Lollipop plot
drink_nutri%>%
  group_by(Beverage_category)%>%
  summarise(avg_cal= mean(Calories)) %>%
  arrange(avg_cal)%>%
  mutate(Beverage_category=factor(Beverage_category, levels = Beverage_category))%>%
  ggplot( aes(x=Beverage_category, y=avg_cal)) +
  geom_segment( aes(xend=Beverage_category, yend=0)) +
  geom_point( size=4, color="orange") +
  labs(title = "Average calories in each category", subtitle = "(in mg)", caption = "Data from starbucks")+
  coord_flip()+
  ylab("")+
  xlab("Calories")
  

#3 Average calories per beverage - columnchart
drink_nutri %>%
  group_by(Beverage)%>%
  summarise(avg_calories= mean(Calories)) %>%
  ggplot() + 
  geom_col(mapping= aes(x =avg_calories , y= Beverage), fill="Brown")+
  labs(title="Average Calories", )


#4 Calories vs sugar in beverage categories- scatter plot

ggplot(data = drink_nutri, aes(x=Sugars, y=Calories )) +
    geom_point(aes(color = Beverage_category)) +
    theme(legend.position="none")+
    facet_wrap(~Beverage_category)


#5 High Caffeine drinks - barplot

drink_nutri%>%
  group_by(Beverage_category)%>%
  summarise(avg_caf= sum(Caffeine_mg)) %>%
  arrange(avg_caf)%>%
  mutate(Beverage_category=factor(Beverage_category, levels = Beverage_category))%>%
  ggplot(aes(x=Beverage_category, y= avg_caf, fill= Beverage_category))+
  geom_col()+
      scale_fill_brewer(palette = "YlOrBr")+
    scale_x_discrete(labels= c ('Classic Espresso Drinks'='ClED'
    ,'Coffee'='C'
    ,'Frappuccino® Blended Coffee'='FBC'
    ,'Frappuccino® Blended Crѐme'='FBCr'
    ,'Frappuccino® Light Blended Coffee'='FLBC'
    ,'Shaken Iced Beverages'='ShIB'
    ,'Signature Espresso Drinks'='SED','Smoothies'='Sm'
    ,'Tazo® Tea Drinks'='TTD'))+
  geom_text(stat="identity", aes(label= signif(avg_caf)), color="white", vjust= 1.5)+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))+
  labs(fill= " Beverage Category")+
  xlab("")+
  ylab("Caffeine in mg")
 
  
```
















