---
title: "AmazonBooks"
author: "Anjali"
date: '2023-03-28'
output: git_document
---


``` {r}
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readxl)
library(lubridate)

```

```{r}

b_2019 <- read_csv("bestsellers with categories.csv")

head(b_2019)
str(b_2019)
colnames(b_2019)
nrow(b_2019)
summary(b_2019)
View(b_2019)

```

```{r}
# change Year to numeric

b_2019$Year <- as.integer(b_2019$Year)
b_2019$Year <- as.character.Date(b_2019$Year)

is.integer(b_2019$Year)
is.double(b_2019$Year)
is.Date(b_2019$Year)
is.character(b_2019$Year)

```
```{r}
# number of authors

b_2019 %>%
  summarise(no_of_authors = n_distinct(Author))

# number of books

b_2019 %>%
  summarise(no_of_books = n_distinct(Name))


```

```{r}
#DISCRIPTIVE ANALYSIS

#number of books in both category
table(b_2019$Genre)

aggregate(b_2019$`User Rating`~ b_2019$Genre, FUN = mean)
aggregate(b_2019$`User Rating`~ b_2019$Genre, FUN = max)
aggregate(b_2019$`User Rating`~ b_2019$Genre, FUN = min)
aggregate(b_2019$`Reviews`~ b_2019$Genre, FUN = mean)
aggregate(b_2019$`Reviews`~ b_2019$Genre, FUN = max)
aggregate(b_2019$`Reviews`~ b_2019$Genre, FUN = min)
```

```{r}
# Year wise avg ratings and reviews

avg_rating <- b_2019 %>%
  group_by(Year, Genre) %>%
  summarise(Number_of_reviews = n()
            ,Avg_rating = mean(`User Rating`)) %>%
  arrange(Genre)
head(avg_rating)
View(avg_rating)

#Popular authors in decade

popular_author <- b_2019 %>%
  group_by(Author) %>%
  summarise(no_of_books = n()) %>%
  arrange(desc(no_of_books)) %>%
  slice(1:10)

View(popular_author)

popular_author$no_of_books <- as.numeric(popular_author$no_of_books)
str(popular_author)

# Popular books in decade

b_2019 %>%
  group_by(Name) %>%
  summarise(no_of_reviews = n()
            , avg_rating = mean(`User Rating`)) %>%
  arrange(desc(avg_rating)) %>%
  slice(1:10)%>%
  ggplot(aes(x=avg_rating, y=Name))+ geom_col()


# popular cataegory each year

b_2019  %>%
  group_by(Year, Genre) %>%
  summarise(Number = n()) %>%
  arrange(Year, Genre) %>%
  ggplot(aes(x= Year, y= Number, fill= Genre)) +
  geom_col(position = "dodge") +
  labs(x = "Year" , y= "Number of books")


# top 10 authors Visualization

ggplot(data = popular_author) + geom_col(mapping= aes(x=no_of_books , y= Author), fill= "orange")+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5))

# top 10 author ratings distribution -- boxplot

      boxplot(b_2019$`User Rating`~ b_2019$Genre)
      
      # using ggplot
      ggplot(b_2019, aes(x=Genre, y=`User Rating`, fill= Genre)) + stat_boxplot(geom = "errorbar",width = 0.25)  +geom_boxplot()

# Relationship ratings-year - scatterplot

ggplot(data = avg_rating) + geom_point(mapping= aes(x= Year, y=Avg_rating , color= Genre, size=3))
 

# ratings distribution - histogram

      rating <- b_2019$`User Rating`

      View(rating)
      min(rating)

      # without using ggplot
      x <- seq(min(rating), max(rating), length = 40)
      f <- dnorm(x, mean = mean(rating), sd = sd(rating))
      hist(rating, prob=TRUE, main="Ratings distribution", xlab = "Ratings", xlim = c(3,5), col= "cyan")
      lines(x, f, col = "red", lwd = 2)

      # with ggplot
      ggplot(data=b_2019, aes(x=rating)) +
           geom_histogram(fill="steelblue", color="black", binwidth = 0.1) +
           ggtitle("Histogram of Rating Values")

# price distribution - histogram

    price <- b_2019$Price

    min(price)
    max(price)

    # without using ggplot
    x2 <- seq(min(price), max(price), length = 40)
    fun <- dnorm(x2, mean = mean(price), sd = sd(price))

    hist(price, prob=TRUE, main="Price distribution", xlab = "Price", col= "cyan")
    lines(x2, fun, col = "red", lwd = 2)

    # using ggplot
    ggplot(data=b_2019, aes(x=Price)) +
        geom_histogram(fill="steelblue", color="black", binwidth = 0.5) +
        ggtitle("Histogram of Price Values")


```
