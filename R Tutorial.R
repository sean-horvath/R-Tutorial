

# Matrix vs Dataframe -----------------------------------------------------
# Matrices and Dataframes have the same structure, but matrices can only
# hold values that all have the same class, i.e. numeric.  Dataframes 
# on the other hand can hold values of different classes in different
# columns.


rm(list=ls())

my_matrix <- matrix(c('a','b','c',1,2,3),nrow=3)
my_matrix
class(my_matrix[1,1]);class(my_matrix[1,2])


my_df <- data.frame(Char=c('a','b','c'),Num=c(1,2,3))
my_df
class(my_df[1,1]);class(my_df[1,2])


my_df <- data.frame(Char=c('a','b','c'),Num=c(1,2,3),
                    stringsAsFactors=F)
my_df
class(my_df[1,1]);class(my_df[1,2])

my_df[1,2]
my_df$Num[1]
#

# Vectorization -----------------------------------------------------------
# Vectorization is a method to speed up processes that are repetitive.
# A function can be applied to all values within a vector rather than
# looping through the vector and applying the function to each value
# independently.

mymatrix <- matrix(sample(1:10,3e8,replace=T),ncol=3000,nrow=100000)

# Slow way - loop through each row of a matrix and apply mean()
begin <- Sys.time()
for(i in 1:dim(mymatrix)[1]){
  tmp <- mean(mymatrix[i,])
}
Sys.time()-begin

# Vectorize - use built in functions like "rowMeans" to apply mean
# to each row simultaneously
begin <- Sys.time()
tmp <- rowMeans(mymatrix)
Sys.time()-begin

# Apply functions, these are a nice replacement for "for loops" that
# you can pass custom functions to
tmp2 <- apply(mymatrix,1,mean)
any(tmp!=tmp2)

tmp3 <- apply(mymatrix,1,function(x){
  sum(x>=8)
})

#
# Reshaping Data ----------------------------------------------------------
# Data often comes in a form that is not what we need to conduct analysis.
# Reshaping data is an important first step in data science.

library(reshape2)

names(airquality) <- tolower(names(airquality))
head(airquality) #head function allows us to look at the first few lines
# of a dataframe or matrix

# melt function allows us to quickly change the shape of a matrix or
# dataframe.  The id value tells the function which columns we want to
# keep as columns.  The rest of the columns will be transformed into
# a new variable stored in a unique column.
aqm <- melt(airquality, id=c("month", "day"), na.rm=TRUE,
            variable.name='Atmosphere',value.name='Measurement')
head(aqm)
aqm[1:1000,]

# Let's say we want to find out some qualities about our data.
# What's the monthly mean for each variable?
ozone <- mean(airquality$ozone[which(airquality$month==5)],
              na.rm=T)

# Repeat for each month, and for each variable. Messy for loops!!

# Nope! Instead...
dcast(aqm, month ~ Atmosphere, mean)
# dcast will perform a summarizing function based on the formula
# we pass to it.

# Using the tidyverse:
# Tidyverse is a compilation of packages that enable quick data
# wrangling and nice data visualization.
library(tidyverse)

# Sample dataset that comes with
diamonds

# Filter values from dataframe that meet a given criteria
filter(diamonds,cut=='Ideal',price<=10000)
filter(diamonds,cut=='Ideal' | cut=='Premium',price<=350)
filter(diamonds,cut %in% c('Ideal','Premium'),price<=350)

# Sort values of dataframe
arrange(diamonds,price,color)
arrange(diamonds,desc(price))

# And select certain data
select(diamonds,carat,clarity,price)

# Perform operation on some columns of data, creating a new column
mt_diamonds <- diamonds
mutate(mt_diamonds,
       volume=x*y*z)

# Or just return the new column
transmute(diamonds,
          volume=x*y*z)

# Produce some statistic of a column
summarise(diamonds,mean.price=mean(price,na.rm=T))

by_quality <- group_by(diamonds,cut,color)
summarise(by_quality,mean.price=mean(price,na.rm=TRUE))

# Pipelines are useful for making your data wrangling process more readable
# Instead of placing functions within functions (onion scripting), pipelines
# let us apply functions more similar to how we would read them.  Pipelines
# are performed with the %>% command.
diamonds %>%
  group_by(cut,color) %>%
  summarise(mean.price=mean(price,na.rm=TRUE))


# Putting it all together
# Let's find which Ideal and Premium diamonds have the highest price per carat
# by cut and color
diamonds %>%
  filter(cut %in% c('Ideal','Premium')) %>%
  mutate(quality=price/carat) %>%
  group_by(cut,color) %>%
  summarise(mean_value=mean(quality,na.rm=T)) %>%
  arrange(desc(mean_value))

# Joining
library(gapminder)
gapminder

tbl_1 <- select(gapminder,country,year,lifeExp)
tbl_2 <- select(gapminder,country,year,pop)

head(tbl_1)
head(tbl_2)
head(cbind(tbl_1,tbl_2[,3]))

tbl_2 <- tbl_2[sample(1:dim(tbl_2)[1]),]
head(tbl_1)
head(tbl_2)

# left_join is a powerful tool to combine two dataframes, and match rows
# based on certain criteria.
tbl_3 <- left_join(tbl_1,tbl_2,by=c('country','year'))
head(tbl_3)
# 

# AISMR -------------------------------------------------------------------
# Lets look at some river flow data:
library(reshape2)

aismr <- read.table('http://civil.colorado.edu/%7Ebalajir/r-session-files/aismr.txt')

# or generate data:
x <- rnorm(12*(70+59),10,3)
aismr <- matrix(x,ncol=12)
aismr <- cbind(seq(1901,by=1,length.out=(70+59)),aismr)
aismr <- as.data.frame(aismr)

colnames(aismr) <- c('Year','Jan','Feb','Mar','Apr','May',
                     'Jun','Jul','Aug','Sep','Oct','Nov','Dec')

head(aismr)

# Plot using base R functions
dev.new() # this creates a pop-out window for you plots.
par(mfrow=c(1,1))
plot(1:dim(aismr)[1],aismr[,2],type='b')

dev.new()
plot(1:dim(aismr)[1],aismr[,2],type='b',col=2,
     ylab='Flow',
     xlab='Year',
     main='January Flows')

dev.new()
par(mfrow=c(3,4))
for(i in 2:dim(aismr)[2]){
  plot(1:dim(aismr)[1],aismr[,i],type='b')
}

# Lets reshape our data:
aismr_neat <- melt(aismr,
                   id.vars = 'Year',
                   variable.name = 'Month',
                   value.name = 'Flow')
head(aismr_neat)

# Or with tidyverse
aismr_neat2 <- aismr %>%
  gather(colnames(aismr)[2:13],
         key = "Month",
         value = "Flow")
head(aismr_neat2)

# ggplot:
# GGplot is a data visualization package that allows us to make neat,
# aesthetically pleasing plots.
p1 <- ggplot(aismr_neat,aes(x=Year,y=Flow,color=Month)) +
  geom_line() +
  ggtitle('All Indian Summer Monsoon') +
  facet_wrap(~Month)
dev.new();p1 # using a semicolon allows us to place multiple functions on
# the same line

p2 <- ggplot(aismr_neat,aes(x=Year,y=Flow,color=Month)) +
  geom_line() +
  ggtitle('All Indian Summer Monsoon')
dev.new();p2

p3 <- ggplot(aismr_neat,aes(x=Month,y=Flow)) +
  geom_boxplot() +
  ggtitle('All Indian Summer Monsoon')
dev.new();p3

p4 <- ggplot(aismr_neat,aes(x=Month,y=Flow)) +
  geom_boxplot() +
  ggtitle('All Indian Summer Monsoon') + 
  geom_jitter(width = 0.2)
dev.new();p4

# Let's split up our data into two "eras", past and present.  Then we 
# can simply tell ggplot to separate our data by these two eras and display
# them side by side.
aismr_neat$Era <- c(rep('Past',70),rep('Present',59))
p5 <- ggplot(aismr_neat,aes(x=Month,y=Flow)) +
  geom_boxplot(aes(color=Era)) +
  ggtitle('All Indian Summer Monsoon')
dev.new();p5

p6 <- ggplot(aismr_neat) +
  geom_histogram(aes(x=Flow,fill=Month),bins=15) +
  ggtitle('AISMR Histogram by Month')
dev.new();p6

p7 <- ggplot(aismr_neat) +
  geom_histogram(aes(x=Flow,fill=Month)) +
  facet_wrap(~Month) +
  ggtitle('AISMR Histograms by Month')
dev.new();p7

# Let's see if the flow is correlated with the previous month
aismr_neat$Prev <- c(aismr_neat$Flow[which(aismr_neat$Month!='Jan')],
                     aismr_neat$Flow[which(aismr_neat$Month=='Jan')])
p8 <- ggplot(aismr_neat,aes(x=Flow,y=Prev,color=Month)) +
  geom_point() +
  facet_wrap(~Month)
dev.new();p8

p9 <- ggplot(aismr_neat, aes(Flow)) + 
  geom_density(aes(fill=factor(Month)), alpha=0.8) + 
  labs(title="Density plot", 
       subtitle="Flow Grouped by Month",
       caption="Source: AISMR Data",
       x="Flow",y='Density',
       fill="Month")
dev.new();p9

# gridExtra is a package for combining plots in a aesthetically pleasing
# way
library(gridExtra);library(grid);library(lattice)

grid.arrange(p1,p2,p4,p5,ncol=2)

t <- textGrob('Monthly AISMR')
r <- rectGrob(gp=gpar(fill='grey50'))
grid.arrange(p1,t,r,p4,ncol=2)

# grid layout
gs <- lapply(1:9, function(ii) 
  grobTree(rectGrob(gp=gpar(fill=ii, alpha=0.5)), textGrob(ii)))
grid.arrange(grobs=gs, ncol=4, 
             top="top label", bottom="bottom\nlabel", 
             left="left label", right="right label")
grid.rect(gp=gpar(fill=NA))

# provide matrix of the layout
lay <- rbind(c(1,1,1,2,3),
             c(1,1,1,4,5),
             c(6,7,8,9,9))
grid.arrange(grobs = gs, layout_matrix = lay)

# specify widths and heights
grid.arrange(grobs=gs[1:3], ncol=2, widths = 1:2, 
             heights=unit(c(1,10), c("in", "mm")))

# nested layout
g1 <- arrangeGrob(grobs = gs, layout_matrix = t(lay))
g2 <- arrangeGrob(grobs = gs, layout_matrix = lay)
grid.arrange(g1, g2, ncol=2)





