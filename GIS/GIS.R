#install packages
install.packages(c("sf", "tmap", "tmaptools", "RSQLite", "tidyverse"), 
                 repos = "https://www.stats.bris.ac.uk/R/")

install.packages(c("spatstat","sp", "rgeos", "maptools","GISTools","geojson","tmaptools","geojsonio"), 
                 repos = "https://www.stats.bris.ac.uk/R/")
install.packages("spdep")

#library packages 

library(spatstat)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)
library(tidyverse)



#load the London borough spatial data
shape <- st_read("London_Borough_Excluding_MHW.shp")
summary(shape)
plot(shape)

shape %>% 
  st_geometry() %>%
  plot()


#read csv
library(tidyverse)
Active_enterprise <-  read_csv("active enterprises.csv") 
Average_GDI <-  read_csv("Average GDI2016.csv")
Ratio <-  read_csv("house price to median earnings.csv")
Wellbeing <-  read_csv("wellbeing.csv",na = "n/a")


#descriptive statistics
#summary
summary(Ratio$`House price to median earnings`)
summary(Active_enterprise$`Active Enterprises in 2016`)
summary(Average_GDI$`Average gross disposable income`)
summary(Wellbeing$`Life Satisfaction`)
#histogram
par(mfrow = c(2,2), oma = c(0.5, 0.5, 0.5, 0.5))
hist(Average_GDI$`Average gross disposable income`,
     col = "blue",
     main="Average gross disposable income",xlab="Pounds")
hist(Ratio$`House price to median earnings`,
     col = "blue",
     main="House price to median income", xlab="ratio")
hist(Active_enterprise$`Active Enterprises in 2016`,
     col = "blue",
     main="Active enterprises", xlab="Numbers")

hist(Wellbeing$`Life Satisfaction`,
     col = "blue",
     main="Life satisfaction", xlab="scores")

# merge csv and spatial datasets by same code
London_hpincome <- shape%>%
  merge(.,
        Ratio,
        by.x="GSS_CODE", 
        by.y="Code")
# check the results
London_hpincome%>%
  head(., n=10)

London_enterprises <- shape%>%
  merge(.,
        Active_enterprise,
        by.x="GSS_CODE", 
        by.y="Code")

London_GDI <- shape%>%
  merge(.,
        Average_GDI,
        by.x="GSS_CODE", 
        by.y="Code")

London_wellbeing <- shape%>%
  merge(.,
        Wellbeing,
        by.x="GSS_CODE", 
        by.y="Code")

#map different data in boroughs to see local differences
Map_GDI <- tm_shape(London_GDI) + 
  tm_fill("Average gross disposable income", 
          title="Average gross disposable income in 2016",
          palette = "PuBu", 
          style = "pretty")+
  tm_credits("(c) ONS and Greater London Authority", 
             position=c(0.0,0.0))+
  tm_scale_bar(position=c(0.08,0.03), text.size=0.5)+
  tm_compass(north=0, position=c(0.9,0.8))+
  tm_layout(title="Average gross disposable income in 2016 for London boroughs")
Map_GDI
Map_hpincome <- tm_shape(London_hpincome) + 
  tm_fill("House price to median earnings", 
          title="House price to median earnings in 2016",
          palette = "PuBu", 
          style = "pretty")+
  tm_credits("(c) ONS and Greater London Authority", 
             position=c(0.0,0.0))+
  tm_scale_bar(position=c(0.08,0.03), text.size=0.5)+
  tm_compass(north=0, position=c(0.9,0.8))+
  tm_layout(title="House price to median earnings in 2016 for London boroughs")
Map_hpincome
Map_enterprises <- tm_shape(London_enterprises) + 
  tm_fill("Active Enterprises in 2016", 
          title="Active Enterprises in 2016",
          palette = "PuBu", 
          style = "pretty")+
  tm_credits("(c) ONS and Greater London Authority", 
             position=c(0.0,0.0))+
  tm_scale_bar(position=c(0.08,0.03), text.size=0.5)+
  tm_compass(north=0, position=c(0.9,0.8))+
  tm_layout(title="Active Enterprises in 2016 for London boroughs")
Map_enterprises
Map_wellbeing <- tm_shape(London_wellbeing) + 
  tm_fill("Life Satisfaction", 
          title="Life Satisfaction in 2016",
          palette = "PuBu", 
          style = "pretty")+
  tm_credits("(c) ONS and Greater London Authority", 
             position=c(0.0,0.0))+
  tm_scale_bar(position=c(0.08,0.03), text.size=0.5)+
  tm_compass(north=0, position=c(0.9,0.8))+
  tm_layout(title="Life Satisfaction in 2016 for London boroughs")
Map_wellbeing

#spatial autocorrelation analysis
#house price to median earnings in different London Boroughs

#define spatial weights matrix

library(spdep)
#First calculate the centroids of all boroughs in London

Centb<- London_hpincome%>%
  st_centroid()%>%
  st_geometry()

plot(Centb,axes=TRUE)

#generate a spatial weights matrix with Contiguity edges corners
#create a neighbours list

Lborough_nb <- London_hpincome %>%
  poly2nb(., queen=T)

#plot them
plot(Lborough_nb, st_geometry(Centb), col="red")
#add a map underneath
plot(London_hpincome$geometry, add=T)
#create a spatial weights object from these weights
Lborough.lw <- Lborough_nb %>%
  nb2listw(., style="C")

head(Lborough.lw$neighbours)

#ratio dataset
#Moran’s I test
I_Lborough_hpincome <- London_hpincome %>%
  pull("House price to median earnings") %>%
  as.vector()%>%
  moran.test(., Lborough.lw)
I_Lborough_hpincome


#Getis Ord statistic for hot and cold spots
G_Lborough_hpincome<- London_hpincome %>%
  pull("House price to median earnings") %>%
  as.vector()%>%
  localG(., Lborough.lw)
head(G_Lborough_hpincome)
G_hpincome <- London_hpincome %>%
  mutate(hpincome_G = as.numeric(G_Lborough_hpincome))
GIColours<- rev(brewer.pal(8, "RdBu"))


tm_shape(G_hpincome) +
  tm_polygons("hpincome_G",
              style="pretty",
              palette=GIColours,
              midpoint=NA,
              title="Gi*, House price to median income")+
  tm_credits("(c) ONS and Greater London Authority", 
             position=c(0.0,0.0))+
  tm_scale_bar(position=c(0.08,0.03), text.size=0.5)+
  tm_compass(north=0, position=c(0.9,0.8))+
  tm_layout(title="Getis Ord statistic for house price to median earnings")

#income dataset
#Moran’s I test
I_Lborough_income <- London_GDI %>%
  pull("Average gross disposable income") %>%
  as.vector()%>%
  moran.test(., Lborough.lw)
I_Lborough_income


#Getis Ord statistic for hot and cold spots
G_Lborough_income<- London_GDI %>%
  pull("Average gross disposable income") %>%
  as.vector()%>%
  localG(., Lborough.lw)

G_income <- London_GDI %>%
  mutate(income_G = as.numeric(G_Lborough_income))


GIColours<- rev(brewer.pal(8, "RdBu"))


tm_shape(G_income) +
  tm_polygons("income_G",
              style="pretty",
              palette=GIColours,
              midpoint=NA,
              title="Gi*, Average gross disposable income")+
  tm_credits("(c) ONS and Greater London Authority", 
             position=c(0.0,0.0))+
  tm_scale_bar(position=c(0.08,0.03), text.size=0.5)+
  tm_compass(north=0, position=c(0.9,0.8))+
  tm_layout(title="Getis Ord statistic for average gross disposable income")

#regression 
library(tidyverse)
Regression <-  read_csv("regression.csv")
Income_enterprises <- Regression %>%
  lm(`Income` ~
       `Active Enterprises`,
     data=.)
summary(Income_enterprises)

S_In_En <- qplot(x = `Income`, 
           y = `Active Enterprises`, 
           data=Regression)

#plot with a regression line 
S_In_En + stat_smooth(method="lm", se=FALSE, size=1) + 
  geom_jitter()

library(broom)

glance(Income_enterprises)

Regression <-  read_csv("regression.csv")
Income_life <- Regression %>%
  lm(`Income` ~
       `Life Satisfaction`,
     data=.)
summary(Income_life)

S_In_life <- qplot(x = `Income`, 
                 y = `Life Satisfaction`, 
                 data=Regression)

#plot with a regression line 
S_In_life + stat_smooth(method="lm", se=FALSE, size=1) + 
  geom_jitter()


glance(Income_life)

