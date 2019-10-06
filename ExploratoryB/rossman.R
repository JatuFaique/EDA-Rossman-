library(tidyverse)
library(lubridate)
library(tibble)
train<-read_csv("E:\\Kaggle DATA\\Rossman\\train.csv")
view(train)
store<-read_csv("E:\\Kaggle DATA\\Rossman\\store.csv")
tibble <- merge(train,store)
#Average sale of type C store
#in the year 2015 on each day of the month 

filter_d <- mutate(tibble , year = year(Date),
                    month = month(Date),
                    day = day(Date))

f1<-filter(filter_d , year == 2015 & StoreType == 'c')
  
arrenged<- f1 %>%
  arrange(f1$Date)

avg <- arrenged %>%
  group_by(month) %>%
  group_by(day) %>%
  summarize(avgsale = mean(Sales))

ggplot(data = avg) +
         geom_col( aes(x = day, y = avgsale))

view(avg)
#Average customers  per day
avgcust <- arrenged %>%
  group_by(month) %>%
  group_by(day) %>%
  summarize(avgctmpd = mean(Customers))

ggplot(data = avgcust) +
  geom_col( aes(x = day, y = avgctmpd))


view(avgcust)

mutate(arrenged , avg)
#A group of only sale of 1 year = 2015 all Store type A and
#store number 

test1 <- filter(tibble , StoreType == 'a' & Store == 256 &
                  (year(tibble$Date) == 2014 | year(tibble$Date) == 2013))

test1 <- test1 %>%
  arrange(test1$Date)

view(test1)

test1<-filter(test1,Open != 0 & Sales != 0 & Promo != 1)

view(test1)

ggplot(data = test1  , mapping = aes( x= Date, 
                                      y= Sales,  col = Customers))+
  geom_smooth()+
  geom_point()+
  facet_wrap( ~ year(test1$Date))

#Yo Store wise analysis
#Average customers per store during Promo & promo2

test2 <- tibble %>%
  group_by(Promo == 0)%>%
  group_by(Store)%>%
  #group_by(month(tibble$Date))%>%
  summarize(meanSale = mean(Sales))

test2promo <- tibble %>%
  group_by(Promo == 1)%>%
  group_by(Store)%>%
  #group_by(month(tibble$Date))%>%
  summarize(meanSale = mean(Sales))

ggplot()+
  geom_point(data = test2,aes( x = Store , y = meanSale), color = "red" ) + 
  #geom_point(data = testpromo2, aes( x = Store , y = meantestpromo2), color = "yellow")+
  geom_col(data = test2promo,aes( x = Store , y = meanSale), color = "blue")
  

view(test2)


testpromo2 <- tibble %>%
  group_by(Promo2 == 1)%>%
  group_by(Store)%>%
  summarize(meantestpromo2 = mean(Sales))

view(testpromo2)




#fressh
#f1 is a dataframe of year 2015 and storetype c
as_tibble(f1)

f2<- filter(f1, Sales != 0)

f2DOW <- f2 %>%
  filter(DayOfWeek==1)
f2DOW2 <- f2 %>%
  filter(DayOfWeek==2)
f2DOW3 <- f2 %>%
  filter(DayOfWeek==3)
f2DOW4 <- f2 %>%
  filter(DayOfWeek==4)
f2DOW5 <- f2 %>%
  filter(DayOfWeek==5)
f2DOW6 <- f2 %>%
  filter(DayOfWeek==6)
f2DOW7 <- f2 %>%
  filter(DayOfWeek==7)


ggplot()+
  geom_freqpoly(data = f2DOW , mapping = aes(Sales))+
  geom_freqpoly(data = f2DOW2 , mapping = aes(Sales))+
  geom_freqpoly(data = f2DOW3 , mapping = aes(Sales))+
  geom_freqpoly(data = f2DOW4 , mapping = aes(Sales))+
  geom_freqpoly(data = f2DOW5 , mapping = aes(Sales),color = "blue")+
  geom_freqpoly(data = f2DOW6 , mapping = aes(Sales),color = "yellow")+
  geom_freqpoly(data = f2DOW7 , mapping = aes(Sales),color = "purple")
  

#Weekdays SALE


f1wd<- f1%>%
  filter( Promo == 0)%>%
  mutate(wday = wday(Date, label = T)) %>%
  ggplot(aes(x = wday)) +
  geom_bar()
f1wd

#what are we doing now
#trend analysis w.r.t store type i.e a,b,c,d
#only in the year 2014
#we have store type a,b,c,d

str(tibble)

#taking mean sale of each store of unique store type

trend1 <- tibble %>%
  filter(year(tibble$Date) == 2014)
  """group_by(StoreType) %>%
  group_by(year(tibble$Date),month(tibble$Date),
           day(tibble$Date))
  summarize(
    Mean = mean(tibble$Sales)
  )"""
trend1 <- trend1 %>%
  arrange(trend1$Date)

#Variations of time series wrt store type

type_a <- trend1 %>%
  filter(StoreType == 'a')


type_b <- trend1 %>%
  filter(StoreType == 'b')

type_c <- trend1 %>%
  filter(StoreType == 'c')

type_d <- trend1 %>%
  filter(StoreType == 'd')

ggplot()+
  geom_smooth(data = type_a, mapping = aes( x= Date, 
                                              y= Sales),col = "red")+
  geom_smooth(data = type_b, mapping = aes( x= Date, 
                                            y= Sales),col = "blue")+
  geom_smooth(data = type_c, mapping = aes( x= Date, 
                                            y= Sales),col = "black")+
  geom_smooth(data = type_d, mapping = aes( x= Date, 
                                            y= Sales),col = "yellow")


 
  



