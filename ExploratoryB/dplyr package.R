install.packages("nycflights13")
library(tidyverse)
library(lubridate)
flights<-nycflights13::flights
nycflights13::flights
view(flights)
filter_1<-filter(flights, month == 1, day == 1)
arrdelay2<-filter(flights , arr_delay >= 2)
arrdelay_3<-filter(arrdelay2, dest == 'IAH' | dest == 'HOU')
view(arrdelay_3)
arrdelay_4<-filter(arrdelay_3, dep_delay == 0)
view(arrdelay_4)
arrdelay_5<-filter(arrdelay_4, dep_time == 600 | dep_time == 1200)
view(arrdelay_5)
arrdelay_6<-between(arrdelay_5,month == 7 , month == 9)
#Arrange
arrange(flights, year, month ,day)
#mutate
#Meaning adding a new column of the existing table (x,y)->(x/y)
flights_sml<- select(flights,year:day,ends_with("delay"),
                     distance,air_time)
view(flights_sml)
mutate<-transmute(flights_sml,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60)
view(mutate)
#
by_day <- group_by( flights , day,month,
                    year)
summarize(by_day , delay = mean(dep_delay, na.rm = T))
flights
f1 <- flights %>% 
  group_by(carrier) %>% 
  group_by(tailnum) %>%
  summarize(delay = mean(dep_delay))
view(f1)


#date time

flights %>% 
  select(year, month, day , hour , minute)


flights %>% 
  select(year, month, day , hour , minute)%>%
  mutate(
    departure = make_datetime(year , month , day , hour , minute)
  )

make_datetime_100 <- function(year, month , day, time)
{
  make_datetime(year , month , day , time %/% 100 ,time %% 100)
}

flights_dt <- flights %>%
  filter(!is.na(dep_time), !is.na(arr_time)) %>%
  mutate(
    dep_time = 
  )



