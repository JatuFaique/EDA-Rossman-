library(tidyverse)
library(hexbin)
ggplot(diamonds, aes(carat,price))+ geom_hex() + ggsave("diamonds.pdf")

write.csv(diamonds, "diamonds.csv")
