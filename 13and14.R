library(ggplot2)
library(dplyr)
mpg_raw = as.data.frame(ggplot2::mpg)
mpg = mpg_raw
mpg_diff = mpg %>%
  select(class, cty) %>% 
  filter(class %in% c("compact", "suv"))

head(mpg_diff)
table(mpg_diff$class)
t.test(data = mpg_diff, cty ~ class, var.equal = T)

mpg_diff2 = mpg %>% 
  select(fl, cty) %>% 
  filter(fl %in% c("r", "p"))

table(mpg_diff2$fl)

?mpg

t.test(data = mpg_diff2, cty ~ fl, var.equal = T)

economics = as.data.frame(ggplot2::economics)
cor.test(economics$unemploy, economics$pce)


head(mtcars)

car_cor = cor(mtcars)

car_cor

round(car_cor, 2)

install.packages("corrplot")
library(corrplot)

corrplot(car_cor)

corrplot(car_cor, method = "number")

install.packages("knitr")







