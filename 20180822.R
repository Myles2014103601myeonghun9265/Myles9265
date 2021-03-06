install.packages("ggmap")
install.packages("ggplot2")
installed.packages("dplyr")
library(ggmap)
library(ggplot2)
library(dplyr)
install.packages("foreign")
library(foreign)
library(readxl)
raw_welfare = read.spss(file = "Koweps_hpc10_2015_beta1.sav",
                        to.data.frame = T)
welfare = raw_welfare
head(welfare)
tail(welfare)
View(welfare)
dim(welfare)
str(welfare)
summary(welfare)
wel = raw_welfare
welfare = rename(welfare,
                 sex = h10_g3,
                 birth = h10_g4,
                 marriage = h10_g10,
                 religion = h10_g11,
                 income = p1002_8aq1,
                 code_job = h10_eco9,
                 code_region = h10_reg7)
class(welfare$sex)
table(welfare$sex)
welfare$sex = ifelse(welfare$sex == 9, NA, welfare$sex)
table(is.na(welfare$sex))
welfare$sex = ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex)
qplot(welfare$sex)
class(welfare$income)
summary(welfare$income)
qplot(welfare$income)
qplot(welfare$income) + xlim(0, 1000)
summary(welfare$income)
welfare$income = ifelse(welfare$income %in% c(0, 9999), NA, welfare$income)
table(is.na(welfare$income))
sex_income = welfare %>% 
  filter(!is.na(welfare$income)) %>%
  group_by(sex) %>% 
  summarise(mean_incoem = mean(income))
sex_income
ggplot(data = sex_income, aes(x = sex, y = mean_incoem)) + geom_col()
head(welfare)
class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)
welfare$birth = ifelse(welfare$birth == 9999, NA, welfare$birth)
table(is.na(welfare$birth))
welfare$age = 2018 - welfare$birth + 1
summary(welfare$age)
qplot(welfare$age)
age_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income))
head(age_income)
ggplot(data = age_income, aes(x = age, y = mean_income)) + geom_line()
welfare = welfare %>% 
  mutate(ageg = ifelse(age<30, "young",
                       ifelse(age <= 59, "middle", "old")))
table(welfare$ageg)
qplot(welfare$ageg)
welfare = welfare %>% 
  mutate(age1 = ifelse(age < 20, "age_10",
                       ifelse(age < 30, "age_20",
                              ifelse(age < 40, "age_30",
                                     ifelse(age < 50, "age_40",
                                            ifelse(age < 60, "age_50", "age_60"))))))
head(welfare$age1)
qplot(welfare$age1)

age_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age1, sex) %>% 
  summarise(mean_income = mean(income))
age_income

ggplot(data = age_income, aes(x = age1, y = mean_income, fill = sex)) + 
  geom_col() +
  scale_x_discrete(limits = c("age_20", "age_30", "age_40", "age_50", "age_60"))

ggplot(data = age_income, aes(x = age1, y = mean_income, fill = sex)) + 
  geom_col(position = "dodge") +
  scale_x_discrete(limits = c("age_20", "age_30", "age_40", "age_50", "age_60"))

sex_age = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age, sex) %>% 
  summarise(mean_income = mean(income))
head(sex_age)
ggplot(data = sex_age, aes(x = age, y = mean_income, col = sex)) + geom_line()
class(welfare$code_job)
table(welfare$code_job)
list_job = read_excel("Koweps_Codebook.xlsx", col_names = T, sheet = 2)
head(list_job)
dim(list_job)
welfare = left_join(welfare, list_job, id = "code_job")
welfare %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job, job) %>% 
  head(10)



job_income = welfare %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income))

head(job_income)
top10 = job_income %>% 
  arrange(desc(mean_income)) %>% 
  head(10)
top10
ggplot(data = top10, aes(x = reorder(job, mean_income), y = mean_income))+
  geom_col() + 
  coord_flip()
