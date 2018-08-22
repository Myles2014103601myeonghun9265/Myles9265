install.packages("ggmap")
install.packages("foreign")
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)
##여기까지는 다 아는 패키지 준비하는 방법!
## 본격적으로 시작!
raw_welfare = read.spss(file = "D://R_homework/Data/Koweps_hpc10_2015_beta1.sav", to.data.frame = T)  #raw_welfare라는 원본을 만들어주고 그 안에 spss데이터를 집어넣어줬음!  to.data.frame = T로 안해주면 데이터프레임으로 읽지를 않는다!
welfare = raw_welfare  #복사본 생성!
##데이터 검토하는 함수들! 이하 생략하겠음
head(welfare)
tail(welfare)
View(welfare)
dim(welfare)
str(welfare)
summary(welfare)

# 변수명 바꾸는 방법 이것들을 쓸거기 때문에!
welfare = rename(welfare,
                 sex = h10_g3,
                 birth = h10_g4,
                 marriage = h10_g10,
                 religion = h10_g11,
                 income = p1002_8aq1,
                 code_job = h10_eco9,
                 code_region = h10_reg7)
class(welfare$sex) #바꿔준 welfare$sex의 클래스 확인
table(welfare$sex)
table(is.na(welfare$sex)) # 이상치 확인
welfare$sex = ifelse(welfare$sex == 9, NA, welfare$sex)  #이상치를 NA로 바꾸는 방법.
table(is.na(welfare$sex))
#sex 테이블 안의 성별에 이름을 부여하는 방법
welfare$sex = ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex) # 빈도 확인
qplot(welfare$sex) # qplot으로 확인

#income클래스 검토와 전처리하는 방법
class(welfare$income)
summary(welfare$income)
qplot(welfare$income)
qplot(welfare$income) + xlim(0, 1000) #너무 많아서 제한을 시켜줬다!
#이상치를 확인
summary(welfare$income)

#이상치는 결측 처리! 여기서 이상치는 9999인거 같음!
welfare$income = ifelse(welfare$income %in% c(0, 9999), NA, welfare$income)
table(is.na(welfare$income)) # 결측치는 정상적으로 처리됨.

#본격적인 분석!
sex_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income = mean(income))
#sex_income이라는 새로운 변수에 성별에 따른 월급의 평균값을 넣어준다.
sex_income
ggplot(data = sex_income, aes(x = sex, y = mean_income)) + geom_col()
#x엔 성별 y 엔 평균 수입을 넣고 그래프를 만들어줬다.

#이번엔 나이와 월급의 관계를 본다.
summary(welfare$birth) # 이상치가 없다.
# 이 birth를 보는 이유는 새로운 변수로 나이를 만들것이기 때문이다.
welfare$age = 2018 - welfare$birth + 1
summary(welfare$age)
qplot(welfare$age)
#그렇다면 이제 나이에 따른 월급 평균표를 만들어보자
age_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income)) #나이로 그룹을 묵고 평균 월급을 보았다.
head(age_income)
ggplot(data = age_income, aes(x = age, y = mean_income)) + geom_line()
# 이번엔 라인으로 그려서 나타내봤다.

#이번에는 연령대에 따른 월급차이를 볼것이다.
#새롭게 변수를 만들어 줄것인데 이때 변수는 연령대별로 나눌것이다.
#10대는 월급에 영향을 끼치지 않으니 제외시킬 것이다.
welfare = welfare %>% 
  filter(age >= 20) %>% 
  mutate(ageg = ifelse(age < 30, "age_20",
                       ifelse(age < 40, "age_30",
                              ifelse(age < 50, "age_40",
                                     ifelse(age < 60, "age_50", "age_old")))))
table(welfare$ageg)
qplot(welfare$ageg) # 나는 20대~50대와 그 이상을 나눴다.

#연령대별과 성별로 월급 평균표를 만들것이다.
ageg_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg, sex) %>% 
  summarise(mean_income = mean(income))
ageg_income
ggplot(data = ageg_income, aes(x = ageg, y = mean_income, fill = sex)) + 
  geom_col() +  #fill = sex 가 성별로 가르는 것!
  scale_x_discrete(limits = c("age_20", "age_30", "age_40", "age_50", "age_old")) #내가 원하는 순서대로 바꾸는 방법!.
ggplot(data = ageg_income, aes(x = ageg, y = mean_income, fill = sex)) + 
  geom_col(position = "dodge") +  #position = "dodge"를 적어주면 분리가 된다!
  scale_x_discrete(limits = c("age_20", "age_30", "age_40", "age_50", "age_old")) 

#나이 및 성별 월급의 차이를 분석하는 방법!
sex_age = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age, sex) %>% 
  summarise(mean_income = mean(income))
ggplot(data = sex_age, aes(x = age, y = mean_income,  col = sex )) + geom_line() #col = sex를 해줘야 성별로 나뉜다.





