library(ggmap)
library(ggplot2)
library(dplyr)
library(foreign)
library(readxl)
raw_welfare = read.spss(file = "D://R_Homework/Data/Koweps_hpc10_2015_beta1.sav",
                        to.data.frame = T)
welfare = raw_welfare
welfare = rename(welfare,
                 sex = h10_g3,
                 birth = h10_g4,
                 marriage = h10_g10,
                 religion = h10_g11,
                 income = p1002_8aq1,
                 code_job = h10_eco9,
                 code_region = h10_reg7)
##여기까지는 전에 했던 데이터들 가져오기!

##여기부터는 오늘 한 내용.!직업별 월급 평균표를 만들어 보자! 
list_job = read_excel("Koweps_Codebook.xlsx", col_names = T, sheet = 2)
#일단 직업을 만들어야되니까 직업 코드표를 가져오자!
welfare = left_join(welfare, list_job, id = "code_job")
# 그다음에 레프트 조인으로 welfare에 직업을 추가한다!
welfare %>%
  filter(!is.na(code_job)) %>%
  select(code_job, job) %>%
  head(10)
#그러면 잘 되있는 것을 볼 수 있다!!
# 그 다음으로는 직업별 월급 평균표를 진짜로 만들어야되는데 먼저 평균 수입을 만들어보자
job_income = welfare %>%
  filter(!is.na(job) & !is.na(income)) %>%
  group_by(job) %>%
  summarise(mean_income = mean(income))
# 10개를 추출해서 표를 만들어보면!
top10 = job_income %>%
  arrange(desc(mean_income)) %>%
  head(10)
# 다음과 같이 나온다!!
ggplot(data = top10, aes(x = reorder(job, mean_income), y = mean_income)) +
  geom_col() +
  coord_flip()# 이 함수는 옆으로 눞이는 함수이다!
# 하위 10개 또한 똑같이 만들어준다.
bottom10 = job_income %>%
  arrange(mean_income) %>%
  head(10)
#아까는 높은 순이라서 위로정렬했으니 이번에는 아래로 정렬시킨다.
ggplot(data = bottom10, aes(x = reorder(job, -mean_income),
                            y = mean_income)) +
  geom_col() +
  coord_flip() +
  ylim(0, 850)
# 다음으로는 성별 직업 빈도를 분석해 보자!
# 먼저 남성직업 빈도 상위 10개를 빼냈다. 많이 쓴 함수는 이하생략 ㅎㅎㅎ
job_male = welfare %>% 
  filter(!is.na(job) & sex == "male") %>% 
  group_by(job) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(10)
# 그래프를 만들어보면 다음과 같이 나온다.
ggplot(data = job_male, aes(x = reorder(job, n), y = n)) +
  geom_col() +
  coord_flip()
# 이번엔 여성을 빼냈다.
job_female = welfare %>%
  filter(!is.na(job) & sex == "female") %>%
  group_by(job) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(10)
# 여성직업의 빈도표는 다음과 같다!
ggplot(data = job_female, aes(x = reorder(job, n), y = n)) +
  geom_col() +
  coord_flip()
# 그렇다면 종교 유무에 따른 이혼율을 봐보자!
welfare$religion = ifelse(welfare$religion == 1, "yes", "no")
# 종교 유무에는 1이 종교를 가지고 있다라고 나오는것 같다!
# 이번에는 이혼 여부 변수를 만드는데 이 데이터에는 여러가지 값이 있기 때문에 거기서 2개만 뽑아 냈다.
welfare$group_marriage = ifelse(welfare$marriage == 1, "marriage",
                                 ifelse(welfare$marriage == 3, "divorce", NA))
# 위에서 만든 종교 유무와 이혼율을 가지고 표를 만들 것인데 여기서 NA값은 없앨 것이다!
religion_marriage = welfare %>%
  filter(!is.na(group_marriage)) %>%
  group_by(religion, group_marriage) %>%
  summarise(n = n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(pct = round(n/tot_group*100, 1))
religion_marriage
# count()를 활용하는 방법도 있는데 
religion_marriage <- welfare %>%
  filter(!is.na(group_marriage)) %>%
  count(religion, group_marriage) %>% ## 이부분이 mutate와 summarise함수 두부분을 축약한 부분이라고 보면 된다!
  group_by(religion) %>%
  mutate(pct = round(n/sum(n)*100, 1))
#그리고 이혼율 표를 만들기 위해서 이혼율을 추출 하자!
divorce = religion_marriage %>%
  filter(group_marriage == "divorce") %>%
  select(religion, pct)
# 그래프를 만들어보면 다음과 같다!
ggplot(data = divorce, aes(x = religion, y = pct)) + 
  geom_col()

# 그러면 이번엔 연령대별 이혼율 표를 만들어보자!
ageg_marriage = welfare %>%
  filter(!is.na(group_marriage)) %>%
  count(ageg, group_marriage) %>%
  group_by(ageg) %>%
  mutate(pct = round(n/sum(n)*100, 1))
#이번에는 카운트함수로 축약했다.
# 어린 사람들을 제외하고 이혼을 추출할 것이다!
# 그러니까 전 수업에 했던 나이만드는 것을 다시 만들자!
welfare$age = 2018 - welfare$birth + 1
welfare = welfare %>%
  mutate(ageg = ifelse(age < 30, "young",
                       ifelse(age <= 59, "middle", "old")))

ageg_divorce = ageg_marriage %>%
  filter(ageg != "young" & group_marriage == "divorce") %>%
  select(ageg, pct)
# 그래프를 만들어서 확인해보자!
ggplot(data = ageg_divorce, aes(x = ageg, y = pct)) + geom_col()
# 그러면 이제 연령대, 종교유무, 결혼상태별 비율표를 만들수 있는데 그것은 다음과 같이 표현할 수 있다.
ageg_religion_marriage = welfare %>%
  filter(!is.na(group_marriage) & ageg != "young") %>%
  count(ageg, religion, group_marriage) %>% 
  group_by(ageg, religion) %>%
  mutate(pct = round(n/sum(n)*100, 1))
#count()함수로 간단하게 표현했다!
#다음으로 연령대 및 종교 유무별 이혼율 표를 만들겠다.
df_divorce = ageg_religion_marriage %>%
  filter(group_marriage == "divorce") %>%
  select(ageg, religion, pct)
# 연령대 및 종교 유무에 따른 이혼율 그래프는 다음과 같다.
ggplot(data = df_divorce, aes(x = ageg, y = pct, fill = religion )) +
  geom_col(position = "dodge")

#그렇다면 이번엔 지역별 연령대 비율을 보겠다.
# 변수를 검토해보면 전처리 단계를 거쳐서 지역코드를 입혀줘야되는데 
list_region = data.frame(code_region = c(1:7),
                          region = c("서울",
                                     "수도권(인천/경기)",
                                     "부산/경남/울산",
                                     "대구/경북",
                                     "대전/충남",
                                     "강원/충북",
                                     "광주/전남/전북/제주도"))
#그건 이렇게 만들어 주겠다.
#확인해보면 코드 region별로 region이 입혀져있는데
welfare = left_join(welfare, list_region, id = "code_region")
# 이 데이터를 welfare에 입히겠다.!
region_ageg = welfare %>%
  count(region, ageg) %>% 
  group_by(region) %>% 
  mutate(pct = round(n/sum(n)*100, 2))
#그 다음으로는 지역별 연령대 비율표를 만들어 주었다!
ggplot(data = region_ageg, aes(x = region, y = pct, fill = ageg)) +
  geom_col() +
  coord_flip()
# 표를 만들면 위와 같이 만들어진다!
