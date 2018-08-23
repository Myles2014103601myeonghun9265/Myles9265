
ageg_income <- welfare %>%
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mean_income = mean(income))

ggplot(data = ageg_income, aes(x = ageg, y = mean_income)) + 
  geom_col() + 
  scale_x_discrete(limits = c("middle", "young", "old"))

sex_income <- welfare %>% filter(!is.na(income)) %>% group_by(ageg, sex) %>% summarise(mean_income = mean(income))

ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) + geom_col(position = "dodge") + scale_x_discrete(limits = c("young", "middle", "old"))

class(welfare$code_job)
table(welfare$code_job)

list_job = read_excel("Koweps_Codebook.xlsx", col_names = T, sheet = 2)
head(list_job)
dim(list_job)
welfare = left_join(welfare, list_job, id = "code_job")
head(welfare$job)
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

ggplot(data = top10, aes(x = reorder(job, mean_income), y = mean_income)) + 
  geom_col() + 
  coord_flip()

bottom10 = job_income %>% 
  arrange(mean_income) %>% 
  head(10)
bottom10

ggplot(data = bottom10, aes(x = reorder(job, -mean_income),
                            y = mean_income)) + 
  geom_col() + 
  coord_flip() + 
  ylim(0,850)

job_male = welfare %>% 
  filter(!is.na(job) & sex == "male") %>% 
  group_by(job) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)
job_male  

job_female = welfare %>% 
  filter(!is.na(job) & sex == "female") %>% 
  group_by(job) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)
job_female
ggplot(data = job_male, aes(x = reorder(job, n), y = n)) +
  geom_col() + 
  coord_flip()
ggplot(data = job_female, aes(x = reorder(job, n), y = n)) +
  geom_col() + 
  coord_flip()

class(welfare$religion)
table(welfare$religion)
welfare$religion = ifelse(welfare$religion == 1, "yes", "no")
table(welfare$religion)
qplot(welfare$religion)
class(welfare$marriage)
table(welfare$marriage)
welfare$group_marriage = ifelse(welfare$marriage == 1, "marriage",
                                ifelse(welfare$marriage == 3, "divorce", NA))
table(welfare$group_marriage)
table(is.na(welfare$group_marriage))
qplot(welfare$group_marriage)


religion_marriage = welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(religion, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100,1)) # round함수는 반올림을 하라는 함수!

religion_marriage = welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  count(religion, group_mariage) %>% 
  group_by(religion) %>% 
  mutate(pct = round(n/sum(n)*100, 1))

divorce = religion_marriage %>% 
  filter(group_marriage == "divorce") %>% 
  select(religion, pct)

divorce

ggplot(data = divorce, aes(x = religion, y = pct)) + geom_col()

ageg_marriage = welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(ageg, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100, 1))

ageg_divorce = ageg_marriage %>% 
  filter(ageg != "young" & group_marriage == "divorce") %>% 
  select(ageg,pct)


ageg_religion_marriage = welfare %>% 
  filter(!is.na(group_marriage) & ageg != "young") %>% 
  group_by(ageg, religion, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100, 1))

df_divorce = ageg_religion_marriage %>% 
  filter(group_marriage == "divorce") %>% 
  select(ageg, religion, pct)

ggplot(data = df_divorce, aes(x = ageg, y = pct, fill = religion)) + 
  geom_col(position = "dodge")


list_region = data.frame(code_region = c(1:7), 
                         region = c("서울",
                                    "수도권(인천/경기)",
                                    "부산/경남/울산",
                                    "대구/경북",
                                    "대전/충남",
                                    "강원/충북",
                                    "광주/전남/전북/제주도"))
list_region
welfare = left_join(welfare, list_region, id = "code_region")

region_ageg = welfare %>% 
  group_by(region, ageg) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100, 2))

head(region_ageg)

ggplot(data = region_ageg, aes(x = region, y = pct, fill = ageg)) + 
  geom_col() + 
  coord_flip()

list_order_old = region_ageg %>% 
  filter(ageg == "old") %>% 
  arrange(pct)
list_order_old
order = list_order_old$region
order
ggplot(data = region_ageg, aes(x = region, y = pct, fill = ageg)) +
  geom_col() + 
  coord_flip() +
  scale_x_discrete(limits = order)
