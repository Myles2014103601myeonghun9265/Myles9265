library(foreign)
library(ggmap)
library(ggplot2)
library(dplyr)
library(foreign)
library(readxl)
raw_welfare = read.spss(file = "E:/Coumputer_Class/R_Homework/Homework/Koweps_hpc10_2015_beta1.sav",
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
list_job <- read_excel("Koweps_Codebook.xlsx", col_names = T, sheet = 2)
welfare <- left_join(welfare, list_job, id = "code_job")
welfare %>% filter(!is.na(code_job)) %>% select(code_job, job) %>% head(10)
job_income <- welfare %>% filter(!is.na(job) & !is.na(income)) %>% group_by(job) %>% summarise(mean_income = mean(income))
top10 = job_income %>% arrange(desc(mean_income)) %>% head(10)

ggplot(data = top10, aes(x = reorder(job, mean_income), y = mean_income)) + 
  geom_col() +
  coord_flip()
### 첫번째 숙제 한국 공항들만 한국 지도에 표시!!
####   https://github.com/keunsooyoon/Rclass/blob/master/web
##################################################################################
# 웹 크롤링! Web Crawling!!!!!
# Rrrrrrrrrrrrr
#
# 1. 공항 이용객 분석!!!
#################################################################################

# 여러번 해봐야 잘 할 수 있당!
#브라우저는 구글 크롬을 이용 (개발자 도구) Ctrl + Shift + I 

#패키지 설치!
install.packages("rvest") # 웹크롤링 할 때 가장 많이 쓰는 방법!!!!
install.packages("ggplot2")
install.packages("ggmap")
install.packages("xml2") # 웹을 다룰 때 사용하는 패키지!
#패키지 로드!
library(rvest)
library(ggplot2)
library(ggmap)
library(xml2)
#install.packages("stringr")
#library(stringr)

# html 파일 가져오기


html.airport = read_html("https://en.wikipedia.org/wiki/List_of_busiest_airports_by_passenger_traffic") # html.airport = read_html(" 여기엔 긁어 올 웹 페이지 주소를 적는다!")

#가져온 파일에서 첫 뻔째 테이블만 추출하기!
View(html.airport)
df = html_table(html_nodes(html.airport,"table")[[1]], fill = TRUE)

head(df)

######전처리 시작####

# 6번째 열의 이름을 total로 변경하기

colnames(df)[6] = "total"

#total 열의 필요 없는 부분 제거하기

df$total

df$total = gsub('\\[[0-9]+\\]', '', df$total)

#total열의 쉼표 제거하기

df$total
df$total = gsub(',', '', df$total) #' 이 안에 '있는 필요 없는 것들을 ''안 처럼 제거하라!!

#total열을 숫자로 변환하기

df$total = as.numeric(df$total)

# 공항들의 위도와 경도값 알아오기

gc = geocode(df$Airport)

gc

# 위도 경도 값과 원래 자료와 합치기!

df = cbind(df, gc)


df

# 세계 지도 불러오기


world = map_data("world")
world

# 지도 위에 그리기

ggplot(df, aes(x = lon, y = lat)) +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey75", color = "grey70") +
  geom_point(color = "dark red", alpha = .25, aes(size = total)) +
  geom_point(color = "dark red", alpha = .75, shape = 1, aes(size = total)) +
  theme(legend.position = 'none')

###############################################################################
#웹 크롤링 (Web Crawling)
#
# 2. 영화 리뷰 분석!!
#
###############################################################################


#패키지 설치!


#패키지 설치!
install.packages("rvest") # 웹크롤링 할 때 가장 많이 쓰는 방법!!!!
install.packages("ggplot2")
install.packages("xml2") # 웹을 다룰 때 사용하는 패키지!
#패키지 로드!
library(rvest)
library(ggplot2)
library(xml2)
#install.packages("stringr")
#library(stringr)

#크롤링 기본 연습
# http://www.naver.com 을 naver_html 이라는 이름으로 읽어오기


naver_html = read_html("http://www.naver.com")

#특정 태그나 특정 요소의 값만 추출.

naver_node = html_node(naver_html, ".ah_l") %>% #.ah_l 이거 붙은걸 한개만 찾아라! (요소를 잡는것!!!)
  html_nodes(".ah_k") #.ah_k가 들어가는 요소를 모두 가져와라!!!
##node와 nodes의 차이는 단수는 한개만 찾고 복수는 여러개를 찾는다!

naver_node

# 글자만 추출하라!!!
# 코드들은 다 날리고 순수한 글자들만 추출하는법!
html_text(naver_node)

#정리

# read_html() 을 통해서 xml문서를 읽기!
#html_nodes()를 통해 특정 태그 or 속성 값을 갖는 요소만 추출
#html_text() 를 통해 () 내에서  텍스트 값만 추출!

movie_url = read_html("https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=140652&type=after&isActualPointWriteExecute=false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false&page=")

#page=뒤에 넣는 방법!!!!
movie_url1 = paste0(movie_url, "2")

# 페이지 자동 변환해서 읽어오기

# 페이지 번호 생성

page = c(1:100)

#변수 만들기!
#아무것도 없는 변수를 생성했음!
alltxt = c()


# 페이지 자동으로 읽어오기!
for(i in page){
  movie_url1 = paste0(movie_url, i)
  movie_html = read_html(movie_url1)
  movie_node = html_nodes(movie_html, css=".score_result") %>% 
    html_nodes("p")
  movie_txt = html_text(movie_node)
  movie_alltxt = append(alltxt,movie_txt)
  print(i)
}
head(movie_alltxt)
movie_alltxt

############################################################
#   분석
############################################################

# 문제점 많음!!!
# 자바 설치 jdk 7
# http://www.oracle.com/technetwork/java/javase/downloads/index-jsp-138363.html

install.packages("rJava")
install.packages("KoNLP")  #자바7을 이용하기 때문에 7깔아야됨!!!!!!!







