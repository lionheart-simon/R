#데이터분석 프로젝트 p.210
# 패키지로드
install.packages("foreign")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readxl")

# 로드하기
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

#3.데이터 불러오기
raw_welfare <- read.spss(file = "d:/down/Koweps_hpc10_2015_beta1.sav",
                         to.data.frame = T)
#복사본 만들기
welfare <- raw_welfare
#4.데이터 검토하기=> 탐색적데이타분석
head(welfare)
head(welfare,1)
tail(welfare)
View(welfare)
dim(welfare)
class(welfare)
str(welfare)
summary(welfare)
#str로 다 안나오는거 glimpse는 항목 다 잘나옴
glimpse(welfare)

#5.변수명 바꾸기
welfare <- rename(welfare,
                  sex = h10_g3,
                  birth = h10_g4,
                  marriage = h10_g10,
                  religion = h10_g11,
                  income = p1002_8aq1,
                  code_job = h10_eco9,
                  code_region = h10_reg7)
 
glimpse(welfare) 

#성별에 따른 월급차이 분석 p.213
#1.변수검토하기
class(welfare$sex)
table(welfare$sex)
#2.전처리
#결측치 확인
table(is.na(welfare$sex))
#성별 항목 이름부여
welfare$sex <- ifelse(welfare$sex == 1 , "male", "female")
table(welfare$sex)
qplot(welfare$sex)

#p.216 월급변수검토 및 전처리
#1.변수검토하기
class(welfare$income)
summary(welfare$income)
qplot(welfare$income)
# 그래프로 더 잘 표현하기 위해 x값을 0~1000으로 설정
qplot(welfare$income) + xlim(0, 1000)

#2.전처리
#이상치확인
summary(welfare$income)
#값이 0이거나 9999이면 이상치 결측처리
welfare$income <- ifelse(welfare$income %in% c(0, 9999), NA, welfare$income)
#결측치 확인
table(is.na(welfare$income))
summary(welfare$income)
# NA값이 12030->12044로 늘어난것 확인가능

#성별에 따른 월급차이 분석하기
sex_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(sex) %>%
  summarise(mean_income = mean(income))

sex_income
class(sex_income)
#그래프 만들기
ggplot(data = sex_income, aes(x = sex, y= mean_income)) + geom_col()

#p.220 나이와 월급의 관계
#나이변수 검토 및 전처리
#1.변수 검토하기
class(welfare$birth)

summary(welfare$birth)
qplot(welfare$birth)

#2.전처리 나이는 1900~2014가 정상, 모름/무응답 9999
summary(welfare$birth)
#결측치 확인
table(is.na(welfare$birth))

#이상치 결측 처리
welfare$birth <- ifelse(welfare$birth == 9999, NA, welfare$birth)
table(is.na(welfare$birth))

#파생변수 만들기 -나이

welfare$age <- 2015 - welfare$birth +1 
summary(welfare$age)

qplot(welfare$age)

#나이와 월급의 관계부석하기
#1.나이에 따른 월급 평균표 만들기
age_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(age) %>%
  summarise(mean_income = mean(income))

head(age_income)
age_income

ggplot(data = age_income, aes(x = age, y = mean_income)) +geom_point()
ggplot(data = age_income, aes(x = age, y = mean_income)) +geom_col()
ggplot(data = age_income, aes(x = age, y = mean_income)) +geom_line()

#p.225 연령대에 따른 월급차이
#파생변수 만들기 -연령대
welfare <- welfare %>%
mutate(ageg = ifelse(age < 30, "young",
                     ifelse(age <= 59, "middle", "old")))
table(welfare$ageg)
qplot(welfare$ageg)

#연령대에 따른 월급차이 분석하기
ageg_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(ageg) %>%
  summarise(mean_income = mean(income))
ageg_income

#그래프 만들기
ggplot(data = ageg_income, aes( x = ageg, y = mean_income)) + geom_col()

#young- middle- old순으로 그래프 정렬 scale_x_discrete(limits = c())
ggplot(data = ageg_income, aes( x = ageg, y = mean_income)) +
  geom_col() + 
  scale_x_discrete(limits = c("young", "middle", "old"))

#p.228 연령대및 성별 월급차이
sex_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(ageg, sex) %>%
  summarise(mean_income = mean(income))

sex_income

#그래프 만들기
ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col() +
  scale_x_discrete(limits = c("young", "middle", "old"))

#position = "dodge"
ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col(position = "dodge") +
  scale_x_discrete(limits = c("young", "middle", "old"))

#p.231 연령대가 아닌 나이 및 성별 월급차이분석하기
sex_age <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(age, sex) %>%
  summarise(mean_income = mean(income))

head(sex_age)

ggplot(data= sex_age, aes(x = age, y = mean_income, col = sex)) + geom_line()
ggplot(data= sex_age, aes(x = age, y = mean_income, col = sex)) + geom_point()
ggplot(data= sex_age, aes(x = age, y = mean_income, col = sex)) + geom_col(position = "dodge")
