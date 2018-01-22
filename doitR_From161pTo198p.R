#doitR from p.162
#결측치 정제하기

df<-data.frame(sex = c("M","F", NA, "M", "F"),
               score = c(5, 4, 3, 4, NA))
df

#결측치 확인 , 결측치는 TRUE로 표기 is.na는 is it NA? 의미임
is.na(df)

table(is.na(df)) #결측치 빈도 출력 (FALSE 몇개, TRUE 몇개)

table(is.na(df$sex))
table(is.na(df$score))

#NA가 있는 데이탈ㄹ 함수에 적용하면 정상적으로 연산되지 않고 NA를 출력
mean(df$score)

#결측치 제거하기
library(dplyr)
#score가 NA인 데이타만 출력
df %>% filter(is.na(score))

#score가 NA가 아닌 데이타만 출력
df %>% filter(!is.na(score))

df_nomiss <- df %>% filter(!is.na(score))
df_nomiss
mean(df_nomiss$score)
sum(df_nomiss$score)

# & 조건을 활용하여 score뿐만 아니라 sex에도 결측치가 없는 데이타 추출하기
df_nomiss <- df %>% filter( !is.na(score) & !is.na(sex) )
df_nomiss

#결측치가 하나라도 있으면 제거하기
#na.omit (null을 거부한다?)
df_nomiss2 <- na.omit(df)
str(df)
summary(df)
glimpse(df)
class(df)
typeof(df)

#na.rm은 na를 remove하고 함수를 계산해줌 
mean(df$score, na.rm = T)
mean(df$score, na.rm = TRUE)
mean(df$score, na.rm = 0)
mean(df$score, na.rm = 1) # TRUE =1

sum(df$score, na.rm = T)

exam <- read.csv("csv_exam.csv")
exam
#math항목에 3,8,15행 값을 NA로 세팅
exam[c(3, 8, 15), "math"] <- NA 
exam

#NA가 있어서 평균 구하면 NA
exam %>% summarise( mean_math = mean(math))

#na.rm으로 결측치를 제외하고 평균 구하자
exam %>% summarise( mean_math = mean(math, na.rm = T)) 

#na.rm으로 결측치를 제외하고 평균, 합계, 중앙값 구하자
exam %>% summarise(mean_math = mean(math, na.rm = T),
                   sum_math  = sum(math, na.rm = T),
                   median_math = median(math, na.rm = T))

#결측치 대체하기(Imputation)
#평균값으로 결측치를 대체해보자
mean(exam$math, na.rm = T)
#평균값 55.23529를 55라고 하면..
exam$math <- ifelse(is.na(exam$math), 55, exam$math)
table(is.na(exam$math))
#NA였던 3,8,15행이 55로 대체됨
exam
mean(exam$math)

#question p.170
#mpg에 결측치를 만들어서 분석문제 해결

mpg1<- as.data.frame(ggplot2::mpg)
mpg1
mpg1[c(65, 124, 131, 153, 212), "hwy"] <- NA
mpg1
summary(mpg1)
tail(mpg1,30)

#q1.drv, hwy에 결측치가 몇개 있는가?
table(is.na(mpg1$drv))
table(is.na(mpg1$hwy))
table(is.na(mpg1$drv), is.na(mpg1$hwy))
table(is.na(mpg1$hwy), is.na(mpg1$drv))
summarise(mpg1)
#q2. filter()를 이횽해 hwy변수의 결측치를 제외하고 
#어떤 구동방식의 hwy평균이 높은지..
mpg1 %>% 
  filter(!is.na(mpg1$hwy)) %>%
  group_by(mg1$drv) %>%
  summarise(mean_hwy=mean(mpg1$hwy))

#위 문장 실행시 아래 오류 남 filter로 hwy만 삭제되었다? drv는 hwy가 na인 행 유지?
#왜 229rows 인데 234라고 할ㄲ
#Error in mutate_impl(.data, dots) : 
#Column `mpg1$drv` must be length 229 (the number of rows) or one, not 234

#이런..mpg1 데이타 프레임값을 %>%로 전달하므로 mpg1$hwy가 아니라 hwy로 해야
#새로운 값이 아닌 앞에서 조건을 준 값을 처리
mpg1 %>% 
  filter(!is.na(hwy)) %>%
  group_by(drv) %>%
  summarise(mean_hwy=mean(hwy))

#p.171 이상치 정제하기
outlier <- data.frame(sex   = c(1, 2, 1, 3, 2, 1),
                      score = c(5, 4, 3, 4, 2, 6))
outlier
#outlier를 확인하기위해 table을 이용해 빈도표를 생성한다.
table(outlier$sex)
table(outlier$score)

# sex가 3일경우, score가 6일경우 이상치를 결측(NA) 처리하기
outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex)
outlier

outlier$score <- ifelse(outlier$score >5, NA, outlier$score)
outlier

#결측치를 제외한후 성별에 따른 평균 점수를 구함
outlier %>%
  filter(!is.na(sex) & !is.na(score)) %>%
  group_by(sex) %>%
  summarise(mean_score = mean(score))

#p.175 boxplot으로 극단치 기준 정하기
boxplot(mpg$hwy)
summary(mpg)
#상자그림 통계치 출력
boxplot(mpg$hwy)$stats

#정상치 벗어나면 결측처리하기
mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)
table(is.na(mpg$hwy))
#hwy가 NA인 데이타를 조회해보자
mpg %>% filter(is.na(hwy))

#결측치 제외하고 평균값을 구하자
mpg %>%
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy, na.rm = T))

#question p.178 mpg 데이터 이용 분석
mpg <- as.data.frame(ggplot2::mpg)
mpg[c(10, 14, 58, 93), "drv"] <- "k"
mpg[c(29, 43, 129, 203), "cty"] <- c(3, 4, 39, 42)

head(mpg, 30)
summary(mpg)

table(mpg$drv)
table(mpg$cty)
boxplot(mpg$cty)
boxplot(mpg$cty)$stat

#q1
table(mpg$drv)
#내가푼 답
mpg$drv <- ifelse(mpg$drv =='k', NA, mpg$drv)
#실제 답
mpg$drv <- ifelse(mpg$drv %in% c("4", "f", "r"), mpg$drv, NA )
table(mpg$drv)
table(is.na(mpg$drv))

#q2
boxplot(mpg$cty)
boxplot(mpg$cty)$stat
mpg$cty <- ifelse(mpg$cty < 9| mpg$cty>26, NA, mpg$cty)
boxplot(mpg$cty)
boxplot(mpg$cty)$stat

#q3
mpg %>%
  filter( !is.na(drv) & !is.na(cty) ) %>%
  group_by(drv) %>%
  summarise(mean_cty = mean(cty))

#p.184 산점도 만들기
library(ggplot2)

#x축은 displ, y축은 hwy로 지정해 배경 생성
ggplot(data = mpg, aes( x = displ, y = hwy))

#배경에 산점도 추가 dylpr은 %>% 기호로연결, ggplot2는 +로 연결
#산점도에 표시된 점들은 각각의 관측치(행)를 의미
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point()

#x축 범위 3~6으로 지정 xlim 사용
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point() + xlim(3, 6)

#x축 범위 3~6으로, y축 범위 10~30으로 지정 xlim, ylim 사용
ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  xlim(3, 6) +
  ylim(10, 30)

#question p.188
#q1
ggplot(data = mpg, aes(x = cty, y= hwy)) +
  geom_point() 

#q2
summary(midwest)
ggplot(data = midwest, aes( x = poptotal, y = popasian)) +
  geom_point()

ggplot(data = midwest, aes( y = poptotal, x = popasian)) +
  geom_point()

#data=,x=, y= 삭제해도 같은 그래프 보여줌
#그래도 가독성면에서 써주는게 명확
ggplot(midwest, aes( poptotal, popasian)) +
  geom_point()

ggplot(data = midwest, aes( x = poptotal, y = popasian)) +
  geom_point() +
  xlim(0 , 500000) +
  ylim(0 , 10000)
# x 축을 지수 1e+05 = 1*10의 5승이 아닌 10000으로 표시하기 위해서는 options(scipen =99) 추가 필요
#
#options("scipen" = 100)
options(scipen = 99)
options(scipen = 0)

?options
options(digits = 5)
print(1e5)
options(scipen = 3); print(1e5)

ggplot(data = midwest, aes( x = poptotal, y = popasian)) +
  geom_point() +
  xlim(0 , 500000) +
  ylim(0 , 10000)

?ggplot

#ggplot Examples

# Generate some sample data, then compute mean and standard deviation
# in each group
df <- data.frame(
  gp = factor(rep(letters[1:3], each = 10)),
  y = rnorm(30)
)
df
ds <- plyr::ddply(df, "gp", plyr::summarise, mean = mean(y), sd = sd(y))
ds
# The summary data frame ds is used to plot larger red points on top
# of the raw data. Note that we don't need to supply `data` or `mapping`
# in each layer because the defaults from ggplot() are used.
ggplot(df, aes(gp, y)) +
  geom_point() +
  geom_point(data = ds, aes(y = mean), colour = 'red', size = 3)

# Same plot as above, declaring only the data frame in ggplot().
# Note how the x and y aesthetics must now be declared in
# each geom_point() layer.
ggplot(df) +
  geom_point(aes(gp, y)) +
  geom_point(data = ds, aes(gp, mean), colour = 'red', size = 3)

# Alternatively we can fully specify the plot in each layer. This
# is not useful here, but can be more clear when working with complex
# mult-dataset graphics
ggplot() +
  geom_point(data = df, aes(gp, y)) +
  geom_point(data = ds, aes(gp, mean), colour = 'red', size = 3) +
  geom_errorbar(
    data = ds,
    aes(gp, mean, ymin = mean - sd, ymax = mean + sd),
    colour = 'red',
    width = 0.4
  )

#p.189 막대 그래프
library(dplyr)

df_mpg <- mpg %>%
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy))

df_mpg
#막대 그래프를 만드는 함수 geom_col()
ggplot(data = df_mpg, aes(x = drv, y = mean_hwy)) + geom_col()

#크기순으로 정렬하기 범주의 알파벳 순서로 정렬하는데
#값의크기순으로 정렬 reorder()사용
ggplot(data = df_mpg, aes(x = reorder(drv, -mean_hwy), y = mean_hwy)) + geom_col()

#p.192 빈도 막대그래프 만들기
#y축 없이 x축만 지정하고 geom_col대신 geom_bar사용, y축은 count가 됨
ggplot(data = mpg, aes( x = drv )) + geom_bar()

ggplot(data = mpg, aes( x = hwy)) + geom_bar()

#뒤쪽을 geom_bar => geom_col로 바꾸면 작동할까?
ggplot(data = mpg, aes( x = hwy)) + geom_col()
# Error in pmin(y, 0) : object 'y' not found 
#y가 없다고 오류남. 요약표는 geom_col, 원자료는 geom_bar

#p.193
glimpse(mpg)
head(mpg)
summary(mpg)
str(mpg)
tail(mpg)
table(mpg$class)
#mpg로 상관관계 그래프 아래 둘다 안그려짐
qplot(mpg)
plot(mpg)
#
plot(mpg, main="Scatter plot Matrix")

df_mpg1 <- mpg %>%
  select(displ, year, cyl, drv, cty, hwy)

plot(df_mpg1)
#위에까지는 됨, 아래는 안됨
df_mpg2 <- mpg %>%
  select(displ, year, cyl, drv, cty, hwy, trans  )

plot(df_mpg2)

#q1  정답
df_suv <- mpg %>%
  filter(class == 'suv') %>%
  group_by(manufacturer) %>%
  summarise(mean_cty = mean(cty)) %>%
  arrange(desc(mean_cty)) %>%
  head(5)

df_suv
#평균연비가 높은순으로 정렬하여 막대 그래프를 그림
ggplot(data = df_suv, aes(x = reorder(manufacturer, - mean_cty), 
                          y = mean_cty)) + geom_col()
          
#q2 어떤 클래스(자동차종류)가 많은지 빈도 그래프를 그림
ggplot(data = mpg, aes(x = class))+ geom_bar()
#빈도 그래프에서 순서 줄때는 어떻게 할까? 아래는 다 틀림, 확인해보자
ggplot(data = mpg, aes(x = reorder(class, count)))+ geom_bar()
ggplot(data = mpg, aes(x = reorder(class)))+ geom_bar()

#p.194 시계열 그래프 만들기
economics
glimpse(economics)
summary(economics)
class(economics)
#geom_line을 사용하여 선으로 실업율을 시계열 표현
ggplot(data = economics, aes( x = date, y = unemploy )) + geom_line()

#p.195 저축율을 선으로 시계열 표현
ggplot(data = economics, aes( x = date, y = psavert )) + geom_line()

#p.196 상자그림 만들기
ggplot (data = mpg, aes(x = drv, y = hwy)) + geom_boxplot()

#p.198 q1
df_class1 <- mpg %>%
  filter(class == "compact"| class == "subcompact"| class == "suv")
#나는 위와 같이 풀었는데 내가 실패한 문장은 아래임
df_class2 <- mpg %>%
  filter(class %in% ("compact","subcompact","suv"))

#문제는 뒤에 값들을 그냥 괄호로 연결하면 안되고 c() 함수로 만들어 줘야 함
df_class2 <- mpg %>%
  filter(class %in% c("compact","subcompact","suv"))

df_class2

x<-c("compact","subcompact","suv")
class(x)
x
df_class1
glimpse(df_class1)
summary(df_class1)
ggplot(data = df_class1, aes(x = class, y = cty)) + geom_boxplot()
