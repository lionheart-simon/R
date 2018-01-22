install.packages("ggplot2")
library(ggplot2)

x <- c("a", "a", "b","c")
x

median(1,2,100)

qplot(x)
mpg
class(mpg)
typeof(mpg)
summary(mpg)

#x축 hwy 고속도로에 1갤런에 몇마일을 가는지 나타내는 변수
qplot(data = mpg, x =hwy)

qplot(data = mpg, x=drv, y=hwy)

qplot(data = mpg, x=drv, y=hwy, geom = "line")

qplot(data = mpg, x=drv, y=hwy, geom = "boxplot")

qplot(data = mpg, x=drv, y=hwy, geom = "boxplot", colour = drv)

?qplot

qplot(mpg, wt, data = mtcars)
qplot(mpg, wt, data = mtcars, colour = cyl)
qplot(mpg, wt, data = mtcars, size = cyl)
qplot(mpg, wt, data = mtcars, facets = vs ~ am)


qplot(1:10, rnorm(10), colour = runif(10))
qplot(1:10, letters[1:10])
mod <- lm(mpg ~ wt, data = mtcars)
qplot(resid(mod), fitted(mod))

f <- function() {
  a <- 1:10
  b <- a ^ 2
  qplot(a, b)
}
f()

# To set aesthetics, wrap in I()
qplot(mpg, wt, data = mtcars, colour = I("red"))

# qplot will attempt to guess what geom you want depending on the input
# both x and y supplied = scatterplot
qplot(mpg, wt, data = mtcars)
# just x supplied = histogram
qplot(mpg, data = mtcars)
# just y supplied = scatterplot, with x = seq_along(y)
qplot(y = mpg, data = mtcars)

# Use different geoms
mtcars
summary(mtcars)
mpg
qplot(mpg, wt, data = mtcars, geom = "path")
qplot(factor(cyl), wt, data = mtcars, geom = c("boxplot", "jitter"))
qplot(mpg, data = mtcars, geom = "dotplot")
#문제에 대한 해답
score <- c(80, 60, 70, 50, 90)
score

mean(score)
total_mean <-mean(score)
median(score)

total_mean

english <- c(90, 80, 60, 70)
english

math <- c(50, 60, 100, 20)
math
class(math)

df_midterm <- data.frame(english, math)
class(df_midterm)

df_midterm

class <- c(1, 1, 2, 2)
class

df_midterm <- data.frame(english, math, class)
df_midterm

mean(df_midterm$english)
mean(df_midterm$math)

df_midterm <- data.frame(english = c(90, 80, 60, 70),
                         math    = c(50, 60, 100, 20),
                         class   = c(1, 1, 2, 2))
df_midterm

#p.88 question -> answer
df_fruit <- data.frame( 제품 = c("사과", "딸기", "수박"),
                        가격 = c(1800, 1500, 3000),
                        판매량 = c(24, 38, 13))
df_fruit

product_mean <-mean(df_fruit$가격)
product_mean

product_sale_mean <-mean(df_fruit$판매량)
product_sale_mean

#p.89 04-3 외부 데이터 이용하기
install.packages("readxl")
library(readxl)

df_exam <- read_excel("excel_exam.xlsx")
df_exam

df_exam1 <- read_excel("d:/Rwork/excel_exam.xlsx")
df_exam1

mean(df_exam$english)
mean(df_exam$science)
summary(df_exam)

#mean function understanding
?mean
x <- c(0:10, 50)
x
xm <- mean(x)
xm
c(xm, mean(x, trim = 0.10))
y<-c(1,2,3)
mean(y)

#no head excel, we lost 1 rows
df_exam_novar <- read_excel("excel_exam_novar.xlsx")
df_exam_novar
#with col_names= F parameter we don't lose 1 rows
df_exam_novar <- read_excel("excel_exam_novar.xlsx", col_names= F)
df_exam_novar

#read csv file
df_csv_exam <- read.csv("csv_exam.csv")
df_csv_exam
summary(df_csv_exam)

df_csv_exam <- read.csv("csv_exam.csv", stringsAsFactors = F)
df_csv_exam
summary(df_csv_exam)
#p.95
df_midterm <- data.frame(english = c(90, 80, 60, 70),
                         math    = c(50, 60, 100, 20),
                         class   = c(1, 1, 2, 2))
df_midterm
write.csv(df_midterm, file = "df_midterm.csv")

df_csv_midterm <- read.csv("df_midterm.csv")
df_csv_midterm

save(df_midterm, file ="df_midterm.rda")

rm(df_midterm)
df_midterm

load("df_midterm.rda")
df_midterm

#p.100 05-1 데이터 파악하기
exam <- read.csv("csv_exam.csv")
exam

head(exam)  # default.. rownum<=6 까지 출력
head(exam, 10) # rownum<=10 까지 출력
head(exam, -4) # 끝에서 4번째까지, 앞에서 16번째까지 나옴
head(exam, -1)


tail(exam)
tail(exam, 10) #뒤에서 10행 출력
tail(exam, 0) #0이면 row.names의 길이가 0 입니다. colmn명만 보여줌
tail(exam,-5)  #-5이면 뒤에서 15행까지 출력(정확히는 앞에서 5개) 

View(exam) #앞에 v가 대문자임
dim(exam) # 행(케이스), 열(변수) 출력
str(exam) #desc 같은것, 변수 속성을 보여줌
summary(exam) # 요약 통계량 산출하기

#p.106
mpg<- as.data.frame(ggplot2::mpg)
head(mpg)
tail(mpg)
dim(mpg)
str(mpg)
View(mpg)
summary(mpg)

#p.110 05-2 변수명 바꾸기
df_raw <- data.frame(var1 = c(1,2,1),
                     var2 = c(2,3,2))
df_raw

install.packages("dplyr")
library(dplyr)

df_new<-df_raw
df_new
class(df_new)

df_new <- rename(df_new, v2 = var2)
df_new

df_raw

class(ggplot2::mpg)

new_mpg <- as.data.frame(ggplot2::mpg)
str(new_mpg)
new_mpg <- rename(new_mpg, city=cty, highway=hwy)
head(new_mpg)

#p.113 05-3 파생변수 만들기
df <- data.frame(var1 = c(4, 3, 8),
                 var2 = c(2, 6, 1))
df

#var_sum, var_mean 컬럼추가
df$var_sum <- df$var1 + df$var2
df

df$var_mean <-(df$var1+df$var2)/2
df
#mpg통합연비변수만들기
mpg$total <- (mpg$cty + mpg$hwy)/2
head(mpg)
str(mpg)
mean(mpg$total)
summary(mpg$total)
hist(mpg$total)

mpg$test <- ifelse(mpg$total >=20, "pass", "fail")
head(mpg, 20)

table(mpg$test)
#hist(mpg$test)  Error in hist.default(mpg$test) : 'x'는 반드시 숫자이어야 합니다
# 문자값으로 빈도그림을 그리려면 ggplot2 패키지에 qplot 함수 사용
library(ggplot2)
qplot(mpg$test)

#mpg_grade 변수 생성, total이 30이상:A, 20~29 :B, 20미만이면 C
mpg$grade <- ifelse(mpg$total >= 30, "A",
                    ifelse(mpg$total >= 20, "B", "C"))
head(mpg, 20)
tail(mpg, 20)
table(mpg$grade)
qplot(mpg$grade)
#table(mpg$test,mpg$grade)
#qplot(mpg$test,mpg$grade)

mpg$grade2 <- ifelse(mpg$total >= 30, "A",
                     ifelse(mpg$total >= 25, "B",
                            ifelse(mpg$total >= 20, "C", "D"))) 
tail(mpg, 20)
table(mpg$grade2)
qplot(mpg$grade2)

#p123 question
midwest <- as.data.frame(ggplot2::midwest)
head(midwest)
str(midwest)

midwest <- rename(midwest, total = poptotal, asian = popasian)
str(midwest)

midwest$asian_total<-(midwest$asian*100/midwest$total)
head(midwest)
hist(midwest$asian_total)
qplot(midwest$asian_total)
summary(midwest$asian_total)
mean(midwest$asian_total)

asian_mean <-mean(midwest$asian_total)
asian_mean

midwest$asia_grade <- ifelse(midwest$asian_total > asian_mean, "large", "small")
head(midwest)
table(midwest$asia_grade)
qplot(midwest$asia_grade)
#p.126 데이터 전처리
library(dplyr)
exam <- read.csv("csv_exam.csv")
exam
summary(exam)
exam %>% filter(class ==1)
exam %>% filter(class != 1)

exam %>% filter(math > 50)
exam %>% filter(english >= 80)

exam %>% filter(class ==1 & math >= 50)
exam %>% filter(math >= 90 | english >= 90)

exam %>% filter(class !=1 | class ==3 | class ==5)

exam %>% filter(class %in% c(1,3,5))
str(exam)
#p.133 question
str(mpg)

mpg1<-mpg %>% filter(displ>=5)
mpg2<-mpg %>% filter(displ<=4)
str(mpg1)
str(mpg2)

mean(mpg1$hwy)
mean(mpg2$hwy)

mf_audi <- mpg %>% filter(manufacturer == "audi")
mf_toyota <- mpg %>% filter(manufacturer == "toyota")

mf_audi
mf_toyota

mean(mf_audi$cty)
mean(mf_toyota$cty)

mf_cfh <- mpg %>% filter(manufacturer %in% c("chevrolet", "ford", "honda"))
mf_cfh
mean(mf_cfh$hwy)

#p.134 06-3 필요한 변수만 추출하기
exam %>% select(math)
exam %>% select(class, math, english)
exam %>% select(-math)
exam %>% select(-math, -english)

exam %>%
  filter(class == 1) %>%
  select(english)

exam %>%
  select(id, math) %>%
  head

#p.138 question 
mpg_cc <- mpg %>% select (class, cty) 
mpg_cc

mpg_cc_suv <- mpg_cc %>% filter(class == "suv") 
mpg_cc_compact <- mpg_cc %>% filter(class == "compact")

mean(mpg_cc_suv$cty)
mean(mpg_cc_compact$cty)

#139p 06-4 순서대로 정렬하기
exam %>% arrange(math)
exam %>% arrange(desc(math))

exam %>% arrange(class, math)
#p.141 question
mpg %>% 
  filter(manufacturer == "audi") %>%
  arrange(desc(hwy)) %>%
  head(5)

#p.142 06-5
exam %>%
  mutate(total = math + english + science) %>%
  head

exam

exam %>%
  mutate(test = ifelse(science >= 60, "pass", "fail")) %>%
  head

exam %>%
  mutate(total = math + english + science) %>%
  arrange(total) %>%
  head

#p.144 question
mpg_copy <- as.data.frame(ggplot2::mpg)
str(mpg_copy)
mpg_copy <- mpg_copy %>% mutate(total_fuel = cty + hwy)
head(mpg_copy)
mpg_copy <- mpg_copy %>% mutate(avg_fuel= total_fuel/2)
head(mpg_copy)
mpg_copy %>%
  arrange(desc(avg_fuel)) %>%
  head(3)

mpg %>% 
  mutate(total_fuel= cty+hwy, avg_fuel=total_fuel/2) %>%
  arrange(desc(avg_fuel)) %>%
  head(3)
#p.145
exam %>% summarise(mean_math = mean(math))

tack <-exam %>%
  group_by(class) %>%
  summarise(mean_math = mean(math))

class(tack)

exam %>%
  group_by(class) %>%
  summarise(mean_math = mean(math),
            sum_math  = sum(math),
            median_math = median(math),
            n = n())

mpg %>%
  group_by(manufacturer, drv) %>%
  summarise(mean_cty = mean(cty)) %>%
  head(10)

mpg %>%
  group_by(manufacturer) %>%
  filter(class =="suv") %>%
  mutate(total_fl_mean = ((cty+hwy)/2)) %>%
  summarise(mean_tot = mean(total_fl_mean)) %>%
  arrange(desc(mean_tot)) %>%
  head(5)

#p.150 question
#install.packages(dplyr)
library(dplyr)
mpg1<-as.data.frame(ggplot2::mpg)
str(mpg1)
#q1
mpg1 %>%
  group_by(class) %>%
  summarise(class_mean=mean(cty))
#q2
mpg1 %>%
  group_by(class) %>%
  summarise(class_mean=mean(cty)) %>%
  arrange(desc(class_mean))
#q3
mpg1 %>%
  group_by(manufacturer) %>%
  summarise(hwy_mean=mean(hwy)) %>%
  arrange(desc(hwy_mean)) %>%
  head(3)
#q4
mpg1 %>%
  arrange(desc(class)) %>% 
  group_by(manufacturer) %>%
  summarise(n=n()) %>%
  arrange(desc(n))

#p.151 데이터 합치기-컬럼합치기
test1 <- data.frame(id = c(1, 2, 3, 4, 5),
                    midterm = c(60, 80, 70, 90, 85))
test2 <- data.frame(id = c(1, 2, 3, 4, 5),
                    final = c(70, 83, 65, 95, 80))
test1
test2
total <- left_join(test1, test2, by = "id")
total

name <- data.frame(class = c(1, 2, 3, 4, 5),
                   teacher = c("kim", "lee", "park", "choi", "jung"))
name
exam
exam_new <- left_join(exam, name, by = "class")
exam_new

#row 합치기
group_a <- data.frame(id = c(1, 2, 3, 4, 5),
                      test = c(60, 80, 70, 90, 85))
group_b <- data.frame(id = c(6, 7, 8, 9, 10),
                      test = c(70, 83, 65, 95, 80))
group_a
group_b

group_all <- bind_rows(group_a, group_b)
group_all

fuel <- data.frame(f1 = c("c", "d", "e", "p", "r"),
                   price_f1 = c(2.35, 2.38, 2.11, 2.76, 2.22),
                   stringsAsFactors = F)
fuel

mpg <- as.data.frame(ggplot2::mpg)
head(mpg)
str(mpg)
#fuel의 컬럼명을 f1으로 잘못만들어서 fl로 rename
help(rename)
fuel <-rename(fuel, fl = f1, price_fl = price_f1)
fuel
mpg <- left_join(mpg, fuel, by = "fl")
mpg

mpg %>%
  select(model, fl, price_fl) %>%
  head(5)

#p.160 question
#q1
class(ggplot2::midwest)
midwest <- as.data.frame(ggplot2::midwest)
head(midwest)
str(midwest)

midwest <-midwest %>%
           mutate(perpopchild = (poptotal-popadults)*100/poptotal)
#q2
midwest %>%
  select(county, perpopchild) %>%
  arrange(desc(perpopchild)) %>%
  head(5)
#q3
midwest1 <-midwest %>%
           mutate(child_gubun = ifelse(perpopchild>=40, "large",
                                       ifelse(perpopchild>=30, "middle", "small")))       
head(midwest1)
View(midwest1)
table(midwest1$child_gubun)

#q4
midwest1 <- midwest1 %>%
            mutate(per_asian = popasian*100/poptotal)
head(midwest1)
str(midwest1)

midwest1 %>%
  select(state, county, per_asian) %>%
  arrange(per_asian) %>%
  head(10)
