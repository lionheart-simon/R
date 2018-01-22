#doitR from p.162
#����ġ �����ϱ�

df<-data.frame(sex = c("M","F", NA, "M", "F"),
               score = c(5, 4, 3, 4, NA))
df

#����ġ Ȯ�� , ����ġ�� TRUE�� ǥ�� is.na�� is it NA? �ǹ���
is.na(df)

table(is.na(df)) #����ġ �� ��� (FALSE �, TRUE �)

table(is.na(df$sex))
table(is.na(df$score))

#NA�� �ִ� ����Ż�� �Լ��� �����ϸ� ���������� ������� �ʰ� NA�� ���
mean(df$score)

#����ġ �����ϱ�
library(dplyr)
#score�� NA�� ����Ÿ�� ���
df %>% filter(is.na(score))

#score�� NA�� �ƴ� ����Ÿ�� ���
df %>% filter(!is.na(score))

df_nomiss <- df %>% filter(!is.na(score))
df_nomiss
mean(df_nomiss$score)
sum(df_nomiss$score)

# & ������ Ȱ���Ͽ� score�Ӹ� �ƴ϶� sex���� ����ġ�� ���� ����Ÿ �����ϱ�
df_nomiss <- df %>% filter( !is.na(score) & !is.na(sex) )
df_nomiss

#����ġ�� �ϳ��� ������ �����ϱ�
#na.omit (null�� �ź��Ѵ�?)
df_nomiss2 <- na.omit(df)
str(df)
summary(df)
glimpse(df)
class(df)
typeof(df)

#na.rm�� na�� remove�ϰ� �Լ��� ������� 
mean(df$score, na.rm = T)
mean(df$score, na.rm = TRUE)
mean(df$score, na.rm = 0)
mean(df$score, na.rm = 1) # TRUE =1

sum(df$score, na.rm = T)

exam <- read.csv("csv_exam.csv")
exam
#math�׸� 3,8,15�� ���� NA�� ����
exam[c(3, 8, 15), "math"] <- NA 
exam

#NA�� �־ ��� ���ϸ� NA
exam %>% summarise( mean_math = mean(math))

#na.rm���� ����ġ�� �����ϰ� ��� ������
exam %>% summarise( mean_math = mean(math, na.rm = T)) 

#na.rm���� ����ġ�� �����ϰ� ���, �հ�, �߾Ӱ� ������
exam %>% summarise(mean_math = mean(math, na.rm = T),
                   sum_math  = sum(math, na.rm = T),
                   median_math = median(math, na.rm = T))

#����ġ ��ü�ϱ�(Imputation)
#��հ����� ����ġ�� ��ü�غ���
mean(exam$math, na.rm = T)
#��հ� 55.23529�� 55��� �ϸ�..
exam$math <- ifelse(is.na(exam$math), 55, exam$math)
table(is.na(exam$math))
#NA���� 3,8,15���� 55�� ��ü��
exam
mean(exam$math)

#question p.170
#mpg�� ����ġ�� ���� �м����� �ذ�

mpg1<- as.data.frame(ggplot2::mpg)
mpg1
mpg1[c(65, 124, 131, 153, 212), "hwy"] <- NA
mpg1
summary(mpg1)
tail(mpg1,30)

#q1.drv, hwy�� ����ġ�� � �ִ°�?
table(is.na(mpg1$drv))
table(is.na(mpg1$hwy))
table(is.na(mpg1$drv), is.na(mpg1$hwy))
table(is.na(mpg1$hwy), is.na(mpg1$drv))
summarise(mpg1)
#q2. filter()�� ���O�� hwy������ ����ġ�� �����ϰ� 
#� ��������� hwy����� ������..
mpg1 %>% 
  filter(!is.na(mpg1$hwy)) %>%
  group_by(mg1$drv) %>%
  summarise(mean_hwy=mean(mpg1$hwy))

#�� ���� ����� �Ʒ� ���� �� filter�� hwy�� �����Ǿ���? drv�� hwy�� na�� �� ����?
#�� 229rows �ε� 234��� �Ҥ�
#Error in mutate_impl(.data, dots) : 
#Column `mpg1$drv` must be length 229 (the number of rows) or one, not 234

#�̷�..mpg1 ����Ÿ �����Ӱ��� %>%�� �����ϹǷ� mpg1$hwy�� �ƴ϶� hwy�� �ؾ�
#���ο� ���� �ƴ� �տ��� ������ �� ���� ó��
mpg1 %>% 
  filter(!is.na(hwy)) %>%
  group_by(drv) %>%
  summarise(mean_hwy=mean(hwy))

#p.171 �̻�ġ �����ϱ�
outlier <- data.frame(sex   = c(1, 2, 1, 3, 2, 1),
                      score = c(5, 4, 3, 4, 2, 6))
outlier
#outlier�� Ȯ���ϱ����� table�� �̿��� ��ǥ�� �����Ѵ�.
table(outlier$sex)
table(outlier$score)

# sex�� 3�ϰ��, score�� 6�ϰ�� �̻�ġ�� ����(NA) ó���ϱ�
outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex)
outlier

outlier$score <- ifelse(outlier$score >5, NA, outlier$score)
outlier

#����ġ�� �������� ������ ���� ��� ������ ����
outlier %>%
  filter(!is.na(sex) & !is.na(score)) %>%
  group_by(sex) %>%
  summarise(mean_score = mean(score))

#p.175 boxplot���� �ش�ġ ���� ���ϱ�
boxplot(mpg$hwy)
summary(mpg)
#���ڱ׸� ���ġ ���
boxplot(mpg$hwy)$stats

#����ġ ����� ����ó���ϱ�
mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)
table(is.na(mpg$hwy))
#hwy�� NA�� ����Ÿ�� ��ȸ�غ���
mpg %>% filter(is.na(hwy))

#����ġ �����ϰ� ��հ��� ������
mpg %>%
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy, na.rm = T))

#question p.178 mpg ������ �̿� �м�
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
#����Ǭ ��
mpg$drv <- ifelse(mpg$drv =='k', NA, mpg$drv)
#���� ��
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

#p.184 ������ �����
library(ggplot2)

#x���� displ, y���� hwy�� ������ ��� ����
ggplot(data = mpg, aes( x = displ, y = hwy))

#��濡 ������ �߰� dylpr�� %>% ��ȣ�ο���, ggplot2�� +�� ����
#�������� ǥ�õ� ������ ������ ����ġ(��)�� �ǹ�
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point()

#x�� ���� 3~6���� ���� xlim ���
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point() + xlim(3, 6)

#x�� ���� 3~6����, y�� ���� 10~30���� ���� xlim, ylim ���
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

#data=,x=, y= �����ص� ���� �׷��� ������
#�׷��� �������鿡�� ���ִ°� ��Ȯ
ggplot(midwest, aes( poptotal, popasian)) +
  geom_point()

ggplot(data = midwest, aes( x = poptotal, y = popasian)) +
  geom_point() +
  xlim(0 , 500000) +
  ylim(0 , 10000)
# x ���� ���� 1e+05 = 1*10�� 5���� �ƴ� 10000���� ǥ���ϱ� ���ؼ��� options(scipen =99) �߰� �ʿ�
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

#p.189 ���� �׷���
library(dplyr)

df_mpg <- mpg %>%
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy))

df_mpg
#���� �׷����� ����� �Լ� geom_col()
ggplot(data = df_mpg, aes(x = drv, y = mean_hwy)) + geom_col()

#ũ������� �����ϱ� ������ ���ĺ� ������ �����ϴµ�
#����ũ������� ���� reorder()���
ggplot(data = df_mpg, aes(x = reorder(drv, -mean_hwy), y = mean_hwy)) + geom_col()

#p.192 �� ����׷��� �����
#y�� ���� x�ุ �����ϰ� geom_col��� geom_bar���, y���� count�� ��
ggplot(data = mpg, aes( x = drv )) + geom_bar()

ggplot(data = mpg, aes( x = hwy)) + geom_bar()

#������ geom_bar => geom_col�� �ٲٸ� �۵��ұ�?
ggplot(data = mpg, aes( x = hwy)) + geom_col()
# Error in pmin(y, 0) : object 'y' not found 
#y�� ���ٰ� ������. ���ǥ�� geom_col, ���ڷ�� geom_bar

#p.193
glimpse(mpg)
head(mpg)
summary(mpg)
str(mpg)
tail(mpg)
table(mpg$class)
#mpg�� ������� �׷��� �Ʒ� �Ѵ� �ȱ׷���
qplot(mpg)
plot(mpg)
#
plot(mpg, main="Scatter plot Matrix")

df_mpg1 <- mpg %>%
  select(displ, year, cyl, drv, cty, hwy)

plot(df_mpg1)
#���������� ��, �Ʒ��� �ȵ�
df_mpg2 <- mpg %>%
  select(displ, year, cyl, drv, cty, hwy, trans  )

plot(df_mpg2)

#q1  ����
df_suv <- mpg %>%
  filter(class == 'suv') %>%
  group_by(manufacturer) %>%
  summarise(mean_cty = mean(cty)) %>%
  arrange(desc(mean_cty)) %>%
  head(5)

df_suv
#��տ��� ���������� �����Ͽ� ���� �׷����� �׸�
ggplot(data = df_suv, aes(x = reorder(manufacturer, - mean_cty), 
                          y = mean_cty)) + geom_col()
          
#q2 � Ŭ����(�ڵ�������)�� ������ �� �׷����� �׸�
ggplot(data = mpg, aes(x = class))+ geom_bar()
#�� �׷������� ���� �ٶ��� ��� �ұ�? �Ʒ��� �� Ʋ��, Ȯ���غ���
ggplot(data = mpg, aes(x = reorder(class, count)))+ geom_bar()
ggplot(data = mpg, aes(x = reorder(class)))+ geom_bar()

#p.194 �ð迭 �׷��� �����
economics
glimpse(economics)
summary(economics)
class(economics)
#geom_line�� ����Ͽ� ������ �Ǿ����� �ð迭 ǥ��
ggplot(data = economics, aes( x = date, y = unemploy )) + geom_line()

#p.195 �������� ������ �ð迭 ǥ��
ggplot(data = economics, aes( x = date, y = psavert )) + geom_line()

#p.196 ���ڱ׸� �����
ggplot (data = mpg, aes(x = drv, y = hwy)) + geom_boxplot()

#p.198 q1
df_class1 <- mpg %>%
  filter(class == "compact"| class == "subcompact"| class == "suv")
#���� ���� ���� Ǯ���µ� ���� ������ ������ �Ʒ���
df_class2 <- mpg %>%
  filter(class %in% ("compact","subcompact","suv"))

#������ �ڿ� ������ �׳� ��ȣ�� �����ϸ� �ȵǰ� c() �Լ��� ����� ��� ��
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