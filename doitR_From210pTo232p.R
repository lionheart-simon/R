#�����ͺм� ������Ʈ p.210
# ��Ű���ε�
install.packages("foreign")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readxl")

# �ε��ϱ�
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

#3.������ �ҷ�����
raw_welfare <- read.spss(file = "d:/down/Koweps_hpc10_2015_beta1.sav",
                         to.data.frame = T)
#���纻 �����
welfare <- raw_welfare
#4.������ �����ϱ�=> Ž��������Ÿ�м�
head(welfare)
head(welfare,1)
tail(welfare)
View(welfare)
dim(welfare)
class(welfare)
str(welfare)
summary(welfare)
#str�� �� �ȳ����°� glimpse�� �׸� �� �߳���
glimpse(welfare)

#5.������ �ٲٱ�
welfare <- rename(welfare,
                  sex = h10_g3,
                  birth = h10_g4,
                  marriage = h10_g10,
                  religion = h10_g11,
                  income = p1002_8aq1,
                  code_job = h10_eco9,
                  code_region = h10_reg7)
 
glimpse(welfare) 

#������ ���� �������� �м� p.213
#1.���������ϱ�
class(welfare$sex)
table(welfare$sex)
#2.��ó��
#����ġ Ȯ��
table(is.na(welfare$sex))
#���� �׸� �̸��ο�
welfare$sex <- ifelse(welfare$sex == 1 , "male", "female")
table(welfare$sex)
qplot(welfare$sex)

#p.216 ���޺������� �� ��ó��
#1.���������ϱ�
class(welfare$income)
summary(welfare$income)
qplot(welfare$income)
# �׷����� �� �� ǥ���ϱ� ���� x���� 0~1000���� ����
qplot(welfare$income) + xlim(0, 1000)

#2.��ó��
#�̻�ġȮ��
summary(welfare$income)
#���� 0�̰ų� 9999�̸� �̻�ġ ����ó��
welfare$income <- ifelse(welfare$income %in% c(0, 9999), NA, welfare$income)
#����ġ Ȯ��
table(is.na(welfare$income))
summary(welfare$income)
# NA���� 12030->12044�� �þ�� Ȯ�ΰ���

#������ ���� �������� �м��ϱ�
sex_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(sex) %>%
  summarise(mean_income = mean(income))

sex_income
class(sex_income)
#�׷��� �����
ggplot(data = sex_income, aes(x = sex, y= mean_income)) + geom_col()

#p.220 ���̿� ������ ����
#���̺��� ���� �� ��ó��
#1.���� �����ϱ�
class(welfare$birth)

summary(welfare$birth)
qplot(welfare$birth)

#2.��ó�� ���̴� 1900~2014�� ����, ��/������ 9999
summary(welfare$birth)
#����ġ Ȯ��
table(is.na(welfare$birth))

#�̻�ġ ���� ó��
welfare$birth <- ifelse(welfare$birth == 9999, NA, welfare$birth)
table(is.na(welfare$birth))

#�Ļ����� ����� -����

welfare$age <- 2015 - welfare$birth +1 
summary(welfare$age)

qplot(welfare$age)

#���̿� ������ ����μ��ϱ�
#1.���̿� ���� ���� ���ǥ �����
age_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(age) %>%
  summarise(mean_income = mean(income))

head(age_income)
age_income

ggplot(data = age_income, aes(x = age, y = mean_income)) +geom_point()
ggplot(data = age_income, aes(x = age, y = mean_income)) +geom_col()
ggplot(data = age_income, aes(x = age, y = mean_income)) +geom_line()

#p.225 ���ɴ뿡 ���� ��������
#�Ļ����� ����� -���ɴ�
welfare <- welfare %>%
mutate(ageg = ifelse(age < 30, "young",
                     ifelse(age <= 59, "middle", "old")))
table(welfare$ageg)
qplot(welfare$ageg)

#���ɴ뿡 ���� �������� �м��ϱ�
ageg_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(ageg) %>%
  summarise(mean_income = mean(income))
ageg_income

#�׷��� �����
ggplot(data = ageg_income, aes( x = ageg, y = mean_income)) + geom_col()

#young- middle- old������ �׷��� ���� scale_x_discrete(limits = c())
ggplot(data = ageg_income, aes( x = ageg, y = mean_income)) +
  geom_col() + 
  scale_x_discrete(limits = c("young", "middle", "old"))

#p.228 ���ɴ�� ���� ��������
sex_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(ageg, sex) %>%
  summarise(mean_income = mean(income))

sex_income

#�׷��� �����
ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col() +
  scale_x_discrete(limits = c("young", "middle", "old"))

#position = "dodge"
ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col(position = "dodge") +
  scale_x_discrete(limits = c("young", "middle", "old"))

#p.231 ���ɴ밡 �ƴ� ���� �� ���� �������̺м��ϱ�
sex_age <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(age, sex) %>%
  summarise(mean_income = mean(income))

head(sex_age)

ggplot(data= sex_age, aes(x = age, y = mean_income, col = sex)) + geom_line()
ggplot(data= sex_age, aes(x = age, y = mean_income, col = sex)) + geom_point()
ggplot(data= sex_age, aes(x = age, y = mean_income, col = sex)) + geom_col(position = "dodge")