# ��οܰ� ȯ�� 303���� ������ heart�����ͷ� AHD�÷��� �� ȯ�ڵ��� ���庴�� �ִ��� ���θ� ����Ѱ�
df<-read.csv('http://www-bcf.usc.edu/~gareth/ISL/Heart.csv')
class(df)
str(df)
View(df)

install.packages("caret")
library(caret)
#��������� �ڵ� ����� ���Ͽ� ����
set.seed(1000)
intrain<-createDataPartition(y= df$AHD, p= 0.7, list= FALSE)
class(intrain)
str(intrain)
head(intrain)
View(intrain)

train<- df[intrain, ]
test<- df[-intrain,]

str(train)
str(test)
# train�� AHD No:115, Yes:98 ����(�� 213)
summary(train)
# train�� AHD No:49, Yes:41 ����(�� 90)
summary(test)


install.packages("tree")
library(tree)
treemod<-tree(AHD~. , data=train)

#dt1(desiontree graph1)
plot(treemod)
text(treemod)

#����ġ�� Pruning cv.tree�Լ��� ���(cross-validation���)
#decion tree�� �(size)�϶� ���� ������ ������ Ȯ�� 
cv.trees <- cv.tree(treemod, FUN= prune.misclass)
class(cv.trees)
#datafram�� �ƴϸ� View �����ؼ� ������
View(cv.trees)
#dtg2 ������Ʈ�� 6���� ���� ������ ���� 7���� ���ƺ��� ���� å��� 6���� ����
plot(cv.trees)

#6���� ����ġ��
prune.trees <- prune.misclass(treemod, best=6)
class(prune.trees)
#tree class��
str(prune.trees)
head(prune.trees)
#View�� dataframe��밡���ؼ� ������
View(prune.trees)
#dfg3 6���� ����ġ���� ���� Ʈ���׸�
plot(prune.trees)
text(prune.trees, pretty=0)

#7���� ����ġ��(���� ������ �̰� �� ����)
prune.trees7 <- prune.misclass(treemod, best=7)
class(prune.trees7)
#tree class��
str(prune.trees7)
head(prune.trees7)

#dfg4 7���� ����ġ���� ���� Ʈ���׸�
plot(prune.trees7)
text(prune.trees7, pretty=0)

#11���� ����ġ��(���� ������ �̰� �� ����)
prune.trees11 <- prune.misclass(treemod, best=11)
class(prune.trees11)
#tree class��
str(prune.trees11)
head(prune.trees11)

#dfg4 11���� ����ġ���� ���� Ʈ���׸�
plot(prune.trees11)
text(prune.trees11, pretty=0)

# �����ϱ�, ����
install.packages("e1071")
library(e1071)
#6��¥�� prune.trees�� test  ����Ÿ�� ��
treepred <- predict(prune.trees, test, type='class')
confusionMatrix(treepred, test$AHD)
#0.7556 Accuracy  38/11 11/30  ����Ŵ� 68����

#7��¥�� prune.trees7�� test  ����Ÿ�� ��
treepred7 <- predict(prune.trees7, test, type='class')
class(treepred7)
head(treepred7)
str(treepred7)
confusionMatrix(treepred7, test$AHD)
#0.7556 Accuracy  36/9 13/32 ����Ŵ� 68����

#11��¥�� prune.trees11�� test  ����Ÿ�� ��
treepred11 <- predict(prune.trees11, test, type='class')
class(treepred11)
head(treepred11)
str(treepred11)
confusionMatrix(treepred11, test$AHD)
#0.7667 Accuracy  38/10 11/31 ����Ŵ� 69 1����



# �̹����� rpart����Ͽ� decison tree �ۼ�
library(rpart)
rpartmod<-rpart(AHD~. , data=train, method="class")
#dfg5 rpart���
plot(rpartmod)
text(rpartmod)
#rpart�� cv.tree��� printcp�� cross-validation��� ����ġ��
printcp(rpartmod)
#xerror�� ���� ���� split���� ����
#dfg6 plotcp����Ͽ� ���� ���� tree���� ����
plotcp(rpartmod)
class(rpartmod)
#class rpart
str(rpartmod)
head(rpartmod)

#rpartmod�� cptable���߿��� xerror�� ���� ���� ���� ����
ptree<-prune(rpartmod, cp= rpartmod$cptable[which.min(rpartmod$cptable[,"xerror"]),"CP"])
#dfg7 rpart���
#dfg9�� ���غ��� Thal=b �϶� normal�� ����
#Thal factor���� abc�� �����Կ��� ǥ��     : chr [1:3] "fixed" "normal" "reversable"
plot(ptree)
text(ptree)

#rpart�𵨷� ��Ȯ����
rpartpred<-predict(ptree, test, type='class')
confusionMatrix(rpartpred, test$AHD) 
#0.7556 accuracy 39/12 10/29 68���� 


#rpart �׸� ����ϰ� plotting�ϱ� 
#dfg9 ���� �����ϱ� ����plot
install.packages("rattle")
library(rattle)				# Fancy tree plot
library(rpart.plot)			# Enhanced tree plots
library(RColorBrewer)			# Color selection for fancy tree 

fancyRpartPlot(rpartmod)


#party��Ű���� decision tree ����
install.packages("party")
library(party)
partymod<-ctree(AHD~., data=train)
plot(partymod)

#party�� ���� �˾Ƽ� pruning���ذ� �� ����̹Ƿ�  �ٷ� test����Ÿ�� Ȯ��
partypred<-predict(partymod, test)
confusionMatrix(partypred, test$AHD)  
#0.7667  ��Ȯ���� ���� ���� (44/16 5/25 69���� 68�� �ι������ ���� ����)


# �������� �� ��� �����ϱ�, ����
install.packages("e1071")
library(e1071)
#����Ʈ������ ���� df����°� �����ϰ� �ݺ��Ͽ� ����
nb_model <- naiveBayes(AHD~.,data = train)
class(nb_model)
str(nb_model)
nb_model

#test ����Ÿ�� �������� �� ��Ȯ�� Ȯ��
nbpred <- predict(nb_model, test, type='class')
confusionMatrix(nbpred, test$AHD)
#0.8222��Ȯ����.  ( 41/8, 8/33���� 74�� ����)

str(nbpred)
class(nbpred)
head(nbpred)
nbpred
#no�� 49, yes�� 41 �ᱹ �������� ���� ������ �����ְ� �ٷ� �𵨸� ���� ����� �˼� ����
summary(nbpred)
summary(test)

#test/train����Ÿ ���� �����°��� �ٲ㺸��
set.seed(1234)
intrain1<-createDataPartition(y= df$AHD, p= 0.7, list= FALSE)
class(intrain1)
str(intrain1)
summary(intrain1)

train1<- df[intrain1, ]
test1<- df[-intrain1,]

str(train1)
str(test1)
# train�� AHD No:115, Yes:98 ����(�� 213)
summary(train1)
# train�� AHD No:49, Yes:41 ����(�� 90)
summary(test1)

nb_model1 <- naiveBayes(AHD~.,data = train1)
class(nb_model1)
str(nb_model1)
nb_model1

#test ����Ÿ�� �������� �� ��Ȯ�� Ȯ��
nbpred1 <- predict(nb_model1, test, type='class')
confusionMatrix(nbpred1, test$AHD)
#0.8556��Ȯ����.  ( 43/7, 6/34���� 77�� ����)

str(nbpred1)
class(nbpred1)
head(nbpred1)
nbpred
#no�� 49, yes�� 41 �ᱹ �������� ���� ������ �����ְ� �ٷ� �𵨸� ���� ����� �˼� ����
summary(nbpred1)
summary(test1)
#test/train����Ÿ ��� �������� �������� ��Ȯ���� �޶�����