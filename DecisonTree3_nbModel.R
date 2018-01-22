# 흉부외과 환자 303명을 관찰한 heart데이터로 AHD컬럼에 각 환자들이 심장병이 있는지 여부를 기록한것
df<-read.csv('http://www-bcf.usc.edu/~gareth/ISL/Heart.csv')
class(df)
str(df)
View(df)

install.packages("caret")
library(caret)
#재생가능한 코드 사용을 위하여 세팅
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
# train은 AHD No:115, Yes:98 생성(총 213)
summary(train)
# train은 AHD No:49, Yes:41 생성(총 90)
summary(test)


install.packages("tree")
library(tree)
treemod<-tree(AHD~. , data=train)

#dt1(desiontree graph1)
plot(treemod)
text(treemod)

#가지치기 Pruning cv.tree함수를 사용(cross-validation기법)
#decion tree가 몇개(size)일때 제일 오류가 적은지 확인 
cv.trees <- cv.tree(treemod, FUN= prune.misclass)
class(cv.trees)
#datafram이 아니면 View 사용못해서 에러남
View(cv.trees)
#dtg2 웹사이트는 6개가 제일 낮은데 나는 7개가 낮아보임 먼저 책대로 6개로 진행
plot(cv.trees)

#6개로 가지치기
prune.trees <- prune.misclass(treemod, best=6)
class(prune.trees)
#tree class임
str(prune.trees)
head(prune.trees)
#View는 dataframe사용가능해서 오류남
View(prune.trees)
#dfg3 6개로 가지치기한 최종 트리그림
plot(prune.trees)
text(prune.trees, pretty=0)

#7개로 가지치기(내가 돌린건 이게 더 낮음)
prune.trees7 <- prune.misclass(treemod, best=7)
class(prune.trees7)
#tree class임
str(prune.trees7)
head(prune.trees7)

#dfg4 7개로 가지치기한 최종 트리그림
plot(prune.trees7)
text(prune.trees7, pretty=0)

#11개로 가지치기(내가 돌린건 이게 더 낮음)
prune.trees11 <- prune.misclass(treemod, best=11)
class(prune.trees11)
#tree class임
str(prune.trees11)
head(prune.trees11)

#dfg4 11개로 가지치기한 최종 트리그림
plot(prune.trees11)
text(prune.trees11, pretty=0)

# 예측하기, 모델평가
install.packages("e1071")
library(e1071)
#6개짜리 prune.trees모델 test  데이타로 평가
treepred <- predict(prune.trees, test, type='class')
confusionMatrix(treepred, test$AHD)
#0.7556 Accuracy  38/11 11/30  맞춘거는 68동일

#7개짜리 prune.trees7모델 test  데이타로 평가
treepred7 <- predict(prune.trees7, test, type='class')
class(treepred7)
head(treepred7)
str(treepred7)
confusionMatrix(treepred7, test$AHD)
#0.7556 Accuracy  36/9 13/32 맞춘거는 68동일

#11개짜리 prune.trees11모델 test  데이타로 평가
treepred11 <- predict(prune.trees11, test, type='class')
class(treepred11)
head(treepred11)
str(treepred11)
confusionMatrix(treepred11, test$AHD)
#0.7667 Accuracy  38/10 11/31 맞춘거는 69 1높음



# 이번에는 rpart사용하여 decison tree 작성
library(rpart)
rpartmod<-rpart(AHD~. , data=train, method="class")
#dfg5 rpart사용
plot(rpartmod)
text(rpartmod)
#rpart는 cv.tree대신 printcp로 cross-validation계산 가지치기
printcp(rpartmod)
#xerror가 가장 낮은 split개수 선택
#dfg6 plotcp사용하여 제일 좋은 tree개수 선택
plotcp(rpartmod)
class(rpartmod)
#class rpart
str(rpartmod)
head(rpartmod)

#rpartmod에 cptable값중에서 xerror이 제일 작은 것을 선택
ptree<-prune(rpartmod, cp= rpartmod$cptable[which.min(rpartmod$cptable[,"xerror"]),"CP"])
#dfg7 rpart사용
#dfg9과 비교해볼것 Thal=b 일때 normal과 매핑
#Thal factor값을 abc로 순서먹여서 표현     : chr [1:3] "fixed" "normal" "reversable"
plot(ptree)
text(ptree)

#rpart모델로 정확성평가
rpartpred<-predict(ptree, test, type='class')
confusionMatrix(rpartpred, test$AHD) 
#0.7556 accuracy 39/12 10/29 68동일 


#rpart 그림 깔끔하게 plotting하기 
#dfg9 제일 이해하기 쉬운plot
install.packages("rattle")
library(rattle)				# Fancy tree plot
library(rpart.plot)			# Enhanced tree plots
library(RColorBrewer)			# Color selection for fancy tree 

fancyRpartPlot(rpartmod)


#party패키지로 decision tree 생성
install.packages("party")
library(party)
partymod<-ctree(AHD~., data=train)
plot(partymod)

#party는 지가 알아서 pruning해준게 위 결과이므로  바로 test데이타로 확인
partypred<-predict(partymod, test)
confusionMatrix(partypred, test$AHD)  
#0.7667  정확성이 제일 좋음 (44/16 5/25 69개로 68인 두방법보다 조금 높음)


# 베이지안 모델 사용 예측하기, 모델평가
install.packages("e1071")
library(e1071)
#사이트에서는 위에 df만드는것 동일하게 반복하여 생략
nb_model <- naiveBayes(AHD~.,data = train)
class(nb_model)
str(nb_model)
nb_model

#test 데이타로 베이지안 모델 정확도 확인
nbpred <- predict(nb_model, test, type='class')
confusionMatrix(nbpred, test$AHD)
#0.8222정확도임.  ( 41/8, 8/33으로 74개 맞춤)

str(nbpred)
class(nbpred)
head(nbpred)
nbpred
#no가 49, yes가 41 결국 베이지안 모델은 설명은 안해주고 바로 모델만 제공 결과만 알수 있음
summary(nbpred)
summary(test)

#test/train데이타 랜덤 나누는것을 바꿔보자
set.seed(1234)
intrain1<-createDataPartition(y= df$AHD, p= 0.7, list= FALSE)
class(intrain1)
str(intrain1)
summary(intrain1)

train1<- df[intrain1, ]
test1<- df[-intrain1,]

str(train1)
str(test1)
# train은 AHD No:115, Yes:98 생성(총 213)
summary(train1)
# train은 AHD No:49, Yes:41 생성(총 90)
summary(test1)

nb_model1 <- naiveBayes(AHD~.,data = train1)
class(nb_model1)
str(nb_model1)
nb_model1

#test 데이타로 베이지안 모델 정확도 확인
nbpred1 <- predict(nb_model1, test, type='class')
confusionMatrix(nbpred1, test$AHD)
#0.8556정확도임.  ( 43/7, 6/34으로 77개 맞춤)

str(nbpred1)
class(nbpred1)
head(nbpred1)
nbpred
#no가 49, yes가 41 결국 베이지안 모델은 설명은 안해주고 바로 모델만 제공 결과만 알수 있음
summary(nbpred1)
summary(test1)
#test/train데이타 어떻게 나누느냐 가지고도 정확도가 달라진다