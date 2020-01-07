# chap13_1_DecisionTree

# 관련 패키지 설치
install.packages("rpart")
library(rpart)

# tree 시각화 패키지
install.packages("rpart.plot")
library(rpart.plot)


# 1. dastset(train/test) : iris

idx <- sample(nrow(iris), nrow(iris)*0.7, replace = F)
train <- iris[idx,]
test <- iris[-idx,]
names(iris)

# 2. 분류 모델
model <- rpart(Species ~ ., data=train)
model # 트리구조를 만드는 구조 표시

# 분류 모델 시각화
rpart.plot(model)
# *표시 자식이 없는 terminal node
# 엔트로피값을 통해 가장 중요한 변수 판단해서 root쪽에 올림
# [중요변수] : rootNode 분할에 사용된 변수 : Petal.Length

# 분류 모델 평가
y_pred <- predict(model,test) #모델의 평가를 위해 결과치를 넣음, # 비율 예측치치

y_pred_group <- predict(model,test,type = "class") #그룹으로 

y_true <- test$Species

# 교차 분할표(confusion matrix)
table(y_true, y_pred_group)
#                     y_pred_group
# y_true       setosa versicolor virginica
# setosa         19          0         0
# versicolor      0          9         2
# virginica       0          3        12

acc <- (19+9+12) / nrow(test)


###########################
### Titanic 분류 분석
###########################

titanic3 <- read.csv("data/titanic3.csv")

str(titanic3)
# titanic3.csv 변수 설명
#'data.frame': 1309 obs. of 14 variables:
#1.pclass : 1, 2, 3등석 정보를 각각 1, 2, 3으로 저장
#2.survived : 생존 여부. survived(생존=1), dead(사망=0)
#3.name : 이름(제외)
#4.sex : 성별. female(여성), male(남성)
#5.age : 나이
#6.sibsp : 함께 탑승한 형제 또는 배우자의 수
#7.parch : 함께 탑승한 부모 또는 자녀의 수
#8.ticket : 티켓 번호(제외)
#9.fare : 티켓 요금
#10.cabin : 선실 번호(제외)
#11.embarked : 탑승한 곳. C(Cherbourg), Q(Queenstown), S(Southampton)
#12.boat     : (제외)Factor w/ 28 levels "","1","10","11",..: 13 4 1 1 1 14 3 1 28 1 ...
#13.body     : (제외)int  NA NA NA 135 NA NA NA NA NA 22 ...
#14.home.dest: (제외)

# 의미없는 변수 제거 : 이름, 티켓번호, 선실번호, 보트, 바디, 집 목ㅍ

# 생존여부로 y변수로 나머지를 분류 모형으로 

# int -> Factor(범주형) 
titanic3$survived <- factor(titanic3$survived, levels =c(0,1))
table(titanic3$survived)

# subset 생성
titanic <- titanic3[-c(3,8,10,12:14)]
str(titanic)

# survived : Facotr(w/2)


idx <- sample(nrow(titanic), nrow(titanic)*0.8, replace = F)
train <- titanic[idx,]
test <- titanic[-idx,]

# 모델 생성
model <- rpart(survived ~ ., data=train)

# 모델 시각화
rpart.plot(model)

y_pred <- predict(model, test, type="class")
y_true <- test$survived

table(y_true, y_pred)

#         y_pred
# y_true   0   1
#      0 142  17
#      1  34  69

accuracy <- (142+69) / nrow(test) # 0.8053435

# 결과 unbalance

recall <- 69/(34+69) 
precision <- 69/(17+69)
f1_score <- 2* (precision*recall) / (precision+recall) #0.7301587




