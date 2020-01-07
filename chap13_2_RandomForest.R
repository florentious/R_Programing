# chap13_2_RandomForest

# 패키지 설치
install.packages("randomForest")
library(randomForest)

names(iris)
# randomForest -> 150개를 500개로 sampling 함

##################################
### 분류트리(y변수 : 범주형)
##################################


# 1. model 생성
model<-randomForest(Species ~ ., data=iris)
# 파라미터 생략하면 500개의 dataset 생성 후 500개 tree model 생성


model
# Number of tress : 500 : 트리의 갯수
# No. of variables tried at each split: 2 -> 노드분류에 사용된 변수 갯수


# OOB estimate of  error rate: 4% -> 분류 정확도 96%

# Confusion matrix:
#            setosa versicolor virginica class.error
# setosa         50          0         0        0.00
# versicolor      0         47         3        0.06
# virginica       0          3        47        0.06

?randomForest

# parameter
#ntree : 최대 트리 갯수, mtry = 노드분할하는ㄷ 쓰는 변수
model2 <- randomForest(Species ~ ., data = iris, ntree=1000,mtry=2, importance = T, na.action = na.omit)

model2
# y 가 범주형이어도 classification(confusion matrix), 연속형이면 회귀분석으로 확인

importance(model2)
# x 변수중에 가장 중요한 역할이 3,4번째 값이다 -> (값이 클수록 중요도가 올라감)
# MeanDecreaseGini : 노드 불순도(불확실성) 개선에 기여하는 변수

varImpPlot(model2) # 중요성 보여주는 표 

##################################
### 회귀트리(y변수 : 연속형)
##################################

library(MASS)
data("Boston")

str(Boston)
#crim : 도시 1인당 범죄율 
#zn : 25,000 평방피트를 초과하는 거주지역 비율
#indus : 비상업지역이 점유하고 있는 토지 비율  
#chas : 찰스강에 대한 더미변수(1:강의 경계 위치, 0:아닌 경우)
#nox : 10ppm 당 농축 일산화질소 
#rm : 주택 1가구당 평균 방의 개수 
#age : 1940년 이전에 건축된 소유주택 비율 
#dis : 5개 보스턴 직업센터까지의 접근성 지수  
#rad : 고속도로 접근성 지수 
#tax : 10,000 달러 당 재산세율 
#ptratio : 도시별 학생/교사 비율 
#black : 자치 도시별 흑인 비율 
#lstat : 하위계층 비율 
#medv(y) : 소유 주택가격 중앙값 (단위 : $1,000)



# y= mdev
# x= 13칼럼

# 노드를 분할하는 갯수는 공식이 따로 있음
# 회귀 트리 : 1/3 *p(전체변수 갯수)
# 분류 트리 : sqrt(p(전체변수 갯수))

p=14

mtry <- round(1/3 *p) #반올림
mtry <- floor(1/3 *p) # 절삭

model3 <- randomForest(medv~. , data = Boston, ntree=500,mtry = mtry,importance=T, na.action = na.omit)

model3

# 중요변수 시각화
varImpPlot(model3)





