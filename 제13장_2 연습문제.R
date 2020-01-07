##########################
## 제13-2장 RM 연습문제 
##########################

# 01. weatherAUS 데이터셋을 대상으로 100개의 Tree와 2개의 분류변수를 파라미터로 지정하여 
# 모델을 생성하고, 분류정확도를 구하시오.
#  조건> subset 생성 : 1,2,22,23 칼럼 제외 

#setwd("c:/Rwork/data")
weatherAUS = read.csv("data/weatherAUS.csv") 

weatherAUS_subset <- weatherAUS[-c(1,2,22,23)]
str(weatherAUS_subset)

# 종속변수(y) : RainTomorrow로 판정
newModel <- randomForest(RainTomorrow ~ ., data = weatherAUS_subset, ntree = 100, mtry = 2, importance = T, na.action = na.omit) #중요변수 시각화 하기위해 importance옵션 True
# 결측치 속성 적용해야됨, 결측치가 에러가 있는 경우에

newModel
# OOB estimate of  error rate: 14.47%

# 02. 변수의 중요도 평가를 통해서 가장 중요한 변수를 확인하고, 시각화 하시오. 

# MeanDecreaseAccuracy : 분류 정확도에 기여하는 변수
# MeanDecreaseGini : 노드 불순도(불확실성)에 기여하는 변수수

varImpPlot(newModel)
# Gumidity3pm, Sunshine, Cloud3pm... 순으로 중요도가 높았다
