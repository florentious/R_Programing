#################################
## <제13장 연습문제>
################################# 

# 01. mpg 데이터 셋을 대상으로 7:3 비율로 학습데이터와 검정데이터로 각각 
# 샘플링한 후 각 단계별로 분류분석을 수행하시오.

# 조건) x,y 변수선택  
#       독립변수(설명변수) : displ + cyl + year 
#       종속변수(반응변수) : cty

library(rpart)
library(ggplot2)
data(mpg)
str(mpg) 

head(mpg)

# 단계1 : 학습데이터와 검정데이터 샘플링

# 단일 변수만 쓰기위한 데이터 분할
mpg_subset <- subset(mpg,select = c("cty", "displ", "cyl", "year"))

idx <- sample(nrow(mpg_subset), nrow(mpg_subset)*0.7, replace = F)

train <- mpg_subset[idx,]
test <- mpg_subset[-idx,]

# 단계2 : 학습데이터 이용 분류모델 생성 

model <- rpart(cty ~ ., data=mpg_subset) # displ + cyl + year : 결과표 보면 year는 없는걸로 보아 연식은 관계성이 부족해보임
model

# 단계3 : 검정데이터 이용 예측치 생성 및 평가 
y_pred <- predict(model, test)
y_true <- test$cty

# y변수 : 연속형인 경우
cor(y_true, y_pred) # 0.9234987 비슷하게 예측됨
# cov(y_true, y_pred)

# 단계4 : 분류분석 결과 시각화
rpart.plot(model)


# 단계5 : 분류분석 결과 해설





# 02. weather 데이터를 이용하여 다음과 같은 단계별로 의사결정 트리 방식으로 분류분석을 수행하시오. 

# 조건1) rpart() 함수 이용 분류모델 생성 
# 조건2) y변수 : RainTomorrow, x변수 : Date와 RainToday 변수 제외한 나머지 변수로 분류모델 생성 
# 조건3) 모델의 시각화를 통해서 y에 가장 영향을 미치는 x변수 확인 
# 조건4) 비가 올 확률이 50% 이상이면 ‘Yes Rain’, 50% 미만이면 ‘No Rain’으로 범주화

# 단계1 : 데이터 가져오기
library(rpart) # model 생성 
library(rpart.plot) # 분류트리 시각화 

#setwd("c:/Rwork/data")
weather = read.csv("data/weather.csv", header=TRUE) 

# 단계2 : 데이터 샘플링
weather.df <- weather[, c(-1,-14)]
idx <- sample(1:nrow(weather.df), nrow(weather.df)*0.7)
weather_train <- weather.df[idx, ]
weather_test <- weather.df[-idx, ]

# head(weather_train)

# weather.df$RainTomorrow : 범주형

# 단계3 : 분류모델 생성
weather_model <- rpart(RainTomorrow ~ ., data=weather_train)


# 단계4 : 분류모델 시각화 - 중요변수 확인 
rpart.plot(weather_model)

# 단계5 : 예측 확률 범주화('Yes Rain', 'No Rain') 

# y_pred_norm <- predict(weather_model,weather_test)
y_pred <- predict(weather_model,weather_test,type="class")
y_true <- weather_test$RainTomorrow

# str(y-pred_norm) : numeric matrix

# y_pred <- ifelse(y_pred_norm[,1] >= 0.5, 'No Rain', 'yes Rain')
# 이방법으로 해결해도 됨

y_pred <- ifelse(y_pred == "Yes", 'Yes Rain','No Rain' )
y_true <- ifelse(y_true == "Yes", 'Yes Rain','No Rain' )

# 단계6 : 혼돈 matrix 생성 및 분류 정확도 구하기

table(y_true, y_pred)

#            y_pred
# y_true     No Rain Yes Rain
# No Rain       85        3
# Yes Rain      12       10

acc <- (85+10) / nrow(weather_test) # 정확도(accuracy) : 0.8636364

# 불균형이므로 f1_score 확인해봐야함
recall <- 10 / (10+12) #재현율, 민감도 : 0.4545455
precision <- 10 / (3+10) #정확율 : 0.7692308

f1_score <- 2* ((precision * recall) / (precision+recall)) # 0.5714286
