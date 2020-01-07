# chap12_2_LogisticRegression

###############################################
# 15_2. 로지스틱 회귀분석(Logistic Regression) 
###############################################

# 목적 : 일반 회귀분석과 동일하게 종속변수와 독립변수 간의 관계를 나타내어 
# 향후 예측 모델을 생성하는데 있다.

# 차이점 : 종속변수가 범주형 데이터를 대상으로 하며 입력 데이터가 주어졌을 때
# 해당 데이터의결과가 특정 분류로 나눠지기 때문에 분류분석 방법으로 분류된다.
# 유형 : 이항형(종속변수가 2개 범주-Yes/No), 다항형(종속변수가 3개 이상 범주-iris 꽃 종류)
# 다항형 로지스틱 회귀분석 : nnet, rpart 패키지 이용 
# a : 0.6,  b:0.3,  c:0.1 -> a 분류 

# 분야 : 의료, 통신, 기타 데이터마이닝

# 선형회귀분석 vs 로지스틱 회귀분석 
# 1. 로지스틱 회귀분석 결과는 0과 1로 나타난다.(이항형)
# 2. 정규분포 대신에 이항분포를 따른다.
# 3. 로직스틱 모형 적용 : 변수[-무한대, +무한대] -> 변수[0,1]사이에 있도록 하는 모형 
#    -> 로짓변환 : 출력범위를 [0,1]로 조정
# 4. 종속변수가 2개 이상인 경우 더미변수(dummy variable)로 변환하여 0과 1를 갖도록한다.
#    예) 혈액형 AB인 경우 -> [1,0,0,0] AB(1) -> A,B,O(0)

# 로짓변환 vs sigmoid function

# 1) logit 변환(값) : odds ratio(p/(1-p))에 log(자연로그)함수 적용
logitFunction <- function(p) {
  return(log(p/(1-p)))
}

p = 0.5 # 성공확률
odds_ratio <- p /(1-p) # 0
logit1 <- log(odds_ratio) # 이 과정이 로짓변환
# logit1 = 0 : 성공확률이 0.5인경우

p = 1 # 성공확률
logit2 <- logitFunction(p) # Inf

p = 0 # 성공확률
logit3 <- logitFunction(p) # -Inf

# [정리] p = 0.5 : 0, p > 0.5 : Inf, p < 0.5 : -Inf

# 2) sigmoid function
sigmoidFunction <- function(logit) {
  return(1/(1+exp(-logit)))
}

sig1 <- sigmoidFunction(logit1) # 0 -> 0.5
sig2 <- sigmoidFunction(logit2) # Inf -> 1
sig3 <- sigmoidFunction(logit3) # -Inf -> 0

# [정리] logit = 0 : 0.5, logit = Inf : 1, logit = -Inf : 0
# 값의 범위 : 0 ~ 1 확률값(cut off = 0.5) -> 이항분류 적합

# 실제 적용되는 함수가 있으니까 그 함수 이용해서 사용하면 됩니다
# x(숫자형) -> Y(범주형)


# 단계1. 데이터 가져오기
weather = read.csv("data/weather.csv", stringsAsFactors = F) # stringsAsFactors : 스트링으로 이루어진 것은 Factor형으로 변환하지 말고 string 형태로 반환
dim(weather)  # 366  15
head(weather)
str(weather)



# chr 칼럼, Date, RainToday 칼럼 제거 
weather_df <- weather[, c(-1, -6, -8, -14)]
str(weather_df)

# RainTomorrow 칼럼 -> 로지스틱 회귀분석 결과(0,1)에 맞게 더미변수 생성 후 계산하기 위해 숫자형태로 변경(as.numberic())
weather_df$RainTomorrow[weather_df$RainTomorrow=='Yes'] <- 1 
weather_df$RainTomorrow[weather_df$RainTomorrow=='No'] <- 0
weather_df$RainTomorrow <- as.numeric(weather_df$RainTomorrow)
head(weather_df)

#  단계2.  데이터 셈플링
idx <- sample(1:nrow(weather_df), nrow(weather_df)*0.7) # sampling 함수 다른 방법(first parameter-> 1:nrow())
train <- weather_df[idx, ]
test <- weather_df[-idx, ]


#  단계3.  로지스틱  회귀모델 생성 : 학습데이터 
weater_model <- glm(RainTomorrow ~ ., data = train, family = 'binomial') # family = 'binomial' y 결과를 이항으로 만들어줌
weater_model 
summary(weater_model) 
# F검정 통계량이나 설명력 같은 것은 제공하지 않음
# p-value를 통해 가장 영향을 미치는 column 만 제공(*의 갯수)


# 단계4. 로지스틱  회귀모델 예측치 생성 : 검정데이터 
# newdata=test : 새로운 데이터 셋, type="response" : 0~1 확률값으로 예측 
pred <- predict(weater_model, newdata=test, type="response")  
pred 
range(pred, na.rm = T) #na.rm = T 결측치 제외 // 0.003336015 0.996114599 : 0~1사이읭 확률값으로 제공함
summary(pred)
str(pred)  # Named num [1:110] -> numeric vector

# cut off = 0.5 적용
cPred <- ifelse(pred >= 0.5,1,0) #cut off를 적용해서 비가 오냐 안오냐를 정하는 것것
y_true <- test$RainTomorrow # 정답을 가져옴
# 둘다 범주 형이지만 같은 형태로 바꿈(0("no"),1("Yes"))

t <- table(y_true,cPred) #교차분할표(confusion matrix)
#        cPred
# y_true  0  1
#      0 90  2     
#      1  7 10

# 0-0, 1-1 정분류, 1-0, 0-1 오분류

# model 평가 : 분류정확도
acc <- (90+13) / sum(t)   # 정분류/전체체
cat('accuray =', acc) # accuray = 0.9449541 정확도

# 특이도(Specificity) : No -> No //실제 정답이 No인 경우 No가 나오는 경우
Specificity <- 90 / 92
# 재현율(Recall : 민감도) : Yes-> Yes //실제 정답이 Yes인 경우 Yes가 나오는 경우
recall <- 10 / 17 # 0.5882353

# 정확률(Precision) : model(yes) -> 10/12
precision <- 10/12 # 0.8333333

cat('yes = ',recall, ' // no=',Specificity)
# yes =  0.5882353  // no= 0.9782609  : 맑으면 예측이 97.8%정도 예측 성공하나, 비가 올때는 예측이 58.8%정도밖에 예측성공율이 좋지 못하다

# 맑은 경우와 비가 온 경우가 약 1:1이 아니기 때문에 정확도로만 평가할수 없고, 특이도와 재현율을 확인해야함

# F1 Score : no != yes(불균형인 경우)
# pdf . 12-2 6page 5. 오분류표 참조

f1_score <- 2* ((precision * recall) / (precision+recall)) 
f1_score # 0.6896552 (불균형인경우 F측정치)

# ROC curve 그래프
# 커브 기준 아래쪽이 클수록 정분류로 했는지 시각화 한 것
# 논문에서 ROC 커브로 오분류를 증명함함

# 자꾸 위에 놓치고 코딩하는 것(메모리 올리는 것)을 까먹지 말기


### ROC Curve를 이용한 모형평가(분류정확도)  ####
# Receiver Operating Characteristic

install.packages("ROCR")
library(ROCR)

# ROCR 패키지 제공 함수 : prediction() -> performance
pr <- prediction(pred, test$RainTomorrow)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

# 가로축(x) : 특이도, 세로축(y) : 재현율



######################################
### 다항형 로지스틱 회귀 분석
######################################

install.packages("nnet") # 신경망을 기반으로한 라이브러리
library(nnet)


# 이항분류 vs 다항분류
# sigmoid function : 이항분류(y : 0~1, cut off : 0.5기준)
# softmax : 다항분류(y : 0~1, 확률의 합 = 1) # 항이 3개인 경우 각 확률의 합이 1이 되게 만들어야함
# ex) class1=0.8, class2=0.1, class3=0.1

names(iris)
idx <- sample(nrow(iris), nrow(iris)*0.7, replace = F)
train_iris <- iris[idx,]
test_iris <- iris[-idx,]

# 다항분류 model : 학습 : train
multi_model <- multinom(Species ~ . , data = train_iris)
# weights:  18 (10 variable)
# initial  value 115.354290 
# iter  10 value 13.006789
# iter  20 value 5.847643
# iter  30 value 5.778360
# iter  40 value 5.762767
# iter  50 value 5.746480
# iter  60 value 5.746460
# final  value 5.746444 
# 최종 학습 결과 오차율 5.74644

# model 평가 : test
y_pred <- predict(multi_model, test_iris, type="probs")
# type ="probs" 0~1 확률(합=1) : 모든 확률을 보기 위해서 -> softmax함수로 구해진 것
y_pred
y_pred_class <- predict(multi_model, test_iris, type="class")
y_pred_class
# type ="class" : 도메인으로 예측하고자 할때는 class type으로 설정
range(y_pred) # 5.661092e-38(0)    1.000000e+00(1) => 0~1 값으로 예측됨

y_true <- test_iris$Species

t<-table(y_true, y_pred_class) # 정분류 완료

#                     y_pred_class
# y_true       setosa versicolor virginica
# setosa         14          0         0
# versicolor      0         14         0
# virginica       0          0        17


acc <- (t[1,1] + t[2,2] + t[3,3]) / sum(t)
acc # 1 (100% 정확)