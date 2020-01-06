# chap12_1_LinearRegration

############################
# 1. 단순선형회귀 분석
############################
#  - 독립변수(1)와 종속변수(1) 미치는 영향 분석



# get dataset()
getwd()

product <- read.csv("data/product.csv")
str(product)
# 'data.frame':	264 obs. of  3 variables:
# 각 변수는 int형 등간척도로 이루어져있음

# 1) x,y 변수선택

x <- product$"제품_적절성"   #한글로 되어있는 column 명은 양쪽에 double quote를 붙여주는게 정석
y <- product$"제품_만족도" 

df <- data.frame(x,y)

# 2) 회귀모델 생성 : lm
model <- lm(y~x, data=df)
model
# Coefficients: 회귀계수 (기울기, 절편)
# intercept : 절편, x : 기울기
#      0.7789       0.7393  

# 회귀계수를 근거로 회귀 방정식 작성
# 회귀 방정식(y) = 0.7393 * x + 0.7789

head(df)

# x=4, y=3 # 1번관측치

result = 0.7393 * 4 + 0.7789
result

# 오차(잔차) = 관측치(정답) - 예측치
err <- 3 - result
abs(err)
mse <- mean(err^2)       # 평균제곱오차
mse                      # 제곱을 취하는 의미 -> (부호+, 패널티)

names(model) #회귀 모델에서 반납해주는 12개의 컬럼 
# 자주쓰는 3가지 모델델
# "coefficients" : 회귀계수 값
# "residuals" : 오차(잔차) 값 
# "fitted.values" : 적합치(예측치)

model$residuals # 실제 관측치와 예측의 차이를 기록해놓음 vector 형태임
model$residuals[1] #vector 형태이므로 index로 추려낼 수 있음
model$fitted.values #예측치에 대한 값을 vector형태로 가지고 있음


# 3) 회귀모델 분석
summary(model)  # 분석용도로 summary 씀, 분산분석과 비슷

# <회귀모델 분석 순서>
# 1. F검정 : 모델의 유의성 검정
#   검정통계량 374, 자유도 262
#   p-value: < 2.2e-16 <0.05 통계적으로 유의하다
# 2. 모델의 설명력(조절의 설명력 : Adjusted R-squared:  0.5865) - 1에가까울수록 설명을 잘하고 있다.
# 3. x 변수 유의성 검정
#    - t-value 가 0.05 이하면 영향력, intercept의 * 개수로 영향력을 가시적 확인가능(많을수록 많음)
#    - p-value < 0.05


# R-squared = R^2
R <- sqrt(0.5865)
R # 0.7658329 : 비교적 높은 상관계수

# 4) 회귀선 : 회귀방정식에 의해 구해진 직선식(예측치)

# x, y 산점도를 배경에 그린다
plot(df$x,df$y)

# 회귀선을 추가(직선)
abline(model, col="red")


############################
# 2. 다중선형회귀 분석
############################
#  - 독립변수(1)와 종속변수(1) 미치는 영향 분석

install.packages("car")
library(car)

Prestige
str(Prestige)
# 102개의 직업군 대상으로 : 교육수준(교육을 얼마나 받았냐라는 근거), 수입, 여성비율, 평판, census(직원수), 유형(type, factor형)
# 'data.frame':	102 obs. of  6 variables:
row.names(Prestige) # 102개 직업군을 확인할 수 잇는 방법

# 1) subset 생성
newData <- Prestige[c(1:4)] # index가지고 서브셋 만들기(subset 함수 안쓰고)
str(newData)

# 2) 상관분석
cor(newData)
# prestige , education (0.85017689) 상관성이 제일 높음 -> 회귀분석에 설명력으로 제공함
# prestige , income (0.7149057) 비교적 높은(대체적으로) 수입도 좋다

#           education     income       women   prestige
# education 1.00000000  0.5775802  0.06185286  0.8501769
# income    0.57758023  1.0000000 -0.44105927  0.7149057

# income (y)., education(x1), women(x2), prestige(x3)

# 3) 회귀모델 생성 및 수식화
model <- lm(income ~ education + women + prestige, data=newData); # 다중 선형회귀를 만들어주는 수식
model # Coefficients: 계수값을 가지고 음과 양이 관계를 확인해 볼 수 있다.

head(newData)

income <- 12351     # 예측치
education <- 13.11  # x1
women <- 11.16      # x2 
prestige <- 68.8    # x3

#예측치
y_pred <- (177.2)*education + (-50.9)*women + (141.4)*prestige + (-253.8)
y_pred # 11229.57

err <- income - y_pred
err # 1121.432 -> 설명력이 높으면 오차가 0에 가까운 형태로 나타남

# 4) 회귀모델 분석
summary(model)
# 모델 유의성 : p-value <2.2e-16 <0.05 : 귀무가설 기각 => 유의함
# Adjusted R-squared:  0.6323 => 중반이상의 매칭력을 보임

# x 유의성 검정 : 
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -253.850   1086.157  -0.234    0.816    
# education    177.199    187.632   0.944    0.347 (영향없음) < 0.05 : 채택역 사이이므로 영향이 없음
# women        -50.896      8.556  -5.948 4.19e-08 ***    (-값의 t-value : 음의 영향(반비례))
# prestige     141.435     29.910   4.729 7.58e-06 ***    (+값의 t-value : 양의 영향(비례))

# 결과 분석 후에 영향이 없는 변수를 빼고 다시 모델을 만들어주는게 효과적인 모델이 됨
# 현재 예시에서는 education 모델은 변수를 빼야함


############################
# 3. 변수 선택법
############################
#  - 최적 모델을 위한 x변수 선택

newData2 <- Prestige[c(1:5)]
dim(newData2) # 변수는 5개만 선정됨

library(MASS) # 변수 선택을 위한 라이브러리

model2 <- lm (income ~ . ,data=newData2 ) #.은 앞에 적은 y값을 제외하고 전부 

step <- stepAIC(model2, direction = "both") # direction 변수 선택 방법
step # lm(formula = income ~ women + prestige, data = newData2) # x변수는 education, census 이 2개는 별로라서 빼는게 좋다 라는 정보 젲공
# 그래서 이렇게 결과도 계산을 coefficients: 제공( 100%는 아니고 하나의 결과 참고자료로 알 수 있다.)

model3 <- lm(formula = income ~ women + prestige, data = newData2)

summary(model3)

############################
# 4. 기계 학습
############################

iris_data <- iris[-5]
str(iris_data)

# 1) train/test set 분리(70 train vs 30 test)
idx <- sample(x=nrow(iris_data), size = nrow(iris_data)*0.7 , replace = F) # 비복원 추출 70% 추출출
idx

train <- iris_data[idx,] # indexing 된거 훈련셋
test <- iris_data[-idx,] # 정반대는 검정셋

dim(train) # train_set 확인

# 2) model(train)
model <- lm(Sepal.Length ~ . , data=train)

# 예측치를 만들어주는 함수


# 3) model 평가
y_pred <- predict(model, test) # y 예측치를 만든다, test set안에 안에 x,y 변수가 있어야함
y_true <- test$Sepal.Length # y 정답(관측치)

# 평가 : mse (mean square err : 평균제곱오차), cor(상관계수)
mse <- mean((y_true - y_pred)^2)
mse # [1] 0.105182 오차차이

cor(y_true, y_pred) # 0.9281724 => 1에 가까울수록 상관관계가 있다. 매우 높은 상관관계 수치를 얻을 수 있다.
# 평균과 표준편차로 비교를 해봤을때 매우 상관성이 높은 것을 보이고있다.


# y real value vs y prediction
plot(y_true, col="blue", type="o", pch=18)
points(y_pred, col="red", type="o", pch=19)
title("real value vs prediction")
# 범례
legend("topleft",legend=c("real","pred"), col=c("blue","red"), pch=c(18,19))
#legend("bottomright",legend=c("real","pred"), col=c("blue","red"), pch=c(18,19))



##########################################
##  5. 선형회귀분석 잔차검정과 모형진단
##########################################

# 1. 변수 모델링 : 변수 선택 y~x
# 2. 회귀모델 생성 
# 3. 모형의 잔차검정 
#   1) 잔차의 등분산성 검정
#   2) 잔차의 정규성 검정 
#   3) 잔차의 독립성(자기상관) 검정 
# 4. 다중공선성 검사 
# 5. 회귀모델 생성/ 평가 


names(iris)

# 1. 변수 모델링 : y:Sepal.Length <- x:Sepal.Width,Petal.Length,Petal.Width
formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width


# 2. 회귀모델 생성 
model <- lm(formula = formula,  data=iris)
model
names(model)

# model$residuals 잔차검정에 사용하는 그림

# 3. 모형의 잔차검정
plot(model)
#Hit <Return> to see next plot: 잔차 vs 적합값 -> 패턴없이 무작위 분포(포물선 분포 좋지않은 적합) 
#Hit <Return> to see next plot: Normal Q-Q -> 정규분포 : 대각선이면 잔차의 정규성 
#Hit <Return> to see next plot: 척도 vs 위치 -> 중심을 기준으로 고루 분포 
#Hit <Return> to see next plot: 잔차 vs 지렛대값 -> 중심을 기준으로 고루 분포 

# (1) 등분산성 검정 
plot(model, which =  1) # 중앙기준으로 있으면 등분산성(이정도면 등분산성이다.) 이게 아닌경우 u자형태 등
methods('plot') # plot()에서 제공되는 객체 보기 

# (2) 잔차 정규성 검정
attributes(model) # coefficients(계수), residuals(잔차), fitted.values(적합값)
res <- residuals(model) # 잔차 추출 
# res <- model$residuals # 위랑 같은 방법임
shapiro.test(res) # 정규성 검정 - p-value = 0.9349 >= 0.05
# 귀무가설 : 정규성과 차이가 없다.

# 정규성 시각화  
hist(res, freq = F) 
qqnorm(res) #잔차를 가지고 정규성을 살펴보는 차트(대각선으로 생기면 정규성임)

# (3) 잔차의 독립성(자기상관 검정 : Durbin-Watson) 
install.packages('lmtest') # 리니어모델을 검정하는 패키지
library(lmtest) # 자기상관 진단 패키지 설치 
dwtest(model) # 더빈 왓슨 값
# DW = 2.0604, p-value = 0.6013 , 2<=DW<=4 : 귀무가설을 채택함, p-value = 0.6013 > 0.05 귀무가설 채택

# 3가지 검증이 전부 검증되는 경우는 드물다

# 4. 다중공선성 검사 
#  - 독립변수 간의 강한 상관관계로 인해서 발생하는 문제
#  - ex) 생년월일, 생일
library(car)
sqrt(vif(model)) > 2 # TRUE 

# Sepal.Width Petal.Length  Petal.Width 
# FALSE         TRUE         TRUE 
# Petal.Length 와 Petal.Width 를 강한 상관관계가 의심되니 하나는 빼고 모델을 생성할것


# 5. 모델 생성/평가 
formula = Sepal.Length ~ Sepal.Width + Petal.Length 
model <- lm(formula = formula,  data=iris)
summary(model) # 모델 평가

# Sepal.Width   0.59552    0.06933    8.59 1.16e-14 ***
# Petal.Length  0.47192    0.01712   27.57  < 2e-16 ***
# 서로 강한 상관성이 보장됨


