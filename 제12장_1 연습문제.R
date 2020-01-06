#################################
## <제12장 연습문제>
################################# 

# 01. mpg의 엔진크기(displ)가 고속도록주행마일(hwy)에 어떤 영향을 미치는가?    
# <조건1> 단순선형회귀모델 생성 
# <조건2> 회귀선 시각화 
# <조건3> 회귀분석 결과 해석 : 모델 유의성검정, 설명력, x변수 유의성 검정  

library(ggplot2)
data(mpg) # 자동차의 연비를 기록한 dataset

mpg

x <- mpg$displ
y <- mpg$hwy

df <- data.frame(x,y)  # 데이터 프레임으로 안하면 값이 이상하게 나옴

# 회귀모델 생성
model <- lm(y~x, data=df)
model

# 회귀모델 시각화
# - 산점도 표현
plot(df$x,df$y)
# text 추가(산점도 안에 y가 가지고 있는 실제값을 넣어줌)
text(df$x, df$y , labels = df$y, cex = 0.5, pos = 3, col="blue") # cex : 글자크기, pos = 3 산점도 위(참고로 1은 산점도 아래쪽에 표기)
abline(model, col="red")

# <조건3> 회귀분석 결과 해석 : 모델 유의성검정, 설명력, x변수 유의성 검정
summary(model)

# F검정 통계량 : F-statistic: 329.5 on 1 and 232 DF : p-value < 2.2e-16 <0.05 통계적으로 유의함
# 설명력 Adjusted R-squared:  0.585 
# x의 유의성 검정 : t=-18.15 < 1.96 : 유의미한 수준 , p<2e-16


# 02. product 데이터셋을 이용하여 다음과 같은 단계로 다중회귀분석을 수행하시오.

product <- read.csv("data/product.csv", header=TRUE)
head(product)
nrow(product)

#  단계1 : 학습데이터(train),검정데이터(test)를 7 : 3 비율로 샘플링

idx <- sample(x=nrow(product),size = nrow(product)*0.7, replace = F)
idx

train <- product[idx,]
test <- product[-idx,]


#  단계2 : 학습데이터 이용 회귀모델 생성 
#           변수 모델링) y변수 : 제품_만족도, x변수 : 제품_적절성, 제품_친밀도

y <- train$제품_만족도


model <- lm(제품_만족도 ~ . , data=train)


#  단계3 : 검정데이터 이용 모델 예측치 생성
y_pred <- predict(model, test)
y_true <- test$제품_만족도



#  단계4 : 모델 평가 : cor()함수 이용  
mse <- mean((y_pred - y_true)^2) # => 애매한 오차율
mse
cor(y_pred, y_true) # 0.7662089 => 적절한 상관관계를 가지고 있다(?)

# 03. ggplot2패키지에서 제공하는 diamonds 데이터 셋을 대상으로 
# carat, table, depth 변수 중 다이아몬드의 가격(price)에 영향을 
# 미치는 관계를 다중회귀 분석을 이용하여 예측하시오.
#조건1) 다이아몬드 가격 결정에 가장 큰 영향을 미치는 변수는?
#조건2) 다중회귀 분석 결과를 정(+)과 부(-) 관계로 해설

library(ggplot2)
data(diamonds)

head(diamonds)

newDiamonds <- diamonds[c("price","carat","table","depth")]
head(newDiamonds)

newModel <- lm(price ~ . , data=newDiamonds)

summary(newModel)
# Coefficients:
#              Estimate Std. Error   t value   Pr(>|t|)    
#   (Intercept) 13003.441    390.918   33.26   <2e-16 ***
#   carat        7858.771     14.151  555.36   <2e-16 ***
#   table        -104.473      3.141  -33.26   <2e-16 ***
#   depth        -151.236      4.820  -31.38   <2e-16 ***

# t-value 가 +- 1.96 이내에 없고 p-value가 0.05보다 작으므로 모두 가격에 관계가 있다.(*의수로도 확인이 가능함)
# t-value 가 0보다 작은 table, depth 에 대해서는 음의 상관관계가 있고
# t-value 가 0보다 큰 carat 에 대해서는 매우 큰 양의 상관관계를 가진다.

# abs(t-value)인 carat 이 3개의 변수(carat, table, depth)에 대해서는 가장 큰 영향을 미치는 것으로 판단된다






