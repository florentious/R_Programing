# chap08_Hypothesis_basic

# 가설(Hypothesis) : 어떤 사건을 설명하기 위한 가정
# 검정(Test) : 검정통계량(표본)으로 가설 채택 or 기각
# 추정 : 표본을 통해서 모집단을 확률적으로 추측
# 신뢰구간 : 모수를 포함하는 구간(채택역), 벗어나면(기각역)
# 유의수준 : 알파 표시, 오차범위의 기준
# 구간추정 : 신뢰구간과 검정통계량을 비교해서 가설 기각


# 1. 가설과 검정

# 귀무가설(H0) : 중2학년 남학생 평균키는 165.2cm과 차이가 없다.
# 대립가설(H1) : 중2학년 남학생 평균키는 165.2cm과 차이가 있다.

# 1] 모집단에서 표본 추출(1000 명 학생에 대한 키)
x <- rnorm(1000, mean = 165.2,sd =1) #정규분포를 따르는 난수 1000개


# 정규분포에 따른 대칭성 확인
hist(x)

# 정규성 검정
# 귀무가설 : 정규분포와 차이가 없다
shapiro.test(x)
# W = 0.99824, p-value = 0.3957
# W : 검정통계량 -> p-value : 유의확률
# p-value : 033957 >= 알파(0.05) -> 채택
# 귀무가설 채택 -> 정규분포의 그래프 형태를 보임

# 2] 평균차이 검정 : 평균 165.2cm
t.test(x,mu=165.2) #t.test(표본, 가설평균값)

# t = -0.24816, df = 999, p-value = 0.8041
# t, df : 검정통계량
# p-value : 유의 확률
# p-value : 0.8041 >= 0.05 -> 귀무가설이 채택이 됨
# 귀무가설 채택 : 165.2와 차이가 없다
# 95 percent confidence interval:
# 165.1305 ~ 165.2539 : 신뢰구간(채택역)
# mean of x : 165.1922 -> 1000명에 대한 평균값 : mean(x) -> 통계량(표본의 통계)

# [해설]  : 검정통계량이 신뢰구간에 포함되므로 모수의 평균키가 165.2cm와 차이가 없다고 볼수 있다.

# 3] 기각역의 평균 검정
t.test(x, mu=165.09, conf.level = 0.95) #conf.level = 신뢰수준(95%)

# 귀무가설 : 평균키 165.09cm와 차이가 없다(x)
# 대립가설 : 평균키 165.09cm와 차이가 있다(o)

# t = 3.2495, df = 999, p-value = 0.001195
# p-value : 0.001195 < 유의수준(0.05) ->기각
# 95 percent confidence interval: 
# 165.1305 ~ 165.2539 : 기대값이 기각역에 있음

# 4] 신뢰수준 병경(95% -> 99%)
t.test(x, mu=165.2, conf.level = 0.99)
# t = -0.24816, df = 999, p-value = 0.8041 >= 0.05
# 99 percent confidence interval:
# 165.1110 ~ 165.2734 
# 신뢰수준 향상 -> 신뢰구간 넓어짐(귀무가설 기각이 어려워짐)

#############################
# 2. 표준화 vs 정규화

# 1] 표준화 : 일반 정규분포를 표준 정규분포로 바꿔주는 작업

# 정규분포 
n <- 2000
x <- rnorm(n,100,10)

shapiro.test(x)
# p-value = 0.3426 > 유의수준(알파)
# 정규분포형태임

# 표준화 공식에 의해 표준화 시작
# 표준화 공식(z) = (x-mu(산술평균)) / sd(표준편차)

mu <- mean(x) # 평균계산
z <- (x-mu) / sd(x) # 각각의 항이 표준정규분포의 값으로 바뀌었음
z # 표준 정규분포 완성
hist(z)

mean(z) # 3.929793e-16 => 3.929793 * 10^-16 => 거의 0에 수렴한다
sd(z) # 표준편차 1

# R에서는 표준화 해주는 함수가 있음 : scale()
z2 <- as.data.frame(scale(x))  #matrix 형태로 반환함 -> data.frame 형태로 변환해야함
str(z2) # 2천개의 obs, 1개의 column
z2 <- z2$V1
hist(z2)


# 2] 정규화
#   - 특정 변수값을 일정한 범위(ex.0~1)로 일치시키는 과정
str(iris)
head(iris)
summary(iris[-5]) #5번인덱스 제외 요약 통계값(5번은 factor형임)

# 1) scale() 함수
# 정규화 -> data.frame 형번환(근데 인덱스로 끌어오면 안되나 흠 여러개일 경우 때문인가?)
iris_nor <- as.data.frame(scale(iris[-5]))
summary(iris_nor)

# 2) 정규화 함수 정의 
# nor 함수로 정의하는데 직접 구현해서 사용할때 
nor <- function(x) {
  n <- (x -min(x)) / (max(x) - min(x))
  return(n)
}

iris_nor2 <- apply(iris[-5], 2, nor)
summary(iris_nor2)
head(iris_nor2)


#############################
# 3. 데이터셋에서 샘플링

# sample(x,size,replace) x:데이터의 수, size: 샘플의 수, replace=true 복원추출(꺼낸것 도 꺼낼수 있음) / false시 비복원추출(꺼낸것은 다시 안꺼냄)

no <- 1:100 # 번호
score <- runif(100, min=40,max=100)  #성적

df <- data.frame(no, score)
# nrow(df)

# 표본추출: 15명 샘플링
idx <- sample(x = nrow(df), size=15)
idx

sam <- df[idx,] # sampling 후 추출된 값 저장장
sam

# train(70%)/test(30%) dataset : 비율추출
idx <- sample(x = nrow(df), size = nrow(df)*0.7)

train <- df[idx,] # 훈련셋만들기 #모델 학습용
test <- df[-idx,] # 검정셋       #모델 평가용
dim(train)
dim(test)


#############################
# tran/test model 적용하기 => 머신러닝을 하기위해 훈련용/훈련확인용으로 나누는것

# 훈련셋 50%, 검정셋 50%
idx <- sample(nrow(iris),nrow(iris)*0.5)

train <- iris[idx,]
test <- iris[-idx,]
dim(train)

head(iris)
# Sepal.Length : y(종속변수) -> output(정답/결과)
# Petal.Length : x(독립변수) -> intput
# model : 꽃잎길이-> 꽃받침 길이가 연관있나
model <- lm(Sepal.Length ~ Petal.Length, data = train)
pred <- model$fitted.values # 모델의 예측치(꽃받침 길이)

# test dataset model
model2 <- lm(Sepal.Length ~ Petal.Length, data = test)
pred2 <- model2$fitted.values # 예측치(꽃받침 길이)
 
# train_y : model 의 결과값
train_y <- train$Sepal.Length # train의 결과

# test_y : model2 의 결과값
test_y <- test$Sepal.Length

# 정답과 예측으로 결과 분석하기 -=> 이걸로 도대체 분석이 맞나 싶은걸걸
plot(train_y,pred,col="blue", pch=18) #plot(x,y) : train에 의해서 만든 모델 예측치
points(test_y,pred2,col="red", pch=19) # 이전 그래프에 점모양 추가하는데 쓰나봄




# 범례(추가형태)
legend("topleft",legend=c("train","test"),col=c("blue","red"), pch=c(18,19))









