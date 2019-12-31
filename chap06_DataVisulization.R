#chap06_DataVisulization

# 1. 이산변수(discrete quantitative data) 시각화
# - 정수단위로 나누어 측정할 수 있는 변수(딱딱 떨어져야함)

# 차트 데이터 생성

chart_data <- c(305,450, 320, 460, 330, 480, 380, 520) 
names(chart_data) <- c("2016 1분기","2017 1분기","2016 2분기","2017 2분기","2016 3분기","2017 3분기","2016 4분기","2017 4분기")
str(chart_data)
chart_data


# 1) 막대 차트 시각화
barplot(chart_data, ylim = c(0,600),col = rainbow(8), main = "2016 vs 2017년도 분기별 매출 현황")

?barplot

# 1-2) 가로막대 차트 ylim -> xlim 축위치 바꾸고, horiz 옵션을 True 로 주면 됨됨
barplot(chart_data, xlim = c(0,600),col = rainbow(8),horiz = T, main = "2016 vs 2017년도 분기별 매출 현황")

# 1행2열 구조
par(mfrow=c(1,2)) # 1행 2열 그래프 보기<그래프 2개보기>

# 개별 막대차트 beside = true
barplot(VADeaths, beside=T,col=rainbow(5), main="미국 버지니아주 하위계층 사망비율")
# 누적형 막대차트 beside = false
barplot(VADeaths, beside=F,col=rainbow(5), main="미국 버지니아주 하위계층 사망비율")

# 2) 점 차트 시각화
par(mfrow=c(1,1)) # 1행 1열
dotchart(chart_data, color = c("green", "red"), labels=names(chart_data), xlab = "매출액", main="분기별 판매 현황")

# 3) 파이 차트 시각화 
# col = color, cex = 글자 확대
pie(chart_data, labels = names(chart_data), col=rainbow(8), cex= 1.2)
title("2016 vs 2017 판매 현황황") # 직전에 그린 차트에 title 넣는 함수

?pie


# 2.연속변수 (continuous quantitative data)
# - 시간, 길이 등과 같이 연속성을 가진 실수 단위 변수값

#  1) 상자 그래프 시각화 : 요약통계치를 좀더 보기 쉽게 표기
#    - 이상치를 표기할 때 표기함
VADeaths
str(VADeaths)

summary(VADeaths)

# box 그래프 range = 0 : 점선 표기기
boxplot(VADeaths, range=0)

# 2) 히스토그램 시각화 : 대칭성 확인
iris
head(iris)

mean(iris$Sepal.Length)
range(iris$Sepal.Length)

hist(iris$Sepal.Length, xlab = "sepal.Length" , col = "magenta", main = "iris 꽃받침 길이", xlim = c(4.3, 7.9))

mean(iris$Sepal.Width)
range(iris$Sepal.Width) # 2.0 4.4

hist(iris$Sepal.Width, xlab = "sepal.Width" , col = "green", main = "iris 꽃받침 넓이", xlim = c(2.0, 4.4))

# 정규 분포에 대한 가설 : 정규분포와 차이가 없다
# 정규성 검정 (p-value 값, )
shapiro.test(iris$Sepal.Length)  # 정규성 떨 어 짐 : 기각
# p-value = 0.01018 < 알파 0.05 : 정규성이 아니다라고 판단 
shapiro.test(iris$Sepal.Width)   # 정규성 띄고있음 : 채택 
# p-value = 0.1012 > 알파 0.05 : 정규성이라고 판단함 


# 3) 산점도 시각화
price <- runif(10,min=1,max=100)
price

plot(price) #기본 plot(x,y)지만 
# parameter가 한개인경우 y값에 들어가고 x값에는 index가 들어감

par(mfrow=c(2,2))
plot(price, type="o") # circle and line
plot(price, type="l") # line
plot(price, type="h") #height
plot(price, type="s") #step


# 만능 차트
par(mfrow=c(1,1))
data()

AirPassengers

# 시계열 데이터 시각화
plot(AirPassengers)
title("time to AirPassengers")

plot(WWWusage)


# 회귀모델 -> 회귀모델 시각화 자료
install.packages("HistData")
library(HistData)

library(hep="HistData")

?HistData

data(Galton)
str(Galton)
# Galton
# 'data.frame':	928 obs. of  2 variables:
# $ parent , $ child

# lm() x~y에 대한 수식을 만들어주는 함수
# lm(y~x) y : 영향을 받는 함수, x : 영향을 주는 함수
model <-lm(child ~ parent, data = Galton)
model

# 회귀모델 관련 시각화 정보를 제공
# 관련모델의 여러가지 차트를 보여줌
plot(model)

# REsiduals, err = y-x : 오차, 잔차 
# 실제 예측치 대비 오차를 보여주는 그래프

methods(plot)
# 다양한 차트를 보여준다리 
# hclust - 군집도 분석


# 3. 변수간의 비교 시각화
?pairs
pairs(iris[1:4]) # 5th index - Factor

#꽃의 종별 변수 비교
unique(iris$Species)
# setosa, versicolor, virginica

pairs(iris[iris$Species== 'setosa',1:4]) # 전체가 아닌 종별로 비교
pairs(iris[iris$Species== 'versicolor',1:4])
pairs(iris[iris$Species== 'virginica',1:4])

# 4. 차트파일 저장
getwd()
jpeg("data/output/iris.jpg", width=720,height = 480)
plot(iris$Sepal.Length, iris$Petal.Length)
title("iris dataset visualization")
dev.off()

##########################
# 3차원 산점도
##########
install.packages('scatterplot3d')
library(scatterplot3d)

# 꽃의 종류별 분류 
iris_setosa = iris[iris$Species == 'setosa',]
iris_versicolor = iris[iris$Species == 'versicolor',]
iris_virginica = iris[iris$Species == 'virginica',]

# scatterplot3d(밑변, 오른쪽변, 왼쪽변, type='n') # type='n' : 기본 산점도 제외 
# frame 만들고 데이터를 집어넣음
d3 <- scatterplot3d(iris$Petal.Length, iris$Sepal.Length, iris$Sepal.Width, type='n')

d3$points3d(iris_setosa$Petal.Length, iris_setosa$Sepal.Length,
            iris_setosa$Sepal.Width, bg='orange', pch=21)

d3$points3d(iris_versicolor$Petal.Length, iris_versicolor$Sepal.Length,
            iris_versicolor$Sepal.Width, bg='blue', pch=23)

d3$points3d(iris_virginica$Petal.Length, iris_virginica$Sepal.Length,
            iris_virginica$Sepal.Width, bg='green', pch=25)

