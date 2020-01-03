# chap10_Ttest_anova


# 1-1. 단일집단 비율차이 검정
#  - 비모수 검정

#  1) data 가져오기

getwd()

data <- read.csv("data/one_sample.csv")
head(data)

#  2) 빈도/통계량
x <- data$survey
summary(x)
table(x)
# 0(불만)   1(만족) 
# 14        136 


#  3) 가설검정
#binom.test( 성공횟수, 시행횟수, p=확률)

binom.test(14,150,p=0.2)
# p-value = 0.0006735 < 0.05 귀무가설 기각

# alternative : 양측검정이니 단측검정이니
binom.test(14,150,p=0.2, alternative ="two.sided", conf.level = 0.95)

?binom.test


# 대립가설 채택 : 방향성이 있는 가설
# 기존의 비율과 새로운 집단의 비율을 확인
# 2014(20%) > 2015(14) : 대립가설 기각 (귀무가설 채택)
binom.test(14,150,p=0.2,alternative = "greater", conf.level = 0.95) # 단측검정에는 greater 넣음
# p-value = 0.9999 > 0.05 

# 2014(20%) < 2015(14)
binom.test(14,150,p=0.2,alternative = "less", conf.level = 0.95) # 단측검정 체크에는 크다: greater, 작다 less
# p-value = 0.0003179 <0.05 : 대립가설 기각(귀무가설 채택)



# 1-2. 단일집단 평균차이 검정

# 1) data 가져오기
data <- read.csv("data/one_sample.csv")
head(data)

time <- data$time
length(time) # 150

# 2) 전처리
mean(time, na.rm = T) # 5.556881

x <- na.omit(time)
x
length(x) # 109

# 3) 정규성 판정
shapiro.test(x)
# p-value = 0.7242 > 0.05 : 정규분포 판정 완료


# 4) 단일집단 평균차이 검정
t.test(x, mu=5.2)
# t = 3.9461, df = 108, p-value = 0.0001417
# p-value < 0.05 기각
?t.test
# t: -1.96 ~ + 1.96 (95%)

# 대립가설 : 단측검정(방향성)
# A회사 > 국내(0)
t.test(x,mu=5.2,alternative = "greater")
# p-value = 7.083e-05 < 0.05

# A회사 < 국내(0) 대립가설 채택택
t.test(x,mu=5.2,alternative = "less")
# p-value = 0.9999 > 0.05



# 2-1. 두 집단 비율검정
# 1) 실습데이터 가져오기
data <- read.csv("data/two_sample.csv", header=TRUE)
data
head(data) # 변수명 확인

# 2) 두 집단 subset 작성
data$method # 1, 2 -> 노이즈 없음
data$survey # 1(만족), 0(불만족)
# 데이터 정체/전처리
x<- data$method # 교육방법(1, 2) -> 노이즈 없음
y<- data$survey # 만족도(1: 만족, 0:불만족)
x;y


table(x,y)
#    y
# x     0   1
#    1  40 110  ->집단1
#    2  15 135  ->집단2

# 3) 두 집단 비율차이검증 - prop.test()
help(prop.test) # prop.test(x,n,p, alternative, conf.level, correct)
# 양측검정
prop.test(c(110,135),c(150,150)) # 방법A 만족도와 방법B 만족도 차이 검정
# p-value = 0.0003422
#sample estimates: 집단 간 비율
# prop 1 prop 2
#0.7333333 0.9000000
prop.test(c(110,135),c(150,150), alternative="two.sided", conf.level=0.95)
# 해설) p-value = 0.0003422 - 두 집단간의 만족도에 차이가 있다.
# 대립가설 : 단측검정(방법1-ppt,방법2-실습)
prop.test(c(110,135),c(150,150), alter="greater", conf.level=0.95)
# 해설) p-value=0.9998 : 방법A가 방법B에 비해 만족도가 낮은 것으로 파악


# 2-2. 두 집단 평균차이 검정
# 1) 실습파일 가져오기
data <- read.csv("data/two_sample.csv", header=TRUE)
data
print(data)
head(data) #4개 변수 확인
summary(data) # score - NA's : 73개

# 2) 두 집단 subset 작성(데이터 정제, 전처리)
result <- subset(data, !is.na(score), c(method, score))
# c(method, score) : data의 전체 변수 중 두 변수만 추출
# !is.na(score) : na가 아닌 것만 추출
# 위에서 정제된 데이터를 대상으로 subset 생성
result # 방법1과 방법2 혼합됨
length(result$score) # 227

# 데이터 분리
# - 1> 교육방법 별로 분리
a <- subset(result,method==1)
b <- subset(result,method==2)
# - 2> 교육방법에서 점수 추출
a1 <- a$score
b1 <- b$score
# 기술통계량 -> 평균값 적용 -> 정규성 검정 필요
length(a1); # 109
length(b1); # 118


# 3) 분포모양 검정 : 두 집단의 분포모양 일치 여부 검정
# 귀무가설 : 두 집단 간 분포의 모양이 동질적이다.
# 두 집단간 동질성 비교(분포모양 분석)
var.test(a1, b1) # p-value = 0.3002 -> 차이가 없다.
# 동질성 분포 : t.test()
# 비동질성 분포 : wilcox.test()

# 4) 가설검정 – 두 집단 평균 차이검정
t.test(a1, b1)
t.test(a1, b1, alter="two.sided", conf.int=TRUE, conf.level=0.95)
# p-value = 0.0411 - 두 집단간 평균에 차이가 있다.
t.test(a1, b1, alter="greater", conf.int=TRUE, conf.level=0.95)
# p-value = 0.9794 : a1을 기준으로 비교 -> a1이 b1보다 크지 않다.
t.test(a1, b1, alter="less", conf.int=TRUE, conf.level=0.95)
# p-value = 0.02055 : a1이 b1보다 작다.



# 3. 분산분석
# 두 집단 이상의 평균차이 검정(집단 간 분산차이 검정)
# 일원배치분산분석 : 독립변수(x), 종속변수(y)
# cf) 이원배치 분산분석 : y ~ x1 + x2

#aov(y~x, data = dataset)

# 독립변수 : 집단변수(범주형)
# 종속변수 : 연속형(비율, 등간척도)
# ex) 쇼핑몰 고객의 연령대(20,30,40,50)별 구매금액(연속)
# 독립변수 : 연령대, 종속변수 : 구매금액

# 귀무가설 : 집단별 평균(분산)의 차이가 없다.
# 대립가설 : 적어도 한 집에 평균 차이가 있다.

##########################
### iris dataset
##########################

# 가설설정 
# 귀무가설 : 꽃의 종별로 꽃받침의 길이 차이가 없다.(x)

# 1. 변수 선택
str(iris)
x <- iris$Species #집단변수
y <- iris$Sepal.Width #연속형

# 2. data 전처리

# 3. 동질성 검정
bartlett.test(y~x, data=iris) #iris 생략가능

bartlett.test(Sepal.Width ~ Species, data=iris)
# p-value = 0.3515 >= 0.05 -> aov()

# 4. 분산분석 : aov(y~x, data)
model <- aov(Sepal.Width ~ Species, data=iris)
model

# 5. 분산분석의 해석

summary(model)

# [해설] : p-value<2e-16 : 적어도 한 집단 이상에 평균차이가 있다.

# 6. 사후검정 : 각집단 차이 상세히 분석석
TukeyHSD(model)
#                        diff    lwr
# versicolor-setosa    -0.658  -0.81885528
# virginica-setosa     -0.454  -0.61485528
# virginica-versicolor  0.204  0.04314472
# p-value : 집단간 평균차이 유무 해설
# diff : 평균차이 정도

plot(TukeyHSD(model))
# 신뢰구간 : 집단 간 평균차이 유무 해설


# 통계검정 : 각 집단의 평균차이
library(dplyr) # dataset %>% function

iris %>% group_by(Species) %>% summarise(avg=mean(Sepal.Width))

# 1 setosa      3.43
# 2 versicolor  2.77
# 3 virginica   2.97

# versicolor - setosa

# virginica-versicolor

###############################
#### 비모수 검정
################################

# 1. 변수 선택
names(iris)
x<- iris$Species
y <- iris$Sepal.Length

# 2. 동질성 검정
bartlett.test(Sepal.Length ~ Species, data=iris)
# p-value = 0.0003345 : 비모수 검정


# 3. 분산분석(비모수 검정) : 평균 -> 중위수 
kruskal.test(Sepal.Length ~ Species, data = iris)
# Kruskal-Wallis chi-squared = 96.937, df = 2, p-value < 2.2e-16

# 4. 사후검정 : 집단별 중위수 비교 
library(dplyr)
iris %>% group_by(Species) %>% summarise(med=median(Sepal.Length))
# 1 setosa       5  
# 2 versicolor   5.9
# 3 virginica    6.5




#############################
#### quakes
#############################

# 1. 전처리
str(quakes)
# 'data.frame':	1000 obs. of  5 variables:
# $ lat     (위도): num  -20.4 -20.6 -26 -18 -20.4 ...
# $ long    (경로): num  182 181 184 182 182 ...
# $ depth   (깊이): int  562 650 42 626 649 195 82 194 211 622 ...
# $ mag     (규모): num  4.8 4.2 5.4 4.1 4 4 4.8 4.4 4.7 4.3 ...
# $ stations(위치): int  41 15 43 19 11 12 43 15 35 19 ...


# 집단변수(연속형 -> 범주형)

range(quakes$depth)
# 40 ~ 680
# 3등분해서 범주형으로 바꿈
div <- round((680-40)/3)

# 코딩변경(연속형 -> 범주형)
quakes$depth2[quakes$depth <= (40+div)] <- "low"
quakes$depth2[quakes$depth > (40+div) & quakes$depth <= (680-div)] <- "mid"
quakes$depth2[quakes$depth > (680-div)] <- "high"

quakes$depth2

y <- quakes$mag #연속형 변수
x <- quakes$depth2

table(quakes$depth2)

# 2. 동질성 검정
bartlett.test(mag ~ depth2, data = quakes)
# p-value = 0.1554 > 0.05 모수검정

# 분산분석 -> 동질성검정에서 결과
model <- aov(mag ~ depth2, data = quakes)
summary(model)
# F value : 31.43   
# Pr(>F) : 5.78e-14
# [해설] 매우 유의미한 수준에서 집단간 차이를 보인다

# 사후검정
TukeyHSD(model)

#           diff        lwr        upr         upr
# low-high  0.17127705  0.1083477  0.23420643  0.0000000 : 평균차이 
# mid-high -0.07543586 -0.1702399  0.01936818  0.1486744 > 0.05 : 평균차이 없다
# mid-low  -0.24671291 -0.3380606 -0.1553652   0.0000000 : 평균차이

plot(TukeyHSD(model))

# 0 : 평균차이가 없다 0을 걸칠경우우
# + :앞에 있는 값이 크다
# - : 뒤의 값이 크다다




