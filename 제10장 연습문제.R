#################################
## <제13장 연습문제>
################################# 

# 01. 중소기업에서 생산한 HDTV 판매율을 높이기 위해서 프로모션을 진행한 결과 
#  기존 구매비율(15%) 보다 향상되었는지를 각 단계별로 분석을 수행하여 검정하시오.
#귀무가설(H0) : 기존 구매비율과 차이가 없다. (양측검정)
#연구가설(H1) : 기존 구매비율과 차이가 있다.

#조건) 구매여부 변수 : buy (1: 구매하지 않음, 2: 구매)

# 단계1 : 데이터셋 가져오기
hdtv <- read.csv("data/hdtv.csv")
head(hdtv)

# 단계2 :  빈도수와 비율 계산
summary(hdtv) 
length(hdtv$buy) # 50개

table(hdtv$buy)
table(hdtv$buy, useNA="ifany") # NA 빈도수 표시


# 단계3 : 가설검정
binom.test(10,50,p=0.15, alternative = "two.sided", conf.level = 0.95) # 양측
binom.test(10,50,p=0.15, alternative = "greater", conf.level = 0.95) # 
binom.test(10,50,p=0.15, alternative = "less", conf.level = 0.95)

# 단계4 : 검정결과 해설 

# 양측기준 p-value = 0.321 > 0.05 이므로 기존의 귀무가설이 채택된다. 고로 연구가설은 기각된다.






# 02. 우리나라 전체 중학교 2학년 여학생 평균 키가 148.5cm로 알려져 있는 상태에서  
# A중학교 2학년 전체 500명을 대상으로 10%인 50명을 표본으로 선정하여 표본평균신장을 
# 계산하고 모집단의 평균과 차이가 있는지를 각 단계별로 분석을 수행하여 검정하시오.

# 귀무가설 : 모집단의 평균과 관계가 없다.

# 단계1 : 데이터셋 가져오기
stheight<- read.csv("data/student_height.csv")
stheight
height <- stheight$height
head(height)

# 단계2 : 기술통계량/결측치 확인
length(height) #50
summary(height) # 149.4 

x1 <- na.omit(height)
x1 # 정제 데이터 
mean(x1) # 149.4 -> 평균신장

# 단계3 : 정규성 검정
shapiro.test(x1)
# p-value = 0.0001853 < 0.05 정규성없다리리

# 정규성 보이면 t.test // 아니면 wilcox.test

# 단계4 : 가설검정 - 양측검정
wilcox.test(x1, mu= 148.5) 
# p-value = 0.067 > 0.05 이므로 귀무가설이 기각, 대립가설인 모집단의 평균과 관계가 있다.


# 03. 대학에 진학한 남학생과 여학생을 대상으로 진학한 대학에 대해서
# 만족도에 차이가 있는가를 검정하시오.

# 힌트) 두 집단 비율 차이 검정
#  조건) 파일명 : two_sample.csv, 변수명 : gender(1,2), survey(0,1)
# gender : 남학생(1), 여학생(2)
# survey : 불만(0), 만족(1)
# prop.test('성공횟수', '시행횟수')

# 단계1 : 실습데이터 가져오기
getwd()
data <- read.csv("data/two_sample.csv")
data
head(data) # 변수명 확인

# 단계2 : 두 집단 subset 작성
table(data$gender, data$survey)

#    0   1
# 1  36 138 -> 남
# 2  19 107 -> 여

# 단계3 : 두집단 비율차이검증 : prop.test()
prop.test(c(138,107), c(174,126))
# p-value = 0.2765 : 남녀에 따라 만족도 차이에 없다




# 04. 교육방법에 따라 시험성적에 차이가 있는지 검정하시오.

# 귀무가설 : 교육방법에 따라 시험성적의 관계가 없다.

#힌트) 두 집단 평균 차이 검정
#조건1) 파일 : twomethod.csv
#조건2) 변수 : method : 교육방법, score : 시험성적
#조건3) 모델 : 교육방법(명목)  ->  시험성적(비율)
#조건4) 전처리 : 결측치 제거 : 평균으로 대체 

#단계1. 실습파일 가져오기
Data <- read.csv("data/twomethod.csv", header=TRUE)
head(Data) #3개 변수 확인 -> id method score

#단계2. 두 집단 subset 작성
# subset 생성 
Data <- Data[c('method', 'score')] 
# score 결측치 처리 
Data$score <- ifelse(is.na(Data$score), mean(Data$score, na.rm=T), Data$score)
summary(Data)


#단계3. 데이터 분리
# 1) 집단(교육방법)으로 분리
x <- data$method
# 2) 교육방법에서 시험성적 추출
y <- data$score

#단계4 : 분포모양 검정
var.test(x,y)
var.test(x,y,alternative = "two.sided")

#단계5: 가설검정
# p-value < 2.2e-16 < 0.05 귀무가설 기각





# 05. airquality를 대상으로 월별(month)로 오존량(ozone)에 차이가 있는지 검정하시오

airquality
str(airquality)

# $Ozone ->y 연속형 변수
# $Month  -> x집단변수

# 단계0 : 가설확인 및 변수 설정

x <- airquality$Month
y <- airquality$Ozone

# 단계1 : 전처리 (결측치 제거)
summary(airquality)
# ozone na's : 37
# month na's : x

dataset <- na.omit(airquality)
?na.omit

# 단계2 : 동질성 검정
bartlett.test(Ozone ~ Month, data=dataset)
#p-value = 0.007395 < 0.05 : 비모수 검정

# 단계3 : 분산분석(모수vs비모수)
kruskal.test(Ozone ~ Month, data=dataset)
# p-value = 2.742e-05 집단별 분산 차이

# 단계4 : 사후검정
dataset %>% group_by(Month) %>% summarise(med = median(Ozone))
#   Month   med
#    <int> <dbl>
# 1     5    18
# 2     6    23
# 3     7    60
# 4     8    45
# 5     9    23