# chap09_CrossTableChiSquare

# 통계지식 : 가설, 검정, 유의확률, 유의수준

# 실습 파일 가져오기

getwd()
data <- read.csv("data/cleanDescriptive.csv")
str(data)

# 변수 선택
x <- data$level2 # 부모의 학력수준
y <- data$pass2 # 자녀의 대학진학 합격유무

table(x)
# 고졸     대졸 대학원졸 
# 93       86       57 

table(y)
# 실패 합격 
# 96  139

###################################
# 1. 교차분석
#  - 범주형(명목,서열) 변수의 관계를 분석하는 방법

df <- data.frame(Level=x,Pass=y)

#  1] 교차분할표
table(df$Level,df$Pass) # table(행,열)

#  2] package 이용
install.packages("gmodels")
library(gmodels)
library(help="gmodels")

CrossTable(x=df$Level,y=df$Pass)
#   |-------------------------|
#   |                       N | 컬럼갯수
#   | Chi-square contribution | 기대비율
#   |           N / Row Total | 행기준 비율
#   |           N / Col Total | 열 기준 비율
#   |         N / Table Total | table 기준 비율율
#   |-------------------------|

# 기대비율(고절 vs 합격 = ) // 어떤 공식에 의해서 계산이 되는지
#  1) 기댓값 = (현재 셀의 행의 합 * 현재셀의 열의합) / 전체 합계
p_value = (89*135)/225
p_value # 53.4




#  2) 기대 비율 = (관측값 - 기댓값)^2 / 기댓값
p_rate = (49-p_value)^2 / p_value
p_rate



###################################
# 2. 카이제곱 검정
?CrossTable

# 귀무가설 : 부모의 학력수준이 자녀의 대학진학에 어떤 영향을 미치는가
CrossTable(x=df$Level, y=df$Pass, chisq=T)
# Chi^2 =  2.766951     d.f. =  2     p =  0.250705
# 검정통계량 : chi^2, d.f.
# 유의확률 : p = 0.2507057 >= 유의수준(0.05)
# 부모의 학력수준이 자녀의 대학진행에 미치진 않는다
# [해설] 학력수준과 대학진학은 관련성이 없다.

chi_2 = 0.544+0.363+1.026+0.684+0.091+0.060
chi_2

# d.f. =2 
# 자유도(D.F) : 샘플수(n)에서 자유롭게 선택한 갯수
# D.F = (행의수-1) * (열의수-1)



#######################################
##  2. 카이제곱 검정 : CrossTable() 이용
#######################################

# 1) 일원카이제곱 

# 적합도/선호도 검정 
# - chisq.test() 함수를 이용하여 관찰치와 기대빈도 일치여부 검정

# (1) 적합성 검정 예
#-----------------------------------------------
# 귀무가설 : 기대치와 관찰치는 차이가 없다.
#            도박사의 주사위는 게임에 적합하다.
# 대립가설 : 기대치와 관찰치는 차이가 있다. 
#            도박사의 주사위는 게임에 적합하지 않다.
#-----------------------------------------------
# 가설 설정 방법
# 귀무가설 : 같다 = 다르지않다 = 차이가 없다 = 효과가 없다
# 대립가설 : 같지않다 = 다르다 = 차이가 있다 = 효과가 있다

# 60회 주사위를 던져서 나온 관측도수/기대도수
# 관측도수 : 4(1), 6(2), 17(3), 16(4), 8(5), 9(6)
# 기대도수 : 10,10,10,10,10,10

chisq.test(c(4,6,17,16,8,9))

#<유의확률 해석>
#유의확률(p-value : 0.01439)이 0.05미만이기 때문에 유의미한 수준(α=0.05)에서 귀무가설을 기각할 수 있다.

#<검정통계량 해석>
# X-squared = 14.2, df=5
# X-squared >= 11.071 #(카이제곱 분포표) => 기각
# 분포표 기준값보다 크면 귀무가설을 기각할수 있다.



# (2) 선호도 분석 
#-----------------------------------------
# 귀무가설 : 기대치와 관찰치는 차이가 없다. 
# 대립가설 : 기대치와 관찰치는 차이가 있다. 
#-----------------------------------------
data <- textConnection(
  "스포츠음료종류  관측도수
  1   41
  2   30
  3   51
  4   71
  5   61
  ")
x <- read.table(data, header=T)
x # 스포츠음료종류 관측도수

chisq.test(x$관측도수)
#X-squared = 20.4882, df = 4, p-value = 0.0003999

#<유의확률 해석>
#유의확률(p-value : 0.0003999)이 0.05미만이기 때문에 유의미한 수준(α=0.05)에서 귀무가설을 기각할 수 있다. 

#2) 이원카이제곱 - 교차분할표 이용

################################
# (1) 독립성/관련성 검정 
################################  
# - 동일 집단의 두 변인(학력수준과 대학진과 여부)을 대상으로 관련성이 있는가 없는가?

# 귀무가설 : 부모의 학력수준과 자녀의 대학진학 여부와 관련성이 없다.
# 대립가설 : 부모의 학력수준과 자녀의 대학진학 여부와 관련성이 있다.

# 독립변수(x)와 종속변수(y) 생성 
getwd()
data <- read.csv("data/cleanDescriptive.csv")
x <- data$level2 # 부모의 학력수준
y <- data$pass2 # 자녀의 대학진학여부 

CrossTable(x, y, chisq = TRUE) #p =  0.2507057    
#Pearson's Chi-squared test 

# <논문에서 교차분석과 카이제곱 검정 결과 제시방법>

################################
# (2) 동질성 검정 
################################
# 두 집단의 분포가 동일한가? 다른 분포인가?
# 예) 교육방법에 따른 만족도 : 집단 간 차이가 없다.(동질성 검정)

# 1. 파일 가져오기

data <- read.csv("data/homogenity.csv", header=TRUE)
head(data) 
# method와 survery 변수만 서브셋 생성
data <- subset(data, !is.na(survey), c(method, survey)) 
head(data)
table(data$method)  #2개개
table(data$survey)  #5개

# 2. 변수리코딩 - 코딩 변경
# method: 1:방법1, 2:방법2, 3:방법3 
# survey: 1:매우만족, 2:만족, 3:보통, 4: 불만족, 5: 매우불만족

# 교육방법2 필드 추가
data$method2[data$method==1] <- "방법1" 
data$method2[data$method==2] <- "방법2"
data$method2[data$method==3] <- "방법3"

# 만족도2 필드 추가
data$survey2[data$survey==1] <- "매우만족"
data$survey2[data$survey==2] <- "만족"
data$survey2[data$survey==3] <- "보통"
data$survey2[data$survey==4] <- "불만족"
data$survey2[data$survey==5] <- "매우불만족"


# 3. 교차분할표 작성 
table(data$method2, data$survey2)  # 교차표 생성 -> table(행,열)
#         만족 매우만족 매우불만족 보통 불만족
# 방법1    8        5          6   15     16 -> 50
# 방법2   14        8          6   11     11 -> 50
# 방법3    7        8          9   11     15 -> 50
# 주의 : 반드시 각 집단별 길이(50)가 같아야 한다.

# 4. 동질성 검정 - 모수 특성치에 대한 추론검정  
chisq.test(data$method2, data$survey2) 
# p-value = 0.5865 >= 0.05


# 5. 동질성 검정 해석
# 교육방법에 따른 만족에 차이가 없다.


#####################
### iris 적용

str(iris)

# 귀문가설 : 종별로 꽃받침의 길이간 차이가 없다.
# Species : 범주형 변수(집단변수)
# Sepal.Length : 연속형 변수

# x,y 변수 선택
x <- iris$Species
y <- iris$Sepal.Length

chisq.test(x,y)  # 집단 변수, 연속형 변수

# p-value = 6.666e-09 < 0.05 이기때문에 귀문가설 기각각
# [해설] 꽃의 종별로 꽃받침의 길이에 따라 차이가 있다고 볼수 있다.


table(x)

install.packages("dplyr")
library(dplyr)

# dataset %>% function() 
iris %>% group_by(Species) %>% summarise(avg=mean(Sepal.Length))






