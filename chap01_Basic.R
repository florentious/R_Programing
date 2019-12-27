# chap01_Basic

# 수업내용 
# 1. session info
# 2. R의 실행 방법
# 3. 패키지와 데이터셋
# 4. 변수와 자료 형
# 5. 기본함수 사용 밑 작업공간

# 1. session 정보
# session : R을 실행환경
sessionInfo()

# R version, 언어(다국어/ locale), base packages(사용가능 패키지)

# 2. R의 실행 방법

# 주요 단축키
# 스크립트 실행 ctrl + enter or ctrl + R
# 자동완성 : crtl + space
# 저장 : ctrl + s
# 여러줄 주석 : ctrl + shift + c(토글 기능)

# 1) 줄단위 실행
  # <- or =
  # n개의 평균이 '0'인 '1'의 표준정규분포
a <- rnorm(n=20) #변수타입은 자동으로 해줌
a       # a의 모든 변수 재공
hist(a) # histogram(chart/graph)
mean(a) # 평균(average)


# 2) 블럭 단위 실행
getwd()  # 작업경로 (get working Directory)
# file이 선언되고 닫힐때까지는 한번에 자동 실행됨
pdf("test_chapter01.pdf") # file open
hist(a)                   # histogram
dev.off()                 # close

# 3. 패키지와 데이터 셋

# 1) 패키지 = function + [dataset]  // 클래스처럼 지원하는 것

# 사용가능한 패키지 
dim(available.packages()) # dim() 차원정보, available.package() 가용한 패키지

# 2차원 배열 형태로 줌 -> 15328(row) 17(col)
# 7개 외의 패키지는 쓰려면 만들거나 다운로드 받아야함

# 패키지 설치/사용
install.packages("stringr")
# 다운로드부터 받아서 자동으로 설치함 설치해서 연결할 필요하지 않고,
# 또한 설치될때 연결되어있는 다른 패키지도 함께 설치함
#  설치만으로 끝이 아니라, 패키지를 사용하기 위해서는 메모리로 업로드해야함

# 패키지 in momory
library(stringr)

#사용가능한 패키지 확인
search()

# 설치 위치 확인
.libPaths()

# 패키지 활용
str <-"홍길동35이순신45유관순25"
str
# [1] str 결과

# 이름 추출
str_extract_all(str, "[가-힣]{3}") # _바 형태로 .대신, 
# str_extract_all(원자재, 조건(정규표현식))

# 나이 추출
str_extract_all(str,"[0-9]{2}")


# 패키지 삭제(재대로 설치 안되거나, 버전 업뎃이라던가 그런용도, 잘 안씀)
remove.packages("stringr")
# 설치랑 달리 함께 설치한거는 그대로 살아있고, 대상만 지움

# 2) 기본데이터셋
data()
# 기본 데이터들을 줌, 연습용 허상데이터 확인

data("Nile") #in memory
Nile
length(Nile)  # [1] 100  -> 1차원 배열로 100개짜리
mode(Nile)    # 자료형 확인(numeric : 숫자로만 되있음
plot(Nile)    # 차트를 그려줌 -> 조건이 랜덤인가? 아니면 자동으로 잡아주는 건가?
mean(Nile)    # 평균값 구하기


# 4.변수와 자료형

# 1) 변수(variable) : 메모리 주소를 저장함
#  - R의 모든 변수는 객체(참조변수)
# 변수 선언시 자료형 타입은 없다.
a = c(1:10)
a

# 2) 변수 작성 규칙
# - 첫자는 영문자
# - 변수에 점(.) 사용 가능(lr.model)
# - 예약어는 사용 불가
# - 대소문자 구분함 : num or NUM 은 다른변수임

var1 <- 0 # var1 = 0
var1 <- 1


var2 <- 10
var3 <- 20
var2; var3   #;로 여러줄을 실행가능함

# 객체.멤버
member.id <- "hong"
member.name <- "홍길동"
member.pwd <- "123"

num <-10
NUM <- 100
num; NUM

# scala(1개변수 저장) vs vector(n개 변수(여러개 변수) 저장)
name <- c("홍길동", "이순신", "유관순")
name

name[2]    # index 시작은 1부터임 

# tensor : scala(0), vector(1), matrix(2)
# 차원형 스타일

# 변수 목록
ls()

# 3) 자료형 
# - 숫자형, 문자형, 논리형

int <- 100 # 숫자형(연산, 차트)
string <- "대한민국" #'대한민국'
boolean <- TRUE # T , FALSE(F) 논리형 자료형

# 자료형 반환 함수
mode(int)   #numeric 자료형의 타입을 결정한다다
mode(string) #character
mode(boolean) #logical

# is.xxx() 자료형을 반환하는 함수
is.numeric(int) #논리형으로 자료형 결과 판단(맞으면 트루)
is.character(string) 
is.logical(boolean) 

x <- c(100,90, NA, 65, 78) # NA : 결측치(null값, )
is.na(x) # null 판단함수(R에서는 NA로 표현)
is.na(x[3])

# 4) 자료 형 변환(casting)
# - 문자열 - > 숫자형
num <- c(10,20,30,40)
num
mode(num)
plot(num)

num <- c(10,20,30,"40")
mode(num)               # 한개가 문자열이면 전체 문자열로 전환됨
mean(num)               # 에러난다리
num <- as.numeric(num)
mode(num)
mean(num)
plot(num)

# - 요인형(Factor) : 동일한 값을 범주로 갖는 집단변수 생성
# ex) 성별) 남(0), 여(1) -> 더미변수

gender <- c("M","F","M","F", "M")
mode(gender)
plot(gender)

# 요인형 형변환 : 문자열 -> 요인형
fgender <- as.factor(gender)
mode(fgender)
# [1] M F M F M
# Levels: F M 
# 영문자 우선 순위대로 레벨1, 레벨2 순으로 부여

str(fgender)
plot(fgender)

# 수준을 직접 정해서 변경한다.
x <- c('M','F')
fgender2 <- factor(gender, levels=x)
str(fgender2)

# mode vs class
# mode() : 자료형 반환
# class() : 자료구조를 반환함

mode(fgender)  # "numeric" 자료의 타입
class(fgender) # "factor" 자료 구조의 형태

# Factor형 고려사항
# c함수는 벡터형을 만들어주는 기본함수
num <- c(4,2,4,2)
mode(num)

fnum <- as.factor(num) #숫자는 작은순으로 레벨 부여
mode(fnum)
fnum
# [1] 4 2 4 2
# Levels: 2(1) 4(2)

str(fnum)
# Factor w/ 2 levels "2","4": 2 1 2 1

# 요인형 -> 숫자형 : 요인형으로 바뀐 숫자로 리턴한다.(레벨 표시)
nNum <- as.numeric(fnum)
mode(nNum)

# 요인형 -> 문자형 -> 숫자형 : 레벨로 리턴하지 않고 원래 숫자로 리턴
sNum <- as.character(fnum)
nNum <- as.numeric(sNum)
nNum


# 5. 기본함수 사용 및 작업 공간
# 1) 함수 도움말
mean(10,20,30,NA) # 평균 : 10 -> 잘못된 값 반환받음
help(mean) # ?mean : 같은 사용 방법

x <- c(10,20,30,NA)
mean(x)           # NA(null)이 들어가면 평균값 못구한다
mean(x,na.rm=TRUE)

sum(x,na.rm= TRUE)

# 2) 작업공간
getwd()
setwd("c:/dev/R/Rwork/script")
getwd()
setwd("c:/dev/R/Rwork/data")

test <- read.csv("test.csv")
test

str(test) #구조를 보여주는 함수수
# 'data.frame':	402 obs. of  5 variables: 
# obs. : 관측치 402(행)
# variables : 변수, 변인(열)