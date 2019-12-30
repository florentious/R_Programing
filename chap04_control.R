# chap04_control

#  제어문 : 조건문과 반복문

# <실습> 산술연산자 
num1 <- 100 # 피연산자1
num2 <- 20  # 피연산자2
result <- num1 + num2 # 덧셈
result # 120
result <- num1 - num2 # 뺄셈
result # 80
result <- num1 * num2 # 곱셈
result # 2000
result <- num1 / num2 # 나눗셈
result # 5


# 나머지와 제곱수 
result <- num1 %% num2 # 나머지 계산
result # 0

result <- num1^2 # 제곱 계산(num1 ** 2)
result # 10000
result <- num1^num2 # 100의 20승
result # 1e+40 -> 1 * 10의 40승과 동일한 결과


# <실습> 관계연산자 
# (1) 동등비교 
boolean <- num1 == num2 # 두 변수의 값이 같은지 비교
boolean # FALSE
boolean <- num1 != num2 # 두 변수의 값이 다른지 비교
boolean # TRUE

# (2) 크기비교 
boolean <- num1 > num2 # num1값이 큰지 비교
boolean # TRUE
boolean <- num1 >= num2 # num1값이 크거나 같은지 비교 
boolean # TRUE
boolean <- num1 < num2 # num2 이 큰지 비교
boolean # FALSE
boolean <- num1 <= num2 # num2 이 크거나 같은지 비교
boolean # FALSE

# <실습> 논리연산자(and, or, not, xor)
logical <- num1 >= 50 & num2 <=10 # 두 관계식이 같은지 판단 
logical # FALSE
logical <- num1 >= 50 | num2 <=10 # 두 관계식 중 하나라도 같은지 판단
logical # TRUE

logical <- num1 >= 50 # 관계식 판단
logical # TRUE
logical <- !(num1 >= 50) # 괄호 안의 관계식 판단 결과에 대한 부정
logical # FALSE

x <- TRUE; y <- FALSE
xor(x,y) # [1] TRUE
x <- TRUE; y <- TRUE
xor(x,y) # FALSE


# 1. 조건문

# 1) if("조건식") {}

x <-10
y <- 5
z <- x*y

if(z>=50) {
  print("z >= 50")
} else {
  print("z < 50")
}

# 학점 구하기
# scan()     // 키보드 입력 받기
score <- scan() #입력이 여러개가 가능하며, 종료하고 싶을떄에는 아무것도 없이 enter 입력하면 된다


grade <-"" # 등급 변수 선언( 나중에 등급을 표시하기 위해서)

if(score >= 90) {
  grade <- "A"
} else if(score >= 80) { #위에서부터 순서대로 돌기때문에 결과가 이렇게 나타남남
  grade <- "B"
} else if(score >= 70) {
  grade <- "C"
} else if(score >= 60) {
  grade <- "D"
} else {
  grade <- "F"
}

cat("점수 : ", score, "등급 : ", grade)

#배수 구하기

# 문제 : 키보드 입력숫자가 5의 배수인지 판별하시오

if(scan() %% 5 == 0) {
  print("5의 배수임")
} else {
  print("5의 배수 아님")
}

# 변수로 빼도 되긴함 그게 더 좋을지도 한번에 실행하긴함


# 2) ifelse() : if+else
#  - 형식) ifelse(조건식, 참, 거짓) : 3항 연산자
# vector -> ifelse  처리 -> vector : 형태로 나타냄 

score <- c(90,65,NA,80,59)

result <- ifelse(score>=70, "Pass", "NonPass")
result

# 결측치 처리하기 : NA -> 0
result2 <- ifelse(is.na(score),0,score)
# 결측치가 확인해서 null값에 대해 0으로 반환

result2
mean(result2)


# 3) which() : 조건에 해당하는 위치(index) 반환

x <- seq(1,10,3)

which(x == 7) # 특정한 위치 인덱스 반환

# 행렬구조에서도 위치를 반환
getwd()
emp <- read.csv("../emp.csv")
emp

class(emp) # data.frame -> 행렬구조
emp$name[1] <- "유관순"

emp[1,2] <- "홍길동"

idx <- which(emp$name == "유관순") # 행의 위치를 알수 있다, 여러개면 vector 형태로 반환함

# subset example
emp_sub <- emp[idx,]
emp_sub

# 변수 선택
library("MASS")

data("Boston")

str(Boston)
# 'data.frame':	506 obs. of  14 variables:
# 1 ~ 13: x 변수(독립변수)
# medv : y변수(종속변수)

# 칼럼 가져오기
cols <- names(Boston)
cols # [1] "crim" , [14] "medv"

idx <- which(cols == "medv")
Boston[idx]

y <- Boston[,idx]   #
x <- Boston[,-idx]  # idx를 제외한 나머지

y # vector
dim(x) # 506 13 : data.frame

#data 자르기

# 2. 반복문
# 1) for(변수 in 값) { 반복문}


x <- rnorm(n=10 , mean=0, sd=1) # n=갯수, mean=평균, sd=표준편차
x
length(x)

y <- 0 # x^2

idx <-1 # index

for(v in x) {
  cat("v=",v,"\n")
  y[idx] <- v^2
  idx <- idx+1
}

y

i<- 1:10

for(v in i) { #python의 for문과 매우 동일함
  print(v)
}

# 키보드 입력 -> 홀수 출력
num <- scan()

for(v in num) {
  if(v %% 2 == 1) {
    print(v)
  }
}

# 홀수의 합 짝수의 합
even <- 0
odd <- 0

i <- 1:100

for(v in i) {
  if(v %% 2 ==0) {
    even <- even + v
  } else {
    odd <- odd + v
  }
}
cat("짝수합", even, ", 홀수 합", odd)

getwd()
setwd("..")

st<- read.table("student.txt")
colnames(st) <- c("sid","name","height","weight")
st

#if+else+for
st$heightCh <- ifelse(st$height >= 180, "high", ifelse(st$height >= 175, "mid", "low") )
st

st$weightCh <- ifelse(st$weight >= 80, "high", ifelse(st$weight >= 70, "mid", "low") )
st


# 2) while(조건식) { 반복문}

i <- 0 # 초기화
while(i<10) {
  print(i)
  i <- i+1 #count 
}


# while + index : x의 각 변량에 제곱
x <- c(2,5,8,6,9)
y <- 0   # y = x^2

i <- 0
while(i<length(x)) {
  i <- i+1
  y[i] <- x[i]^2
  cat("y=",y[i],"\n")
}

# while() -> for()
i<- 0
y <- 0
for(v in x) {
  i <- i+1
  y[i] <- v^2
  cat("y=",y[i],"\n")
}
