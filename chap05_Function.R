# chap05_Function

# 함수 : 사용자정의함수, 내장함수

# 1. 사용자 정의함수

#  - 함수명 <- function(parameter) {
#     statement

#     return (반환값)
# }

# 1) 매개변수가 없는 함수
user_fun1 <- function() {
  cat("user_fun1")
}

# 함수의 호출

user_fun1()

# 2) 매개변수가 있는 함수
user_fun2 <- function(x,y) {
  z <- x+y
  cat('result=',z)
}

user_fun2(10,20)

# 3) 반환값이 있는 함수
user_fun3 <- function(x,y) {
  return(x+y)
}

z <- user_fun3(100,200)
z

# 문제 다음과 같은 함수를 만드시오
# 조건1> 함수명 calc
# 조건2> 인수 2개
# 조건3> 처리내용 사칙연산

calc <- function(x,y,oper) {
  if(oper == '+') {
    return(x+y)
  }else if(oper == '-') {
    return(x-y)
  }else if(oper == '*') {
    return(x*y)
  }else if(oper == '/') {
    return(x/y)
  }else if(oper == '%%') {
    return(x%%y)
  }
}

calc <- function(x,y) {
  add <- x+y
  sub <- x-y
  div <- x/y
  mul <- x*y
  calc_re <- data.frame(add,sub,div,mul)
  return(calc_re)
}

result <- calc(100,50)
class(result)
result


# ex1) 결측치 자료 처리함수
na <- function(data) {
  # 1차 : 결측치를 제거하는 방법
  print(data)
  print(mean(data, na.rm = T))
  # 2차 : NA-> 0 으로 대체
  tmp <- ifelse(is.na(data),0,data)
  print(tmp)
  print(mean(tmp))
  # 3차 : NA -> mean() 으로 대체
  tmp <- ifelse(is.na(data), mean(data, na.rm=T), data)
  print(tmp)
  print(mean(tmp))
  
}

data<- c(10,2,5,NA,60,NA,3)
data

na(data)

#ex2) 특수문자 처리 함수
library("stringr")
library(help="stringr") # stringr에 대한 정보(api)

string <- "홍길동35이순신45유관순25"
name <- str_extract(string, "[가-힣]{3}")
names <- str_extract_all(string, "[가-힣]{3}")

name;names # names : list 자료구조 key <-[[1]] 인 리스트
names <- unlist(names) #list->vecotr 만들어주는 함수

string2 <-"$(125,457)%"
tmp <- str_replace_all(string2,"\\$|\\(|\\)|\\,|\\%","")
tmp
num <- as.numeric(tmp)

num2 <- as.numeric(cat(unlist(str_extract_all(string2, "[0-9]{1}"))))
num2

# 특수문자 처리 함수 정의
data_pro <- function(data) {
  library(stringr)  #in memory
  tmp <- str_replace_all(data,"\\$|\\(|\\)|\\,|\\%","")
  num <- as.numeric(tmp)
  
  return(num)
}


getwd()
stock <- read.csv("stock.csv")
str(stock)

# 1. subset : 1~15까지 컬럼으로 새로운 데이터셋

stock_df <- stock[c(1:15)]
str(stock_df)

head(stock_df)

# 숫자(7~15)와 특수문자 섞인 것은 특수문자 제거, NA->0
stock_num <- apply(stock_df[c(7:15)],2,data_pro)

stock_num

new_stock <- cbind(stock_df[c(1:6)], stock_num)

head(new_stock)
                            


# 2. 내장함수
data <- runif(20, min=0, max=100)
data
min(data)
max(data)
range(data)
mean(data) # => sum / length
median(data) # => 중위수 : 정렬을 한 뒤 가운데값과 그 다음값의 중간값

sdata <- sort(data) #기본이 오른차순이되, 2번째 파라미터에 T를 입력하면 내림차순으로 바뀜
(sdata[10] + sdata[11])/2

# 요약통계량
summary(data)

sum(data)
var(data)
sd(data)

# 제곱/제곱근
4^2
sqrt(16)


# 정렬 : sort() / order()
data("iris")  # 붓꽃 데이터셋
str(iris)

head(iris)

# 칼럼 단위 정렬
sort(iris$Sepal.Length) # 오름차순(값으로 반환)
sort(iris$Sepal.Length,decreasing = T)

# 행 단위 정렬
dim(iris) 
idx <- order(iris$Sepal.Length) # 오름차순(index로 반환(행번호))
iris_df <- iris[idx,]           # ~를 기준으로 행단위로 정렬할 때 사용함함
head(iris_df)

summary(iris)

# 2) 로그, 지수

# 함수 사용하는데 사용함
#   - (1) 일반로그 : log10(x) -> x는 10이 밑수인 몇제곱인가

log10(10) # 1 -> log10(x) 값을 구해줍니다
log10(100) # 100 = 10^2 -> log10(100) = 2

#   - (2) 자연로그 : log(x) -> x는 e(밑수)의 몇제곱
log(10)

e <- exp(1)
e^2.302585

#   - (3) 지수함수 : exp(x) -> e^x의 결과값
exp(2)

# 로그 vs 지수
x <- c(0.12,1,12,999,99999)
x

exp(x)   # Inf : 양의 무한대
log(x)

range(log(x))

# 로그함수 : 정규화(편향제거) -> 범위를 안정하게게
# 지수함수 : 활성함수(sigmoid, softmax) : x증가 -> y급격

# 3) 난수 생성과 확률분포

# 1. 정규분포를 따르는 난수 생성
# 형식) rnorm(n,mean, sd)

?rnorm

n <- 1000
r <- rnorm(n, mean = 0, sd= 1)
r

# 대칭 확인
hist(r)

# 2. 균등분포를 따르는 난수 생성
r2 <- runif(n, min=0, max=1)   # 0~1 임의의 난수 생성
hist(r2)

# 3. 이항분포를 따르는 난수 생성
# rbinom(n,size,prob)
# size : simple size, prob : 나올수 있는 확률
n<-10
r3 <- rbinom(n,size=1,prob=0.5) # 1/2 로 지정함
r3

r3 <- rbinom(n, size=1, prob=0.25)
r3

# 4. 종자값(seed)
set.seed(123)
r <- rnorm(10)
r
