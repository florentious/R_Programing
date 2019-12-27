# chap02_DataStructure 

# 1. vector dataStructure
#  - 동일 자료형을 갖는 1차원 배열 구조
#  - c(), seq(), rep() 

# c()
x <- c(1:5)
x
y<- c(1,3,5)

# seq() - sequncer
seq(1,9,by=2)   # (start, end, step) ->( 시작, 끝, 증감)
seq(9,1,by=-2)   

# rep() 똑같은 값이 몇번 반복하니
rep(1:3, 3)      # 1,3까지 반복하는데 그것을 3번반복한다 
                 # each로 만들면 각각 반복횟수만큼 반복해서 올려줌
rep(1:3, 3)      # [1] 1 2 3 1 2 3 1 2 3
rep(1:3, each=3) # [1] 1 1 1 2 2 2 3 3 3

# index(색인) : 자료의 위치(index)
# R index = 1번 부터 시작함
a <- c(1:100)
a # [1] <- index,    1 <- content
a[100]
a[50:60]   # 50-60번째 데이터 확인
length(a)  # 데이터의 길이 확인
length(a[50:60])
a[(length(a)-5):length(a)]   # 괄호 안치니까 이상한 값이 나옴 

# boolean : 특정 조건 index check
a[a>=10 & a<=50]

# 2. matrix datastructure
#  - 동일한 자료형을 갖는 2차원(행,열) 배열
#  - 생성하는 함수 : matrix(), rbind() - rows bind  ,cbind() - column bind

m1 <- matrix(c(1:9), nrow = 3)  # 행열 구조로 row가 3개면 이렇게 됨
m1
dim(m1)                         # 3 3


x1 <- 1:3    #c(1:5) 
x2 <- 2:6
x1;x2

m2 <- rbind(x1,x2)   # 사이즈가 달라지면 문제가 생긴다 circle 형태로 반복되서 들어간다
m2

m3 <- cbind(x1,x2)
m3

# matrix index
m3[1,2]
m3[1,] #1행 전체
m3[,2] #2열 전체

m2[,2:4]

# box 형태
m2[c(1:2),c(2:4)]

y <- matrix(1:9, nrow =3, ncol=3)
x <- matrix(1:9, nrow =3, ncol=3, byrow=TRUE) # byrow 행 우선순으로 채워줌
x; y


colnames(x) <-c("one", "two", "three")        # column 명으로 부를수있고 사용할수 있음(별칭 넣는것인듯)
x
x[,"one"]                                     # 호출할 때는 이렇게 호출해야함


# scala(0)차원, vector(1)차원, matrix(2)차원

# broadcast 연산

# vector vs scala
x <- 1:10
x * 0.5        # 상수나 한가지 값만 있는 것은 스칼라라고 함
               # vector * scala -> 보통 차원이 작은쪽이 늘어나는 것
               # vector(10) * scala(10) 형태로 변경됨

# 2) vector vs matrix
x <- 1:3
y <- matrix(1:6, nrow=2)
y

z <- x*y     # 작은쪽이 사이즈가 늘어나면서 broadcast 연산
z            # 곱셈 연산이 좀 특이함 - 일반 행렬 기준인듯 테스트 해봐야한다

# apply() 처리
apply(z, 1, sum)  #apply(대상,조건, 함수) 조건에 1은 행단위
                  #apply(x, margin(1/2), function) margin 1 : 행단위, margin 2 : 열단위

apply(z, 2, mean)
apply(z,2, max)


# 3. array
#  - 동일한 자료형을 갖는 3차원 배열구조
arr <- array(1:12, c(3,2,2))
arr
dim(arr)    #3(row) 2(col) 2(side)

arr[,,1] #1면의 정보
arr[,,2]


# 4. data.frame
#  - 행렬구조를 가지고 자료구조
#  - column 서로 다른 자료형을 갖는다.
#  - db 와같은 데이터 구조를 가지고 있다

# vector
no <- 1:3
name <- c("홍길동", "이순신", "유관순")
age <- c(35,45,25)
pay <- c(200,300,150)

emp <- data.frame(No=no, Name=name, Age=age, Pay=pay)
emp

# obj$colum 특정 컬럼만 가져올수 있다.
e.pay <- emp$Pay # 벡터화
e.pay  # numeric data type

mode(e.pay)

# 산포도 : 분산과 표준편차
var(e.pay) 
sd(e.pay) # sqrt(var(e.pay)) 퍼진정도를 분산, 그의 루트값을 가져가면 표준편차값을 가져감

score <- c(90,85,83)
# 분산

var(score) # 13 -> 분산 : 중심(평균)으로부터 떨어진 정도

sqrt(var(score)) # 3.60.. -> 표준편차 : 분산의 양의 제곱근
sd(score)

# 분산 = sum((대상벡터 각 값 - 산술평균)^2) / 대상백터의 길이-1
avg <- mean(score)
var <- sum((score - mean(score))^2) / (length(score) -1) # 이게 분산 공식

sd <- sqrt(var)
sd


# 5. list data structure
#  - 서로 다른 자료형(숫자, 문자, 논리형)과 자료구조(1,2,3)를 갖는 자료구조
#  - key = value (java : Map) (python : dictionary)

# 1) key 생략 :[key=] value
list1 <- list('lee','이순신',35, 'hong', '홍길동', 45)
list1

# [[1]]       -> default key (key값이없으면 인덱스 값을 준다리)
# [1] "lee"   -> value       (value(string))

# [[6]]       -> default key (키에는 대괄호가 2개 중첩된 모양)
# [1] 45      -> value       (index 도 당연히 주어진다 )

# list {"key, indexNumber, value"}

#key로 접근함
list1[[1]]

#index로 접근되
list1[1:3]

list2 <- list('a'='aa', ''='bb', 3='cc')

library(stringr)

str <- "홍길동35이순신45"
name <- str_extract_all(str,"[가-힣]{3}")
name                  # 자르면 list 타입으로 들어간다

class(name)           #
name[[1]][2]          # 앞에는 키값 뒤에는 인덱스값

name[1][2]            # index로는 왜 찾아지지 않는가? 

# 2) key = value
member <- list(name = c("홍길동", "이순신"),
               age = c(35,45),
               gender = c('M','M'))

member


member[1]            # 2중 배열형태는 안되고 키값으로 해당 객체로 들어가야함
member$name
member$name[2]

# $기호
# data.frame vs list 안에 행으로 가는 내용은 똑같음
# data.frame$column 
# list$key






