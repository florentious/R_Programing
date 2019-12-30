# chap03_FileIO

# 1. 파일 자료 읽기

setwd("c:/dev/R/Rwork/data")
getwd()

# 1) read.table() : 칼럼 구분자( 공백, 특수문자)

#  - 칼럼명이 없는 경우 read.table("파일명") default 옵션션

student <- read.table("student.txt") # header = FALSE(제목이 없다), sep=""(각컬럼은 공백으로 구분되어 있다.)
student

# 컬럼명이 null인경우 , 기본 컬럼 명을 주게됨 : v1, v2..

#  - 컬럼명이 있는 경우 header=
student1 <- read.table("student1.txt", header = TRUE, sep="")
student1

class(student1)

#  - 컬럼이 있지만 컬럼구분자가 '특수문자' 인경우 sep=""
student2 <- read.table("student2.txt", header=TRUE, sep=";")
student2


# 2) read.csv() : 칼럼구분자가 콤마(,) 인경우/확장자가 csv인경우는 이렇게 읽어와야함
bmi <- read.csv("bmi.csv") # 문자형으로 되어있는 경우 factor형으로 바뀌어짐
bmi
str(bmi) # structure 파일 정보보

# int : 숫자형
# Factor : 범주형 (grade같은거)


mean(bmi$height)
mean(bmi$weight)

# 범주형 빈도를 확인하는 것(카운트해주는 것인듯)
table(bmi$label)

# fat(1) normal(2) thin(3) 
# 7425   7677      4898 

# 문자형 -> 문자형 // 자동으로 factor형으로 읽어오는데 그걸 안해주는 옵션
bmi2 <- read.csv("bmi.csv", stringsAsFactors = FALSE)
str(bmi2)
# $ label : chr 문자열 형태로 읽어오는 것을 확인할 수 있음

# 파일 탐색기 이용
setwd("..")
getwd()
setwd("data")

test <- read.csv(file.choose()) #파일 탐색기가 열려서 선택할 수 있음
test

# 3) 엑셀 파일 읽기 : read.xlsx() : 별도의 패키지 설치가 필요
install.packages("xlsx")
library(xlsx)

sam_kospi <- read.xlsx("sam_kospi.xlsx", sheetIndex = 1) # 몇번쨰 인덱스에서 가져올것인가
sam_kospi
head(sam_kospi)

# 한글 엑셀 파일 읽기 : encoding 지정 안하면 한글이 꺠짐 
#  - 
st_excel <- read.xlsx("studentexcel.xlsx", sheetIndex = 1, encoding = "UTF-8")

st_excel

# 데이터 셋 제공 사이트 
# http://www.public.iastate.edu/~hofmann/data_in_r_sortable.html - Datasets in R packages
# https://vincentarelbundock.github.io/Rdatasets/datasets.html
# https://r-dir.com/reference/datasets.html - Dataset site
# http://www.rdatamining.com/resources/data


# 4) 인터넷 파일 읽기
titanic <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/COUNT/titanic.csv")
titanic

dim(titanic)

# 성별 빈도수
table(titanic$sex)
# 생존 빈도수 
table(titanic$survived)


# 교차 분할표 : 2개 범주형(행, 열)
#  - 범주형으로 이루어진거(titanic 기준 성별, 생존여부)
# 성별에 따른 생존 여부를 확인할 수 있음
tab <- table(titanic$sex, titanic$survived)
tab


# 막대 차트
barplot(tab, col=rainbow(2), main="생존여부") # col : 색상은 여러가지로 보여짐



# 2. 파일 자료 저장(output)

# 1) 화면 출력
a <- 10
b <- 20
c <- a*b
print(c)
c # print 함수는 생략 가능
cat('c=',c) #문자열 합산함수

# 2) 파일 저장
# read.csv <-> write.csv           // 저장 타입
# read.xlsx <-> write.xlsx         // 저장타입 확인

getwd()
setwd("output")


# 1) write.csv() : 콤마구분

str(titanic)

# 1칼럼 제외, 따옴표 제거, 행번호는 제거
write.csv(titanic[-1],"titanic.csv",quote =F, row.names = F)

titan <- read.csv("titanic.csv")
head(titan)

# 2) write.xlsx() : 엑셀파일 형식으로 저장해줌
library(xlsx)
write.xlsx(sam_kospi,"sam_kospi.xlsx",sheetName = "sheet1", row.names = F)

