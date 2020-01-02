#################################
## <제9장 연습문제>
################################# 

# 01. 교육수준(education)과 흡연율(smoking) 간의 관련성을 분석하기 위한 가설을 수립하고, 
# 이를 토대로 가설을 검정하시오.[독립성 검정]

#귀무가설 : 교육수준과 흡연율과 관련성이 없다.
#대립가설 : 교육수준과 흡연율과 관련성이 있다.

#<단계 1> 파일 가져오기

smoke <- read.csv("data/smoke.csv", header=TRUE)
# 변수 보기
head(smoke) # education, smoking 변수


#<단계 2> 코딩 변경 - 변수 리코딩 <- 가독성 제공    
# education(독립변수) : 1:대졸, 2:고졸, 3:중졸 
# smoking(종속변수): 1:과다흡연, 2:보통흡연, 3:비흡연

# education 변수 리코딩 
table(smoke$education, smoke$smoking)
smoke$education2[smoke$education==1] <- "1.대졸" 
smoke$education2[smoke$education==2] <- "2.고줄" 
smoke$education2[smoke$education==3] <- "3.중졸"

# smoking 변수 리코딩 
smoke$smoking2[smoke$smoking==1] <- "1.과대흡연" 
smoke$smoking2[smoke$smoking==2] <- "2.보통흡연" 
smoke$smoking2[smoke$smoking==3] <- "3.비흡연" 
smoke # 가독성을 위한 변수값 변경 결과


#<단계 3> 교차분할표 작성
table(smoke$education2, smoke$smoking2)


#<단계 4> 독립성 검정
CrossTable(smoke$education2, smoke$smoking2, chisq = T)
chisq.test(smoke$education2, smoke$smoking2)

#<단계 5> 검정결과 해석

# p-value = 0.0008183 < 유의수준(0.05) 이므로 유의미한 수준에서 귀무가설을 기각한다.




# 02. 나이(age3)와 직위(position) 간의 관련성을 단계별로 분석하시오. [독립성 검정]
# 귀무가설 : 나이와 직위은 관련성이 없다.
# 대립가설 : 나이와 직위은 관련성이 있다.

#[단계 1] 파일 가져오기
data <- read.csv("data/cleanData.csv")
head(data)



#[단계 2] 변수 선택   
x <- data$position # 행 - 직위 변수 이용
y <- data$age3 # 열 - 나이 리코딩 변수 이용

#[단계 3] 산점도를 이용한 변수간의 관련성 보기 - plot(x,y) 함수 이용
plot(y,x)

#[단계 4] 독립성 검정
CrossTable(x,y,chisq=T)

chisq.test(x,y)


#[단계 5] 검정결과 해석

# p-value < 2.2e-16 < 유의수준 이므로 귀무가설이 유의미하게 기각될수 있다.


# 03. 직업유형에 따른 응답정도에 차이가 있는가를 단계별로 검정하시오.[동질성 검정]

# 가설(귀무) : 직업유형에 따른 응답정도에 차이가 없다.

#[단계 1] 파일 가져오기
response <- read.csv("data/response.csv")
head(response) # 변수 보기

# [단계 2] 코딩 변경 
# job 칼럼 코딩 변경 : 1:학생, 2:직장인, 3:주부 
# response 칼럼 코딩 변경 : 1:무응답, 2:낮음, 3:높음

response$job
response$job2[response$job==1] <- "1:학생"
response$job2[response$job==2] <- "2:직장인"
response$job2[response$job==3] <- "3:주부"

response$response2[response$response==1] <- "1:무응답"
response$response2[response$response==2] <- "2:낮음"
response$response2[response$response==3] <- "3:높음"
response

# [단계 3] 교차분할표 작성
table(response$job2, response$response2)


# [단계 4] 동질성 검정  
CrossTable(response$job2, response$response2, chisq = T)
chisq.test(response$job2, response$response2)

#[단계 5] 검정결과 해석

# p-value = 6.901e-12 < 0.05 이므로 귀무가설은 유의미하게 기각
# 직업유형에 따른 응답정도는 유의미하게 차이가 있다.




