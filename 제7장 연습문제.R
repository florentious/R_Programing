#################################
## <제7장 연습문제>
################################# 

# 01. 본문에서 생성된 dataset2의 resident 칼럼을 대상으로 NA 값을 제거하시오.(힌트 : subset()함수 이용)

dataset2 <- subset(dataset2, !is.na(resident))

# 02. dataset2의 gender 칼럼을 대상으로 1->"남자", 2->"여자" 형태로 코딩 변경하여 
# gender2 칼럼에 추가하고, 파이 차트로 결과를 확인하시오.
dataset2$gender2 <- ifelse(dataset2$gender == 1, "남자", ifelse(dataset2$gender == 2, "여자","데이터없음"))

pie(table(dataset2$gender2))

# 03. 나이를 30세 이하 -> 1, 31~55 -> 2, 56이상 -> 3 으로 코딩변경하여 age3 칼럼에 추가한 후 
# age, age2, age3 칼럼만 확인하시오.

dataset2$age3 <- ifelse(dataset2$age <= 30, 1, ifelse(dataset2$age <=55, 2, 3))

dataset2[c("age","age3")] # age2가 없는뎁쇼 선생님

# 04. ggplot2 패키지에서 제공하는 mpg 데이터셋을 대상으로 이상치를 발견하고, 제거하시오.
install.packages("ggplot2")
library(ggplot2)
mpg <- as.data.frame(mpg)

mpg

# 단계1) 상자그래프와 통계량 

# 단계2) 이상치 제거 


