# chap11_Correlation

# dataset 가져오기
product <- read.csv("data/product.csv")
# 친밀도, 적절성, 만족도 : 5점(등간척도)
str(product)

# 1. 상관분석
#  - 두 변수의 확률 분포의 상관관계 정도를 나타내는 계수(-1~+1)

# cor(x,y)
corr <- cor(product, method = "pearson") # 0.9~0.7 어느정도 높은 상관성
# 제품이 적절하면 만족도가 높은 상관관계가 있다.
# 제품친밀도와 적절성은 어느정도 상관관계가 있다.

cor(x=product$제품_적절성, y=product$제품_만족도)


# 2. 상관분석 시각화
install.packages("corrplot")
library(corrplot)

corrplot(corr,method="number")
corrplot(corr,method="square")

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(product)


# 3. 공분산
#  - 두 변수의 확률분포의 상관관계 정도를 나타내는 값
cov(product)
corr


# 상관계수 vs 공분산
# 상관계수 : 크기, 방향(-,+)
# 공분산 : 크기


###############################
#### iris
###############################

cor(iris[-5])

#              Sepal.Length Sepal.Width Petal.Length Petal.Width
# Sepal.Length    1.0000000  -0.1175698    0.8717538   0.8179411
# Sepal.Width    -0.1175698   1.0000000   -0.4284401  -0.3661259
# Petal.Length    0.8717538  -0.4284401    1.0000000   0.9628654
# Petal.Width     0.8179411  -0.3661259    0.9628654   1.0000000

# Petal.Length와 Petal.Width는 가장 높은 상관관계를 가지고 있다. (양의 상관관계 -> 정방향 곧은 직선)
# 음의 상관관계는 반비례 느낌으로 정반대 수식으로 이루어진다다

# 양의 상관관계
plot(iris$Petal.Length, iris$Petal.Width)

# 음의 상관관계
plot(iris$Sepal.Width, iris$Petal.Length)





