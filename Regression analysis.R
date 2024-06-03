# s1.단순 선형 회귀
# 단순선형 회귀식을 구하기 위해서는 이론적인 이해가 필요하지만, R에서 제공하는
# lm() 함수를 이용하여 쉽게 회귀식을 구할 수 있음 - 선형 회귀 모델을 피팅(fitting)하는 함수
# 3.1 주행속도와 제동거리 사이의 회귀모델 구하기
head(cars)
plot(dist~speed, data=cars) # 산점도를 통해 선형 관계 확인

model <- lm(dist~speed, cars) # 회귀모델 구하기  # 자동차의 속도와 제동거리 관계를 분석 
model

abline(model) # 회귀선을 산점도 위에 표시
# coef(model) -> c(2, 3)이라면 이는 절편 b=2와 기울기 W=3을 의미
coef(model)[1] # b 값 출력 - (coefficients : 계수)
coef(model)[2] # W 값 출력
# y = 3.932409x -17.57909

# 3.2 주행속도에 따른 제동거리 구하기
# 계수를 변수에 할당 후 독립변수(스피드)에 따른 종속변수(제동거리)의 변화를 나타낼 수도 있다.
b <- coef(model)[1]
W <- coef(model)[2]
speed <- 30 # 주행속도
dist <- W*speed + b
dist # 제동거리
speed <- 35 # 주행속도
dist <- W*speed + b
dist # 제동거리
speed <- 40 # 주행속도
dist <- W*speed + b
dist # 제동거리

# 3.3 예상 제동거리, 실제 제동거리, 오차 구하기
speed <- cars[,1] # 주행속도
pred <- W * speed + b
pred # 예상 제동거리
compare <- data.frame(pred, cars[,2], pred-cars[,2])
colnames(compare) <- c('예상','실제','오차')
head(compare)


# s2. 다중선형 회귀분석 
# 단순선형 회귀가 하나의 독립변수를 다룬다면 다중선형 회귀는 여러 개의 독립변수를 다룸 
# ex)키와 몸무게를 가지고 혈당 수치를 예측하는 문제
# R 에서는 다중 회귀모델도 lm() 함수로 구함
library(car)
head(Prestige)
newdata <- Prestige[,c(1:4)] # 회귀식 작성을 위한 데이터 준비
plot(newdata, pch=16, col="blue", # 산점도를 통해 변수 간 관계 확인
     main="Matrix Scatterplot")
# pch=16은 점의 모양을 지정하고, col="blue"는 점의 색상을 파란색으로 지정/ 
# main="Matrix Scatterplot"은 그래프의 제목을 지정
mod1 <- lm(income ~ education + prestige + # 회귀식 도출
             women, data=newdata)
# income을 종속 변수로 하고 education, prestige, women을 독립 변수
summary(mod1)
# 잔차는 실제 값과 회귀 모델이 예측한 값의 차이
# (Residuals) 잔차의 요약 통계량(최솟값, 1사분위수, 중앙값, 3사분위수, 최댓값)을 보여줌
# 이 부분은 각 독립 변수의 회귀 계수(Estimate)와 표준 오차(Std. Error), t값(t value), 그리고 p값(Pr(>|t|))을 보여줍니다.

# (Intercept): 절편 값으로, 다른 독립 변수들이 0일 때의 종속 변수 값 - (회귀 모델의 절편은 독립 변수의 값이 0일 때, 종속 변수의 예측 값을 나타냄)
# 여기서는 -253.850/ p값이 0.816으로, 이는 절편이 통계적으로 유의하지 않다는 것을 의미
# education: 교육 수준이 1단위 증가할 때, income이 평균적으로 177.199 증가 / p값이 0.347로, 이는 통계적으로 유의하지x
# prestige: 직업의 명성이 1단위 증가할 때, income이 평균적으로 141.435 증가 / p값이 7.58e-06으로, 이는 통계적으로 매우 유의
# women: 여성 비율이 1단위 증가할 때, income이 평균적으로 50.896 감소 / p값이 4.19e-08으로, 이는 통계적으로 매우 유의
# t값은 각 회귀 계수가 0인지 아닌지를 테스트하며, p값은 그 유의성을 나타냄. 별표 ***는 유의성 수준을 나타냄(0.001 이하).

# 2.1 기본 if-else문
library(MASS) # stepAIC( ) 함수 제공 AIC(Akaike Information Criterion)
newdata2 <- Prestige[,c(1:5)] # 모델 구축에 사용할 데이터셋 생성
head(newdata2)
mod2 <- lm(income ~ education + prestige +
             women + census, data= newdata2)
mod3 <- stepAIC(mod2) # 변수 선택 진행 - stepAIC 함수는 AIC 값을 기준으로 모델에서 가장 중요한 변수를 선택 
mod3 # 변수 선택 후 결과 확인
summary(mod3) # 회귀모델 상세 내용 확인


# 함수는 모델에서 가장 중요한 변수를 자동으로 선택 / 
# 여기서 "가장 중요한 변수 하나만" 선택하는 것이 아니라, 최적의 모델을 찾기 위해 필요한 변수를 선택
# 최적의 모델은 AIC 값을 기준으로 평가됨 (변수를 하나씩 제거해서 제일 낮은 값일시 해당 변수 제거 / 아무것도 안지운게 제일 낮으면 유지 )
# 절편이 유의하지 않음(0.05 이하) -> 독립 변수들이 0일 때 종속 변수의 값이 통계적으로 유의미하지 않음


# s3. 로지스틱 회귀분석
# 로지스틱 회귀(logistic regression) : 회귀모델에서 종속변수의 값의 형태가 연속형
# 숫자가 아닌 범주형 값인 경우를 다루기 위해서 만들어진 통계적 방법
# ex) iris 데이터셋에서 4개의 측정값을 가지고 품종을 예측. 품종이 범주형 값
#3.1 ▪ R에서 로지스틱 회귀 모델은 glm() 함수 이용
iris.new <- iris
iris.new$Species <- as.integer(iris.new$Species) # 범주형 자료를 정수로 변환
head(iris.new)
mod.iris <- glm(Species ~., data= iris.new) # 로지스틱 회귀모델 도출
summary(mod.iris) # 회귀모델의 상세 내용 확인

# 위 코드에서 Species 변수를 정수로 변환하지 않으면, glm 함수는 Species 변수를 범주형 변수로 인식
# 이는 glm 함수가 기본적으로 범주형 변수를 처리하지 못하기 때문에 에러가 발생

# 3.2 로지스틱 회귀모델을 이용한 예측
# 수작업으로 계산하여 품종을 예측하는 방법 대신, 구해놓은 회귀모델을 이용하여 
# 보다 편리한 방법으로 품종을 예측
# 예측 대상 데이터 생성(데이터프레임)

unknown <- data.frame(rbind(c(5.1, 3.5, 1.4, 0.2))) #2차 배열의 형태
# Sepal.Length, Sepal.Width, Petal.Length, Petal.Width에 해당
names(unknown) <- names(iris)[1:4]
unknown # 예측 대상 데이터 (새로운)
pred <- predict(mod.iris, unknown) # 품종 예측    (mod.iris는 이전에 생성한 로지스틱 회귀 모델)
pred # 예측 결과 출력
round(pred) # 예측 결과 출력(소수 첫째 자리에서 반올림)
# 실제 품종명 알아보기
levels(iris$Species)
levels(iris$Species)[pred]


test <- iris[,1:4] # 예측 대상 데이터 준비
pred <- predict(mod.iris, test) # 모델을 이용한 예측
pred <- round(pred)
pred # 예측 결과 출력
answer <- as.integer(iris$Species) # 실제 품종 정보
pred == answer # 예측 품종과 실제 품종이 같은지 비교
acc <- mean(pred == answer) # 예측 정확도 계산
acc # 예측 정확도 출력
