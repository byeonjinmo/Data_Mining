# 군집화 분류와 분류 

# ▪ 군집화(clustering): 주어진 대상 데이터들을 유사성이 높은 것끼리 묶어주는 기술
# 이러한 묶음을 군집, 범주, 그룹 등 다양한 용어로 부름
# ▪ 분류(classification): 그룹의 형태로 알려진 데이터들이 있을 때 그룹을 모르는 어
# 떤 데이터에 대해 어느 그룹에 속하는지를 예측하는 기술

# 1.K-평균 군집화

# k-평균 군집화의 방법을 정리하면 먼저 군집의 중심점을 찾고, 다른 점들은 거리
# 가 가장 가까운 중심점의 군집에 속하는 것으로 결정

mydata <- iris[,1:4] # 데이터 준비
fit <- kmeans(x=mydata, centers=3) 
# kmeans 함수를 사용하여 클러스터링을 수행
# centers=3: 3개의 군집으로 데이터를 나눕
fit
fit$cluster # 각 데이터에 대한 군집 번호
fit$centers # 각 군집의 중심점 좌표
# 차원 축소 후 군집 시각화
library(cluster)
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)
# clusplot 함수를 사용하여 군집을 시각화 / fit$cluster: 각 데이터 포인트의 군집 번호.
# lines=0: 군집 간의 연결선을 표시하지 않습니다.
subset(mydata, fit$cluster==2)
# 데이터에서 두 번째 군집의 데이터만 추출

# 2.대상 데이터 표준화 후 군집화

# 한계점: 거리 계산에 있어서 키의 값은 많이 반영되는데(100), 시력은 거리 계산에 
# 있어서 거의 영향을 미치지 못함(0.09) 그래서 0~1 사이로 표준화 후 군집화 진행 
std <- function(X) { # 표준화 함수
  return((X-min(X)) / (max(X)-min(X)))
}
mydata <- apply(iris[,1:4], 2, std) # 표준화된 데이터 준비 
# 2: 열 단위로 함수를 적용하겠다는 의미/std: 적용할 함수 (표준편차)
fit <- kmeans(x=mydata, centers=3)
fit

# 3. k-최근접 이웃 분류
# 분류는 그룹이 있는 데이터에 대해 그룹을 모르는 데이터가 들어왔을 때 
# 어떤 그룹에 속하는지를 예측하는 기술

library(class)
# 훈련용 데이터와 테스트용 데이터 준비
tr.idx <- c(1:25,51:75, 101:125) # 훈련용 데이터의 인덱스
ds.tr <- iris[tr.idx, 1:4] # 훈련용 데이터셋
ds.ts <- iris[-tr.idx, 1:4] # 테스트용 데이터셋
cl.tr <- factor(iris[tr.idx, 5]) # 훈련용 데이터셋의 그룹(품종) 정보
cl.ts <- factor(iris[-tr.idx, 5]) # 테스트용 데이터셋의 그룹(품종) 정보 (- 는 제외의 뜻)
pred <- knn(ds.tr, ds.ts, cl.tr, k=3, prob=TRUE)
pred
acc <- mean(pred==cl.ts) # 예측 정확도
acc
table(pred,cl.ts) # 예측값과 실제값 비교 통계

# 4.K-fold 교차 검증

library(cvTools) # cvFolds() 함수 지원
k = 10 # 10-fold
folds <- cvFolds(nrow(iris), K=k) # 폴드 생성
acc <- c() # 폴드별 예측 정확도 저장용 벡터
for (i in 1:k) {
  ts.idx <- folds$which == i # 테스트용 데이터의 인덱스
  ds.tr <- iris[-ts.idx, 1:4] # 훈련용 데이터셋
  ds.ts <- iris[ts.idx, 1:4] # 테스트용 데이터셋
  cl.tr <- factor(iris[-ts.idx, 5]) # 훈련용 데이터셋의 그룹(품종) 정보
  cl.ts <- factor(iris[ts.idx, 5]) # 테스트용 데이터셋의 그룹(품종) 정보
  pred <- knn(ds.tr, ds.ts, cl.tr, k = 5)
  acc[i] <- mean(pred==cl.ts) # 예측 정확도
}
acc # 폴드별 예측 정확도
mean(acc) # 폴드평균 예측 정확도

