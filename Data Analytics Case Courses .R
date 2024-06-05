# 1. 분석 데이터 준비 및 로드 
setwd("/Users/mac/Desktop/24년 대학/1학기/빅데이터/기말/giHorse_Datamining/a")
library(ggplot2)
library(ggmap)
library(readxl)
files <- c("201512","201606","201612","201706","201712")
columns <- c( "상가업소번호", "상호명", "상권업종대분류명", "상권업종중
분류명", "상권업종소분류명", "시군구명", "행정동명", "경도", "위도")
ds.total <- NULL # 병합할 데이터프레임 
for (i in 1:length(files)) {
  filename <- paste("seoul_", files[i], ".xlsx", sep="") # 문자열 결합
  cat("read ", filename, "...\n") # 읽을 파일 이름 출력
  ds <- read_excel(filename) # 엑셀 파일 읽기
  ds <- data.frame(ds) # 데이터프레임으로 변환
  ds <- ds[,columns] # 분석에 필요한 변수만 추출
  ds$수집연월 <- rep(i, nrow(ds)) # 데이터 수집 시점
  ds.total <- rbind(ds.total,ds) # 데이터 통합
}
head(ds.total)

# 2. 데이터 탐색 - (1) 데이터 기본 정보 확인 및 분석 대상 데이터 추출
str(ds.total)
# 특정 열에서 고유한 값을 추출
unique(ds.total$수집연월) # 수집연월   
unique(ds.total$상권업종대분류명) # 상권업종 대분류
unique(ds.total$상권업종중분류명) # 상권업종 중분류
unique(ds.total$상권업종소분류명) # 상권업종 소분류
# NA 포함여부 확인
sum(is.na(ds.total))  # is.na 함수는 데이터프레임에서 결측치(NA)를 확인 / sum 함수는 결측치의 총 개수를 계산
# 201712 수집 데이터만 추출
ds.201712 <- subset(ds.total, ds.total$수집연월== 5)   # subset 함수는 조건에 맞는 행을 추출하여 새로운 데이터프레임으로 추출
dim(ds.201712) # dim 함수는 데이터프레임의 행(row)과 열(column)의 수를 반환

# 2. 데이터 탐색 - (2) 업종별 점포수 그래프의 작성
# 업종별 점포수(대분류)
store.level_1 <- aggregate(ds.201712[,1],  # aggregate 함수는 데이터를 그룹별로 집계할 때 사용  # 임의의 첫 컬럼 
                           by=list(대분류=ds.201712$상권업종대분류명), # 상권업종대분류명 컬럼을 기준으로 데이터를 그룹화 / 그룹의 이름을 대분류로 지정
                           FUN=length)  # FUN=length: 각 그룹의 행(row) 수를 계산
store.level_1

names(store.level_1)[2] = c("count") # store.level_1 데이터프레임의 두 번째 컬럼 이름을 "count"로 변경
ggplot(store.level_1, aes(x=대분류, y=count)) + # aes 함수는 aesthetic 매핑을 설정/여기서는 x축에 대분류, y축에 count를 매핑
  geom_bar(stat="identity", width=0.7, fill="steelblue") + # geom_bar 함수는 막대 그래프/ stat="identity"는 y값이 데이터의 실제 값 사용
  ggtitle("업종별 점포수") +
  theme(plot.title = element_text(color="black", size=14, face="bold"))

# 2.데이터 탐색- (3) 구별 점포수 그래프 작성 
store.region <- aggregate(ds.201712[,1],
                          by=list(구이름=ds.201712$시군구명),  # 그륩화 
                          FUN=length)
store.region

names(store.region)[2] = c("count")
ggplot(store.region, aes(x=구이름, y=count)) +
  geom_bar(stat="identity", width=0.7, fill="steelblue") +
  ggtitle("구별 점포수") +
  theme(plot.title = element_text(color="black", size=14, face="bold"),
        axis.text.x = element_text(angle = 45)) # x축 텍스트 레이블을 45도 회전시키고, hjust로 정렬을 설정

# 2. 데이터 탐색 - (4) 점포수가 많은 상위 10개 동 확인
# 점포수가 많은 상위 10개 동 확인
store.dong <- aggregate(ds.201712[,1],
                        by=list(동이름=ds.201712$행정동명),
                        FUN=length)

names(store.dong)[2] = c("count")
store.dong <- store.dong[order(store.dong$count,decreasing=T),] # order 함수는 데이터프레임의 행을 정렬 / 내림차순으로 정렬
dong.top10 <- store.dong[1:10,]
dong.top10


ggplot(dong.top10, aes(x=reorder(동이름,-count), y=count)) + # reorder(동이름, -count)는 동이름을 count 기준으로 내림차순 정렬
  geom_bar(stat="identity", width=0.7, fill="steelblue") +
  ggtitle("점포수 많은 상위 10개동") +
  theme(plot.title = element_text(color="black", size=14, face="bold"),
        axis.text.x = element_text(angle = 45))

# 3. 기간별 분석 - (1) 업종별 점포수의 변화
# 업종별 점포수의 변화
store.change <- aggregate(ds.total[,1],
                          by=list(연월=ds.total$수집연월,  # by: 그룹화할 기준을 설정
                                  업종대분류=ds.total$상권업종대분류명),
                          FUN=length)
head(store.change)

ggplot(store.change, aes(x=연월, y=count, colour=업종대분류,
                         group=업종대분류)) +
  geom_line( ) +  # 선 그래프를 추가
  geom_point(size=6, shape=19, alpha=0.5) +
  ggtitle("업종별 점포수 변화 (대분류)") +
  ylab("점포수") +
  scale_x_continuous(breaks=1:5,        # # breaks=1:5: x축의 눈금 위치를 설정/ # x축의 연속형 스케일을 설정
                     labels=files) +  # labels=files: 눈금 라벨을 files 벡터의 값으로 설정
  theme(plot.title = element_text(color="black", size=14, face="bold"))

# 3. 기간별 분석 - (2) 점포수 변화가 큰 상위 10개 업종
#ds.total 데이터프레임에서 업종별로 수집연월에 따른 데이터 개수(count)를 계산하여 store.tmp에 저장
store.tmp <- aggregate(ds.total[,1],
                       by=list(연월=ds.total$수집연월,
                               업종소분류=ds.total$상권업종소분류명),
                       FUN=length)
# store.tmp 데이터프레임의 세 번째 열 이름을 "count"로 변경
names(store.tmp)[3] <- c("count")
# 연월이 1인 데이터(2015년 12월 데이터)를 store.201512에 저장
store.201512 <- store.tmp[store.tmp$연월==1,]
# store.201512 데이터프레임의 세 번째 열 이름을 "cnt_2015"로 변경
names(store.201512)[3] <- c("cnt_2015")
# 연월이 5인 데이터(2017년 12월 데이터)를 store.201712에 저장
store.201712 <- store.tmp[store.tmp$연월==5,]
# store.201712 데이터프레임의 세 번째 열 이름을 "cnt_2017"로 변경
names(store.201712)[3] <- c("cnt_2017")
# store.201512와 store.201712를 업종소분류를 기준으로 병합
store.diff <- merge(store.201512[,2:3], store.201712[,2:3])     # 병합할 때, 두 데이터프레임의 2번째와 3번째 열(업종소분류와 데이터 개수)을 사용
# 2015년과 2017년 데이터 개수 차이를 계산하여 diff 열에 저장
store.diff$diff <- abs(store.diff$cnt_2015 - store.diff$cnt_2017)  # abs 함수는 차이의 절대값을 계산
# diff 값을 기준으로 내림차순 정렬
store.diff <- store.diff[order(store.diff$diff, decreasing=T),]
# 상위 10개의 업종소분류명을 추출하여 top10에 저장
top10 <- store.diff[1:10,1]
# 그래프 그리깅 ~
store.change <- subset(store.tmp, store.tmp$업종소분류 %in% top10)
# store.tmp$업종소분류 %in% top10은 store.tmp의 업종소분류 열의 각 값이 top10 벡터에 포함되어 있는지를 확인
ggplot(store.change, aes(x=연월, y=count, colour=업종소분류,
                         group=업종소분류)) +
  geom_line() +
  geom_point(size=6, shape=19, alpha=0.5) +
  ggtitle("점포수 변화 Top 10 업종(소분류)") +
  ylab("점포수") +
  scale_x_continuous(breaks=1:5,
                     labels=files) +
  theme(plot.title = element_text(color="black", size=14, face="bold"))
# 3. 기간별 분석 - (3) 구별 점포수의 변화
store.gu <- aggregate(ds.total[,1],
                      by=list(연월=ds.total$수집연월,
                              구이름=ds.total$시군구명),
                      FUN=length)
names(store.gu)[3] <- c("count")
ggplot(store.gu, aes(x=연월, y=count, colour=구이름, group=구이름)) +
  geom_line( ) +
  geom_point(size=6, shape=19, alpha=0.5) +
  ggtitle("구별 점포수 변화 (대분류)") +
  ylab("점포수") +
  scale_x_continuous(breaks=1:5,
                     labels=files) +
  theme(plot.title = element_text(color="black", size=14, face="bold"))

# 3. 기간별 분석 - (4) 점포수 변화가 큰 상위 10개 동 확인

# ds.total 데이터프레임에서 동이름과 수집연월에 따른 데이터 개수(count)를 계산하여 store.tmp에 저장
store.tmp <- aggregate(ds.total[,1],
                       by=list(연월=ds.total$수집연월,
                               동이름=ds.total$행정동명),
                       FUN=length)
# store.tmp 데이터프레임의 세 번째 열 이름을 "count"로 변경
names(store.tmp)[3] <- c("count")
# 연월이 1인 데이터(2015년 12월 데이터)를 store.dong.201512에 저장
store.dong.201512 <- store.tmp[store.tmp$연월==1,]
# store.dong.201512 데이터프레임의 세 번째 열 이름을 "cnt_2015"로 변경
names(store.dong.201512)[3] <- c("cnt_2015")
# 연월이 5인 데이터(2017년 12월 데이터)를 store.dong.201712에 저장
store.dong.201712 <- store.tmp[store.tmp$연월==5,]
# store.dong.201712 데이터프레임의 세 번째 열 이름을 "cnt_2017"로 변경
names(store.dong.201712)[3] <- c("cnt_2017")
# store.dong.201512와 store.dong.201712를 동이름을 기준으로 병합
store.diff <- merge(store.dong.201512[,2:3], store.dong.201712[,2:3])
# 2015년과 2017년 데이터 개수 차이를 계산하여 diff 열에 저장
store.diff$diff <- abs(store.diff$cnt_2015 - store.diff$cnt_2017)
# diff 값을 기준으로 내림차순 정렬
store.diff <- store.diff[order(by=store.diff$diff, decreasing=T),]
# 상위 10개의 동이름을 추출하여 top10에 저장
top10 <- store.diff[1:10,1]
top10
# 그래프 그리깅 ~
# 중요 x 