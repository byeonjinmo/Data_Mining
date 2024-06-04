# s.1 워드클라우드 분석

# b 워드클라우드 문서 파일 준비
# ▪ 워드클라우드를 작성할 대상 문서는 일반적으로 텍스트 파일 형태로 준비
# ▪ 파일의 끝부분 처리를 [그림 10-2]와 같이 마지막 문장이 끝나면 반드시 줄 바꿈을 한 후 저장
# ▪ 파일을 저장할 때, [다른 이름으로 저장]을 선택하고 [그림 10-3]과 같이 인코딩을 ‘UTF-8’로 선택을 하여 저장
# ▪ 파일 이름이나 파일이 저장된 폴더 경로에 한글이 포함되어 있으면 파일을 읽을 때 에러가 발생하는 경우가 있으므로 파일을 저장할 때는 파일 이름을 영어로 설정
# multilinguer 패키지 설치
install.packages("multilinguer")
library(multilinguer)
# jdk 설치
install_jdk
# KoNLP 설치 *주의* 한줄씩 설치
install.packages("remotes")
remotes::install_github("haven-jeon/KoNLP",
                          force = T,
                          upgrade = "never",
                          INSTALL_opts = c("--no-multiarch"))

library(KoNLP)

useNIADic()

SimplePos09("KoNLP 설치 정말 어렵네요!")
extractNoun("KoNLP 설치 정말 어렵네요!")


library(wordcloud2) # 워드클라우드
library(KoNLP) # 한국어 처리
library(RColorBrewer) # 색상 선택
text <- readLines("C:/r_workspace/mis_document.txt", encoding ="UTF-8" )
# 파일 읽기
buildDictionary(ext_dic = "woorimalsam") # '우리말씀' 한글사전 로딩
pal2 <- brewer.pal(8, "Dark2") # 팔레트 생성
noun <- sapply(text,extractNoun, USE.NAMES=F) # 명사 추출 s1에서는 여기만 보자!!!!!!!*** 
# USE.NAMES=F는 결과 벡터의 이름을 사용하지 않도록 설정
noun

# 코드 10-2
noun2 <- unlist(noun) # 추출된 명사 통합
wordcount <- table(noun2) # 단어 빈도수 계산
temp <- sort(wordcount, decreasing=T)[1:10] # 빈도수 높은 단어 10개 추출
temp
temp <- temp[-1] # 공백 단어 제거
barplot(temp, # 막대그래프 작성
        names.arg = names(temp), # 막대 이름을 단어로 표시
        col ="lightblue", # 막대의 색상 지정
        main ="빈도수 높은 단어", ylab = "단어 빈도수")

# S.2 구매 패턴 분석 

# ▪ 아프리오리(Apriori) 알고리즘 - > 구매 행렬 
# 지지도(support): 상품 X,Y를 함께 구매한 비율이 전체 거래에서 차지하는 비율을 측정하는 척도
# X,Y를 함께 구매한 수/전체 거리수

# 신뢰도(confidence): 상품 X를 구매했다는 전제하에 상품 X와 Y를 동시에 구매한 빈도수를 계산하는 척도

# 향상도(confidence): 연관 규칙 {X}→{Y}에서 X를 구매했을 때 Y의 비율이 그러한 조건이 없던 때에 비해
# 얼마나 증가하는가를 보여주는 척도 신뢰도 / 지지도 
# - 값이 1보다 크면 X를 샀을 때 Y를 살 확률이 높은 것을 의미
# - 값이 1 미만이면 X를 샀을 때 Y를 사지않을 확률이 높은 것을 의미
# - 향상도가 1이면 X를 산 것과 Y를 산 것은 관계가 없다는 의미


library(arules) # 아프리오리 알고리즘
library(arulesViz) # 연관규칙 시각화 도구
# 데이터 불러오기와 관찰
ds <- read.csv("/Users/mac/Desktop/tlqkf/BreadBasket_DMS.csv") # 거래 데이터 읽기 
str(ds)  # 문자열로 변경 
head(ds)
unique(ds$Item)
# 'NONE' item 삭제
ds.new <- subset(ds, Item != 'NONE')
write.csv(ds.new, "/Users/mac/Desktop/tlqkf/BreadBasket_DMS_upd.csv")

# 트랜잭션 포맷으로 데이터 읽기. ***
trans <- read.transactions("/Users/mac/Desktop/tlqkf/BreadBasket_DMS_upd.csv",
                           format="single", header=T,
                           cols=c(3,4), sep=",", rm.duplicates=T)
trans 
# 트랜잭션 데이터 요약정보
dimnames(trans)[[2]] # 상품 목록 확인
toLongFormat(trans) # 거래별 상품 목록
inspect(head(trans, 10)) # 앞부분 10개 트랜잭션 출력


# 2.2 연관 규칙의 검색과 시각화

# 상품 판매 빈도
itemFrequencyPlot(trans, topN=10, type="absolute", xlab="상품명",
                  ylab="절대 판매빈도", main="판매량 많은 상품", col="green")
itemFrequencyPlot(trans, topN=10, type="relative", xlab="상품명",
                  ylab="상대 판매빈도", main="판매량 많은 상품", col="blue")
# 연관규칙 찾기
rules <- apriori(trans, parameter = list(supp = 0.001, conf = 0.7))
rules
# 앞쪽 10개의 규칙 출력
options(digits=2) # 평가 척도 값의 자릿수 지정
inspect(rules[1:10])
# 신뢰도 상위 10개 규칙 출력
rules.sort <- sort(rules, by='confidence', decreasing = T)
inspect(rules.sort[1:10])
# 산점도 (지지도-향상도)
plot(rules.sort, measure=c("support", "lift"), shading="confidence")
# Graph plot
plot(rules.sort, method="graph")
# Grouped Matrix Plot
plot(rules.sort, method="grouped")
## 연관 규칙의 저장
write(rules.sort, file="BreadBasket_rules.csv", sep=',', quote=T,
      row.names=F)
