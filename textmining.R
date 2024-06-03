# 텍스트마이닝 
# 먼저 텍스트는 비정형 데이터 형식x, 지 맘대로임 like 변진모
# 텍스트 마이닝 -  텍스트 데이터에서 유용한 정보 또는 지식을 찾아내는 일
# 말뭉치corpus - • 특정 분야에서 발생하는 문서의 집합
# DTM (Document Term Matrix) - 문서에 나타난 단어의 빈도를 표현하는 행렬 
# 데이터 전처리 파이프라인 - 소문자 변경, 숫자 제거, 불용어 제거, 구두점 제거, 어간 추출


# 02 DTM 구축 
library(RCurl)
library(XML)

t = readLines('https://en.wikipedia.org/wiki/Data_science')
d = htmlParse(t, asText = TRUE)  #  HTML 문서를 트리 구조로 변환
clean_doc = xpathSApply(d,"//p", xmlValue)  # xpathSApply 함수는 지정된 XPath 표현식을 사용하여 XML/HTML 문서에서 데이터를 추출
# "//p"는 모든 <p> 태그를 선택하는 XPath 표현식
# xmlValue 함수는 선택된 태그의 텍스트 값을 반환
# 결과적으로, clean_doc 변수에는 모든 <p> 태그 내부의 텍스트가 저장

library(tm)
library(SnowballC)

doc = Corpus(VectorSource(clean_doc))  # 텍스트 벡터를 Corpus 객체(텍스트 문서의 컬렉션)로 변환
inspect(doc)  # Corpus 객체 생성

doc = tm_map(doc, content_transformer(tolower)) # 소문자로 변환
doc = tm_map(doc, removeNumbers) # 숫자 제거
doc = tm_map(doc, removeWords, stopwords('english')) # 불용어 제거
doc = tm_map(doc, removePunctuation) # 구두점 제거
doc = tm_map(doc, stripWhitespace) # 불필요한 공백 제거

# DocumentTermMatrix 함수로 DTM 구축
dtm = DocumentTermMatrix(doc)
dim(dtm)

inspect(dtm)  # inspect(dtm)은 일반적으로 텍스트 데이터를 분석할 때 사용 
# 문서-단어 행렬의 구조와 내용을 확인

# 가시화 시작 

# 03 단어 구름 #
library(wordcloud)

m = as.matrix(dtm)  # as.matrix 함수는 DTM을 행렬 표현으로 변환
v = sort(colSums(m), decreasing = TRUE)    # sort 함수는 빈도(중요도)가 높은 순서로 단어를 정렬
# colSums 함수는 각 단어의 전체 빈도를 계산합니다. (DTM의 각 열의 합을 구합니다) sort 함수는 빈도를 내림차순으로 정렬합니다.
d = data.frame(word = names(v), freq = v) # word 열에는 단어가, freq 열에는 해당 단어의 빈도가 저장
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 100, random.order = FALSE, rot.per = 0.35)
# rot.per 세로로 배치할 단어의 비율을 35%로 하라는 옵션 /  # max.words중요도가 높은 상위 100개만 그리라는 옵션
# FALSE로 설정하면 빈도가 높은 단어가 중앙에 배치됨
library(RColorBrewer)

pal = brewer.pal(11,"Spectral")   # Spectral" 색상 팔레트를 사용하여 11개의 색상을 생성
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 50, random.order = FALSE, rot.per = 0.50, colors = pal)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 50, random.order = FALSE, rot.per = 0.50, colors = pal, family = "mono", font = 2)

# 보다 뛰어난 단어구름 2!
library(wordcloud2)
wordcloud2(d)

d1 = d[1:200, ]        # 200개 단어만 표시 # wordcloud2에는 단어 개수 지정하는 옵션이 없어 미리 추출
wordcloud2(d1, shape = 'star')
wordcloud2(d1, minRotation = pi/4, maxRotation = pi/4, rotateRatio = 1.0) # 단어 방향 범위 지정 / 회전할 확률

# 03.3 빈도 표시하기
findFreqTerms(dtm, lowfreq = 12) # 적어도 12번 이상 빈도발생한 단어만 표시하라는 옵션
findAssocs(dtm, terms = 'training', corlimit = 0.7) # training’과 상관관계가 0.7이상인 단어를 표시하라는 옵션
barplot(d[1:10, ]$freq, las = 2, names.arg = d[1:10,]$word, col = 'lightblue', main = '발생 빈도 상위 단어', ylab = '단어 빈도')
# 1: 축 레이블이 항상 수평으로 배치됩니다. /2: 축 레이블이 축에 수직으로 배치됩니다.

# 03.4 텍스트 이외의 응용: gapminder 예제
library(gapminder)
library(dplyr)
#2007sus 전세계 대륙의의 인구
pop_siz = gapminder%>%filter(year==2007)%>%group_by(continent)%>%summarize(sum(as.numeric(pop)))
d = data.frame(word = pop_siz[, 1], freq = pop_siz[, 2])
wordcloud(words = d[, 1], freq = d[, 2], min.freq = 1, max.words = 100, random.order = FALSE, rot.per = 0.35)
# wordcloud는 데이터 프레임을 사용
#  첫번째 열은 단어, 두번째 열은 해당 단어의 빈도수인 데이터 프레임
#  텍스트 이외에도 이런 형식을 갖추면 단어 구름 가능함
#  예) gapminder에서 첫번째 열은 대륙, 두번째 열은 해당 대륙의 인구가 되도록 데이터 추출
wordcloud2(d)

# 04 문서 분류 #
library(text2vec)
library(caret)

str(movie_review)
head(movie_review)

# 데이터를 훈련 집합(mtrain)과 테스트 집합(mtest)로 나눔
train_list = createDataPartition(y= movie_review$sentiment, p = 0.6, list =  FALSE)
# createDataPartition 함수는 데이터 프레임을 훈련 집합과 테스트 집합으로 나누는 데 사용
# 데이터 프레임 movie_review 데이터 프레임은 종속 변수(예: sentiment)와 독립 변수(예: 리뷰 텍스트 등)를 포함 그래서 x 필요없음
mtrain = movie_review[train_list, ]
mtest = movie_review[-train_list, ]

# 훈련 집합으로 DTM 구축
doc = Corpus(VectorSource(mtrain$review))
doc = tm_map(doc, content_transformer(tolower))
doc = tm_map(doc, removeNumbers)
doc = tm_map(doc, removeWords, stopwords('english'))
doc = tm_map(doc, removePunctuation)
doc = tm_map(doc, stripWhitespace)

dtm = DocumentTermMatrix(doc)
dim(dtm)
str(dim)

inspect(dtm) # 세부 내용 보는거

# DTM을 모델링 가능한 형태로 변환
#  사전이 아주 커서 그대로 적용하면 메모리 오류 발생  removeSparseTerms 함수로 줄임
#  cbind 함수로 반응 변수 sentiment를 덧붙임
dtm_small = removeSparseTerms(dtm, 0.90)
X = as.matrix(dtm_small)
dataTrain = as.data.frame(cbind(mtrain$sentiment, X))
dataTrain$V1 = as.factor(dataTrain$V1)
colnames(dataTrain)[1] = 'y'

# 결정 트리로 학습
library(rpart)
r = rpart(y~., data = dataTrain)
printcp(r)

par(mfrow = c(1, 1), xpd = NA)
plot(r)
text(r, use.n = TRUE)


#  랜덤 포리스트로 학습
#  랜덤 포리스트는 여러 개의 결정 트리를 결합하여 예측을 수행
library(randomForest)
f = randomForest(y~., data = dataTrain)
f

# 04.2 예측과 성능 평가
#  테스트 목적으로 남겨둔 mtest로 성능을 평가
# 훈련 집합으로 DTM 구축

# 먼저 mtest에 전처리 적용 (학습 과정과 동일한 과정을 적용해야 함)
#  훈련 집합이 사용한 사전을 테스트 과정에도 그대로 사용해야 함

docTest = Corpus(VectorSource(mtest$review))
docTest = tm_map(docTest, content_transformer(tolower))
docTest = tm_map(docTest, removeNumbers)
docTest = tm_map(docTest, removeWords, stopwords('english'))
docTest = tm_map(docTest, removePunctuation)
docTest = tm_map(docTest, stripWhitespace)

dtmTest = DocumentTermMatrix(docTest, control=list(dictionary=dtm_small$dimnames$Terms)) 
# control 옵션으로 학습 과정이 사용한 사전을 여기서도 사용함
dim(dtmTest)
str(dtmTest)
inspect(dtmTest)

X = as.matrix(dtmTest)
dataTest = as.data.frame(cbind(mtest$sentiment, X))
dataTest$V1 = as.factor(dataTest$V1)
colnames(dataTest)[1] = 'y'
pr = predict(r, newdata = dataTest, type = 'class'). # 결정 트리 r
table(pr, dataTest$y)

pf = predict(f, newdata = dataTest). # 랜덤 포리스트 f
table(pf, dataTest$y)


# 05 영어 텍스트 마이닝을 이용한 한국어 처리 #
library(tm)
library(XML)
library(wordcloud2)
library(SnowballC)
library(RCurl)

t = readLines('https://ko.wikipedia.org/wiki/%EB%B9%85_%EB%8D%B0%EC%9D%B4%ED%84%B0')
d = htmlParse(t, asText = TRUE)
clean_doc = xpathSApply(d, "//p", xmlValue)

doc = Corpus(VectorSource(clean_doc))
inspect(doc)

doc = tm_map(doc, content_transformer(tolower))
doc = tm_map(doc, removeNumbers)
doc = tm_map(doc, removePunctuation)
doc = tm_map(doc, stripWhitespace)

dtm = DocumentTermMatrix(doc)
dim(dtm)
inspect(dtm)

m = as.matrix(dtm)
v = sort(colSums(m), decreasing = TRUE)
d = data.frame(word = names(v), freq = v)
d1 = d[1:500, ]                # 500개 단어만 표시
wordcloud2(d1)