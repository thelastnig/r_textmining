
# 관련 라이브러리 불러오기
library(tm)
library(NLP)
library(openNLP)
library(dplyr)
library(stringr)
library(wordcloud)
library(RColorBrewer)


# 사전 설정하기



# 데이터 불러오기(뉴스 기사)
txt <- readLines("enews.txt")


# 원데이터 가공(특수 문자 제거)
txt <- str_replace_all(txt, "\\W", " ")


# 명사 추출
nouns <- sapply(txt, extractNoun, USE.NAMES = F)
wordcount <- table(unlist(nouns))


# 데이터 타입 변환
df_word <- as.data.frame(wordcount, stringsAsFactors = F)


#변수(컬럼)명 재설정
df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq)


# 데이터 가공 (두 글자 이상만 추출)
df_word <- filter(df_word, nchar(word) >= 2)


# 색상 설정
pal <- brewer.pal(8, "Dark2")
set.seed(1234)


# 시각화(워드클라우드)
wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 1,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          scale = c(4, 0.5),
          colors = pal)
