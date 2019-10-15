library(rvest)    # 관련 라이브러리 불러오기
library(KoNLP)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(dplyr)


useNIADic()    # 사전 설정하기


naver_url_1 <- 'http://news.naver.com/main/list.nhn?sid2=230&sid1=105&mid=shm&mode=LS2D&date='
date <- 20191004:20191005
naver_url_2 <- '&page='
page <- 1:2


news_url <- c()
news_date <- c() 

for (dt in date){
  
  for (page_num in page){
    naver_url <- paste0(naver_url_1,dt,naver_url_2,page_num)
    html <- read_html(naver_url)
    temp <- unique(html_nodes(html,'#main_content')%>%
                     html_nodes(css='.list_body ')%>%
                     html_nodes(css='.type06_headline')%>%
                     html_nodes('a')%>%
                     html_attr('href'))
    news_url <- c(news_url,temp)
    news_date <- c(news_date,rep(dt,length(temp)))
    print(c(dt,page_num))
  }
  
}

news_content <- c()
for (i in 1:length(news_url)){
  html <- read_html(news_url[i])
  temp <- repair_encoding(html_text(html_nodes(html,'#articleBodyContents')),from = 'utf-8')
  news_content <- c(news_content,temp)
}

news <- cbind(date=news_date,url=news_url,content=unlist(news_content))
news <- as.data.frame(news)

news_content <- as.character(news$content)
news_content

news_content <- str_replace_all(news_content, "\\W", " ")    # 원데이터 가공(특수 문자 제거)
news_content <- str_replace_all(news_content, "\\d", " ") 
news_content <- str_replace_all(news_content, "flash", " ")
news_content <- str_replace_all(news_content, "function", " ")
news_content <- str_replace_all(news_content, "removeCallback", " ")
news_content <- str_replace_all(news_content, "com", " ")
news_content <- str_replace_all(news_content, "co", " ")
news_content <- str_replace_all(news_content, "if", " ")
news_content <- str_replace_all(news_content, "무단전재", " ")
news_content <- str_replace_all(news_content, "재배포", " ")
news_content <- str_replace_all(news_content, "금지", " ")
news_content <- str_replace_all(news_content, "구독하기", " ")
news_content <- str_replace_all(news_content, "오류", " ")
news_content <- str_replace_all(news_content, "우회", " ")
news_content <- str_replace_all(news_content, "하기", " ")
news_content <- str_replace_all(news_content, "추가", " ")
news_content <- str_replace_all(news_content, "함수", " ")
news_content <- str_replace_all(news_content, "네이버", " ")
news_content <- str_replace_all(news_content, "채널", " ")
txt <- str_replace_all(news_content, "kr", " ")

nouns <- extractNoun(txt)    # 명사 추출
wordcount <- table(unlist(nouns))



df_word <- as.data.frame(wordcount, stringsAsFactors = F)    # 데이터 타입 변환


df_word <- rename(df_word,    # 변수(컬럼)명 재설정
                  word = Var1,
                  freq = Freq)



df_word <- filter(df_word, nchar(word) >= 2)    # 데이터 가공 (두 글자 이상만 추출)



pal <- brewer.pal(9, "Blues")[5:9]    # 색상 설정
set.seed(1234)



wordcloud(words = df_word$word,    # 시각화(워드클라우드)
          freq = df_word$freq,
          min.freq = 2,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          scale = c(4, 0.3),
          colors = pal)


