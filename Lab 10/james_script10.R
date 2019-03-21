library(tidyverse)
library(stringr)
answers <- read_csv("Answers_trunc.csv", col_names = TRUE)%>%
  select (Id, OwnerUserId, CreationDate, ParentId, Score, Body)

questions <- read_csv("Questions_trunc.csv", col_name= TRUE)%>%
  select(Id, OwnerUserId, CreationDate, Score, Title, Body)

## Join if you want (run all code)
answersj <- answers%>%
  rename(AnswerId= Id, Id= ParentId)

QnA <- full_join(questions, answersj, by= "Id")%>%
  rename(QDate= CreationDate.x,
         QScore= Score.x,
         QTitle= Title,
         QBody= Body.x,
         ADate= CreationDate.y,
         AScore= Score.y,
         ABody= Body.y,
         AUserId= OwnerUserId.y,
         QUserId= OwnerUserId.x,
         Aid= AnswerId,
         Qid= Id
  )

QnA <- QnA%>%
  select(Qid, QTitle, QDate, QScore, QUserId, QBody, 
         Aid, ADate, AScore, AUserId, ABody)

view(answersj)
view(QnA)

questions<- questions%>%
  mutate(wc=str_length(Body),title_wc=str_length(Title))%>%
  mutate(l_body=str_to_lower(Body))%>%
  mutate(q_count=sum(str_count(l_body,c('who','what','when','why','how'))))
view(questions)

