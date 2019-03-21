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
  mutate(test=title_wc/wc)

view(questions)
ggplot(data=questions)+geom_point(aes(x=test,y=Score))+labs(x='Title Word Count / Body Word Count',y='Score',title='Title and Body Length Vs. Score')

answersj<-answersj%>%
  mutate(q=str_detect(Body,'\\?'))
view(answersj)

ggplot(data=answersj)+geom_density(aes(x=Score))+facet_wrap(~q)
answersj2<-answersj%>%
  filter(Score<100)
ggplot(data=answersj2)+geom_histogram(aes(x=Score,color=q),binwidth = 10)+labs(x="Score",y="Count",color="Contains ?",title='Effect of Asking Questions in Answer')
