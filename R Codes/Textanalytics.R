library(openNLP)
library(NLP)
s<-paste(c("Bhanuja Nagore, 28 years old, will join the board as a",
           "I stay in Framingham","I will be a data scientist"),collapse = "")
s<-as.String(s)
sent_token_annotator<-Maxent_Sent_Token_Annotator()
word_token_annotator<-Maxent_Word_Token_Annotator()
a2<- annotate(s,list(sent_token_annotator,word_token_annotator))
pos_tag_annotator<-Maxent_POS_Tag_Annotator()
pos_tag_annotator
a3<-annotate(s,pos_tag_annotator,a2)
a3
head(annotate(s,Maxent_POS_Tag_Annotator(probs = TRUE),a2))

a3w<-subset(a3,type =="word")
tags<-sapply(a3w$features, '[[', "POS")
tags
table(tags)

sprintf("%s/%s",s[a3w],tags)
a3ws2<-annotations_in_spans(subset(a3,type=="word"),
                  subset(a3, type =="sentence")[2L])[[1L]]
sprintf("%s/%s",s[a3ws2],sapply(a3ws2$features, '[[',"POS"))

