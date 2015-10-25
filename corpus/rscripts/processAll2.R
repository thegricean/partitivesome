theme_set(theme_bw(18))
setwd("/Users/titlis/cogsci/projects/partitivesome/corpus/")
source("rscripts/helpers.r")

d = read.table("data/all/swbdext_correctedprobs.txt",sep="\t",header=T,quote="")
# delete all the Head prob columns that are outdated
d$JFQ_Head = NULL
d$CndP_Head = NULL
d$Information_Head_3gram = NULL
d$Length_Head_3gram = NULL
d$FQ_Head = NULL
d$P_Head = NULL
d$logHeadFreq = log(as.numeric(as.character(d$FQ_CorrectedHead)))

nrow(d)
summary(d)
d = droplevels(d[!is.na(d$JFQ_CorrectedHead),])

word_after_all = as.data.frame(table(droplevels(d[d$Partitive == "the",]$WordAfterAll)))
word_after_all = word_after_all[order(word_after_all[,c("Freq")],decreasing=T),]
colnames(word_after_all) = c("Word","Frequency")
word_after_all$Word = factor(x=word_after_all$Word,levels=word_after_all$Word)

ggplot(word_after_all, aes(x=Word,y=Frequency)) +
  geom_bar(stat="identity")
ggsave("graphs/word_after_all.pdf")

# HEAD STUFF
head = d %>% 
  group_by(Partitive) %>%
  summarise(MeanCndP=mean(CndP_CorrectedHead),CiLowCndP=ci.low(CndP_CorrectedHead),CiHighCndP=ci.high(CndP_CorrectedHead),MeanInfo=mean(Information_CorrectedHead_3gram),CiLowInfo=ci.low(Information_CorrectedHead_3gram),CiHighInfo=ci.high(Information_CorrectedHead_3gram),MeanP=mean(P_CorrectedHead),CiLowP=ci.low(P_CorrectedHead),CiHighP=ci.high(P_CorrectedHead))
head=as.data.frame(head)
head(head)

ggplot(head,aes(x=Partitive,y=MeanCndP)) +
  geom_bar(fill="gray60",stat="identity") +
  geom_errorbar(aes(ymin=MeanCndP-CiLowCndP,ymax=MeanCndP+CiHighCndP),width=.25)

ggplot(head,aes(x=Partitive,y=MeanInfo)) +
  geom_bar(fill="gray60",stat="identity") +
  geom_errorbar(aes(ymin=MeanInfo-CiLowInfo,ymax=MeanInfo+CiHighInfo),width=.25)

ggplot(head,aes(x=Partitive,y=MeanP)) +
  geom_bar(fill="gray60",stat="identity") +
  geom_errorbar(aes(ymin=MeanP-CiLowP,ymax=MeanP+CiHighP),width=.25)

# PREVIOUS WORD STUFF
pre = d[!is.na(d$P_Pre),] %>% 
  group_by(Partitive) %>%
  summarise(MeanCndP=mean(CndP_Pre),CiLowCndP=ci.low(CndP_Pre),CiHighCndP=ci.high(CndP_Pre),MeanInfo=mean(Information_Pre_3gram),CiLowInfo=ci.low(Information_Pre_3gram),CiHighInfo=ci.high(Information_Pre_3gram),MeanP=mean(P_Pre),CiLowP=ci.low(P_Pre),CiHighP=ci.high(P_Pre))
pre=as.data.frame(pre)
head(pre)

ggplot(pre,aes(x=Partitive,y=MeanCndP)) +
  geom_bar(fill="gray60",stat="identity") +
  geom_errorbar(aes(ymin=MeanCndP-CiLowCndP,ymax=MeanCndP+CiHighCndP),width=.25)

ggplot(pre,aes(x=Partitive,y=MeanInfo)) +
  geom_bar(fill="gray60",stat="identity") +
  geom_errorbar(aes(ymin=MeanInfo-CiLowInfo,ymax=MeanInfo+CiHighInfo),width=.25)

ggplot(pre,aes(x=Partitive,y=MeanP)) +
  geom_bar(fill="gray60",stat="identity") +
  geom_errorbar(aes(ymin=MeanP-CiLowP,ymax=MeanP+CiHighP),width=.25)


## DO ANALYSIS ON JUST REDUCED VS FULL PARTITIVE
# this very preliminary model suggests: marginally more full partitive uses with increasing surprisal of "all" given the previous word; but marginally less partitive use with increasing surprisal of "all" given the NP head. weird! 
rf = droplevels(d[d$Partitive != "no",])
nrow(rf)
table(rf$Partitive)
centered = cbind(rf,myCenter(rf[,c("Information_CorrectedHead_3gram", "logHeadFreq","Information_Pre_3gram")]))
m = glmer(Partitive~cInformation_CorrectedHead_3gram  + cInformation_Pre_3gram + (1|Speaker_ID),data=centered,family="binomial")
summary(m)
contrasts(centered$Partitive)
