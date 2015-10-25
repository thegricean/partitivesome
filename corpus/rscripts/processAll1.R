theme_set(theme_bw(18))
setwd("/Users/titlis/cogsci/projects/partitivesome/corpus/")
source("rscripts/helpers.r")

#d = read.table("data/all/swbdext.tab",sep="\t",header=T,quote="")
dd = read.table("data/all/swbdext_badNPs-unix.txt",sep="\t",header=T,quote="")
nrow(dd)
head(dd)
summary(dd)

# to see which types are getting excluded because it's a bad NP:
ggplot(dd, aes(x=Partitive)) +
  geom_histogram() +
  facet_wrap(~BadNP)

d = droplevels(subset(dd, BadNP != "X"))
nrow(d) # 1396 of 2136 left (very similar to "some", excluding 35%)
d$Head = as.character(d$Head)
d$RevisedHead = as.character(d$RevisedHead)
d$CorrectedHead = d$Head
d[d$RevisedHead != "",]$CorrectedHead = d[d$RevisedHead != "",]$RevisedHead
d$CorrectedHead = as.factor(as.character(d$CorrectedHead))
summary(d)

write.table(d, file="data/all/swbdext_correctedhead.txt", row.names=F, sep="\t", quote=F)

