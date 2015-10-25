1. create preliminary database on slate

``ssh slate
cd projects/some/all/
run -c swbdext -e -o``

2. download and preprocess, including some hand-annotation in excel to mark cases where the head or partitive info was wrongly assigned, or to exclude cases that aren't interesting for our purpose, including  cases like "i ate all day long" or "i don't care all that much"

cd ~/cogsci/projects/partitivesome/corpus/data/all/
scp slate:./projects/some/all/results/swbdext.tab .

...open in excel and mark all the crap, save to swbdext_badNPs.txt, then remove dos line endings:
python dos2unix.py swbdext_badNPs.txt swbdext_badNPs-unix.txt

3. preprocess in R using processAll1.R: create a column CorrectedHead to get frequency, conditional probs and info measures for, save to swbdext_correctedhead.txt and re-upload to slate

scp swbdext_correctedhead.txt slate:./projects/some/all/results/swbdext.tab
ssh slate
cd projects/some/all/results/
addConditionalProbability.pl -c swbdext -f CorrectedHead
addInformationDensity.pl -roc swbdext -f CorrectedHead 3
addUnigram.pl -c swbdext -f CorrectedHead
addConditionalProbability.pl -c swbdext -f Pre
addInformationDensity.pl -roc swbdext -f Pre 3
addUnigram.pl -c swbdext -f Pre
addConversationInfo.pl -roc swbdext -f ID.t2o

4. download database and continue processing in R with processAll2.R

scp slate:./projects/some/all/results/swbdext.tab swbdext_correctedprobs.txt