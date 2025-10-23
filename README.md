# Building institutional capacity for data science undergraduate instruction 
from a domain perspective

This repository includes R code and data analyzing the impact of a summer 
faculty professional development opportunity to increase data science training 
in the undergraduate classroom. This work was part of the NSF-funded project, 
"Building Capacity in Data Science through Biodiversity, Conservation, and 
General Education" (Awards [2122967](https://www.nsf.gov/awardsearch/showAward?AWD_ID=2122967) 
and [2122991](https://www.nsf.gov/awardsearch/showAward?AWD_ID=2122991)).

The curriculum for this workshop is available at 
[https://biodiversitydatasciencecorp.github.io/Instructional-Pedagogy/](https://biodiversitydatasciencecorp.github.io/Instructional-Pedagogy/).
 
## Code description

Scripts should be run in the following order:

+ 01-data-preparation.R: Transform raw pre- and post-workshop survey data into 
format more amenable for statistical analyses and data visualization.
+ 02-pre-post-analyses.R: Run t-tests compare pre- and post-workshop self-
assessments of knowledge and confidence of pedagogical concepts and data 
science tools.
+ 03-pre-post-display.R: Create figures showing impact of workshop on knowledge 
and confidence and statistical significance (or not) thereof.
+ 04-stipend-analyses.R: Run non-parametric test for the influence of stipend 
on respondents' ability to participate in the workshop.

### Dependencies

Code in this repository relies on the following libraries:

+ dplyr
+ ggplot2
+ stringr
+ tidyr

## Data files

All files are in the 'data' directory.

+ question-info.csv: 
  + question_id: Question unique identifier
  + question_text: Question text
  + question_category: Category of question (pedagogy vs skills vs data 
  science; knowledge vs confidence)
+ stipend-data-processed.csv: 
  + Term: Year (YYYY) in which respondent participated in workshop.
  + Participant.Code: Unique integer identifier for respondent.
  + Status: Employment status (career-track faculty, tenure-track faculty, 
  staff, or other).
  + Stipend_influence: Whether or not stipend influenced ability/decision to 
  participate in workshop.
+ stipend-data-raw.csv: 
  + Term: Year (YYYY) and session of participation (in all cases, "_1")
  + Participant Code: Unique integer identifier for respondent.
  + Status: Employment status (career-track faculty, tenure-track faculty, 
  staff, or other).
  + Did stipend influence your ability to participate? (yes, maybe, no): 
  Whether or not stipend influenced ability/decision to participate in 
  workshop.
+ survey-data-processed.csv:
  + Term: Year (YYYY) in which respondent participated in workshop.
  + Participant.Code: Unique integer identifier for respondent.
  + Question_number: Question unique identifier
  + Question_text: Question text
  + Pre: Response to question in pre-workshop survey
  + Post: Response to question in post-workshop survey 
+ survey-data-raw-pd1.csv: Wide-formatted data responses to pre- and 
post-workshop surveys about pedagogical skills and concepts.
  + Term: Year (YYYY) and session of participation (in all cases, "_1")
  + Participant Code: Unique integer identifier for respondent.
  + Multiple columns, one for each question, with integer response to question
+ survey-data-raw-pd2.csv:  Wide-formatted data responses to pre- and 
post-workshop surveys about data science skills and concepts.
  + Term: Year (YYYY) and session of participation (in all cases, "_2")
  + Participant Code: Unique integer identifier for respondent.
  + Multiple columns, one for each question, with integer response to question
