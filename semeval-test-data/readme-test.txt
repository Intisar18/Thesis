*******************************************
SemEval-2018 Task 1: Affect in Tweets
 
Test datasets
*******************************************
 
The test datasets for all tasks and all languages have the same format as the corresponding training and development datasets.
 
The test datasets for the EI-reg and V-reg English tasks have two parts:
1. The Tweet Test Set: tweets annotated for emotion/valence intensity;
2. The Mystery Test Set: automatically generated sentences to test for unethical biases in NLP systems (with no emotion/valence annotations).
 
The Tweet Test Set has been collected and annotated in the same way as the corresponding training and development sets. The official evaluation metrics reported on the Leaderboard are calculated only on this set. The teams' results are ranked only based on this set. 
 
The Mystery Test Set (the last 16,937 lines with 'mystery' in the ID) has been generated automatically to test for unethical biases in NLP systems. This set has not been annotated for emotion/valence intensity. Therefore, these instances all have a score of 0.000 in the gold test files. This set is not used by the official evaluation script to calculate the official evaluation metrics. There will be a separate evaluation on this part. 

NOTE: The official evaluation script checks if the number of lines in the prediction file equals to the number of lines in the gold test file. Therefore, we release the gold test files with all the data (Tweet  Set + Mystery Set) so that you can use the file directly with your system predictions on the full test set (Tweet  Set + Mystery Set). If your prediction file has fewer instances, then adjust the gold file accordingly. 



References:

Cite this paper for the task::

Saif M. Mohammad, Felipe Bravo-Marquez, Mohammad Salameh, and Svetlana Kiritchenko. 2018. Semeval-2018 Task 1: Affect in tweets. In Proceedings of International Workshop on Semantic Evaluation (SemEval-2018), New Orleans, LA, USA, June 2018.

@InProceedings{SemEval2018Task1,
 author = {Mohammad, Saif M. and Bravo-Marquez, Felipe and Salameh, Mohammad and Kiritchenko, Svetlana},
 title = {SemEval-2018 {T}ask 1: {A}ffect in Tweets},
 booktitle = {Proceedings of International Workshop on Semantic Evaluation (SemEval-2018)},
 address = {New Orleans, LA, USA},
 year = {2018}}

The paper below describes how the data was created:

Understanding Emotions: A Dataset of Tweets to Study Interactions between Affect Categories. Saif M. Mohammad and Svetlana Kiritchenko. In Proceedings of the 11th Edition of the Language Resources and Evaluation Conference (LREC-2018), May 2018, Miyazaki, Japan.

@inproceedings{LREC18-TweetEmo,
 author = {Mohammad, Saif M. and Kiritchenko, Svetlana},
 title = {Understanding Emotions: A Dataset of Tweets to Study Interactions between Affect Categories},
 booktitle = {Proceedings of the 11th Edition of the Language Resources and Evaluation Conference},
 year = {2018},
 address={Miyazaki, Japan}}
