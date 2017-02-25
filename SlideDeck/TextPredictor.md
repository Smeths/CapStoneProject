TextPredictor
========================================================
My online content:

- Source Code: https://github.com/Smeths/CapStoneProject
- Exploratory Analysis: https://rpubs.com/Smeths/238057
- Application: https://jaysmey.shinyapps.io/ShinyApp/

Useful online resources:

- https://www.youtube.com/user/afigfigueira/playlists?shelf_id=5&view=50&sort=dd
- https://www.youtube.com/playlist?list=PL4LJlvG_SDpxQAwZYtwfXcQr7kGnl9W93

What is TextPredictor?
========================================================

TextPredictor is an text prediction application and language model.This presentation describes:

- How TextPredictor works
- How TextPredictor performance is assessed
- How the App works

How TextPredictor Works
========================================================

1. Data frames of trigrams, bigrams and unigrams with associated conditional probabilities have been formed from large corpi of twitter, blogs and news sources 
2. Given two words the trigram data frame is searched for a match and the word with the largest probability is returned
3. If there is no match in the trigram data frame, the first word is ignored and bigram dataframe is searched
4. As the user enters words an associated probability is calculated using the "Katz backoff model"


```
           wordAB wordC trigram_probs
1      who issued   the             1
2       were able    to             1
3 victim reported   the             1
4           to st louis             1
5  to prosecutors   who             1
6    to interview  ford             1
```

How TextPredictor Performance is Assessed
========================================================

Perplexity has been used to assess performance of the model's I have developed

1. A training set and test set have been formed
2. Trigram, bigram and unigram models have been built using the training data
3. A random selection of senctences s1,s2,s3,....,sm are taken from the test set
4. Perplexity is the evaluated according to the following:

$Perplexity = 2^{-l}$
$l = \frac{1}{M} \sum\limits_{i=1}^{m}\log(p(s_{i}))$  

Where M is the number of words in the test set

How to run TextPredictor
========================================================

The input section of the app is on the left hand side and outputs are on the right. The following options are avaible:

- Enter one word the second will be predicted and the probability of the first will be calculated
- Enter two words and next will be predicted and the probability of the first two calculated
- Enter three or more words and the next will be predicted and the probability of the last 3 calculated

All probability calculations are based on "Katz backoff model"

