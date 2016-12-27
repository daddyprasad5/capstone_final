Data Science Capstone
========================================================
author: Raj Prasad
date: December 2016

Why Build a Next-Word Predictor?
========================================================
left: 30%
<div align="left">
<img src="iphone.png" width=80% height=80%>
</div>

***
<br>
- Typing is slow on small touch screens
- Typing is error prone on such devices
- Wouldn't it be great if the device could predict the word you need and pop it in with a single click?

Example App
========================================================
left: 40%
<div align="left">
<img src="whats next.png" width=100% height=100%>
</div>

***

- The [example app](https://daddyprasad5.shinyapps.io/capstoneshiny/) is available on  shiny.io.
- After you type a few words, guesses for your next word will appear below the text box and button.
- Press Enter or click the button to add the Best Guess to what you're typing.
- Below the Best Guess, you'll see the top 10 guesses for your next word, presented in a cumulative distribution.

The Model
========================================================

- The code for the model build is available [here](https://github.com/daddyprasad5/capstone_final).
- It is an absolute discount, back-off, trigram language model.
- It is a "trigram language model" because it assigns probability to 3-word sequences given a training corpus.
- It is a "back-off" model because it falls back (or backs off) to lower order (bigram, unigram) models after looking for trigram matches.
- It is an "absolute discount" model because it redistributes a set probability weight from higher- to lower-order n-gram guesses.

Accuracy & Performance
========================================================
left: 50%

Perplexity: 206

Accuracy with...
- 1 guess: 15%
- 3 guesses: 24%
- 10 guesses: 38%

Best guess accuracy after typing...<br>
- 1 letter: 37%
- 2 letters: 47%
- 3 letters: 53%
</font>

***

Performance<br><br>
Time to inital guess*: 60.6 ms
<br>
<div align="center">
<img src="targetandstopwatch.png" width=150 height=112>
</div>
<br>
*[performance report](http://daddyprasad5.github.io/perfGetDistro.html)<br>
**network and ui factors will increase perceived latency

Roadmap
========================================================
left: 70%
I'm exploring these improvements:<br> 
- Improve performance of the model build using the [parallel package](https://stat.ethz.ch/R-manual/R-devel/library/parallel/doc/parallel.pdf)
- Improve accuracy using topic modeling with the [topicmodels package](https://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf)
- Improve accuracy by weighting guesses that produce more likely part-of-speech sequences, using the [OpenNLP package](https://cran.r-project.org/web/packages/openNLP/openNLP.pdf)

***

<br>
<div align="left">
<img src="road.jpeg" width=80% height=80%>
</div>
<br>
