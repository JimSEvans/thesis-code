#thesis-code

This is a collection of code that I am writing to do my thesis project, which involves collecting tweets and doing text classification. I used [my branch of applied-nlp](https://github.com/JimSEvans/applied-nlp) (i.e. my code submission for [this homework assignment](https://github.com/utcompling/applied-nlp/wiki/Homework4)) as a starting point. Some of that code was from the applied-nlp master stub files for the assignment, and was not written by me.

#setting up

Put both of these in your .bash_profile and/or .bashrc:

export TC_DIR=/path/to/wherever/you/put/thesis-code   #e.g. on a Mac, /Users/john/repositories/thesis-code
export PATH=$PATH:$TC_DIR/bin

This repository is meant to be used in conjunction with [Nak](https://github.com/scalanlp/nak) to do text classification. My code feauturizes the text and spits out a feature vector file in the proper format for Nak to use.

#example:

$ cd TC_DIR/data
$ mkdir out
$ tc run thesiscode.classify.ScFeatures sc/raw.training.txt > out/training
$ tc run thesiscode.classify.ScFeatures sc/raw.test.txt > out/test
$ nak classify --train out/training --eval out/test
 