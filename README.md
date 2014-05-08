#thesis-code

This is a collection of code that I am writing to do my thesis project, which involves collecting tweets and doing text classification.

#setting up

Put both of these in your .bash_profile and/or .bashrc:

export TC_DIR=/path/to/wherever/you/put/thesis-code   #e.g. on a Mac, /Users/john/repositories/thesis-code
export PATH=$PATH:$TC_DIR/bin

To do any of the classification stuff, you'll need my Twitter corpus. I haven't made that available yet, but I will. To use some kinds of features, you will also need the Linguistic Inquiry and Word Count (LIWC) dictionary file. I cannot provide it, and it is not available free of charge.

#example:

$ cd TC_DIR
$ tc classify full/train/all full/dev/all uni-c=1-hashing:500000 none none

