man(jim).
man(mary).
mortal(X) :- man(X).
likes(X,A) :- man(X), dog(A).
dog(rex).
dog(lassie).

cat(mart).

hates(X, Y) :- cat(X), dog(Y).
