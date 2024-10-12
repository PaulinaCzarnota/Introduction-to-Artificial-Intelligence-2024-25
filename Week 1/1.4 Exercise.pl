% Facts
killer(butch).
married(mia, marsellus).
married(marsellus, mia).
dead(zed).

% Rules
kills(marsellus, X) :- gives(X, mia, footmassage).
loves(mia, X) :- good_dancer(X).
eats(jules, X) :- nutritious(X).
eats(jules, X) :- tasty(X).
