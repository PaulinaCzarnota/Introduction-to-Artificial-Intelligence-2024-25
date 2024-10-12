happy(yolanda).
listens2Music(mia).

happy(tom).

% rules
listens2Music(P) :-  happy(P).

playsAirGuitar(mia) :-  listens2Music(mia).
playsAirGuitar(yolanda):-  listens2Music(yolanda). 
