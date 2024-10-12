% Exercise 3.3: Train Travel

% Knowledge base for direct train connections
directTrain(saarbruecken, dudweiler).
directTrain(forbach, saarbruecken).
directTrain(freyming, forbach).
directTrain(stAvold, freyming).
directTrain(fahlquemont, stAvold).
directTrain(metz, fahlquemont).
directTrain(nancy, metz).

% Recursive predicate to check travel possibility
travelFromTo(Town1, Town2) :- directTrain(Town1, Town2).
travelFromTo(Town1, Town2) :- directTrain(Town1, Town3), travelFromTo(Town3, Town2).
