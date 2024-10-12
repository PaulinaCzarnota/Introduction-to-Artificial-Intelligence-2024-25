% Exercise 3.2
% Knowledge Base

% Define direct containment relationships between dolls
directlyIn(katarina, olga).     % katarina is directly in olga
directlyIn(olga, natasha).      % olga is directly in natasha
directlyIn(natasha, irina).     % natasha is directly in irina

% Recursive predicate to determine if doll X is contained in doll Y
in(X, Y) :- directlyIn(X, Y).                % X is directly in Y
in(X, Y) :- directlyIn(Z, Y), in(X, Z).      % X is indirectly in Y through Z

% To run this program, you can try the following options:

% 1.
% ?- in(katarina, natasha).
% true.

% 2.
% ?- in(olga, katarina). 
% false.

% 3.
% ?- in(katarina, irina).
% true.

% 4.
% ?- in(natasha, katarina).
% false.
