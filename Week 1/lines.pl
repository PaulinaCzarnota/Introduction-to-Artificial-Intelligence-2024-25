point(1,2).
point(3,2).
point(3,3).

segment(point(1,2), point(3,2)).
segment(point(1,2), point(3,3)).


horizontal(S) :- S = segment(P1, P2),
                    P1 = point(X, Y),
                    P2 = point(Z, Y).

# vertical(segment(point(1,2), point(1,5))).
vertical(S) :- S = segment(P1, P2),
                    P1 = point(Y, X),
                    P2 = point(Y, Z).
                    