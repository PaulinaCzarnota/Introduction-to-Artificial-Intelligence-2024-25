% Define the predicates and facts for digestion
is_digesting(X, Y) :- just_ate(X, Y).
is_digesting(X, Y) :- just_ate(X, Z), is_digesting(Z, Y).

just_ate(mosquito, blood(john)).
just_ate(frog, mosquito).
just_ate(stork, frog).

% Define the predicates and facts for descendants
child(bridget, caroline).
child(caroline, donna).
child(anne, bridget).
child(donna, emily).

descend(X, Y) :- child(X, Y).
descend(X, Y) :- child(X, Z), descend(Z, Y).
