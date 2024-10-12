% Semantic Network example 1 in Prolog
% with some inference rules for navigating the knowledge network

is_a(cat, mammal).
is_a(bear, mammal).
is_a(mammal, animal).
is_a(fish, animal).
is_a(whale, mammal).
is_a(trout, fish).

has(cat, fur).
has(bear, fur).
has(mammal, verebra).

lives_in(fish, water).
lives_in(whale, water).

isa(A,B) :- is_a(A,B).
isa(A,B) :- is_a(A,C), isa(C,B).


has(A,B) :- isa(A,C), has(C,B).
lives_in(A,B) :- isa(A,C), lives_in(C, B).
