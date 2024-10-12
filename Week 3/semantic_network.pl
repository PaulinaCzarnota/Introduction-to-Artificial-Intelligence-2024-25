% Declare predicates as discontiguous
:- discontiguous is_a/2.
:- discontiguous has/2.
:- discontiguous lives_in/2.
:- discontiguous isa/2.

% Base knowledge
is_a(cat, mammal).
is_a(bear, mammal).
is_a(mammal, animal).
is_a(fish, animal).
is_a(whale, mammal).
is_a(trout, fish).

has(cat, fur).
has(bear, fur).
has(mammal, vertebra).
has(whale, blubber). % New knowledge
has(trout, scales).  % New knowledge

lives_in(fish, water).
lives_in(whale, water).
lives_in(bear, forest). % New knowledge
lives_in(cat, house).   % New knowledge

% Inference rules
isa(A, B) :- is_a(A, B).
isa(A, B) :- is_a(A, C), isa(C, B).

has(A, B) :- isa(A, C), has(C, B).
lives_in(A, B) :- isa(A, C), lives_in(C, B).

% Instance knowledge
instance(tommy, bear).
instance(moby, whale).
instance(whiskers, cat). % New instance
instance(nemo, trout).    % New instance

% Inference for instances
isa(X, Y) :- instance(X, Y).

% Additional rules for inference
can_fly(X) :- isa(X, bird).  % Birds can fly
can_fly(X) :- instance(X, sparrow). % Specific instance

% Example additional knowledge
is_a(sparrow, bird). % New knowledge
has(bird, feathers).  % New knowledge
lives_in(bird, trees). % New knowledge

% Query examples
% To find out if an instance has certain features:
% ?- has(tommy, fur).
% To check if something can fly:
% ?- can_fly(sparrow).
% To find out where an instance lives:
% ?- lives_in(tommy, X).
