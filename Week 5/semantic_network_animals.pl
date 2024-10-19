% Semantic Network Example in Prolog
% This code represents a semantic network for animals, particularly focusing on cats and mammals.
% It includes relationships, properties, actions, and inference rules.

% Discontiguous declarations to avoid warnings about clause organization
:- discontiguous has/2.
:- discontiguous lives_in/2.
:- discontiguous caught/2.
:- discontiguous is_a/2.

% Is-a relationships define categories and hierarchies in the semantic network.
is_a(cat, mammal).
is_a(bear, mammal).
is_a(whale, mammal).
is_a(dolphin, mammal).
is_a(mammal, animal).
is_a(fish, animal).
is_a(trout, fish).
is_a(bird, animal).
is_a(penguin, bird).
is_a(amphibian, animal).
is_a(salamander, amphibian).
is_a(reptile, animal).
is_a(snake, reptile).
is_a(crocodile, reptile).
is_a(eagle, bird).
is_a(animal, living_thing).

% Properties of entities define characteristics or attributes associated with each entity.
has(cat, fur).
has(bear, fur).
has(mammal, vertebra).
has(fish, fins).
has(whale, blubber).
has(dolphin, sonar).
has(bird, feathers).
has(amphibian, moist_skin).
has(reptile, scales).
has(tommy, color(ginger)).
has(tommy, likes(cream)).
has(tommy, owner(john)).

% Actions and behaviors describe what entities can do or their activities.
caught(tommy, bird).
sat_on(tommy, mat).

% Habitat relationships indicate where entities live.
lives_in(cat, home).
lives_in(fish, water).
lives_in(whale, water).
lives_in(dolphin, water).
lives_in(penguin, water).
lives_in(bear, forest).
lives_in(eagle, trees).
lives_in(salamander, damp_areas).
lives_in(snake, various_environments).
lives_in(crocodile, rivers_and_lakes).

% Instance definitions connect specific entities to their types.
instance(tommy, cat).
instance(john, person).
instance(moby, whale).
instance(sam, dolphin).
instance(squawk, parrot).
instance(freddy, frog).
instance(slinky, snake).
instance(chuck, crocodile).
instance(bob, eagle).

% Inference rules for navigating the knowledge network.
isa(A, B) :- is_a(A, B).            % Direct is-a relationship.
isa(A, B) :- is_a(A, C), isa(C, B). % Indirect is-a relationship (transitive).
isa(X, Y) :- instance(X, Y).         % Instance-to-class inference.

% Property inheritance based on is-a relationships.
has(A, B) :- isa(A, C), has(C, B).  

% Habitat inheritance.
lives_in(A, B) :- isa(A, C), lives_in(C, B).

% Additional inference rules to retrieve information.
likes(Animal, Food) :- has(Animal, likes(Food)).
owner(Animal, Owner) :- has(Animal, owner(Owner)).
caught_by(Animal, Prey) :- caught(Animal, Prey).
find_habitat(Entity, Habitat) :- lives_in(Entity, Habitat).

% Querying relationships
find_animals(Type, Animals) :-              
    findall(Animal, isa(Animal, Type), Animals).

find_properties(Entity, Properties) :-      
    findall(Property, has(Entity, Property), Properties).

% --- Instructions to Run the Program --- 
% To run the program: 
% Load the code in SWI-Prolog and run the following queries to interact with the knowledge base: 
% ?- find_animals(mammal, Animals).           
% ?- find_properties(tommy, Properties).      
% ?- find_habitat(tommy, Habitat).             
% ?- owner(tommy, Owner).                       
% ?- likes(tommy, What).                        
% ?- caught_by(tommy, Prey).                    