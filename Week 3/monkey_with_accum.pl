% Adapted from Bratko, I. (1986) "Prolog: Programming for Artificial Intelligence." Addison-Wesley.  
% Simple version, nice implementation

% For TU856-3 in 2024 AD.

% Monkey and Banana Problem

% Initial state: Monkey is at door,
%                Monkey is on floor,
%                Box is at window,
%                Monkey doesn't have banana.

% Functor state() with 4 parameters to describe the state of the system.
% state(Monkey location in the room, Monkey onbox/onfloor, box location, has/hasnot banana)

% Legal actions

do(state(middle, onbox, middle, hasnot),    % grasp banana
   grasp,
   state(middle, onbox, middle, has)).      % After grasping, the monkey has the banana

do(state(L, onfloor, L, Banana),            % climb box
   climb,
   state(L, onbox, L, Banana)).             % After climbing, the monkey is on the box

do(state(L1, onfloor, L1, Banana),          % push box from L1 to L2
   push(L1, L2),
   state(L2, onfloor, L2, Banana)).         % After pushing, the box is at L2

do(state(L1, onfloor, Box, Banana),         % walk from L1 to L2
   walk(L1, L2),
   state(L2, onfloor, Box, Banana)).        % After walking, the monkey is at L2

do(state(L, onbox, L, has),                 % climb down from box
   climbdown,
   state(L, onfloor, L, has)).              % After climbing down, the monkey is on the floor with the banana

% Clause added: allows the monkey to climb down from the box
% only if it is on the box and has the banana.

do(state(L, onfloor, L, has),                 % eat banana
   eat,                                       % Action: Eat the banana
   state(L, onfloor, L, hasnot)).             % Resulting state: Monkey is still on the floor but no longer has the banana

% Clause added: allows the monkey to eat the banana
% only if it is on the floor and has the banana.

% canget(State): monkey can get banana in State

canget(state(_, _, _, has)).                % Monkey already has it, goal state

canget(State1) :-                           % not goal state, do some work to get it
   do(State1, _, State2),                   % do something (grasp, climb, push, walk)
   canget(State2).                          % canget from State2

% Get plan = list of actions 

canget(state(_, _, _, has), []).            % Monkey already has it, goal state is reached

canget(State1, Plan) :-                     % not goal state, do some work to get it
      do(State1, Action, State2),           % do something (grasp, climb, push, walk) 
      canget(State2, PartialPlan),          % canget from State2
      add(Action, PartialPlan, Plan).       % add action to Plan

add(X,L,[X|L]).

% To run this program, you can try the following options:

% 1.
% ?- canget(state(atdoor, onfloor, atwindow, hasnot)).
% true.

% 2.
% ?- canget(state(atdoor, onfloor, atwindow, hasnot), Plan).
% Plan = [walk(atdoor, atwindow), push(atwindow, middle), climb, grasp]
% false.

% 3.
% ?- canget(state(atwindow, onbox, atwindow, hasnot), Plan).
% false.

% 4.
% ?- canget(state(Monkey, onfloor, atwindow, hasnot), Plan).
% Monkey = atwindow,
% Plan = [push(atwindow, middle), climb, grasp]
% true.
