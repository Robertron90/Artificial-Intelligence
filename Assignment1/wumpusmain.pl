% Several Paths

  % % % % % % % % % %
% _ _ _ _ _ _ _ _ _ _%
% _ _ _ _ _ _ _ _ _ _%
% _ _ _ _ _ _ _ _ _ _%
% _ _ _ _ _ _ _ _ _ _%
% _ _ _ _ _ _ _ _ _ _%
% _ _ _ _ _ _ _ _ _ _%
% _ _ _ G _ _ _ _ _ _%
% _ _ _ _ _ _ _ _ _ _%
% W _ _ _ _ _ _ _ _ _%
% S W _ _ _ _ _ _ _ _%
  % % % % % % % % % % 

% walls
bump([_,1], south).
bump([_,10], north).
bump([1,_], west).
bump([10,_], east).

stench([1,1]).
stench([2,2]).
stench([1,3]).
stench([3,1]).

glint([4,4]).




thereIsStench(yes):-
    stench([1,1]);
    stench([2,2]);
    stench([1,3]);
    stench([3,1]).

thereIsStench(no).


breeze(X) :-
	false.


:- use_module(library(lists)).
:- use_module(library(random)).

:- dynamic	 
	  wumpus_world_extent/1,	  
	  wumpus_health/1,	  
	  agent_location/2,
	  agent_orientation/1,	 
	  agent_health/1,
	  agent_gold/1,
	  agent_arrows/1,
	  agent_score/1.

getReadyToShoot(Ready):-
    thereIsStench(Ready).


wumpusIsKilled([X, Y]):-
    shoot_arrow(Scream).


% shoot_arrow(Scream): If agent has an arrow, then shoot it in the
%   direction the agent is facing and listen for Scream.

shoot_arrow(Scream) :-
  getReadyToShoot(Ready),
  agent_arrows(Arrows),
  Arrows > 0, !,                  % agent has an arrow and will use it!
  Arrows1 is Arrows - 1,          %   update number of arrows
  retract(agent_arrows(Arrows)),
  assert(agent_arrows(Arrows1)), 
  agent_location(X,Y),
  agent_orientation(Angle),
  propagate_arrow(X,Y,Angle,Scream).

shoot_arrow(no).



% propagate_arrow(X,Y,Angle,Scream): If wumpus is at X,Y then hear its
%   woeful scream as you vanquish the creature.  If not, then move arrow
%   one square along Angle and try again.  If arrow hits a wall, then
%   you missed.

kill_wumpus :-
  retract(wumpus_health(alive)),
  assert(wumpus_health(dead)).

propagate_arrow(X,Y,_,yes) :-
  wumpus([X, Y]), !,
  kill_wumpus.

propagate_arrow(X,Y,0,Scream) :-
  X1 is X + 1,
  wumpus_world_extent(E),
  X1 =< E,
  !,
  propagate_arrow(X1,Y,0,Scream).

propagate_arrow(X,Y,90,Scream) :-
  Y1 is Y + 1,
  wumpus_world_extent(E),
  Y1 =< E,
  !,
  propagate_arrow(X,Y1,90,Scream).

propagate_arrow(X,Y,180,Scream) :-
  X1 is X - 1,
  X1 > 0,
  !,
  propagate_arrow(X1,Y,180,Scream).

propagate_arrow(X,Y,270,Scream) :-
  Y1 is Y - 1,
  Y1 > 0,
  !,
  propagate_arrow(X,Y1,270,Scream).

propagate_arrow(_,_,_,no).



pit([X, Y]) :-
	NNX is X - 1, NPX is X + 1, NNY is Y - 1, NPY is Y + 1,
	(breeze([NNX, Y]); bump([X,Y], west)),
	(breeze([NPX, Y]); bump([X,Y], east)),
	(breeze([X, NNY]); bump([X,Y], south)),
	(breeze([X, NPY]); bump([X,Y], north)).

wumpus([X, Y]) :-
  NNX is X - 1, NPX is X + 1, NNY is Y - 1, NPY is Y + 1,
	(stench([NNX, Y]); bump([X,Y], west)),
	(stench([NPX, Y]); bump([X,Y], east)),
	(stench([X, NNY]); bump([X,Y], south)),
	(stench([X, NPY]); bump([X,Y], north)).

safe(X) :-
	not(pit(X)),
	not(wumpus(X));
    wumpusIsKilled(X).

forward([[X, Y], D], M, forward) :-
	(D = east, NX is X + 1, not(bump([X, Y], D)), safe([NX, Y]), M = [[NX, Y], D]);
	(D = north, NY is Y + 1, not(bump([X, Y], D)), safe([X, NY]), M = [[X, NY], D]);
	(D = south, NY is Y - 1, not(bump([X, Y], D)), safe([X, NY]), M = [[X, NY], D]);
	(D = west, NX is X - 1, not(bump([X, Y], D)), safe([NX, Y]), M = [[NX, Y], D]).

left([[X, Y], D], M, left) :-
	(D = east, M = [[X, Y], north]);
	(D = north, M = [[X, Y], west]);
	(D = west, M = [[X, Y], south]);
	(D = south, M = [[X, Y], east]).

right([[X, Y], D], M, right) :-
	(D = west, M = [[X, Y], north]);
	(D = north, M = [[X, Y], east]);
	(D = east, M = [[X, Y], south]);
	(D = south, M = [[X, Y], west]).

opposite(forward, forward).
opposite(left, right).
opposite(right, left).

moves(X, M, S, O) :-
	(forward(X, M, S); right(X, M, S); left(X, M, S)),
	opposite(S, O).

member(X, [X|_]).
member(X, [_|CL]) :-
	member(X, CL).

move([X, _], [], _, [pickup, right, right], []) :-
	glint(X).
move([X, D], [M | P], CL, [S|F], [O|R]) :-
	not(glint(X)), % force pickup to reduce paths
	moves([X, D], M, S, O),
	not(member(M,CL)),
	move(M, P, [M|CL], F, R).

reverse([], X, X).
reverse([X|B], Y, R) :- 
	reverse(B, [X|Y], R).

join([], R, R).
join([X|F], R, [X|S]) :-
	join(F, R, S).

findPath(S) :-
	X = [[1,1], north],
	move(X, _, [X], F, B),
	reverse(B, [], R),
	join(F, R, S).
