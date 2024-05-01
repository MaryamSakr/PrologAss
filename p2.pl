:- discontiguous move_to_goal/2.

% Input
initial_board([
    [red, red, blue, blue],
    [red, red, red, red],
    [yellow, yellow, yellow, yellow],
    [red, red, red, red]
]).
initial_state(state(0, 0, red)).



% Get the neighbors of a cell with the same color
neighborhood(state(X, Y, Color), Neighbors) :-
    initial_board(Board),
    length(Board, Rows),
    length(Board, Cols),
    findall(state(X1, Y1, Color),
            (   between(0, Rows, X1),
                between(0, Cols, Y1),
                adjacent(X, Y, X1, Y1),
                nth0(X1, Board, Row),
                nth0(Y1, Row, Color)
            ),
            Neighbors).

% Check if two positions are adjacent (no diagonal moves)
adjacent(X1, Y1, X2, Y2) :-
    (X1 =:= X2, abs(Y1 - Y2) =:= 1);
    (Y1 =:= Y2, abs(X1 - X2) =:= 1).

% Define the goal state
goal_state(state(_, Y, _)) :-
    Y =:= 3. % Goal state is any cell in the last column

% Move from current state to the goal state
move_to_goal(CurrentState, Path) :-
    move_to_goal_helper(CurrentState, [CurrentState], Path).

move_to_goal_helper(state(X, Y, Color), Visited, [state(X, Y, Color) | Visited]) :-
    goal_state(state(X, Y, Color)).

move_to_goal_helper(CurrentState, Visited, Path) :-
    neighborhood(CurrentState, Neighbors),
    member(Neighbor, Neighbors),
    \+ member(Neighbor, Visited),
    move_to_goal_helper(Neighbor, [Neighbor | Visited], Path).

% Move to the state below the current state
move_to_goal(CurrentState, Path) :-
    state_below(CurrentState, NextState),
    move_to_goal(NextState, Path).

state_below(state(X, Y, _), state(BelowX, BelowY, Color)) :-
    initial_board(Board),
    length(Board, Rows),
    BelowX is X + 1,
    BelowY is Y,
    BelowX < Rows,
    nth0(BelowX, Board, Row),
    nth0(BelowY, Row, Color).

print_path([]).
print_path([state(X, Y, _) | Path]) :-
    format("~d,~d", [X, Y]),
    (Path = [] -> writeln(''); format(' -> ', [])),
    print_path(Path).

% Predicate to find and print the path from the initial state to the goal state
search :-
    initial_state(S),
    move_to_goal(S, Path),
    reverse(Path, PathReversed),
    print_path(PathReversed).
