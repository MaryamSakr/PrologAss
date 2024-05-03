:- discontiguous move_to_goal/2.

% Input
initial_board([
    [red, red, red, blue],
    [red, red, red, red],
    [yellow, yellow, yellow, yellow],
    [red, red, red, red]
]).
%state representation
initial_state(state(0, 0, Color)) :-
    initial_board(Board),
    nth0(0, Board, FirstRow),
    nth0(0, FirstRow, Color).

num_columns(NumColumns) :-
    initial_board(Board),
    nth0(0, Board, FirstRow),
    length(FirstRow, NumColumns).

goal_state(state(_, Y, _)) :-
    num_columns(NumCols),
    Y is NumCols - 1.  
 

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



% Move 
move_to_goal(CurrentState, Path) :-
    move(CurrentState, [CurrentState], Path).

move(state(X, Y, Color), Visited, [state(X, Y, Color) | Visited]) :-
    goal_state(state(X, Y, Color)).

move(CurrentState, Visited, Path) :-
    neighborhood(CurrentState, Neighbors),
    sort_neighbors(Neighbors, SortedNeighbors), % Sort Neighbors
    member(Neighbor, SortedNeighbors),
    \+ member(Neighbor, Visited),
    move(Neighbor, [Neighbor | Visited], Path).

% Move to the state below the current state
move_to_goal(CurrentState, Path) :-
    state_below(CurrentState, NextState),
    NextState \== CurrentState, % Ensure not revisiting the initial state
    move_to_goal(NextState, Path).

% heuristic function 
sort_neighbors(Neighbors, SortedNeighbors) :-
    map_list_to_pairs(difference_to_goal, Neighbors, NeighborsWithDiff),
    keysort(NeighborsWithDiff, SortedNeighborsWithDiff),
    pairs_values(SortedNeighborsWithDiff, SortedNeighbors).

% Calculate the difference between the column of a state and the goal column (3)
difference_to_goal(state(_, Y, _), Diff) :-
    num_columns(NumCols),
    Diff is abs(NumCols -1 - Y).




state_below(state(X, Y, _), state(BelowX, BelowY, Color)) :-
    initial_board(Board),
    length(Board, Rows),
    BelowX is X + 1,
    BelowY is Y,
    BelowX < Rows,
    nth0(BelowX, Board, Row),
    nth0(BelowY, Row, Color).


% output
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
