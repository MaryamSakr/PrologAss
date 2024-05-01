head([Head|_], Head).

is_in_board(Board, Row, Col) :-
    length(Board, Rows),
    Rows > 0,
    head(Board, Head),
    length(Head, Cols),
    Row >= 0,
    Row < Rows,
    Col >= 0,
    Col < Cols.

get_color(Row, Col, Board, Color) :-
    nth0(Row, Board, RowList),
    nth0(Col, RowList, Color).

is_same_color(Col1, Row1, Col2, Row2, Board) :-
    get_color(Row1, Col1, Board, Color),
    get_color(Row2, Col2, Board, Color).

move_right(Board, Row, Col, NewCol) :-
    NewCol is Col + 1,
    is_in_board(Board, Row, NewCol),
    is_same_color(Col, Row, NewCol, Row, Board).

move_down(Board, Row, Col, NewRow) :-
    NewRow is Row + 1,
    is_in_board(Board, NewRow, Col),
    is_same_color(Col, Row, Col, NewRow, Board).

move_up(Board, Row, Col, NewRow) :-
    NewRow is Row - 1,
    is_in_board(Board, NewRow, Col),
    is_same_color(Col, Row, Col, NewRow, Board).

move_left(Board, Row, Col, NewCol) :-
    NewCol is Col - 1,
    is_in_board(Board, Row, NewCol),
    is_same_color(Col, Row, NewCol, Row, Board).

is_cycle(RootRow, RootCol, Visited, CurrentRow, CurrentCol) :-
    length(Visited, Length),
    Length >= 4,
    CurrentCol =:= RootCol,
    CurrentRow =:= RootRow + 1,
    member((CurrentRow, CurrentCol, _), Visited).  % Include color in the visited list

find_cycle(Board, RootRow, RootCol, Visited, Row, Col, CurrentRow, CurrentCol, Cycle) :-
    move_right(Board, CurrentRow, CurrentCol, RightCol),
    \+ member((CurrentRow, RightCol, _), Visited),  % Include color in the visited list
    get_color(CurrentRow, RightCol, Board, Color),  % Get the color at the current position
    find_cycle(Board, RootRow, RootCol, [(CurrentRow, RightCol, Color) | Visited], Row, Col, CurrentRow, RightCol, Cycle).

find_cycle(Board, RootRow, RootCol, Visited, Row, Col, CurrentRow, CurrentCol, Cycle) :-
    move_down(Board, CurrentRow, CurrentCol, DownRow),
    \+ member((DownRow, CurrentCol, _), Visited),  % Include color in the visited list
    get_color(DownRow, CurrentCol, Board, Color),  % Get the color at the current position
    find_cycle(Board, RootRow, RootCol, [(DownRow, CurrentCol, Color) | Visited], Row, Col, DownRow, CurrentCol, Cycle).

find_cycle(Board, RootRow, RootCol, Visited, Row, Col, CurrentRow, CurrentCol, Cycle) :-
    move_left(Board, CurrentRow, CurrentCol, LeftCol),
    \+ member((CurrentRow, LeftCol, _), Visited),  % Include color in the visited list
    get_color(CurrentRow, LeftCol, Board, Color),  % Get the color at the current position
    find_cycle(Board, RootRow, RootCol, [(CurrentRow, LeftCol, Color) | Visited], Row, Col, CurrentRow, LeftCol, Cycle).

find_cycle(_, RootRow, RootCol, Visited, _, _, CurrentRow, CurrentCol, Cycle) :-
    is_cycle(RootRow, RootCol, Visited, CurrentRow, CurrentCol),
    reverse(Visited, Cycle).

find_cycle(Board, Cycle) :-
    length(Board, Rows),
    Rows > 0,
    head(Board, Head),
    length(Head, Cols),
    find_cycle(Board, 0, 0, [(0, 0, _)], Rows, Cols, 0, 0, Cycle).  % Include a placeholder for color

example_board(Board, Cycle) :-
    length(Board, Rows),
    Rows > 0,
    head(Board, Head),
    length(Head, Cols),
    (
        between(0, Rows, Row),
        between(0, Cols, Col),
        find_cycle(Board, Row, Col, [(Row, Col, _)], Rows, Cols, Row,Col, Cycle),
        print_cycle(Cycle, Board),  
        !
    ).

print_cycle([], _).
print_cycle([(Row, Col, Color)|_], Board) :-
    get_color(Row, Col, Board, Color).
