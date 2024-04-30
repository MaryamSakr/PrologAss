
head([Head|_], Head).

is_in_board(Board ,StartRow , StartCol):-
    head(Board, Head),
    length(Head , Col),
    length(Board , Row),
    StartRow < Row,
    StartRow >= 0,
    StartCol < Col,
    StartCol >=0.

get_color(Col , Row, Board , Color):-
    nth0(Row , Board , RowList),
    nth0(Col , RowList , Color).

is_same_color(Col , NewCol , Row , NewRow , Board ):-
    get_color(Col , Row , Board ,Color),
    get_color(NewCol , NewRow , Board ,Color).

move_right(Board , Row , Col , CurrentRow , CurrentCol , NewCol):-
    NewCol is CurrentCol+1,
    is_in_board(Board , CurrentRow , NewCol),
    is_same_color(CurrentCol , NewCol , CurrentRow , CurrentRow , Board).
    

move_down(Board , Row , Col , CurrentRow , CurrentCol , NewRow):-
    NewRow is CurrentRow+1,
    is_in_board(Board , NewRow , CurrentCol),
    is_same_color(CurrentCol , CurrentCol , CurrentRow , NewRow , Board).
    

move_up(Board , Row , Col , CurrentRow , CurrentCol , NewRow):-
    NewRow is CurrentRow-1,
    is_in_board(Board , NewRow , CurrentCol),
    is_same_color(CurrentCol , NewCol , CurrentRow , NewRow , Board).
    

move_left(Board , Row , Col , CurrentRow , CurrentCol , NewCol):-
    NewCol is CurrentCol-1,
    is_in_board(Board , CurrentRow , NewCol),
    is_same_color(CurrentCol , NewCol , CurrentRow , CurrentRow , Board).


    

find_cycle(Board , RootRow , RootCol ,Visited , Row , Col , CurrentRow , CurrentCol , Cycle):-
    move_right(Board , Row , Col , CurrentRow , CurrentCol , RightCol),
    \+(member([(CurrentRow , RightCol)] , Visited)),
    find_cycle(Board , CurrentRow , CurrentCol , [[(CurrentRow , RightCol)]|Visited] , Row , Col , CurrentRow , RightCol , Cycle);
     move_down(Board , Row , Col , CurrentRow , CurrentCol , DownRow),
    \+(member([(DownRow , RightCol)] , Visited)),
    find_cycle(Board , CurrentRow , CurrentCol , [[(DownRow , RightCol)]|Visited] , Row , Col , DownRow , RightCol , Cycle);
     move_left(Board , Row , Col , CurrentRow , CurrentCol , LeftCol),
    \+(member([(DownRow , LeftCol)] , Visited)), 
    find_cycle(Board ,CurrentRow , CurrentCol , [[(DownRow , LeftCol)]|Visited] , Row , Col , DownRow , LeftCol , Cycle);
    move_up(Board , Row , Col , CurrentRow , CurrentCol , UpRow),
	\+(member([(UpRow , LeftCol)] , Visited)).
    
example_board(Board ,Cycle):-
    head(Board, Head),
    length(Head , Col),
    length(Board , Row),
    find_cycle(Board , 0 , 0 , [(0,0)] ,Row , Col , 0 , 0 , Cycle).
    