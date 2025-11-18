% User wants to find the path to the exit (FIRST CLAUSE)
find_exit(M, A) :-
    \+ is_list(A),

    % Get the size of the Maze
    M = [FirstRow|_],
    length(M, NumRows),
    length(FirstRow, NumCols),
    
    % Find the starting coordinates (s)
    find_start(M, R_start, C_start),

    % Define the starting position
    StartPos = coord(R_start, C_start),

    % Recursively solve the maze to search for the exit
    solve_maze_search(M, NumRows, NumCols, StartPos, [StartPos], A).

% User provides an action list to reach the exit (SECOND CLAUSE)
find_exit(M, A) :-
    is_list(A),

    % Get the size of the Maze
    M = [FirstRow|_],
    length(M, NumRows),
    length(FirstRow, NumCols),
    
    % Find the starting coordinates (s)
    find_start(M, R_start, C_start),

    % Define the starting position
    StartPos = coord(R_start, C_start),

    % Recursively solve the maze with the provided actions
    traverse_path(M, NumRows, NumCols, StartPos, A, FinalPos),

    FinalPos = coord(R_final, C_final),
    cell_type(M, R_final, C_final, e).

% ==============================================================================


% LOGIC FOR SEARCHING FOR A PATH TO THE EXIT
% Base case for solving the maze
solve_maze_search(Map, _, _, coord(R, C), _, []) :-
    cell_type(Map, R, C, e).

% Recursive case for solving the maze
solve_maze_search(Map, NumRows, NumCols, CurrPos, VisitedList, [Action|Remaining]) :-
    move(CurrPos, Action, NextPos),

    validate_move(Map, NumRows, NumCols, NextPos),

    \+ member(NextPos, VisitedList),

    solve_maze_search(Map, NumRows, NumCols, NextPos, [NextPos|VisitedList], Remaining).

%==============================================================================
% LOGIC FOR TRAVERSING A PATH BASED ON PROVIDED ACTIONS
% Base case for traversing the path
traverse_path(_, _, _, CurrPos, [], CurrPos).

% Recursive case for traversing the path
traverse_path(Map, NumRows, NumCols, CurrPos, [Action|Remaining], FinalPos) :-
    move(CurrPos, Action, NextPos),

    validate_move(Map, NumRows, NumCols, NextPos),

    traverse_path(Map, NumRows, NumCols, NextPos, Remaining, FinalPos).

%=============================================================================

% Validating the move/action
validate_move(Map, NumRows, NumCols, coord(R, C)) :-
    R >= 0, R < NumRows,
    C >= 0, C < NumCols,
    
    cell_type(Map, R, C, CellType),
    CellType \= w.

    \+ member(coord(R, C), VisitedList).

%=============================================================================
% find the start coordinates
find_start(Map, R, C) :-
    findall(coord(Row, Col), cell_type(Map, Row, Col, s), [coord(R, C)]).

cell_type(Map, R, C, Type) :-
    nth0(R, Map, Row),
    nth0(C, Row, Type).

% =============================================================================
% Defining possible moves
move(coord(R, C), left, coord(R, C1)) :- C1 is C - 1. % left
move(coord(R, C), right, coord(R, C1)) :- C1 is C + 1. % right
move(coord(R, C), up, coord(R1, C)) :- R1 is R - 1. % up
move(coord(R, C), down, coord(R1, C)) :- R1 is R + 1. % down