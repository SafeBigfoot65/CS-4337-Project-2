find_exit(M, A) :-
    % Get the size of the Maze
    M = [FirstRow|_],
    length(M, NumRows),
    length(FirstRow, NumCols),
    
    % Find the starting coordinates (s)
    find_start(M, R_start, C_start),

    % Define the starting position
    StartPos = coord(R_start, C_start),

    % Recursively solve the maze
    solve_maze(M, NumRows, NumCols, StartPos, [StartPos], A).

% find the start coordinates
find_start(Map, R, C) :-
    findall(coord(Row, Col), cell_type(Map, Row, Col, s), [coord(R, C)]).

cell_type(Map, R, C, Type) :-
    nth0(R, Map, Row),
    nth0(C, Row, Type).