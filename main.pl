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

