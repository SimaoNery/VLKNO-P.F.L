% Initialize the GameState based on the GameConfig given
initial_state(
        game_config(BoardSize, Player1Type, Player2Type, Player1Name, Player2Name, _), 
        game_state(Board, Player1Name, PlayerInfo, [Player1Pos, Player2Pos])) :-

    % Build the Board
    build_board(BoardSize, BoardSize, Board),

    % Set Player Info
    PlayerInfo = [Player1Type-Player1Name, Player2Type-Player2Name],

    % Set Initial Positions for both Players
    Player1Pos = (1, 1),
    Player2Pos = (BoardSize, BoardSize).

% Builds the initial board with the desired size (all stacks size 1)
build_board(0, _, []). % Base Case
build_board(RowSize, ColSize, [Row | Board]) :-
    build_row(ColSize, Row), % Dispplays a row at a time
    NewRowSize is RowSize - 1,
    build_board(NewRowSize, ColSize, Board).

% Builds the rows for the board (all stacks size 1)
build_row(0, []).
build_row(Size, [1 | Row]) :-
    NewSize is Size - 1,
    build_row(NewSize, Row). % Recursively generates the rest of the row


% Displays the current game state to the terminal (PlayerInfo and Board)
display_game(game_state(Board, CurrentPlayer, PlayerInfo, [Player1Pos, Player2Pos])) :-
    write('Current Player: '), write(CurrentPlayer), nl, nl, nl,
    display_board(Board, [Player1Pos, Player2Pos]), nl.

% Displays the board
display_board(Board, PlayerPositions) :-
    length(Board, BoardLength),
    display_rows(Board, BoardLength, PlayerPositions), % Starts from the last row
    write('   _ _ _ _ _ _ _ _ _'), nl,
    write('    '),
    display_columns(1, BoardLength).

% Displays the number of the columns above the board
display_columns(Current, End) :-
    Current =< End,
    write(' '), write(Current), write(' '),
    Next is Current + 1,
    display_columns(Next, End).

display_columns(_, _). % Terminates if Current > End

% Displays all rows and their numbers to the side
display_rows([], _, _). 
display_rows([Row | Rest], RowNum, PlayerPositions) :-
    write(RowNum), write(' | '), % Print row number + separator
    display_tiles(Row, RowNum, 1, PlayerPositions), nl,
    NextRowNum is RowNum - 1,
    display_rows(Rest, NextRowNum, PlayerPositions).

% Display "P1" if this is Player1 tile
display_tiles([_ | Rest], RowNum, ColNum, [(RowNum, ColNum), Player2Pos]) :-
    write('[P1]'),
    NextColNum is ColNum + 1,
    display_tiles(Rest, RowNum, NextColNum, [(RowNum, ColNum), Player2Pos]), !.

% Display "P2" if this is Player2 tile
display_tiles([_ | Rest], RowNum, ColNum, [Player1Pos, (RowNum, ColNum)]) :-
    write('[P2]'),
    NextColNum is ColNum + 1,
    display_tiles(Rest, RowNum, NextColNum, [Player1Pos, (RowNum, ColNum)]), !.

% Display regular stack of tiles 
display_tiles([Stone | Rest], RowNum, ColNum, [Player1Pos, Player2Pos]) :-
    write('['), write(Stone), write(']'),
    NextColNum is ColNum + 1,
    display_tiles(Rest, RowNum, NextColNum, [Player1Pos, Player2Pos]).

display_tiles([], _, _, _). % Base Case