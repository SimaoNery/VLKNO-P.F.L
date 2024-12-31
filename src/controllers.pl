:- use_module(library(lists))

% Runs the game between the players while it isn't Game Over
game_loop(game_state(Board, CurrentPlayer, PlayersInfo, PlayersPositions)) :-
    % Display the current game state
    display_game(game_state(Board, CurrentPlayer, PlayersInfo, PlayersPositions)),

    % TO DO: Checks if the game is over

    % Ask Player For a Move
    write(CurrentPlayer), write('It\'s Your Turn!'), nl, nl,

    % Show the player moves he can do
    valid_moves(game_state(Board, CurrentPlayer, PlayersInfo, PlayersPositions), ValidMoves),
    write('This are your valid moves: '), nl, 
    write(ValidMoves), nl,

    write('Enter Your Move: '), nl,
    read(Move),

    % Validate the move and execute it if valid, otherwise, asks for a new valid move
    move(game_state(Board, CurrentPlayer, PlayersInfo, PlayerPositions), Move, NewGameState).

    % TO DO: Give a value to the current state
    
    % Return to the loop

    % TO DO: See How To Handle AI

% Initialize the GameState based on the GameConfig given
initial_state(
        game_config(BoardSize, Player1Type, Player2Type, Player1Name, Player2Name, _), 
        game_state(Board, CurrentPlayer, PlayerInfo, [Player1Name-(1,1), Player2Name-(BoardSize, BoardSize)])) :-

    % Build the Board
    build_board(BoardSize, BoardSize, Board),

    % Set Player Info
    PlayerInfo = [Player1Type-Player1Name-, Player2Type-Player2Name],

% ---Board Building---------------------------------------------------------

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
%---------------------------------------------------------------------------


% ---Board Displaying-------------------------------------------------------

% Displays the current game state to the terminal (PlayerInfo and Board)
display_game(game_state(Board, CurrentPlayer, PlayerInfo, PlayerPositions)) :-
    display_board(Board, PlayerPositions), nl, nl,nl, % Display Board
    write('Current Player: '), write(CurrentPlayer), nl. % Say who turn is

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
display_tiles([_ | Rest], RowNum, ColNum, [Player-(RowNum, ColNum) | Player2Pos]) :-
    write('[P1]'),
    NextColNum is ColNum + 1,
    display_tiles(Rest, RowNum, NextColNum, [Player-(RowNum, ColNum) | Player2Pos]).

% Display "P2" if this is Player2 tile
display_tiles([_ | Rest], RowNum, ColNum, [Player1Pos | Player-(RowNum, ColNum)]) :-
    write('[P2]'),
    NextColNum is ColNum + 1,
    display_tiles(Rest, RowNum, NextColNum, [Player1Pos | Player-(RowNum, ColNum)]).

% Display regular stack of tiles 
display_tiles([Stone | Rest], RowNum, ColNum, PlayerPositions) :-
    write('['), write(Stone), write(']'),
    NextColNum is ColNum + 1,
    display_tiles(Rest, RowNum, NextColNum, PlayerPositions).

display_tiles([], _, _, _). % Base Case
%---------------------------------------------------------------------------


% --- Player Actions -------------------------------------------------------

% Changes the game state accordingly to a player new move
move(game_state(Board, CurrentPlayer, PlayersInfo, PlayersPosition),
     Move,
     game_state(NewBoard, NextPlayer, PlayersInfo, NewPlayersPositions)) :-
    
    % Validate the Move
    valid_moves(game_state(Board, CurrentPlayer, PlayersINfo, PlayersPositions), ValidMoves),
    member(Move, ValidMoves) % If the move is in valid moves it's valid

    % Execute Move
    execute_move(Board, CurrentPlayer, PlayersPositions, Move, NewBoard, NewPlayersPositions),
    % Switch players
    switch_players(CurrentPlayer, PlayersInfo, NextPlayer).


% Executes a valid move
executes_move(Board, CurrentPlayer, PlayersPositions, Move, NewBoard, NewPlayersPositions) :-
    % Get the coordinates for the players piece
    get_piece_position(CurrentPlayer, PlayersPositions, CurrentPosition),

    % Translate the move into new coordinates
    translate_move(Move, CurrentPosition, FinalPosition),

    % Move the piece
    update_piece_coordinates(CurrentPlayer, FinalPosition, PlayersPositions, NewPlayersPositions),

    % Changes a stone of location based on player choice
    pick_and_place_stone(Board, CurrentPosition, FinalPosition, NewBoard).


% Will choose a stone and change its location based on user input
pick_and_place_stone(Board, CurrentPosition, FinalPosition, NewBoard) :-
    % Determine the smallest unoccupied stack (excluding the previous position)
    find_smallest_stack(Board, CurrentPosition, SmallestStackPosition),

    % Ask the player where to place it
    write('Choose the coordinates to place the stone!'), nl,
    write('     =>X: '), nl,
    read(X)
    write('     =>Y: '), nl,
    read(Y),

    % Ensure the coordinates are valid
    validate_stone_placement(Board, CurrentPosition, FinalPosition, (X, Y)),

    % Update the stacks on the board
    move_stone(Board, SmallestStackPosition, (X, Y), NewBoard).


% Switch Players
switch_players(Player1, [Player1-_, Player2-_], Player2).
switch_players(Player2, [Player1-_, Player2-_], Player1).

%------------------------------------------------------------------------------------------

% --- Auxiliary Move Predicates ---------------------------------------------------------------------

% Creates a list with all the valid moves (This calls validate_move())
valid_moves(game_state(Board, CurrentPlayer, _, PlayersPositions), ValidMoves) :-
    get_piece_position(CurrentPlayer, PlayersPositions, CurrentPosition),
    findall(Move, validate_move(Board, CurrentPlayer, PlayersPositions, Move), ValidMoves).


% Validate the move to be done
validate_move(Board, CurrentPlayer, PlayersPositions, Direction) :-
    % Get the coordinates for the players piece
    get_piece_position(CurrentPlayer, PlayersPositions, CurrentPosition),

    % Translate the move into new coordinates
    translate_move(Move, CurrentPosition, FinalPosition),

    % Validate final position
    validate_final_position(Board, CurrentPosition, FinalPosition, PlayersPositions).

% Gets the coordinates of the current players piece
get_piece_position(Player, [Player-(X, Y) | _], (X, Y)).
get_piece_position(Player, [_ | Player-(X, Y)], (X, Y)).

% Translates the move choosen by the player to the coordinates resulting of that move
translate_move(up, (X, Y), (X, Y1)) :- Y1 is Y + 1.
translate_move(down, (X, Y), (X, Y1)) :- Y1 is Y - 1.
translate_move(left, (X, Y), (X1, Y)) :- X1 is X - 1.
translate_move(right, (X, Y), (X1, Y)) :- X1 is X + 1.
translate_move(up_left, (X, Y), (X1, Y1)) :- X1 is X - 1, Y1 is Y + 1.
translate_move(up_right, (X, Y), (X1, Y1)) :- X1 is X + 1, Y1 is Y + 1.
translate_move(down_left, (X, Y), (X1, Y1)) :- X1 is X - 1, Y1 is Y - 1.
translate_move(down_right, (X, Y), (X1, Y1)) :- X1 is X + 1, Y1 is Y - 1.


% Validates the position to where the piece wants to move
validate_final_position(Board, CurrentPosition, FinalPosition, PlayersPosition) :-
    % Ensure the piece will not move out of the board
    is_within_bounds(Board, FinalPosition),

    % Ensure the height difference is acceptable
    is_valid_height(Board, CurrentPosition, FinalPosition),

    % Ensure the position is not occupied
    is_occupied(FinalPosition, PlayersPositions).

% Ensure the piece will not move out of the board
is_within_bounds(Board, (X, Y)) :-
    length(Boad, N),
    X > 0, X =< N,
    Y > 0, Y =< N.

% Ensure the height difference is acceptable
is_valid_height(Board, (X1, Y1), (X2, Y2)) :-
    get_height(Board, (X1, Y1), H1),
    get_height(Board, (X2, Y2), H2),

    Diff is abs(H1 - H2),
    Diff =< 1.

% Gets the height of a stack at a given position
get_height(Board, (X, Y), Height) :-
    nth1(X, Board, Row), % Gets X Row
    nth1(Y, Row, Height). % Gets Y Element (Height in that Position)

% Ensure the position is not occupied
is_occupied((X, Y), [_-(X, Y) | _]).
is_occupied((X, Y), [_ | _-(X, Y)]).


% Update piece coordinates with the coordinates after the valid move
update_piece_coordinates(Player, NewPosition, [Player-_| _], [Player-NewPosition| _]).
update_piece_coordinates(Player, NewPosition, [_ | Player-_], [_ | Player-NewPosition]).
%------------------------------------------------------------------------------------------

% --- Auxiliary Stone Movement Predicates ----------------------------------------------------------------------

find_smallest_stack(Board, CurrentPosition, SmallestStackPosition) :-


validate_stone_placement(Board, CurrentPosition, FinalPosition, (X, Y)) :-


move_stone(Board, SmallestStackPosition, (X, Y), NewBoard):-
%------------------------------------------------------------------------------------------