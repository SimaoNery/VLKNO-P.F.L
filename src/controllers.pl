:- use_module(library(lists)).

% Runs the game between the players while it isn't Game Over
game_loop(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions)) :- 
    % Display the current game state
    display_game(game_state(Board, PlayerName, PlayersInfo, PlayersPositions)),

    % TO DO: Checks if the game is over

    write('Current player: '), write(PlayerName), nl,

    % Show the player moves he can do
    valid_moves(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), ValidMoves),
    write('These are your valid moves => '), 
    write(ValidMoves), nl, nl,

    write('Choose your move: '), read(Move), nl, nl,


    % Validate the move and execute it if valid, otherwise, asks for a new valid move
    move(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), Move, NewGameState),

    % TO DO: Give a value to the current state

    % TO DO: See How To Handle AI
    
    % Start the Loop All Over Again
    game_loop(NewGameState).


% Initialize the GameState based on the GameConfig given
initial_state(
        game_config(BoardSize, Player1Type, Player2Type, Player1Name, Player2Name, _), 
        game_state(Board, (Player1-Player1Name), PlayerInfo, [Player1Name-(1,1), Player2Name-(BoardSize, BoardSize)])) :-

    % Build the Board
    build_board(BoardSize, BoardSize, Board),

    % Set Player Info
    PlayerInfo = [Player1Type-Player1Name, Player2Type-Player2Name].

% ---Board Building---------------------------------------------------------

% Builds the initial board with the desired size (all stacks size 1)
build_board(0, _, []). % Base Case
build_board(RowSize, ColSize, [Row | Board]) :-
    build_row(ColSize, Row), % Displays a row at a time
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
display_game(game_state(Board, CurrentPlayerName, PlayerInfo, PlayerPositions)) :-
    display_board(Board, PlayerPositions), nl, nl,nl. % Display Board

% Displays the board
display_board(Board, PlayerPositions) :-
    length(Board, BoardLength),
    display_rows(Board, BoardLength, PlayerPositions), % Starts from the last row
    write('   _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _'), nl,
    write('    '),
    display_columns(1, BoardLength).

% Displays the number of the columns above the board
display_columns(Current, End) :-
    Current =< End,
    write('   '), write(Current), write('    '),
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
display_tiles([_ | Rest], RowNum, ColNum, [Player1-(ColNum, RowNum), Player2-(X, Y)]) :-   
    write('['), write(Player1), write('] '),
    NextColNum is ColNum + 1,
    display_tiles(Rest, RowNum, NextColNum, [Player1-(ColNum, RowNum), Player2-(X, Y)]).

% Display "P2" if this is Player2 tile
display_tiles([_ | Rest], RowNum, ColNum, [Player1-(X, Y), Player2-(ColNum, RowNum)]) :-
    write('['), write(Player2), write('] '),
    NextColNum is ColNum + 1,
    display_tiles(Rest, RowNum, NextColNum,[Player1-(X, Y), Player2-(ColNum, RowNum)]).

% Display regular stack of tiles 
display_tiles([Stone | Rest], RowNum, ColNum, [Player1-(X1, Y1), Player2-(X2, Y2)]) :-
    write('[  '), write(Stone), write('  ] '),
    NextColNum is ColNum + 1,
    display_tiles(Rest, RowNum, NextColNum,[Player1-(X1, Y1), Player2-(X2, Y2)]).

display_tiles([], _, _, _). % Base Case
%---------------------------------------------------------------------------


% --- Player Actions -------------------------------------------------------

% Changes the game state accordingly to a player new move
move(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions),
     Move,
     game_state(NewBoard, NextPlayer, NewPlayersInfo, UpdatedPlayersPositions)) :-

    % Validate the Move
    valid_moves(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), ValidMoves),
    member(Move, ValidMoves), % If the move is in valid moves it's valid

    % Execute Move
    execute_move(Board, (CurrentPlayer-PlayerName), PlayersPositions, Move, NewBoard, NewPlayersPositions),

    % Switch Players
    switch_players((CurrentPlayer-PlayerName), NewPlayersPositions, PlayersInfo, NextPlayer, UpdatedPlayersPositions, NewPlayersInfo).


% Executes a valid move
execute_move(Board, (CurrentPlayer-PlayerName), PlayersPositions, Move, NewBoard, NewPlayersPositions) :-
    % Get the coordinates for the players piece
    get_piece_position((CurrentPlayer-PlayerName), PlayersPositions, CurrentPosition),

    % Translate the move into new coordinates
    translate_move(Move, CurrentPosition, FinalPosition),

    % Move the piece
    update_piece_coordinates((CurrentPlayer-PlayerName), FinalPosition, PlayersPositions, NewPlayersPositions),

    % Changes a stone of location based on player choice
    pick_and_place_stone(Board, CurrentPosition, FinalPosition, PlayersPositions, NewBoard).


% Will choose a stone and change its location based on user input
pick_and_place_stone(Board, CurrentPosition, FinalPosition, PlayersPositions, NewBoard) :-
    % Determine the smallest unoccupied stack (excluding the previous position)
    find_smallest_stack(Board, CurrentPosition, SmallestStackPosition),
    write('Smallest Stack: '), write(SmallestStackPosition), nl, nl,

    % Ask the player where to place it
    write('Choose the coordinates to place the stone!'), nl,
    write('     =>X: '), nl,
    read(X),
    write('     =>Y: '), nl,
    read(Y),

    % Ensure the coordinates are valid
    validate_stone_placement(Board, CurrentPosition, FinalPosition, (X, Y), PlayerPositions),

    % Update the stacks on the board
    move_stone(Board, SmallestStackPosition, (X, Y), NewBoard).


%------------------------------------------------------------------------------------------

% --- Auxiliary Move Predicates ---------------------------------------------------------------------

% Creates a list with all the valid moves
valid_moves(game_state(Board, (CurrentPlayer-PlayerName), _, PlayersPositions), ValidMoves) :-
    get_piece_position((CurrentPlayer-PlayerName), PlayersPositions, CurrentPosition),
    findall(
        Move,
        (
            translate_move(Move, CurrentPosition, FinalPosition),
            validate_final_position(Board, CurrentPosition, FinalPosition, PlayersPositions)
        ),
        RawMoves
    ),
    sort(RawMoves, ValidMoves).

% Gets the coordinates of the current players piece
get_piece_position((Player-PlayerName), [PlayerName-(X, Y) | _], (X, Y)).
get_piece_position((Player-PlayerName), [_ | PlayerName-(X, Y)], (X, Y)).

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
    length(Board, N),
    X > 0, X =< N,
    Y > 0, Y =< N.

% Ensure the height difference is acceptable
is_valid_height(Board, (X1, Y1), (X2, Y2)) :-
    get_height(Board, (X1, Y1), H1),
    get_height(Board, (X2, Y2), H2),

    H2 > 0,
    Diff is abs(H1 - H2),
    Diff =< 1.

% Gets the height of a stack at a given position
get_height(Board, (X, Y), Height) :-
    length(Board, BoardLength),       
    InvertedY is BoardLength - Y + 1,
    nth1(InvertedY, Board, Row),
    nth1(X, Row, Height). 

% Check if a position is occupied by any player
is_occupied((X, Y), [PlayerName-(X, Y) | _]) :- !.
is_occupied((X, Y), [_ | PlayerName-(X, Y)]) :- !.


update_piece_coordinates((Player-PlayerName), NewPosition, [PlayerName-OldPosition | Player2Pos], [PlayerName-NewPosition| Player2Pos]).
update_piece_coordinates((Player-PlayerName), NewPosition, [Player1Pos | PlayerName-OldPosition], [Player1Pos | PlayerName-NewPosition]).
%------------------------------------------------------------------------------------------

% --- Auxiliary Stone Movement Predicates ----------------------------------------------------------------------
% Finds the smallest stack in the map
find_smallest_stack(Board, CurrentPosition, SmallestStackPosition) :-
    length(Board, BoardLength),

    % Generate all valid positions on the board
    findall(
        (X, Y),
        (between(1, BoardLength, X), between(1, BoardLength, Y)),
        AllPositions
    ),

    % Gets all places heights and guarantees its different from where the player came from
    findall(
        Height-(X, Y),
        (
            member((X, Y), AllPositions),
            (X, Y) \= CurrentPosition,
            get_height(Board, (X, Y), Height),
            Height > 0
        ),
        Stacks
    ),
    keysort(Stacks, SortedStacks), % Sorts the stacks to get the one with the smallest amount of stones
    SortedStacks = [(_-SmallestStackPosition)|_].

% Will generate all positions between the numbers (Shoudl be include by default, but add problems with that)
between(Lower, Upper, Lower) :- 
    Lower =< Upper.
between(Lower, Upper, X) :-
    Lower < Upper,
    Next is Lower + 1,
    between(Next, Upper, X).

validate_stone_placement(Board, CurrentPosition, FinalPosition, (X, Y), PlayersPositions) :-
    is_within_bounds(Board, (X, Y)),
    (X, Y) \= CurrentPosition,
    (X, Y) \= FinalPosition,
    is_occupied((X, Y), PlayersPositions).

move_stone(Board, (X1, Y1), (X2, Y2), NewBoard):-
    get_height(Board, (X1, Y1), H1),
    get_height(Board, (X2, Y2), H2),

    % Decrement the height at the source position
    UpdatedH1 is H1 - 1,
    update_board(Board, (X1, Y1), UpdatedH1, TempBoard),

    % Increment the height at the destination position
    UpdatedH2 is H2 + 1,
    update_board(TempBoard, (X2, Y2), UpdatedH2, NewBoard).

% Update the board at a specific position
update_board(Board, (X, Y), NewValue, UpdatedBoard) :-
    length(Board, BoardLength),            % Get the number of rows
    InvertedY is BoardLength - Y + 1,      % Invert the Y-coordinate
    nth1(InvertedY, Board, Row),           % Access row
    nth1(X, Row, _, RestOfRow),            % Access and replace the column in the row
    nth1(X, NewRow, NewValue, RestOfRow),  % Create the updated row
    nth1(InvertedY, Board, _, RestOfBoard),% Replace the old row in the board
    nth1(InvertedY, UpdatedBoard, NewRow, RestOfBoard). % Construct the updated board

%------------------------------------------------------------------------------------------

% Switch Players
switch_players((Player1-Player1Name), [Player1Name-(X1, Y1), Player2Name-(X2, Y2)], 
               [Player1Type-Player1Name, Player2Type-Player2Name], (Player2Type-Player2Name), 
               [Player2Name-(X2, Y2), Player1Name-(X1, Y1)], [Player2Type-Player2Name, Player1Type-Player1Name]).

switch_players(_, _, _, _, _) :- write('Fallback clause called'), fail.

