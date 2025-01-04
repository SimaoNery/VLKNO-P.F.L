:- use_module(library(lists)).


% Runs the game between the players while it isn't Game Over
game_loop(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions)) :-
    % Check if the game is over
    game_over(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), Winner),

    % Show End Game Results
    write_game_results(Winner).

% If the game isn't over proceed with the normal loop
game_loop(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions)) :- 
    % Display the current game state

    write('It\'s '), write(PlayerName), write('\'s turn!'),
    nl, nl, 
    display_game(game_state(Board, PlayerName, PlayersInfo, PlayersPositions)),

    choose_move(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), CurrentPlayer, Move), !,

    write('Moving pawn...'), nl, nl,

    % Validate the move and execute it if valid, otherwise, asks for a new valid move
    move(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), Move, NewGameState),
    
    % Start the Loop All Over Again
    game_loop(NewGameState).


% Initialize the GameState based on the GameConfig given
initial_state(
        game_config(BoardSize, Player1Type, Player2Type, Player1Name, Player2Name, _), 
        game_state(Board, (Player1Type-Player1Name), PlayerInfo, [Player1Name-(1,1), Player2Name-(BoardSize, BoardSize)])) :-

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

    write('Executing move...'), nl, nl,

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

    write('Picking and placing stone...'), nl, nl,

    % Changes a stone of location based on player choice
    pick_and_place_stone(Board, CurrentPlayer, CurrentPosition, FinalPosition, PlayersPositions, NewBoard).


% Will choose a stone and change its location based on user input
pick_and_place_stone(Board, 0, CurrentPosition, FinalPosition, PlayersPositions, NewBoard) :-
    % Determine the smallest unoccupied stack (excluding the previous position)
    find_smallest_stack(Board, CurrentPosition, FinalPosition, SmallestStackPositions, PlayersPositions),

    % Select smallest stack in SmallestStackPositions
    select_smallest_stack_position(SmallestStackPositions, SmallestStackPosition),

    % Ask the player where to place it
    write('Choose the coordinates to place the stone!'), nl,
    write('     =>X: '), nl,
    read(X),
    write('     =>Y: '), nl,
    read(Y),

    % Ensure the coordinates are valid
    validate_stone_placement(Board, CurrentPosition, FinalPosition, (X, Y), PlayersPositions),

    % Update the stacks on the board
    move_stone(Board, SmallestStackPosition, (X, Y), NewBoard).

extract_second_elements([], []).
extract_second_elements([_-Position | Rest], [Position | PositionsRest]) :-
    extract_second_elements(Rest, PositionsRest).

get_non_empty_positions(Board, Positions) :-
    findall((X, Y),
            (
                nth1(Y, Board, Row),
                nth1(X, Row, Height),
                Height > 0
            ),
            Positions).

in_exclude(ExcludePositions, X) :-
    member(X, ExcludePositions).

% Use exclude/3 to filter positions
filter_positions(Positions, ExcludePositions, FilteredPositions) :-
    exclude(in_exclude(ExcludePositions), Positions, FilteredPositions).
    % Combine the predicates to get the desired positions

get_valid_positions(Board, SmallestStackPosition, CurrentPosition, FinalPosition, PlayersPositions, ValidPositions) :-
    get_non_empty_positions(Board, AllPositions),
    extract_second_elements(PlayersPositions, PlayersPositionsList),
    append([SmallestStackPosition, CurrentPosition, FinalPosition], PlayersPositionsList, ExcludePositions),
    filter_positions(AllPositions, ExcludePositions, ValidPositions).

pick_and_place_stone(Board, 1, CurrentPosition, FinalPosition, PlayersPositions, NewBoard) :-

    write('AI thinking what stone to remove...'), nl,
    % Determine the smallest unoccupied stack (excluding the previous position)
    find_smallest_stack(Board, CurrentPosition, FinalPosition, SmallestStackPositions, PlayersPositions),

    % Randomly select a position to remove the stone
    random_member(SmallestStackPosition, SmallestStackPositions),

    write('AI chose: '), write(SmallestStackPosition), nl,
    write('AI thinking where to place the stone...'), nl,

    get_valid_positions(Board, SmallestStackPosition, CurrentPosition, FinalPosition, PlayersPositions, ValidPositions),

    % Randomly select a position to place the stone
    random_member(PlacePosition, ValidPositions),

    write('AI chose: '), write(PlacePosition), nl,

    % Update the stacks on the board
    move_stone(Board, SmallestStackPosition, PlacePosition, NewBoard).
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
get_piece_position((Player-PlayerName), [PlayerName-(X, Y), Player2Pos], (X, Y)).
get_piece_position((Player-PlayerName), [Player1Pos, PlayerName-(X, Y)], (X, Y)).

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
validate_final_position(Board, CurrentPosition, FinalPosition, PlayersPositions) :-
    % Ensure the piece will not move out of the board
    is_within_bounds(Board, FinalPosition),

    % Ensure the height difference is acceptable
    is_valid_height(Board, CurrentPosition, FinalPosition),

    % Ensure the position is not occupied
    \+ is_occupied(FinalPosition, PlayersPositions).

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
is_occupied((X, Y), [Player1Pos, Player2Name-(X, Y)]).
is_occupied((X, Y), [Player1Name-(X, Y) | Player2Pos]).

update_piece_coordinates((Player-PlayerName), NewPosition, [PlayerName-OldPosition | Player2Pos], [PlayerName-NewPosition| Player2Pos]).
update_piece_coordinates((Player-PlayerName), NewPosition, [Player1Pos | PlayerName-OldPosition], [Player1Pos | PlayerName-NewPosition]).
%------------------------------------------------------------------------------------------

% --- Auxiliary Stone Movement Predicates ----------------------------------------------------------------------
% Finds the smallest stack in the map
find_smallest_stack(Board, CurrentPosition, FinalPosition, SmallestStackPositions, PlayersPositions) :-
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
            (X, Y) \= FinalPosition,
            get_height(Board, (X, Y), Height),
            \+ is_occupied((X, Y), PlayersPositions),
            Height > 0
        ),
        Stacks
    ),
    keysort(Stacks, SortedStacks), % Sorts the stacks to get the one with the smallest amount of stones
    SortedStacks = [SmallestStackPosition-_|_],

    findall(
        (X, Y),
        member(SmallestHeight-(X, Y), SortedStacks),
        SmallestStackPositions
    ).

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
    \+ is_occupied((X, Y), PlayersPositions).

move_stone(Board, (X, Y), (X, Y), Board). % In the case the coords are the same, leave it unchanged
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


% Select a position when there is only one option
select_smallest_stack_position([SinglePosition], SmallestStackPositions) :-
    write('Smallest Stack Position: '), write(SmallestStackPosition), nl.

% Allow the user to choose when there are multiple options
select_smallest_stack_position(SmallestStackPositions, SmallestStackPosition) :-
    write('Choose the coordinates from where to pick the stone!'), nl,

    write_positions(SmallestStackPositions, 1),

    write('Enter the index of the coordinates you want: '), nl,
    read(Coords),
    nth1(Coords, SmallestStackPositions, SmallestStackPosition).

% Writes all the positions
write_positions([], _).
write_positions([(X, Y)|Rest], Index) :-
    write(Index), write(': ('), write(X), write(', '), write(Y), write(')'), nl,
    NextIndex is Index + 1,
    write_positions(Rest, NextIndex).
%------------------------------------------------------------------------------------------

% Switch Players
switch_players((Player1-Player1Name), [Player1Name-(X1, Y1), Player2Name-(X2, Y2)], 
               [Player1Type-Player1Name, Player2Type-Player2Name], (Player2Type-Player2Name), 
               [Player2Name-(X2, Y2), Player1Name-(X1, Y1)], [Player2Type-Player2Name, Player1Type-Player1Name]).

switch_players(_, _, _, _, _) :- write('Fallback clause called'), fail.
%------------------------------------------------------------------------------------------

% --- Bot Moves ---------------------------------------------------------------------------

choose_move(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), 0, Move) :-
    valid_moves(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), ValidMoves),
    write('These are your valid moves => '), 
    write(ValidMoves), nl, nl,
    write('Choose your move: '), read(Move), nl, nl.

choose_move(GameState, 1, Move) :-
    write('AI thinking...'), nl,
    valid_moves(GameState, ValidMoves),
    length(ValidMoves, Length),
    random_member(Move, ValidMoves),
    write('AI chose: '), write(Move), nl, !.



choose_move(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), 2, BestMove) :-
    write('AI thinking...'), nl,
    valid_moves(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), ValidMoves),
    
    % Evaluate each valid move
    findall(Value-Move,
            (
                member(Move, ValidMoves),
                
                % Get the coordinates for the player's piece
                get_piece_position((CurrentPlayer-PlayerName), PlayersPositions, CurrentPosition),
                
                % Translate the move into new coordinates
                translate_move(Move, CurrentPosition, FinalPosition),
                
                % Move the piece
                update_piece_coordinates((CurrentPlayer-PlayerName), FinalPosition, PlayersPositions, NewPlayersPositions),
                
                % Evaluate the new game state
                value(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, NewPlayersPositions), current, PositiveValue),
                value(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, NewPlayersPositions), other, NegativeValue),
                Value is PositiveValue - NegativeValue
            ),
            MovesWithValues),
    
    % Sort moves by their values
    keysort(MovesWithValues, SortedMovesWithValues),
    
    % Select the move with the highest value
    last(SortedMovesWithValues, _-BestMove),
    
    write('AI chose: '), write(BestMove), nl, !.



value(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), current, Value) :-
    valid_moves(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), ValidMoves),
    length(ValidMoves, Value).

value(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), other, Value) :-
    switch_players((CurrentPlayer-PlayerName), PlayersPositions, PlayersInfo, NextPlayer, UpdatedPlayersPositions, UpdatedPlayersInfo),
    valid_moves(game_state(Board, NextPlayer, UpdatedPlayersInfo, UpdatedPlayersPositions), ValidMoves),
    length(ValidMoves, Value).

%------------------------------------------------------------------------------------------



% --- Game Over Functions ---------------------------------------------------------------------------

% Checks if the game is over and identifies the winner or a draw
game_over(game_state(Board, CurrentPlayer, PlayersInfo, [Player1Name-(X1, Y1), Player2Name-(X2, Y2)]), Player2Name) :-
    % Check if Player 1 cannot move but Player 2 can
    \+ player_can_move(game_state(Board, (Player1-Player1Name), PlayersInfo, [Player1Name-(X1, Y1), Player2Name-(X2, Y2)])),
    player_can_move(game_state(Board, (Player2-Player2Name), PlayersInfo, [Player1Name-(X1, Y1), Player2Name-(X2, Y2)])).

game_over(game_state(Board, CurrentPlayer, PlayersInfo, [Player1Name-(X1, Y1), Player2Name-(X2, Y2)]), Player1Name) :-
    % Check if Player 2 cannot move but Player 1 can
    player_can_move(game_state(Board, (Player1-Player1Name), PlayersInfo, [Player1Name-(X1, Y1), Player2Name-(X2, Y2)])), nl,
    \+ player_can_move(game_state(Board, (Player2-Player2Name), PlayersInfo, [Player1Name-(X1, Y1), Player2Name-(X2, Y2)])).

game_over(Board, CurrentPlayer, PlayersInfo, [Player1Name-(X1, Y1), Player2Name-(X2, Y2)], draw) :-
    % Check if neither player can move
    \+ player_can_move(game_state(Board, (Player1-Player1Name), PlayersInfo, [Player1Name-(X1, Y1), Player2Name-(X2, Y2)])),
    \+ player_can_move(game_state(Board, (Player2-Player2Name), PlayersInfo, [Player1Name-(X1, Y1), Player2Name-(X2, Y2)])).

% Determines if a player can move their piece
player_can_move(game_state(Board, (CurrentPlayer-PlayerName), _, PlayersPositions)) :-
    valid_moves(game_state(Board, (CurrentPlayer-PlayerName), _, PlayersPositions), ValidMoves),
    ValidMoves \= [].


write_game_results(Winner) :-
    Winner \= draw,
    nl, write(Winner), write(' just WON the game! CONGRATULATIONS!!'), nl, nl.

write_game_results(draw) :-
    nl, write('The game is a draw!'), nl.
%------------------------------------------------------------------------------------------
