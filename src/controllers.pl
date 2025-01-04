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
    nl, nl, 
    display_game(game_state(Board, PlayerName, PlayersInfo, PlayersPositions)),

    write('Current player: '), write(PlayerName), nl,

    % Show the player moves he can do
    valid_moves(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), ValidMoves),
    write('These are your valid moves => '), 
    write(ValidMoves), nl, nl,

    write('Choose your move: '), read(Move), nl,


    % Validate the move and execute it if valid, otherwise, asks for a new valid move
    move(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), Move, NewGameState),
    
    % Start the Loop All Over Again
    game_loop(NewGameState).


% Initialize the GameState based on the GameConfig given
initial_state(
        game_config(BoardSize, Player1Type, Player2Type, PawnNumber, Player1Name, Player2Name, _), 
        game_state(Board, (Player1-Player1Name), PlayerInfo, PlayerPositions)) :-

    % Build the Board
    build_board(BoardSize, BoardSize, Board),

    % Set Player Info
    PlayerInfo = [Player1Type-Player1Name, Player2Type-Player2Name],

    % Set starting positions based on PawnNumber
    initialize_pawn_positions(BoardSize, PawnNumber, Player1Name, Player2Name, PlayerPositions).

% ---Initial State Helpers---------------------------------------------------------

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

% 1 pawn per player
initialize_pawn_positions(BoardSize, 1, Player1Name, Player2Name, PlayerPositions) :-
    PlayerPositions = [Player1Name-(1,1), Player2Name-(BoardSize, BoardSize)].

% 2 pawns
initialize_pawn_positions(BoardSize, 2, Player1Name, Player2Name, PlayerPositions) :-
    PlayerPositions = [Player1Name-(1,1), Player1Name-(BoardSize,BoardSize), Player2Name-(1, BoardSize), Player2Name-(BoardSize, 1)].
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

% Display a tile with a player
display_tiles([Stone | Rest], RowNum, ColNum, PlayerPositions) :-
    player_at_position(ColNum, RowNum, PlayerPositions, Player), !,
    write('['), write(Player), write('] '),
    NextColNum is ColNum + 1,
    display_tiles(Rest, RowNum, NextColNum, PlayerPositions).

% Display a regular tile
display_tiles([Stone | Rest], RowNum, ColNum, PlayerPositions) :-
    write('[  '), write(Stone), write('  ] '),
    NextColNum is ColNum + 1,
    display_tiles(Rest, RowNum, NextColNum, PlayerPositions).

display_tiles([], _, _, _). % Base Case

% Checks all coordinates to see if there is a player there
player_at_position(ColNum, RowNum, [Player-(ColNum, RowNum) | _], Player).
player_at_position(ColNum, RowNum, [_ | Rest], Player) :-
    player_at_position(ColNum, RowNum, Rest, Player).
%---------------------------------------------------------------------------


% --- Player Actions -------------------------------------------------------

% Changes the game state accordingly to a player new move
move(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions),
     Move,
     game_state(NewBoard, NextPlayer, PlayersInfo, NewPlayersPositions)) :-

    % Validate the Move
    valid_moves(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), ValidMoves),
    member(Move, ValidMoves), % If the move is in valid moves it's valid

    % Execute Move
    execute_move(Board, (CurrentPlayer-PlayerName), PlayersPositions, Move, NewBoard, NewPlayersPositions),

    % Switch Players
    switch_players((CurrentPlayer-PlayerName), PlayersInfo, NextPlayer).


% Executes a valid move
execute_move(Board, (CurrentPlayer-PlayerName), PlayersPositions, Move, NewBoard, NewPlayersPositions) :-
    % Get the coordinates for the players piece
    get_piece_position(PlayerName, PlayersPositions, CurrentPosition),

    % Translate the move into new coordinates
    translate_move(Move, CurrentPosition, FinalPosition),

    % Move the piece
    update_piece_coordinates((CurrentPlayer-PlayerName), FinalPosition, PlayersPositions, NewPlayersPositions),

    % Changes a stone of location based on player choice
    pick_and_place_stone(Board, CurrentPosition, FinalPosition, PlayersPositions, NewBoard).


% Will choose a stone and change its location based on user input
pick_and_place_stone(Board, CurrentPosition, FinalPosition, PlayersPositions, NewBoard) :-
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
    validate_stone_placement(Board, CurrentPosition, FinalPosition, (X, Y), PlayerPositions),

    % Update the stacks on the board
    move_stone(Board, SmallestStackPosition, (X, Y), NewBoard).


%------------------------------------------------------------------------------------------

% --- Auxiliary Move Predicates ---------------------------------------------------------------------

% Creates a list with all the valid moves
valid_moves(game_state(Board, (CurrentPlayer-PlayerName), _, PlayersPositions), ValidMoves) :-
    get_piece_position(PlayerName, PlayersPositions, CurrentPosition),
    findall(
        Move,
        (
            translate_move(Move, CurrentPosition, FinalPosition),
            validate_final_position(Board, CurrentPosition, FinalPosition, PlayersPositions)
        ),
        RawMoves
    ),
    sort(RawMoves, ValidMoves).

% Base case: the player and piece position match
get_piece_position(PlayerName, [PlayerName-(X, Y) | _], (X, Y)) :- !.

% Recursive case: keep searching in the rest of the list
get_piece_position(PlayerName, [_ | Rest], Position) :-
    get_piece_position(PlayerName, Rest, Position).


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

% Base case: the position matches the current player's pawn
is_occupied((X, Y), [PlayerName-(X, Y) | _]) :- !.

% Recursive case: keep checking the rest of the list
is_occupied((X, Y), [_ | Rest]) :-
    is_occupied((X, Y), Rest).


% Base case: found the player whose position needs updating
update_piece_coordinates((Player-PlayerName), NewPosition, [PlayerName-OldPosition | Rest], [PlayerName-NewPosition | Rest]) :- !.

% Recursive case: keep checking the rest of the list
update_piece_coordinates((Player-PlayerName), NewPosition, [OtherPlayer-OtherPosition | Rest], [OtherPlayer-OtherPosition | UpdatedRest]) :-
    update_piece_coordinates((Player-PlayerName), NewPosition, Rest, UpdatedRest).
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
    is_occupied((X, Y), PlayersPositions).

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
switch_players((CurrentPlayer-CurrentName), [Player1Type-CurrentName, Player2Type-Player2Name], (NextPlayer-Player2Name)).
switch_players((CurrentPlayer-CurrentName), [Player1Type-Player1Name, Player2Type-CurrentName], (NextPlayer-Player1Name)).
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