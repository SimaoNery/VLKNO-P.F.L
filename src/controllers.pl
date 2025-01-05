:- use_module(library(lists)).


% Runs the game between the players while it isn't Game Over
game_loop(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions)) :-
    % Check if the game is over
    game_over(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), Winner),

    % Show End Game Results
    write_game_results(Winner).

% If the game isn't over proceed with the normal loop
game_loop(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions)) :- 
    nl, nl, 
    % Display the current game state
    display_game(game_state(Board, PlayerName, PlayersInfo, PlayersPositions)), nl, nl,
    write('It\'s '), write(PlayerName), write('\'s turn!'), nl, nl,

    % Choose the move based on the player type
    choose_move(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), CurrentPlayer,  SelectedPawn, Move),

    % Validate the move and execute it if valid, otherwise, asks for a new valid move
    move(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), SelectedPawn, Move, NewGameState),
    
    
    % Start the Loop All Over Again
    game_loop(NewGameState).


% Initialize the GameState based on the GameConfig given
initial_state(
        game_config(BoardSize, Player1Type, Player2Type, PawnNumber, Player1Name, Player2Name), 
        game_state(Board, (Player1Type-Player1Name), PlayerInfo, PlayerPositions)) :-

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
display_game(game_state(Board, _, _, PlayerPositions)) :-
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
display_tiles([_ | Rest], RowNum, ColNum, PlayerPositions) :-
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


% Gives list of pawns for a player
pawn_list(Gamestate, PlayerName, PlayersPositions, Pawns) :-
    findall(
        (X, Y), 
        (
            member(PlayerName-(X, Y), PlayersPositions), 
            valid_moves(Gamestate, (X, Y), ValidMoves),
            ValidMoves \= []
        ),
        Pawns
    ).

% Selects a pawn to move
select_pawn(Gamestate, PlayerName, PlayersPositions, SelectedPawn) :-
    pawn_list(Gamestate, PlayerName, PlayersPositions, Pawns),
    select_pawn_from_list(Pawns, SelectedPawn).

% If there is only 1 pawn per player
select_pawn_from_list([Pawn], Pawn).

% For more than 1 pawn per player
select_pawn_from_list(Pawns, SelectedPawn) :-
    write('Enter the index of the pawn you want to move: '), nl,
    write_positions(Pawns, 1),
    read(Index), 
    nth1(Index, Pawns, SelectedPawn).


% Changes the game state accordingly to a player new move
move(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions),
    SelectedPawn,
    Move,
    game_state(NewBoard, NextPlayer, PlayersInfo, NewPlayersPositions)) :-


    % Validate the Move
    valid_moves(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), SelectedPawn, ValidMoves),
    member(Move, ValidMoves), % If the move is in valid moves it's valid

    % Execute Move
    execute_move(Board, (CurrentPlayer-PlayerName), PlayersPositions, SelectedPawn, Move, NewBoard, NewPlayersPositions),

    % Switch Players
    switch_players((CurrentPlayer-PlayerName), PlayersInfo, NextPlayer).


% Executes a valid move
execute_move(Board, (CurrentPlayer-PlayerName), PlayersPositions, SelectedPawn, Move, NewBoard, NewPlayersPositions) :-

    write('executing move'), nl,
    % Translate the move into new coordinates
    translate_move(Move, SelectedPawn, FinalPosition),

    write('translated move'), nl,

    % Move the piece
    update_piece_coordinates((CurrentPlayer-PlayerName), SelectedPawn, FinalPosition, PlayersPositions, NewPlayersPositions),

    write('updated piece coordinates'), nl,

    % Changes a stone of location based on player choice
    pick_and_place_stone(Board, CurrentPlayer, (CurrentPlayer-PlayerName), _, SelectedPawn, FinalPosition, PlayersPositions, NewBoard).


% Will choose a stone and change its location based on user input
extract_second_elements([], []).
extract_second_elements([_-Position | Rest], [Position | PositionsRest]) :-
    extract_second_elements(Rest, PositionsRest).

get_non_empty_positions(Board, Positions) :-
    length(Board, BoardLength),
    findall((X, InvertedY),
            (
                nth1(Y, Board, Row),
                nth1(X, Row, Height),
                Height > 0,
                InvertedY is BoardLength - Y + 1
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

get_with_value(SortedValues, HighestValue, BestValues) :-
    findall(Value-Move-Pawn, 
            (member(Value-Move-Pawn, SortedValues), Value =:= HighestValue), 
            BestValues).

get_with_value_2(SortedValues, HighestValue, BestValues) :-
    findall(Value-Move, 
            (member(Value-Move, SortedValues), Value =:= HighestValue), 
            BestValues).


pick_and_place_stone(Board, 0, _, _, CurrentPosition, FinalPosition, PlayersPositions, NewBoard) :-
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


pick_and_place_stone(Board, 1, _, _, CurrentPosition, FinalPosition, PlayersPositions, NewBoard) :-

    % Determine the smallest unoccupied stack (excluding the previous position)
    find_smallest_stack(Board, CurrentPosition, FinalPosition, SmallestStackPositions, PlayersPositions),

    % Randomly select a position to remove the stone
    random_member(SmallestStackPosition, SmallestStackPositions),

    get_valid_positions(Board, SmallestStackPosition, CurrentPosition, FinalPosition, PlayersPositions, ValidPositions),

    % Randomly select a position to place the stone
    random_member(PlacePosition, ValidPositions),


    write('AI moved stone from '), write(SmallestStackPosition), write(' to '), write(PlacePosition), nl,

    % Update the stacks on the board
    move_stone(Board, SmallestStackPosition, PlacePosition, NewBoard).



pick_and_place_stone(Board, 2, (CurrentPlayer-PlayerName), PlayersInfo, CurrentPosition, FinalPosition, PlayersPositions, NewBoard) :-
    % Determine the smallest unoccupied stack (excluding the previous position)

    write('finding smallest stack'), nl,
    find_smallest_stack(Board, CurrentPosition, FinalPosition, SmallestStackPositions, PlayersPositions),

    write('found smallest stack'), nl,
    % Randomly select a position to remove the stone
    findall(RemoveValue-Remove,
            (
                member(Remove, SmallestStackPositions),
                
                % Get the stack size of the coordinate
                get_height(Board, Remove, RemoveHeight),


                % Decrement the height at the given position
                NewRemoveHeight is RemoveHeight - 1,
                update_board(Board, Remove, NewRemoveHeight, TempRemoveBoard),
                
                % Evaluate the new game state for the given move
                value(game_state(TempRemoveBoard, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), current, RemovePositiveValue),
                value(game_state(TempRemoveBoard, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), other, RemoveNegativeValue),
                RemoveValue is RemovePositiveValue - RemoveNegativeValue
            ),
            RemoveWithValues),
    
    write('findall remove done '), nl,
    % Sort moves by their values
    keysort(RemoveWithValues, SortedRemoveWithValues),

    write('sorted remove done'), nl,
    
    % Find the highest value
    last(SortedRemoveWithValues, HighestRemoveValue-_),

    write('found highest remove value'), nl,

    get_with_value_2(SortedRemoveWithValues, HighestRemoveValue, BestRemoves),



    % Filter moves to keep only those with the highest value

    
    % Choose randomly among the best moves
    random_member(_-BestRemove, BestRemoves),

    get_height(Board, BestRemove, HeightRemove),

    NewHeightRemove is HeightRemove - 1,

    update_board(Board, BestRemove, NewHeightRemove, TempRemoveBoard),

    get_valid_positions(Board, BestRemove, CurrentPosition, FinalPosition, PlayersPositions, ValidPositions),


    findall(AddValue-Add,
            (
                member(Add, ValidPositions),
                
                % Get the stack size of the coordinate
                get_height(TempRemoveBoard, Add, HeightAdd),

                % Decrement the height at the given position
                NewHeightAdd is HeightAdd + 1,
                update_board(TempRemoveBoard, Add, NewHeightAdd, TempAddBoard),
                
                % Evaluate the new game state for the given move
                value(game_state(TempAddBoard, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), current, AddPositiveValue),
                value(game_state(TempAddBoard, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), other, AddNegativeValue),
                AddValue is AddPositiveValue - AddNegativeValue
            ),
            AddWithValues),


     % Sort moves by their values
    keysort(AddWithValues, SortedAddWithValues),
    

    % Find the highest value
    last(SortedAddWithValues, HighestAddValue-_),


    get_with_value_2(SortedAddWithValues, HighestAddValue, BestAdds),

    % Filter moves to keep only those with the highest value

    
    % Choose randomly among the best moves
    random_member(_-BestAdd, BestAdds),


    write('AI moved stone from '), write(BestRemove), write(' to '), write(BestAdd), nl,

    % Update the stacks on the board
    move_stone(Board, BestRemove, BestAdd, NewBoard).


%------------------------------------------------------------------------------------------

% --- Auxiliary Move Predicates ---------------------------------------------------------------------

% Creates a list with all the valid moves
valid_moves(game_state(Board, (_-_), _, PlayersPositions), SelectedPawn, ValidMoves) :-
    findall(
        Move,
        (
            translate_move(Move, SelectedPawn, FinalPosition),
            validate_final_position(Board, SelectedPawn, FinalPosition, PlayersPositions)
        ),
        RawMoves
    ),
    sort(RawMoves, ValidMoves).

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
is_occupied((X, Y), [_-(X, Y) | _]) :- !.

% Recursive case: keep checking the rest of the list
is_occupied((X, Y), [_ | Rest]) :-
    is_occupied((X, Y), Rest).


% Base case: found the player whose position needs updating
update_piece_coordinates((_-PlayerName), SelectedPawn, NewPosition, [PlayerName-SelectedPawn | Rest], [PlayerName-NewPosition | Rest]) :- !.

% Recursive case: keep checking the rest of the list
update_piece_coordinates((Player-PlayerName), SelectedPawn, NewPosition, [OtherPlayer-OtherPosition | Rest], [OtherPlayer-OtherPosition | UpdatedRest]) :-
    update_piece_coordinates((Player-PlayerName), SelectedPawn, NewPosition, Rest, UpdatedRest).
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
    SortedStacks = [SmallestHeight-_|_],

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
    \+ is_occupied((X, Y), PlayersPositions),
    get_height(Board, (X, Y), Height),
    Height > 0.

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
select_smallest_stack_position([SinglePosition], SinglePosition) :-
    write('Smallest Stack Position: '), write(SinglePosition), nl.

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
switch_players((CurrentPlayer-CurrentName), [CurrentPlayer-CurrentName, Player2Type-Player2Name], (Player2Type-Player2Name)).
switch_players((CurrentPlayer-CurrentName), [Player1Type-Player1Name, CurrentPlayer-CurrentName], (Player1Type-Player1Name)).
%------------------------------------------------------------------------------------------

% --- Bot Moves ---------------------------------------------------------------------------

choose_move(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), 0, SelectedPawn, Move) :-
    % In 2 pawn game mode, choose the piece
    select_pawn(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), PlayerName, PlayersPositions, SelectedPawn),

    % Show the player moves he can do
    valid_moves(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), SelectedPawn, ValidMoves),
    write('These are your valid moves => '), 
    write(ValidMoves), nl, nl,
    write('Choose your move: '), read(Move), nl, nl.

choose_move(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), 1, SelectedPawn, Move) :-
    write('AI thinking...'), nl,

    % Get the list of pawns for the player
    pawn_list(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), PlayerName, PlayersPositions, Pawns),

    % Choose a random pawn
    random_member(SelectedPawn, Pawns),

    % Get the valid moves for the selected pawn
    valid_moves(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), SelectedPawn, ValidMoves),

    % Choose a random move
    random_member(Move, ValidMoves),


    write('AI chose: '), write(Move), write(' for pawn in ('), write(SelectedPawn), write(')'), nl.



choose_move(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), 2, SelectedPawn, BestMove) :-
    write('AI thinking...'), nl,

    % Get the list of pawns for the player
    pawn_list(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), PlayerName, PlayersPositions, Pawns),

    write('Pawns: '), write(Pawns), nl,

    
    % Evaluate each valid move
    findall(Value-Move-Pawn,
            (
                member(Pawn, Pawns),    
                valid_moves(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), Pawn, ValidMoves),
                member(Move, ValidMoves),


                % Translate the move into new coordinates
                translate_move(Move, Pawn, FinalPosition),

                
                % Move the piece
                update_piece_coordinates((CurrentPlayer-PlayerName), Pawn, FinalPosition, PlayersPositions, NewPlayersPositions),
                
                % Evaluate the new game state
                value(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, NewPlayersPositions), current, PositiveValue),
                value(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, NewPlayersPositions), other, NegativeValue),
                Value is PositiveValue - NegativeValue
            ),
            MovesWithValues),
    
    % Sort moves by their values
    keysort(MovesWithValues, SortedMovesWithValues),
    
    % Find the highest value
    last(SortedMovesWithValues, HighestValue-_-_),


    get_with_value(SortedMovesWithValues, HighestValue, BestMoves),



    % Choose randomly among the best moves
    random_member(_-BestMove-SelectedPawn, BestMoves),
    
    write('AI chose: '), write(BestMove), write(' for pawn in ('), write(SelectedPawn), write(')'), nl.



sum_list([], 0).
sum_list([H|T], Sum) :-
    sum_list(T, Rest),
    Sum is H + Rest.


value(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), current, Value) :-
    pawn_list(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), PlayerName, PlayersPositions, Pawns),
    findall(
        TempValue,
        (
            member(Pawn, Pawns),
            valid_moves(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), Pawn, ValidMoves),
            length(ValidMoves, TempValue)
        ),
        Values
    ),

    sum_list(Values, Value2),

    Value is 2 * Value2.

value(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), other, Value) :-
    switch_players((CurrentPlayer-PlayerName), PlayersInfo, (NextPlayer-NextPlayerName)),
    pawn_list(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), NextPlayerName, PlayersPositions, Pawns),
    findall(
        TempValue,
        (
            member(Pawn, Pawns),
            valid_moves(game_state(Board, (NextPlayer-NextPlayerName), PlayersInfo, PlayersPositions), Pawn, ValidMoves),
            length(ValidMoves, TempValue)
        ),
        Values
    ),
    sum_list(Values, Value).

%------------------------------------------------------------------------------------------



% --- Game Over Functions ---------------------------------------------------------------------------

% Checks if the game is over and identifies the winner or a draw
game_over(game_state(Board, _, [Player1-Player1Name, Player2-Player2Name], PlayersPositions), draw) :-
    % Check if neither player can move
    \+ player_can_move(game_state(Board, (Player1-Player1Name), PlayersInfo, PlayersPositions)),
    \+ player_can_move(game_state(Board, (Player2-Player2Name), PlayersInfo, PlayersPositions)).

game_over(game_state(Board, _, [Player1-Player1Name, Player2-Player2Name], PlayersPositions), Player2Name) :-
    % Check if Player 1 cannot move but Player 2 can
    \+ player_can_move(game_state(Board, (Player1-Player1Name), PlayersInfo, PlayersPositions)),
    player_can_move(game_state(Board, (Player2-Player2Name), PlayersInfo, PlayersPositions)).

game_over(game_state(Board, _, [Player1-Player1Name, Player2-Player2Name], PlayersPositions), Player1Name) :-
    % Check if Player 2 cannot move but Player 1 can
    player_can_move(game_state(Board, (Player1-Player1Name), PlayersInfo, PlayersPositions)),
    \+ player_can_move(game_state(Board, (Player2-Player2Name), PlayersInfo, PlayersPositions)).

% Determines if a player can move their piece
player_can_move(game_state(Board, (Player-PlayerName), _, PlayersPositions)) :-
    findall(
        Moves,
        (
            member(PlayerName-(X, Y), PlayersPositions), % Locate a pawn for the player
            valid_moves(game_state(Board, (Player-PlayerName), _, PlayersPositions), (X, Y), Moves) % Get valid moves for this pawn
        ),
        CombinedMoves
    ),
    append(CombinedMoves, FlatList),
    FlatList \= []. % At least one pawn must have valid moves



write_game_results(Winner) :-
    Winner \= draw,
    nl, write(Winner), write(' just WON the game! CONGRATULATIONS!!'), nl, nl.

write_game_results(draw) :-
    nl, write('The game is a draw!'), nl.
%------------------------------------------------------------------------------------------
