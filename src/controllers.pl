:- use_module(library(lists)).


% game_loop/1
% The main game loop. It keeps running until the game is over.
% If the game is over, the winner is displayed.
% Otherwise, the game continues by processing the current player's turn.

% Base case: Game is over, display the winner.
game_loop(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions)) :-
    game_over(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), Winner),
    write_game_results(Winner).

% Recursive case: Game is not over, proceed with the current player's turn.
game_loop(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions)) :- 
    nl, nl, 

    % Display the current game state
    display_game(game_state(Board, PlayerName, PlayersInfo, PlayersPositions)), nl, nl,

    % Announce whose turn it is
    write('It\'s '), write(PlayerName), write('\'s turn!'), nl, nl,

    % Choose the move for the current player
    choose_move(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), CurrentPlayer, SelectedPawn, Move),

    % Apply the move if valid, resulting in a new game state
    move(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), SelectedPawn, Move, NewGameState),

    % Continue the loop with the updated game state
    game_loop(NewGameState).



% initial_state/2
% Initializes the game state based on the provided game configuration (GameConfig).
% It sets up the board, player information, and initial positions for the pawns.

initial_state(
        game_config(BoardSize, Player1Type, Player2Type, PawnNumber, Player1Name, Player2Name), 
        game_state(Board, (Player1Type-Player1Name), PlayerInfo, PlayerPositions)) :-

    % Build the board with the specified size
    build_board(BoardSize, BoardSize, Board),

    % Set player information (type and name for both players)
    PlayerInfo = [Player1Type-Player1Name, Player2Type-Player2Name],

    % Initialize the pawn positions based on the number of pawns each player has
    initialize_pawn_positions(BoardSize, PawnNumber, Player1Name, Player2Name, PlayerPositions).

% middle_state/1
% Initializes the game state with a middle game predefined board and player positions.
% This state is useful for testing and showcasing the game logic.

middle_state(GameState) :-
    Board = [[0,2,1,2,0],
             [0,3,0,0,4],
             [0,3,1,2,0],
             [0,0,1,0,0],
             [4,0,0,0,2]],

    Player1Type = 0,
    Player1Name = smith,
    Player2Type = 0,
    Player2Name = jones,
    PlayerInfo = [Player1Type-Player1Name, Player2Type-Player2Name],
    PlayerPositions = [smith-(5,1), smith-(3,3), jones-(2,4), jones-(4,5)],

    GameState = game_state(Board, (Player1Type-Player1Name), PlayerInfo, PlayerPositions).

% end_state/1
% Initializes the game state with an end game predefined board and player positions.
% This state is useful for testing and showcasing the game over logic.

end_state(GameState) :-
    Board = [[0,1,0,0,0],
             [0,6,0,4,0],
             [1,0,0,6,2],
             [1,0,0,0,0],
             [0,0,2,2,0]],

    Player1Type = 0,
    Player1Name = smith,
    Player2Type = 0,
    Player2Name = jones,
    PlayerInfo = [Player1Type-Player1Name, Player2Type-Player2Name],
    PlayerPositions = [smith-(1,2), smith-(5,3), jones-(2,5), jones-(4,1)],

    GameState = game_state(Board, (Player1Type-Player1Name), PlayerInfo, PlayerPositions).
    

% ---Initial State Helpers---------------------------------------------------------

% build_board/3
% Constructs the board with the given size (each stack initially has size 1).
% This predicate generates a list of rows for the board.
% The base case handles a board of size 0 (empty).
build_board(0, _, []). % Base case: an empty board.
build_board(RowSize, ColSize, [Row | Board]) :-
    % Build each row for the board
    build_row(ColSize, Row),
    % Recursively build the next row
    NewRowSize is RowSize - 1,
    build_board(NewRowSize, ColSize, Board).

% build_row/2
% Creates a single row with the specified size, where each stack initially has a size of 1.
% The base case handles a row of size 0 (empty).
build_row(0, []). % Base case: an empty row.
build_row(Size, [1 | Row]) :-
    % Add a stack with size 1 to the row
    NewSize is Size - 1,
    build_row(NewSize, Row).

% initialize_pawn_positions/5
% Initializes the positions for the pawns based on the given number of pawns per player.
% The format for player positions is a list of player names paired with their (X, Y) coordinates.

% 1 pawn per player: Player1 starts at (1,1) and Player2 at (BoardSize, BoardSize).
initialize_pawn_positions(BoardSize, 1, Player1Name, Player2Name, PlayerPositions) :-
    PlayerPositions = [Player1Name-(1,1), Player2Name-(BoardSize, BoardSize)].

% 2 pawns per player: Player1 has pawns at (1,1) and (BoardSize, BoardSize), 
% Player2 has pawns at (1, BoardSize) and (BoardSize, 1).
initialize_pawn_positions(BoardSize, 2, Player1Name, Player2Name, PlayerPositions) :-
    PlayerPositions = [
        Player1Name-(1,1), 
        Player1Name-(BoardSize, BoardSize), 
        Player2Name-(1, BoardSize), 
        Player2Name-(BoardSize, 1)
    ].


% ---Board Displaying-------------------------------------------------------

% display_game/1
% Displays the current game state, including the player information and the game board.
% The game state includes the board and the positions of the players.
% It first displays the board and then prints additional newlines for formatting.

display_game(game_state(Board, _, _, PlayerPositions)) :-
    display_board(Board, PlayerPositions), nl, nl, nl.

% display_board/2
% Displays the game board along with player positions.
% It handles the display of the board rows and columns.
% First, it displays the rows, then prints the column numbers at the top of the board.
display_board(Board, PlayerPositions) :-
    length(Board, BoardLength),
    display_rows(Board, BoardLength, PlayerPositions), % Display rows from the last one
    write('   _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _'), nl,  % Horizontal separator
    write('    '),
    display_columns(1, BoardLength).  % Display column numbers

% display_columns/2
% Displays the column numbers above the board for reference.
% It prints numbers from 1 to the length of the board.
display_columns(Current, End) :-
    Current =< End,
    write('     '), write(Current), write('     '),
    Next is Current + 1,
    display_columns(Next, End).

display_columns(_, _).  % Termination condition: stops when Current > End

% display_rows/3
% Displays the rows of the board, starting from the last row.
% Each row is prefixed with its row number for easy reference.
display_rows([], _, _).  % Base case: no rows left to display
display_rows([Row | Rest], RowNum, PlayerPositions) :-
    write(RowNum), write(' | '),  % Print row number + separator
    display_tiles(Row, RowNum, 1, PlayerPositions), nl,  % Display the tiles in the row
    NextRowNum is RowNum - 1,
    display_rows(Rest, NextRowNum, PlayerPositions).  % Recurse for the next row

% display_tiles/4
% Displays the tiles in a row. Each tile can either be a stone or a player piece.
% It checks if a player is at a specific position and displays the corresponding symbol with the stone (value) in parenthesis after the symbol.
display_tiles([Stone | Rest], RowNum, ColNum, PlayerPositions) :-
    player_at_position(ColNum, RowNum, PlayerPositions, Player), !,
    write('['), write(Player), write('('), write(Stone), write(')'), write('] '),  % Display player piece
    NextColNum is ColNum + 1,
    display_tiles(Rest, RowNum, NextColNum, PlayerPositions).  % Recurse for next column

% display_tiles/4 (Regular tile case)
% If no player is at the current position, display the stone (value) on the tile.
display_tiles([Stone | Rest], RowNum, ColNum, PlayerPositions) :-
    write('[    '), write(Stone), write('   ] '),  % Display regular stone
    NextColNum is ColNum + 1,
    display_tiles(Rest, RowNum, NextColNum, PlayerPositions).  % Recurse for next column

display_tiles([], _, _, _).  % Base case: no tiles left to display

% player_at_position/4
% Checks if a player is at a given position (specified by column and row coordinates).
% It looks through the list of player positions and returns the player if found.
player_at_position(ColNum, RowNum, [Player-(ColNum, RowNum) | _], Player).
player_at_position(ColNum, RowNum, [_ | Rest], Player) :-
    player_at_position(ColNum, RowNum, Rest, Player).

% --- Player Actions -------------------------------------------------------

% pawn_list/4
% Generates a list of pawns for a given player that have valid moves.
% It checks each player's position and retrieves pawns that have valid moves available.
% Valid pawns are those that can make a valid move (i.e., their valid_moves list is not empty).
pawn_list(Gamestate, PlayerName, PlayersPositions, Pawns) :-
    findall(
        (X, Y), 
        (
            member(PlayerName-(X, Y), PlayersPositions),  % Find the player's positions
            valid_moves(Gamestate, (X, Y), ValidMoves),    % Get valid moves for the position
            ValidMoves \= []  % Only include pawns with valid moves
        ),
        Pawns
    ).

% select_pawn/4
% Allows the player to select a pawn to move. If only one pawn is available, it is selected automatically.
% If multiple pawns are available, the player is prompted to choose one by index.
select_pawn(Gamestate, PlayerName, PlayersPositions, SelectedPawn) :-
    pawn_list(Gamestate, PlayerName, PlayersPositions, Pawns),
    select_pawn_from_list(Pawns, SelectedPawn).

% select_pawn_from_list/2
% If only one pawn is available, it is automatically selected.
% If multiple pawns are available, it prompts the player to select one by entering the index.
select_pawn_from_list([Pawn], Pawn).  % Base case: only one pawn
select_pawn_from_list(Pawns, SelectedPawn) :- 
    write('Enter the index of the pawn you want to move: '), nl, 
    write_positions(Pawns, 1),  % Display all available pawns with their indices
    read(Index),  % Get the player's input for the pawn index
    nth1(Index, Pawns, SelectedPawn).  % Select the pawn based on index

% move/4
% Changes the game state according to the player's move.
% It validates the selected move, executes the move, and then switches players.
move(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions),
    SelectedPawn,
    Move,
    game_state(NewBoard, NextPlayer, PlayersInfo, NewPlayersPositions)) :- 

    % Validate the Move
    valid_moves(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), SelectedPawn, ValidMoves),
    member(Move, ValidMoves),  % Ensure the move is valid

    % Execute the Move
    execute_move(Board, (CurrentPlayer-PlayerName), PlayersPositions, SelectedPawn, Move, NewBoard, NewPlayersPositions),

    % Switch Players
    switch_players((CurrentPlayer-PlayerName), PlayersInfo, NextPlayer).

% execute_move/7
% Executes the selected valid move by updating the board and player positions.
% It translates the move into coordinates, updates the piece's position, and modifies the board.
execute_move(Board, (CurrentPlayer-PlayerName), PlayersPositions, SelectedPawn, Move, NewBoard, NewPlayersPositions) :-
    
    % Translate the move into new coordinates
    translate_move(Move, SelectedPawn, FinalPosition),

    % Move the piece to the new position
    update_piece_coordinates((CurrentPlayer-PlayerName), SelectedPawn, FinalPosition, PlayersPositions, NewPlayersPositions),

    % Modify the board based on the player's move
    pick_and_place_stone(Board, CurrentPlayer, (CurrentPlayer-PlayerName), _, SelectedPawn, FinalPosition, PlayersPositions, NewBoard).

% extract_second_elements/2
% Extracts the second elements (coordinates) from a list of player-position pairs.
% This helps to get a list of player positions in a simplified format (coordinates only).
extract_second_elements([], []).  % Base case: no more elements to extract
extract_second_elements([_-Position | Rest], [Position | PositionsRest]) :-
    extract_second_elements(Rest, PositionsRest).  % Recurse for the rest of the list

% get_non_empty_positions/2
% Retrieves all positions with non-zero stacks from the board.
% The positions of the non-zero stacks are used to identify potential valid move destinations.
get_non_empty_positions(Board, Positions) :-
    length(Board, BoardLength),
    findall((X, InvertedY), 
            ( 
                nth1(Y, Board, Row),
                nth1(X, Row, Height),
                Height > 0,  % Only consider non-zero stacks
                InvertedY is BoardLength - Y + 1  % Adjust Y coordinate for display
            ),
            Positions).

% in_exclude/1
% Helper predicate to check if a position is in the exclusion list.
in_exclude(ExcludePositions, X) :-
    member(X, ExcludePositions).  % Return true if X is in ExcludePositions

% filter_positions/3
% Filters out positions that are in the exclusion list.
% The valid positions are those that are not in ExcludePositions.
filter_positions(Positions, ExcludePositions, FilteredPositions) :-
    exclude(in_exclude(ExcludePositions), Positions, FilteredPositions).

% get_valid_positions/6
% Retrieves a list of valid positions for the player to move to.
% The valid positions are calculated by excluding positions that are occupied by other players or out of bounds.
get_valid_positions(Board, SmallestStackPosition, CurrentPosition, FinalPosition, PlayersPositions, ValidPositions) :-
    get_non_empty_positions(Board, AllPositions),  % Get all non-zero stack positions
    extract_second_elements(PlayersPositions, PlayersPositionsList),  % Get player positions
    append([SmallestStackPosition, CurrentPosition, FinalPosition], PlayersPositionsList, ExcludePositions),  % Exclude certain positions
    filter_positions(AllPositions, ExcludePositions, ValidPositions).  % Filter out invalid positions

% get_with_value/3
% Filters the sorted values to find the ones with the highest value.
% It returns the moves with the highest value, useful for AI decisions.
get_with_value(SortedValues, HighestValue, BestValues) :-
    findall(Value-Move-Pawn, 
            (member(Value-Move-Pawn, SortedValues), Value =:= HighestValue), 
            BestValues).

% get_with_value_2/3
% Similar to `get_with_value/3`, but only returns moves with the highest value.
get_with_value_2(SortedValues, HighestValue, BestValues) :-
    findall(Value-Move, 
            (member(Value-Move, SortedValues), Value =:= HighestValue), 
            BestValues).


% pick_and_place_stone/8 (Human Player)
% Handles the stone placement for Human Player type. This version allows the player to manually select
% the coordinates where they want to place the stone.
pick_and_place_stone(Board, 0, _, _, CurrentPosition, FinalPosition, PlayersPositions, NewBoard) :-
    % Determine the smallest unoccupied stack (excluding the previous position)
    find_smallest_stack(Board, CurrentPosition, FinalPosition, SmallestStackPositions, PlayersPositions),

    % Select the smallest stack from the list of possible positions
    select_smallest_stack_position(SmallestStackPositions, SmallestStackPosition),

    % Prompt the player to choose coordinates for placing the stone
    write('Choose the coordinates to place the stone!'), nl,
    write('     =>X: '), nl,
    read(X),  % Read the X coordinate input from the player
    write('     =>Y: '), nl,
    read(Y),  % Read the Y coordinate input from the player

    % Ensure that the chosen coordinates are valid
    validate_stone_placement(Board, CurrentPosition, FinalPosition, (X, Y), PlayersPositions),

    % Update the board with the new stone placement
    move_stone(Board, SmallestStackPosition, (X, Y), NewBoard).


% pick_and_place_stone/8 (Easy AI)
% Handles the stone placement for Easy AI. The AI will choose the move randomly
% from valid positions, ensuring strategic behavior based on the current game state.
pick_and_place_stone(Board, 1, _, _, CurrentPosition, FinalPosition, PlayersPositions, NewBoard) :-

    % Determine the smallest unoccupied stack (excluding the previous position)
    find_smallest_stack(Board, CurrentPosition, FinalPosition, SmallestStackPositions, PlayersPositions),

    % Randomly select one of the smallest stacks from the list of available positions
    random_member(SmallestStackPosition, SmallestStackPositions),

    % Get valid positions to place the stone after selecting the smallest stack
    get_valid_positions(Board, SmallestStackPosition, CurrentPosition, FinalPosition, PlayersPositions, ValidPositions),

    % Randomly select a valid position from the list of possible placements
    random_member(PlacePosition, ValidPositions),

    % Display AI's move
    write('AI moved stone from '), write(SmallestStackPosition), write(' to '), write(PlacePosition), nl,

    % Update the board with the new stone placement
    move_stone(Board, SmallestStackPosition, PlacePosition, NewBoard).


% pick_and_place_stone/8 (Hard AI)
% A more strategic approach for Hard AI. The AI evaluates potential moves by calculating 
% values for the current and opponent's game state, sorting the moves, and selecting the best one.
pick_and_place_stone(Board, 2, (CurrentPlayer-PlayerName), PlayersInfo, CurrentPosition, FinalPosition, PlayersPositions, NewBoard) :-

    % Determine the smallest unoccupied stack (excluding the previous position)
    find_smallest_stack(Board, CurrentPosition, FinalPosition, SmallestStackPositions, PlayersPositions),

    % Randomly select one of the smallest stacks from the available positions
    findall(RemoveValue-Remove,
            (
                member(Remove, SmallestStackPositions),
                
                % Get the current height of the stack at the selected position
                get_height(Board, Remove, RemoveHeight),

                % Decrease the height at the chosen position to simulate removing the stone
                NewRemoveHeight is RemoveHeight - 1,

                % Update the board after removing the stone
                update_board(Board, Remove, NewRemoveHeight, TempRemoveBoard),

                % Evaluate the new game state after the move
                value(game_state(TempRemoveBoard, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), current, RemovePositiveValue),
                value(game_state(TempRemoveBoard, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), other, RemoveNegativeValue),

                % Calculate the overall value of the move (positive value - negative value)
                RemoveValue is RemovePositiveValue - RemoveNegativeValue
            ),
            RemoveWithValues),

    % Sort the potential moves by their evaluated value
    keysort(RemoveWithValues, SortedRemoveWithValues),

    % Find the highest value move
    last(SortedRemoveWithValues, HighestRemoveValue-_),

    % Filter the list to retain only the best moves with the highest value
    get_with_value_2(SortedRemoveWithValues, HighestRemoveValue, BestRemoves),

    % Choose randomly from the best moves (highest value)
    random_member(_-BestRemove, BestRemoves),

    % Get the height of the stack at the selected position
    get_height(Board, BestRemove, HeightRemove),

    % Decrease the height of the selected stack to simulate removing the stone
    NewHeightRemove is HeightRemove - 1,

    % Update the board after removing the stone
    update_board(Board, BestRemove, NewHeightRemove, TempRemoveBoard),

    % Get valid positions where the stone can be placed after removal
    get_valid_positions(Board, BestRemove, CurrentPosition, FinalPosition, PlayersPositions, ValidPositions),

    % Find all possible valid moves for placing the stone
    findall(AddValue-Add,
            (
                member(Add, ValidPositions),
                
                % Get the current stack height at the valid position
                get_height(TempRemoveBoard, Add, HeightAdd),

                % Increase the height at the chosen position to simulate placing the stone
                NewHeightAdd is HeightAdd + 1,

                % Update the board after placing the stone
                update_board(TempRemoveBoard, Add, NewHeightAdd, TempAddBoard),

                % Evaluate the new game state after placing the stone
                value(game_state(TempAddBoard, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), current, AddPositiveValue),
                value(game_state(TempAddBoard, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), other, AddNegativeValue),

                % Calculate the overall value of the placement move
                AddValue is AddPositiveValue - AddNegativeValue
            ),
            AddWithValues),

     % Sort the potential placements by their evaluated value
    keysort(AddWithValues, SortedAddWithValues),

    % Find the highest value placement move
    last(SortedAddWithValues, HighestAddValue-_),

    % Filter the list to retain only the best placement moves with the highest value
    get_with_value_2(SortedAddWithValues, HighestAddValue, BestAdds),

    % Choose randomly from the best placement moves (highest value)
    random_member(_-BestAdd, BestAdds),

    % Display AI's move
    write('AI moved stone from '), write(BestRemove), write(' to '), write(BestAdd), nl,

    % Update the board with the final stone placement
    move_stone(Board, BestRemove, BestAdd, NewBoard).

% --- Auxiliary Move Predicates ---------------------------------------------------------------------

% valid_moves/3:
% Creates a list of all valid moves for the selected pawn.
% It checks all possible moves and filters out the ones that are invalid.
valid_moves(game_state(Board, (_-_), _, PlayersPositions), SelectedPawn, ValidMoves) :-
    findall(
        Move,
        (
            % Translate the move direction to final coordinates
            translate_move(Move, SelectedPawn, FinalPosition),

            % Validate that the final position is acceptable
            validate_final_position(Board, SelectedPawn, FinalPosition, PlayersPositions)
        ),
        RawMoves
    ),
    % Sort the valid moves to maintain order
    sort(RawMoves, ValidMoves).


% translate_move/3:
% Translates a given move direction into the final position coordinates.
% Each direction is mapped to a change in either the X or Y coordinate.

% Move upwards (increases Y coordinate)
translate_move(up, (X, Y), (X, Y1)) :- Y1 is Y + 1.

% Move downwards (decreases Y coordinate)
translate_move(down, (X, Y), (X, Y1)) :- Y1 is Y - 1.

% Move left (decreases X coordinate)
translate_move(left, (X, Y), (X1, Y)) :- X1 is X - 1.

% Move right (increases X coordinate)
translate_move(right, (X, Y), (X1, Y)) :- X1 is X + 1.

% Move diagonally up-left (decreases X and increases Y)
translate_move(up_left, (X, Y), (X1, Y1)) :- X1 is X - 1, Y1 is Y + 1.

% Move diagonally up-right (increases X and increases Y)
translate_move(up_right, (X, Y), (X1, Y1)) :- X1 is X + 1, Y1 is Y + 1.

% Move diagonally down-left (decreases X and decreases Y)
translate_move(down_left, (X, Y), (X1, Y1)) :- X1 is X - 1, Y1 is Y - 1.

% Move diagonally down-right (increases X and decreases Y)
translate_move(down_right, (X, Y), (X1, Y1)) :- X1 is X + 1, Y1 is Y - 1.


% validate_final_position/4:
% Validates if a move to the final position is valid based on the current game state.
% It checks if the new position is within bounds, if the height difference between positions is valid,
% and whether the final position is unoccupied.
validate_final_position(Board, CurrentPosition, FinalPosition, PlayersPositions) :-
    % Ensure the final position is within board bounds
    is_within_bounds(Board, FinalPosition),

    % Ensure the height difference between current and final positions is acceptable
    is_valid_height(Board, CurrentPosition, FinalPosition),

    % Ensure the final position is not already occupied by another player
    \+ is_occupied(FinalPosition, PlayersPositions).


% is_within_bounds/2:
% Checks if the given position (X, Y) is within the bounds of the board.
% The X and Y coordinates must lie within the board dimensions.
is_within_bounds(Board, (X, Y)) :-
    length(Board, N),
    X > 0, X =< N,  % Ensure X is within the range
    Y > 0, Y =< N.  % Ensure Y is within the range


% is_valid_height/3:
% Ensures the height difference between the current and target position is valid.
% The difference in stack heights must not exceed 1, and the target position must have a height greater than 0.
is_valid_height(Board, (X1, Y1), (X2, Y2)) :-
    get_height(Board, (X1, Y1), H1),
    get_height(Board, (X2, Y2), H2),

    H2 > 0,  % Target position must have a stone (height > 0)
    Diff is abs(H1 - H2),  % Absolute difference between heights
    Diff =< 1.  % The difference must be at most 1


% get_height/3:
% Retrieves the height of a stack at a given position (X, Y) on the board.
% The height is the number of stones stacked at that position.
get_height(Board, (X, Y), Height) :-
    length(Board, BoardLength),       
    InvertedY is BoardLength - Y + 1,  % Adjust Y coordinate due to Prolog indexing
    nth1(InvertedY, Board, Row),  % Retrieve the specific row based on Y
    nth1(X, Row, Height).  % Get the stack height at the X coordinate


% is_occupied/2:
% Checks if a position (X, Y) is occupied by a player's pawn.
% This predicate checks through the list of players' positions.
% Base case: The position matches the current player's pawn.
is_occupied((X, Y), [_-(X, Y) | _]) :- !.

% Recursive case: Keep checking the rest of the list if the position is not found.
is_occupied((X, Y), [_ | Rest]) :-
    is_occupied((X, Y), Rest).


% update_piece_coordinates/5:
% Updates the coordinates of the selected pawn in the players' positions list.
% It updates the position of the current player in the list, leaving other players' positions unchanged.
% Base case: The position matches the current player's pawn, so we update it.
update_piece_coordinates((_-PlayerName), SelectedPawn, NewPosition, [PlayerName-SelectedPawn | Rest], [PlayerName-NewPosition | Rest]) :- !.

% Recursive case: Keep checking the rest of the list until the correct player is found.
update_piece_coordinates((Player-PlayerName), SelectedPawn, NewPosition, [OtherPlayer-OtherPosition | Rest], [OtherPlayer-OtherPosition | UpdatedRest]) :-
    update_piece_coordinates((Player-PlayerName), SelectedPawn, NewPosition, Rest, UpdatedRest).
%------------------------------------------------------------------------------------------

% --- Auxiliary Stone Movement Predicates ----------------------------------------------------------------------

% find_smallest_stack/4:
% Finds the smallest stack on the board, excluding the current and final positions.
% This predicate identifies all the positions with stacks of stones, sorts them based on height, 
% and then selects the positions with the smallest stack(s).
find_smallest_stack(Board, CurrentPosition, FinalPosition, SmallestStackPositions, PlayersPositions) :-
    length(Board, BoardLength),

    % Generate all valid positions on the board (coordinates)
    findall(
        (X, Y),
        (between(1, BoardLength, X), between(1, BoardLength, Y)),
        AllPositions
    ),

    % Retrieves heights of all positions (excluding current and final positions),
    % ensuring the positions are not occupied, and the stack height is greater than zero.
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
    
    % Sort the stacks by height (smallest to largest)
    keysort(Stacks, SortedStacks),

    % Select the position(s) with the smallest stack
    SortedStacks = [SmallestHeight-_|_],

    % Collect all positions with the smallest stack height
    findall(
        (X, Y),
        member(SmallestHeight-(X, Y), SortedStacks),
        SmallestStackPositions
    ).


% between/3:
% Generates all integer values between Lower and Upper, inclusive.
% This is a custom version of the built-in `between/3` predicate.
between(Lower, Upper, Lower) :- 
    Lower =< Upper.
between(Lower, Upper, X) :- 
    Lower < Upper,
    Next is Lower + 1,
    between(Next, Upper, X).


% validate_stone_placement/4:
% Validates the placement of a stone on the board.
% Ensures the target position is within bounds, not equal to the current or final positions,
% not occupied by another player's piece, and has a height greater than zero.
validate_stone_placement(Board, CurrentPosition, FinalPosition, (X, Y), PlayersPositions) :-
    is_within_bounds(Board, (X, Y)),
    (X, Y) \= CurrentPosition,
    (X, Y) \= FinalPosition,
    \+ is_occupied((X, Y), PlayersPositions),
    get_height(Board, (X, Y), Height),
    Height > 0.


% move_stone/4:
% Moves a stone from one position to another on the board.
% It decrements the height at the source position and increments the height at the destination.
% If the source and destination coordinates are the same, the board remains unchanged.
move_stone(Board, (X, Y), (X, Y), Board). % No change if coordinates are the same
move_stone(Board, (X1, Y1), (X2, Y2), NewBoard) :-
    get_height(Board, (X1, Y1), H1),
    get_height(Board, (X2, Y2), H2),

    % Decrease the height at the source position
    UpdatedH1 is H1 - 1,
    update_board(Board, (X1, Y1), UpdatedH1, TempBoard),

    % Increase the height at the destination position
    UpdatedH2 is H2 + 1,
    update_board(TempBoard, (X2, Y2), UpdatedH2, NewBoard).


% update_board/4:
% Updates the height value at a specific position on the board.
% This modifies the board at the coordinates (X, Y) by setting the new value for the stack height.
update_board(Board, (X, Y), NewValue, UpdatedBoard) :-
    length(Board, BoardLength),             % Get the number of rows in the board
    InvertedY is BoardLength - Y + 1,       % Invert the Y-coordinate for correct list access
    nth1(InvertedY, Board, Row),            % Access the specified row using Y
    nth1(X, Row, _, RestOfRow),             % Access and replace the column value at X
    nth1(X, NewRow, NewValue, RestOfRow),   % Construct the updated row with the new value
    nth1(InvertedY, Board, _, RestOfBoard), % Remove the old row from the board
    nth1(InvertedY, UpdatedBoard, NewRow, RestOfBoard). % Replace with the updated row


% select_smallest_stack_position/2:
% Selects a position for picking a stone from a list of smallest stack positions.
% If there is only one valid position, it automatically selects it and prints it.
% If there are multiple options, it prompts the user to choose by providing an index.
select_smallest_stack_position([SinglePosition], SinglePosition) :-
    write('Smallest Stack Position: '), write(SinglePosition), nl.

% Allows the user to choose the smallest stack position when multiple options are available
select_smallest_stack_position(SmallestStackPositions, SmallestStackPosition) :-
    write('Choose the coordinates from where to pick the stone!'), nl,

    % Write all available positions with their index
    write_positions(SmallestStackPositions, 1),

    % Prompt the user to enter the index of the desired position
    write('Enter the index of the coordinates you want: '), nl,
    read(Coords),
    nth1(Coords, SmallestStackPositions, SmallestStackPosition).


% write_positions/2:
% Displays all positions with their respective index for the user to choose from.
write_positions([], _).  % Base case: no positions to display
write_positions([(X, Y)|Rest], Index) :- 
    write(Index), write(': ('), write(X), write(', '), write(Y), write(')'), nl,
    NextIndex is Index + 1,
    write_positions(Rest, NextIndex).  % Recurse for the rest of the positions
%------------------------------------------------------------------------------------------

% --- Player Switching Predicates ----------------------------------------------------------------------

% switch_players/3:
% Switches the current player to the next player in the list. It identifies the current player 
% and returns the other player (the next player to take their turn).
% If the current player is the first player in the list, it switches to the second player.
% If the current player is the second player in the list, it switches to the first player.

% Example 1: Current player is Player 1, and Player 2 is the other player.
% switch_players((Player1Type-Player1Name), [Player1Type-Player1Name, Player2Type-Player2Name], (Player2Type-Player2Name)).
% The result is Player 2's information.

% Example 2: Current player is Player 2, and Player 1 is the other player.
% switch_players((Player2Type-Player2Name), [Player1Type-Player1Name, Player2Type-Player2Name], (Player1Type-Player1Name)).
% The result is Player 1's information.
switch_players((CurrentPlayer-CurrentName), [CurrentPlayer-CurrentName, Player2Type-Player2Name], (Player2Type-Player2Name)).
switch_players((CurrentPlayer-CurrentName), [Player1Type-Player1Name, CurrentPlayer-CurrentName], (Player1Type-Player1Name)).

%------------------------------------------------------------------------------------------

% --- Bot Moves ---------------------------------------------------------------------------

% choose_move/4: 
% Decides the next move for the player based on the game mode (human, easy AI, or hard AI).
% 0 indicates a human player, 1 indicates a random bot (easy mode), and 2 indicates a greedy bot (hard mode).

% For a human player (0):
% - The player is prompted to select one of their pawns.
% - A list of valid moves for the selected pawn is shown.
% - The player is asked to input their move from the available options.
choose_move(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), 0, SelectedPawn, Move) :-
    select_pawn(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), PlayerName, PlayersPositions, SelectedPawn),
    valid_moves(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), SelectedPawn, ValidMoves),
    write('These are your valid moves => '), write(ValidMoves), nl, nl,
    write('Choose your move: '), read(Move), nl, nl.

% For a random bot (1):
% - The AI selects a random pawn from the list of the player’s pawns.
% - The AI then selects a random valid move for that pawn.
choose_move(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), 1, SelectedPawn, Move) :-
    write('AI thinking...'), nl,
    pawn_list(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), PlayerName, PlayersPositions, Pawns),
    random_member(SelectedPawn, Pawns),
    valid_moves(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), SelectedPawn, ValidMoves),
    random_member(Move, ValidMoves),
    write('AI chose: '), write(Move), write(' for pawn in ('), write(SelectedPawn), write(')'), nl.

% For a greedy bot (2):
% - The AI evaluates all valid moves for each of its pawns.
% - The move that maximizes the value (based on a heuristic function) is chosen.
choose_move(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), 2, SelectedPawn, BestMove) :-
    write('AI thinking...'), nl,
    pawn_list(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), PlayerName, PlayersPositions, Pawns),
    write('Pawns: '), write(Pawns), nl,

    % Evaluate each valid move for each pawn
    findall(Value-Move-Pawn,
            (
                member(Pawn, Pawns),
                valid_moves(game_state(Board, (CurrentPlayer-PlayerName), PlayersInfo, PlayersPositions), Pawn, ValidMoves),
                member(Move, ValidMoves),
                translate_move(Move, Pawn, FinalPosition),
                update_piece_coordinates((CurrentPlayer-PlayerName), Pawn, FinalPosition, PlayersPositions, NewPlayersPositions),
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


% sum_list/2: Sums the elements of a list.
sum_list([], 0).
sum_list([H|T], Sum) :- sum_list(T, Rest), Sum is H + Rest.

% value/3: Computes the value of the game state for a given player.
% For the current player:
% - Computes the total number of valid moves for all pawns and multiplies the result by 2.
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

% For the opponent:
% - Computes the total number of valid moves for the opponent’s pawns.
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

% game_over/2: Checks if the game is over and identifies the winner or if it's a draw.

% If neither player can move, the game is a draw.
game_over(game_state(Board, _, [Player1-Player1Name, Player2-Player2Name], PlayersPositions), draw) :-
    \+ player_can_move(game_state(Board, (Player1-Player1Name), PlayersInfo, PlayersPositions)),
    \+ player_can_move(game_state(Board, (Player2-Player2Name), PlayersInfo, PlayersPositions)).

% If Player 1 cannot move but Player 2 can, Player 2 wins.
game_over(game_state(Board, _, [Player1-Player1Name, Player2-Player2Name], PlayersPositions), Player2Name) :-
    \+ player_can_move(game_state(Board, (Player1-Player1Name), PlayersInfo, PlayersPositions)),
    player_can_move(game_state(Board, (Player2-Player2Name), PlayersInfo, PlayersPositions)).

% If Player 2 cannot move but Player 1 can, Player 1 wins.
game_over(game_state(Board, _, [Player1-Player1Name, Player2-Player2Name], PlayersPositions), Player1Name) :-
    player_can_move(game_state(Board, (Player1-Player1Name), PlayersInfo, PlayersPositions)),
    \+ player_can_move(game_state(Board, (Player2-Player2Name), PlayersInfo, PlayersPositions)).

% player_can_move/2: Determines if a player can move any of their pieces.
% A player can move if there exists at least one piece with a valid move and the number of unoccupied positions with height > 0 is greater than or equal to 3.
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
    FlatList \= [], % At least one pawn must have valid moves

    length(Board, BoardLength),

    % Generate all positions on the board
    findall(
        (X, Y),
        (between(1, BoardLength, X), between(1, BoardLength, Y)),
        AllPositions
    ),

    % Get all valid positions with height > 0 that are not occupied
    findall(
        Height-(X,Y),
        (
            member((X, Y), AllPositions),
            get_height(Board, (X, Y), Height),
            \+ is_occupied((X, Y), PlayersPositions),
            Height > 0
        ),
        ValidPositions
    ),

    length(ValidPositions, Result),
    Result >= 3.  % Must have at least 3 valid positions with height > 0

% write_game_results/1: Outputs the result of the game (win or draw).
write_game_results(Winner) :-
    Winner \= draw,
    nl, write(Winner), write(' just WON the game! CONGRATULATIONS!!'), nl, nl.

write_game_results(draw) :-
    nl, write('The game is a draw!'), nl.
%------------------------------------------------------------------------------------------
