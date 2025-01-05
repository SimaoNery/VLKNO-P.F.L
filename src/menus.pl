% File for menus, displays, and game configuration logistics

:- [controllers]. % Load the controllers module

% ======================== Main Menu ==========================
% Displays the main menu and handles user input for menu options.
main_menu(GameConfig) :-
    write('============================ MENU ============================'), nl, nl,
    write('     1 => START GAME'), nl, nl,
    write('     2 => Choose the Game Type'), nl, nl,
    write('     3 => Set Board Size'), nl, nl,
    write('     4 => Set Pawn Number'), nl, nl,
    write('     5 => Rules'), nl, nl, nl,
    write('Enter Your Choice(1. - 2. - 3. - 4. - 5.): '), nl,
    read(Choice),
    menu_options(Choice, GameConfig).

% ======================= Menu Options ========================
% Handles the functionality of each main menu option.
menu_options(1, GameConfig) :- 
    % Start the game
    set_player_names(GameConfig, NewGameConfig), nl, % Prompt for player names
    initial_state(NewGameConfig, GameState),         % Initialize the game state
    game_loop(GameState),                            % Start the game loop
    handle_play_again_prompt(GameConfig).            % Handle whether to play again

menu_options(2, GameConfig) :- 
    % Configure game type (e.g., Human vs Computer)
    set_game_type(GameConfig, NewGameConfig),
    main_menu(NewGameConfig).

menu_options(3, GameConfig) :- 
    % Configure board size
    set_board_size(GameConfig, NewGameConfig),
    main_menu(NewGameConfig).

menu_options(4, GameConfig) :- 
    % Configure number of pawns per player
    set_pawn_number(GameConfig, NewGameConfig),
    main_menu(NewGameConfig).

menu_options(5, GameConfig) :- 
    % Display game rules
    write('========== VLKNO GAME RULES =========='), nl, nl,
    write('VLKNO is a 2-player abstract strategy game played on a 5x5 grid covered in 25 stackable stones.'), nl,
    write('Players each have 2 pawns in their color, placed on stones in diagonally opposite corners of the grid at the start.'), nl, nl,
    write('Each turn, you do the following actions:'), nl,
    write('    - Move one of your pawns to a stack of stones 1 taller, shorter, or the same height as your stack.'), nl,
    write('    - Pick up a stone from the smallest unoccupied stack (excluding the one you just left).'), nl,
    write('    - Place the stone on any other unoccupied stack (not the one you just left).'), nl, nl,
    write('You win if your opponent cannot complete each step of their turn.'), nl, nl,
    main_menu(GameConfig).

menu_options(_, GameConfig) :- 
    % Handle invalid menu choices
    write('Invalid Choice! Please select 1, 2, 3, 4, or 5.'), nl,
    main_menu(GameConfig).

% ======================= Player Names ========================
% Prompt players for their names and validate them.
set_player_names(GameConfig, NewGameConfig) :-
    write('========== Player Names =========='), nl, nl,
    write('Enter Player1 Name: '), nl,
    read(Player1Name),
    write('Enter Player2 Name: '), nl,
    read(Player2Name),
    validate_names(Player1Name, Player2Name, GameConfig, NewGameConfig).

% Ensure that player names are different.
validate_names(Player1Name, Player2Name, GameConfig, NewGameConfig) :-
    Player1Name \= Player2Name,
    GameConfig = game_config(BoardSize, Player1Type, Player2Type, PawnNumber, _, _),
    NewGameConfig = game_config(BoardSize, Player1Type, Player2Type, PawnNumber, Player1Name, Player2Name).

% If names are identical, prompt the user again.
validate_names(Player1Name, Player1Name, GameConfig, NewGameConfig) :-
    nl, nl, write('Error: Player names must be different!'), nl, nl,
    set_player_names(GameConfig, NewGameConfig).

% ==================== Handle Replay Choice ===================
% Ask if the player wants to play again.
handle_play_again_prompt(GameConfig) :-
    write('Want to play again?'), nl, nl,
    write('     => yes'), nl,
    write('     => no'), nl,
    read(PlayAgain), !,
    handle_play_again(PlayAgain, GameConfig).

% Replay the game if the player says "yes".
handle_play_again(yes, GameConfig) :-
    main_menu(GameConfig).

% Exit the game if the player says "no" or provides an invalid response.
handle_play_again(_, _) :-
    nl, write('Thanks for playing! Goodbye!'), nl.

% ===================== Game Configuration ====================
% Set the type of players (Human or AI) and validate input.
set_game_type(GameConfig, NewGameConfig) :-
    write('========== Game Type =========='), nl, nl,
    write('Enter Player1 Type: '), nl,
    write('     => Human(0.)'), nl,
    write('     => Computer Easy(1.)'), nl,
    write('     => Computer Hard(2.)'), nl,
    read(Player1Type),
    validate_player_type(Player1Type),
    write('Enter Player2 Type: '), nl,
    write('     => Human(0.)'), nl,
    write('     => Computer Easy(1.)'), nl,
    write('     => Computer Hard(2.)'), nl,
    read(Player2Type),
    validate_player_type(Player2Type),
    GameConfig = game_config(BoardSize, _, _, PawnNumber, Player1Name, Player2Name),
    NewGameConfig = game_config(BoardSize, Player1Type, Player2Type, PawnNumber, Player1Name, Player2Name).

% Validate input for player type.
validate_player_type(0). % Human
validate_player_type(1). % Computer Easy
validate_player_type(2). % Computer Hard
validate_player_type(_) :-
    write('Invalid Input! Please enter "0.", "1.", or "2."'), nl, fail.

% Set the board size and validate input.
set_board_size(GameConfig, NewGameConfig) :-
    write('========== Board Size =========='), nl, nl,
    write('Enter the Board Size: '), nl,
    write('     => 4x4 (4.)'), nl,
    write('     => 5x5 (5.)'), nl,
    write('     => 6x6 (6.)'), nl,
    read(BoardSize),
    validate_board_size(BoardSize),
    GameConfig = game_config(_, Player1Type, Player2Type, PawnNumber, Player1Name, Player2Name),
    NewGameConfig = game_config(BoardSize, Player1Type, Player2Type, PawnNumber, Player1Name, Player2Name).

% Validate board size input.
validate_board_size(4). % 4x4
validate_board_size(5). % 5x5
validate_board_size(6). % 6x6
validate_board_size(_) :-
    write('Invalid Input! Please enter "4.", "5.", or "6."'), nl, fail.

% Set the number of pawns per player and validate input.
set_pawn_number(GameConfig, NewGameConfig) :-
    write('========== Pawn Number =========='), nl, nl,
    write('Enter the Number of Pawns: '), nl,
    write('     => 1 per player (1.)'), nl,
    write('     => 2 per player (2.)'), nl,
    read(PawnNumber),
    validate_pawn_number(PawnNumber),
    GameConfig = game_config(BoardSize, Player1Type, Player2Type, _, Player1Name, Player2Name),
    NewGameConfig = game_config(BoardSize, Player1Type, Player2Type, PawnNumber, Player1Name, Player2Name).

% Validate pawn number input.
validate_pawn_number(1). % 1 pawn per player
validate_pawn_number(2). % 2 pawns per player
validate_pawn_number(_) :-
    write('Invalid Input! Please enter "1." or "2."'), nl, fail.
