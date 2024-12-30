% File for menus displays and logistics

:- [game].

% Main Menu
main_menu(GameConfig) :-
    write('============================ MENU ============================'), nl, nl,
    write('     1 => START GAME'), nl, nl,
    write('     2 => Choose the Game Type'), nl, nl,
    write('     3 => Set Difficulty Levels for AI'), nl, nl,
    write('     4 => Set Board Size'), nl, nl,
    write('     5 => Rules'), nl, nl, nl,
    write('Enter Your Choice(1. - 2. - 3. - 4. - 5.): '), nl,
    
    read(Choice),
    menu_options(Choice, GameConfig).


% Main Menu Options
menu_options(1, GameConfig) :- 
    % Set Human Player Names
    set_player_names(GameConfig, NewGameConfig),

    % Initialize the game state
    initial_state(GameConfig, GameState),

    %Display the initial game state
    display_game(GameState),

    %Starts the game Loop
    game_loop(GameState).

menu_options(2, GameConfig) :- 
    clear_screen,
    set_game_type(GameConfig, NewGameConfig),
    clear_screen,
    main_menu(NewGameConfig).

menu_options(3, GameConfig) :- 
    clear_screen,
    set_difficulty_level(GameConfig, NewGameConfig),
    clear_screen,
    main_menu(NewGameConfig).

menu_options(4, GameConfig) :-
    clear_screen,
    set_board_size(GameConfig, NewGameConfig),
    clear_screen,
    main_menu(NewGameConfig).

menu_options(5, GameConfig) :-
    clear_screen,
    write('========== VLKNO GAME RULES =========='), nl, nl,
    write('VLKNO is a 2-player abstract strategy game played on a 5x5 grid covered in 25 stackable stones.'), nl,
    write('Players each have 2 pawns in their color, which are placed on stones in diagonally opposite corners of the grid at the start of the game.'), nl, nl,
    write('Each turn, you do the following actions:'), nl,
    write('    - Move either of your pawns one space in any direction to a stack of 1+ stones that\'s the same height, or 1 stone taller, or 1 stone shorter than yours.'), nl,
    write('    - Pick up a stone from the smallest unoccupied stack on the board, except the one you just came from (if there\'s a tie for shortest stack, you choose which one).'), nl,
    write('    - Place it on any unoccupied stack of 1+ stones except the one you just came from.'), nl, nl,
    write('You win when your opponent cannot complete each step of their turn.'), nl, nl,
    clear_screen,
    main_menu(GameConfig).

menu_options(_, GameConfig) :- 
    write('Choose between 1. - 2. - 3. - 4. - 5. => Try Again!'), nl, 
    main_menu(GameConfig).


% Set Players Names
set_player_names(GameConfig, NewGameConfig) :-
    write('========== Player Names =========='), nl, nl,
    write('Enter Player1 Name: '), nl,
    read(Player1Name),

    write('Enter Player2 Name: '), nl,
    read(Player2Name)

    GameConfig = game_config(BoardSize, Player1Type, Player2Type, _, _, AiLevel),
    NewGameConfig = game_config(BoardSize, Player1Type, Player2Type, Player1Name, Player2Name, AiLevel).


% Set game type
set_game_type(GameConfig, NewGameConfig) :-
  write('========== Game Type =========='), nl, nl,
    write('Enter Player1 Type: '), nl, 
    write('     => Human(h.)'), nl,
    write('     => Computer(pc.)'), nl,
    read(Player1Type),
    validate_player_type(Player1Type),

    write('Enter Player2 Type: '), nl, 
    write('     => Human(h.)'), nl,
    write('     => Computer(pc.)'), nl,
    read(Player2Type),
    validate_player_type(Player2Type),

    % Update game configuration to reflect user choice of Players
    GameConfig = game_config(BoardSize, _, _, Player1Name, Player2Name, AiLevel),
    NewGameConfig = game_config(BoardSize, Player1Type, Player2Type, Player1Name, Player2Name, AiLevel).

% Validate Input For Player Type
validate_player_type(h).
validate_player_type(pc).
validate_player_type(_) :-
    write('Invalid Input! Please enter "h." or "pc."'), nl, fail.


% Set Difficulty Levels for AI
set_difficulty_level(GameConfig, NewGameConfig) :-
  write('========== Difficulty Level =========='), nl, nl,
    write('Enter the Difficulty Level for the AI: '), nl,
    write('     => Easy (1.)'), nl, 
    write('     => Hard (2.)'), nl,
    read(DifficultyLevel),
    validate_difficulty_level(DifficultyLevel),

    % Update game configuration to reflect user choice of AI Difficulty Level
    GameConfig = game_config(BoardSize, Player1Type, Player2Type, Player1Name, Player2Name, _),
    NewGameConfig = game_config(BoardSize, Player1Type, Player2Type, Player1Name, Player2Name, DifficultyLevel).

% Validate Input For AI Difficulty Level
validate_difficulty_level(1).
validate_difficulty_level(2).
validate_difficulty_level(_) :-
    write('Invalid Input! Please enter "1." or "2."'), nl, 
    fail.


% Set Board Size
set_board_size(GameConfig, NewGameConfig) :-
  write('========== Board Size =========='), nl, nl,
    write('Enter the Board Size: '), nl,
    write('     => 4x4 (4.)'), nl,
    write('     => 5x5 (5.)'), nl,
    write('     => 6x6 (6.)'), nl,
    read(BoardSize),
    validate_board_size(BoardSize),

    % Update game configuration to reflect user choice of Board Size
    GameConfig = game_config(_, Player1Type, Player2Type, Player1Name, Player2Name, AiLevel),
    NewGameConfig = game_config(BoardSize, Player1Type, Player2Type, Player1Name, Player2Name, AiLevel).

% Validate Input For AI Difficulty Level
validate_board_size(4).
validate_board_size(5).
validate_board_size(6).
validate_board_size(_) :-
    write('Invalid Input! Please enter "4.", "5." or "6."'), nl, 
    fail.

% Function to clear the screen
clear_screen :-
    nl, nl, nl, nl, nl, nl, nl, nl, nl, nl, 
    nl, nl, nl, nl, nl, nl, nl, nl, nl, nl,
    nl, nl, nl, nl, nl, nl, nl, nl, nl, nl.
