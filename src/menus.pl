% File for menus displays and logistics

% Main Menu
main_menu(GameConfig) :-
    write('============================ MENU ============================'), nl, nl,
    write('     1 => START GAME'), nl, nl,
    write('     2 => Choose the Game Type'), nl, nl,
    write('     3 => Set Difficulty Levels for AI'), nl, nl,
    write('     4 => Rules'), nl, nl, nl,
    write('Enter Your Choice(1. - 2. - 3. - 4.): '), nl,
    
    read(Choice),
    menu_options(Choice, GameConfig).


% Main Menu Options
menu_options(1, GameConfig) :- 
    write('Starting Game...'), nl, nl,
    main_menu(GameConfig).

menu_options(2, GameConfig) :- 
    set_game_type(GameConfig, NewGameConfig),
    main_menu(NewGameConfig).

menu_options(3, GameConfig) :- 
    set_difficulty_level(GameConfig, NewGameConfig),
    main_menu(NewGameConfig).

menu_options(4, GameConfig) :-
    write('========== VLKNO GAME RULES =========='), nl, nl,
    write('VLKNO is a 2-player abstract strategy game played on a 5x5 grid covered in 25 stackable stones.'), nl,
    write('Players each have 2 pawns in their color, which are placed on stones in diagonally opposite corners of the grid at the start of the game.'), nl, nl,
    write('Each turn, you do the following actions:'), nl,
    write('    - Move either of your pawns one space in any direction to a stack of 1+ stones that\'s the same height, or 1 stone taller, or 1 stone shorter than yours.'), nl,
    write('    - Pick up a stone from the smallest unoccupied stack on the board, except the one you just came from (if there\'s a tie for shortest stack, you choose which one).'), nl,
    write('    - Place it on any unoccupied stack of 1+ stones except the one you just came from.'), nl, nl,
    write('You win when your opponent cannot complete each step of their turn.'), nl, nl,
    main_menu(GameConfig).

menu_options(_, GameConfig) :- 
    write('Choose between 1. - 2. - 3. -4. => Try Again!'), nl, 
    main_menu(GameConfig).


% Set game type
set_game_type(GameConfig, NewGameConfig) :-
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
    write('Invalid Input! Please enter "1." or "2."'), nl, fail.