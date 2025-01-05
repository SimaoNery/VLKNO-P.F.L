% Load dependencies and modules
:- [ascii]. % Module for drawing ASCII art (e.g., game title)
:- [menus]. % Module for handling menus and game configuration
:- use_module(library(random)). % Provides random number generation
:- use_module(library(system)). % Provides access to system-related predicates

% ========================= Start the Game =========================
% Entry point for the game. Initializes random seed and starts the main menu.
play :-
    % Get the current system time to use as the seed for the random number generator.
    now(Time),

    % Seed the random number generator to ensure bots behave differently on each run.
    setrand(Time),

    % Default Game Configuration:
    % game_config(BoardSize, Player1Type, Player2Type, PawnNumber, Player1Name, Player2Name)
    DefaultConfig = game_config(5, 0, 0, 1, 'Player1', 'Player2'),

    % Display the game title using ASCII art
    draw_title, nl, nl, nl,

    % Welcome message
    write('Welcome to VLKNO!'), nl, nl,

    % Start the main menu with the default configuration
    main_menu(DefaultConfig).
