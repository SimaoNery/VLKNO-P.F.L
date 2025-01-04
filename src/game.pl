:- [ascii].
:- [menus].

:- use_module(library(random)).
:- use_module(library(system)).




% Access Game Menu
play :-
    now(Time),
    setrand(Time),
    % Default Game Configuration => (BoardSize, Player1 Type, Player2 Type, Player1 Name, Player2 Name, AI Difficulty Level)
    DefaultConfig = game_config(5, 0, 0, 'Player1', 'Player2', 1),

    draw_title, nl, nl, nl,
    write('Welcome to VLKNO!'), nl, nl,
    main_menu(DefaultConfig).
