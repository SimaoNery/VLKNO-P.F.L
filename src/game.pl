:- [ascii].
:- [menus].

% Access Game Menu
play :-
    % Default Game Configuration => (BoardSize, Player1 Type, Player2 Type, Pawn Number, Player1 Name, Player2 Name, AI Difficulty Level)
    DefaultConfig = game_config(5, Human, Human, 1, 'Player1', 'Player2', 1),

    draw_title, nl, nl, nl,
    write('Welcome to VLKNO!'), nl, nl,
    main_menu(DefaultConfig).
