:- [ascii].
:- [menus].

% Access Game Menu
play(0) :-
    % Default Game Configuration => (BoardSize, Player1 Type, Player2 Type, Player1 Name, Player2 Name, AI Difficulty Level)
    DefaultConfig = game_config(5, Human, Human, 'Player1', 'Player2', 1),

    draw_title, nl, nl, nl,
    write('Welcome to VLKNO!'), nl, nl,
    main_menu(DefaultConfig).


% Initialize the GameState based on the GameConfig given
initial_state(
        game_config(BoardSize, Player1Type, Player2Type, Player1Name, Player2Name, AiLevel), 
        game_state(Board, Player1Name, Info)) :-

    % Build the Board
    build_board(BoardSize, Board),

    % Set Player Info
    Info = [Player1Type-Player1Name, Player2Type-Player2Name].


% Buils the initial board with the desired size (all stacks size 1)
build_board(0, []). % Base Case
build_board(Size, [Row | Board]) :-
    build_row(Size, Row), % Will build a row with the given size
    
    NewSize is Size - 1,
    generate_board(NewSize, Board). % Recursively generates the other rows

% Builds the rows for the board
build_row(0, []).
build_row(Size, [1 | Row]) :-
    NewSize is Size - 1
    build_row(NewSize, Row) % Recursively generates the rest of the row


% Displays the current game state to the terminal
