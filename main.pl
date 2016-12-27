:- dynamic get_size/2, main_window/1, cell/3, space/2, final/2.

init_maze(0, 0).
init_maze(X, Y):-
    get_size(W, H),
    (
	X =:= 0, Y > 0 -> X1 is W - 1, Y1 is Y - 1;
        X1 is X - 1, Y1 is Y
    ),
    init_maze(X1, Y1),
    (
	X mod 2 =:= 1, Y mod 2 =:= 1,
	W2 is W - 1, H2 is H - 1,
	X < W2, Y < H2 -> assert(cell(X, Y, uv));
	true
    ).
init_maze:-
    (
        cell(_, _, _) -> retractall(cell(_, _, _));
        true
    ),
    (
	space(_, _) -> retractall(space(_, _));
	true
    ),
    get_size(W, H),
    W1 is W - 1, H1 is H - 1,
    init_maze(W1, H1).

remove_wall(X, Y, X1, Y1):-
    (
       X =:= X1, Y > Y1 -> Y2 is Y1 + 1, assert(space(X, Y2));
       X =:= X1, Y < Y1 -> Y2 is Y + 1, assert(space(X, Y2));
       Y =:= Y1, X < X1 -> X2 is X + 1, assert(space(X2, Y));
       Y =:= Y1, X > X1 -> X2 is X1 + 1, assert(space(X2, Y))
    ).

get_neighbours(_, _, 4, []) :- !.
get_neighbours(X, Y, Dir, L):-
    get_size(W, H),
    (
	Dir =:= 0, X1 is X + 2, X1 < W, Y < H, X > 0, Y > 0, cell(X1, Y, uv) ->
	Dir1 is Dir + 1, get_neighbours(X, Y, Dir1, L1), append(L1, [(X1, Y)], L);
        Dir =:= 1, Y1 is Y + 2, Y1 < H, X < W, X > 0, Y > 0, cell(X, Y1, uv) ->
	Dir1 is Dir + 1, get_neighbours(X, Y, Dir1, L1), append(L1, [(X, Y1)], L);
        Dir =:= 2, X1 is X - 2, X1 > 0, X < W, Y < H, Y > 0, cell(X1, Y, uv) ->
	Dir1 is Dir + 1, get_neighbours(X, Y, Dir1, L1), append(L1, [(X1, Y)], L);
        Dir =:= 3, Y1 is Y - 2, Y < H, X < W, X > 0, Y1 > 0, cell(X, Y1, uv) ->
	Dir1 is Dir + 1, get_neighbours(X, Y, Dir1, L1), append(L1, [(X, Y1)], L);
        Dir1 is Dir + 1, get_neighbours(X, Y, Dir1, L)
    ).

find_n([X | _], 0, X).
find_n([_ | T], N, X):-
    N1 is N - 1, find_n(T, N1, X).

randlist(_, 0, L, L) :- ! .
randlist(Max, N, L, L_res):-
    N > 0,
    random_between(0, Max, X),
    check_in_list(X, L, Max ,XT),
    N1 is N - 1,
    randlist(Max, N1, [XT | L], L_res).

check_in_list(X, L, _, X):-
    not(member(X, L)), !.
check_in_list(_, L, N, Y):-
    random_between(0, N, X_res), check_in_list(X_res, L, N, Y).

mix_l(_, [], L, L).
mix_l(L, [N | T], L1, LM):-
    find_n(L, N, (X, Y)),
    mix_l(L, T, [(X, Y) | L1], LM).
mix_l(L, LM):-
    length(L, N),
    N1 is N - 1,
    randlist(N1, N, [], LM_num),
    mix_l(L, LM_num, [], LM).

visited(X, Y):-
    retract(cell(X, Y, uv)),
    assert(cell(X, Y, v)).

next_step(X, Y, XN, YN):-
    get_neighbours(X, Y, 0, L), mix_l(L, LM), !, member((XN, YN), LM), visited(XN, YN).

carve_maze_coord(_, _):-
    not(cell(_, _, uv)), !.
carve_maze_coord(X, Y):-
    next_step(X, Y, XN, YN), remove_wall(X, Y, XN, YN), carve_maze_coord(XN, YN).

carve_maze:-
    visited(1, 1),
    carve_maze_coord(1, 1).

check_size(W1, H1, W, H):-
    (
        W1 mod 2 =:= 0 ->
        W is W1 - 1; W is W1
    ),
    (
	H1 mod 2 =:= 0 ->
	H is H1 - 1; H is H1
    ).

mk_window(Window):-
    get_size(W, H), W1 is W * 5, H1 is H * 5,
    new(Window, new(_, picture('Maze', size(W1, H1)))).

print_block(Window, 1, 0):-
    new(Box, box(5, 5)),
    send(Box, fill_pattern, colour(red)),
    send(Window, display, Box, point(5, 0)).
print_block(Window, X, Y):-
    get_size(W, H),
    W1 is W - 2, H1 is H - 1,
    X =:= W1, Y =:= H1,
    X1 is X * 5, Y1 is Y * 5,
    new(Box, box(5, 5)),
    send(Box, fill_pattern, colour(red)),
    send(Window, display, Box, point(X1, Y1)).
print_block(Window, X, Y):-
    (
        not(cell(X, Y, _)), not(space(X, Y)) ->
        new(Box, box(5, 5)),
        send(Box, fill_pattern, colour(black)),
        X1 is X * 5, Y1 is Y * 5,
        send(Window, display, Box, point(X1, Y1))
    ).
print_block(_, _, _).
print_maze(0, _, _, _).
print_maze(N, Window, X, Y):-
    print_block(Window, X, Y),
    N1 is N - 1,
    get_size(W, _),
    W1 is W - 1,
    (
        X =:= W1 -> X1 is 0, Y1 is Y + 1, print_maze(N1, Window, X1, Y1);
        X1 is X + 1, print_maze(N1, Window, X1, Y)
    ).
print_maze:-
    (
        main_window(_) ->
        retractall(main_window(_));
        true
    ),
    mk_window(Window),
    assert(main_window(Window)),
    main_window(Window),
    send(Window, open),
    get_size(W, H),
    T is W * H,
    print_maze(T, Window, 0, 0).

maze(W1, H1):-
    (
        get_size(_, _) ->
        retractall(get_size(_, _));
        true
    ),
    check_size(W1, H1, W, H),
    assert(get_size(W, H)),
    init_maze, !,
    carve_maze,
    print_maze.

next_point(X, Y, X1, Y1):-
    X1 is X + 1, Y1 is Y;
    X1 is X, Y1 is Y + 1;
    X1 is X - 1, Y1 is Y;
    X1 is X, Y1 is Y - 1.
next_step_path(X, Y, XN, YN, X_prev, Y_prev):-
    next_point(X, Y, XN, YN), (space(XN, YN); cell(XN, YN, _)), (not(XN =:= X_prev); not(YN =:= Y_prev)).

solve_maze(X, Y, Xp, Yp):-
    (
        final(X, Y) -> !, retract(cell(X, Y, v)), assert(cell(X, Y, p));
	next_step_path(X, Y, Xn, Yn, Xp, Yp),
        solve_maze(Xn, Yn, X, Y), !,
        (
	    cell(X, Y, v) -> retract(cell(X, Y, v));
	    space(X, Y) -> retract(space(X, Y))
	),
        assert(cell(X, Y, p))
    ).
solve_maze:-
    (final(_, _) -> retractall(final(_, _)); true),
    (cell(X, Y, p) -> retract(cell(X, Y, p)), assert(cell(X, Y, v)); true),
    get_size(W, H),
    W1 is W - 2, H1 is H - 2,
    assert(final(W1, H1)),
    !,
    solve_maze(1, 1, 1, 1), !,
    print_solution.

print_start_and_finish(Window):-
    new(Box, box(5, 5)),
    send(Box, fill_pattern, colour(red)),
    send(Window, display, Box, point(5, 0)),
    get_size(W, H),
    W1 is W - 2, H1 is H - 1,
    X is W1 * 5, Y is H1 * 5,
    new(Box, box(5, 5)),
    send(Box, fill_pattern, colour(red)),
    send(Window, display, Box, point(X, Y)).

print_solution(_, []).
print_solution(Window, [(X, Y) | T]):-
    new(Box, box(5, 5)),
    send(Box, fill_pattern, colour(green)),
    X1 is X * 5, Y1 is Y * 5,
    send(Window, display, Box, point(X1, Y1)),
    print_solution(Window, T).
print_solution:-
    main_window(Window),
    findall((X, Y), cell(X, Y, p), L),
    print_solution(Window, L),
    print_start_and_finish(Window),
    send(Window, open).

main:-
    new(DW, dialog('Maze Creator / Solver')),
    send_list(DW, append,
        [
            new(W, int_item(width, low := 5, high := 300, default := 30)),
            new(H, int_item(height, low := 5, high := 300, default := 30)),
            button(create, message(@prolog, maze, W?selection, H?selection)),
	    button(solve, message(@prolog, solve_maze)),
            button(exit, message(DW, destroy))
        ]),
    send(DW, open).
