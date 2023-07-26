/** <module> Chess Board Representation
 *
 * Represents the piece layout on a chess board without considering other aspects of the game state.
 * A base board is modeled as a list of contents/2 predicates which describe the piece positions.
 * A contents/2 predicates describes the piece (type + side) and its location (square) on a chess board.
 * e.g., contents(piece(white, king), square(e1)) represents a white king situated at e1 on the chess board.
 *
 * @author Abhijeet Krishnan
 * @copyright (c)2022 Abhijeet Krishnan.
 * @license All rights reserved. Used with permission.
 */
:- module(base_board, [
    piece_at/3, 
    is_empty/2, 
    can_attack/3, 
    attacks/3, 
    is_attacked/3, 
    remove_piece_at/3, 
    set_piece_at/4, 
    set_board_contents/2,
    valid_piece_at/4
]).

:- use_module(colors).
:- use_module(piece_types).
:- use_module(squares).
:- use_module(pieces).
:- use_module(moves).
:- use_module(square_set).

/**
 * pieces(+BaseBoard:baseboard, +Type:p_type, +Color:color, -SquareSet:sqset) is nondet
 *
 * Gets all the locations of a piece of a given type and color
 *
 * @param BaseBoard
 * @param Type
 * @param Color
 * @param SquareSet
 */
pieces([], _, _, []).
pieces([contents(piece(Type, Color), square(Square))|T], Type, Color, [square(Square)|T1]) :-
    pieces(T, Type, Color, T1).
pieces([_|T], Type, Color, SquareSet) :-
    pieces(T, Type, Color, SquareSet).

/**
 * piece_at(+BaseBoard:baseboard, -Piece:piece, +At:square) is nondet
 *
 * Gets the piece at a given square.
 * Returns a unique 'empty' atom if the given square is empty.
 *
 * @param BaseBoard
 * @param At
 * @param Piece
 */
piece_at(BaseBoard, piece(Type, Side), At) :-
    member(contents(piece(Type, Side), square(At)), BaseBoard).
piece_at(BaseBoard, empty, At) :-
    \+ member(contents(piece(_, _), square(At)), BaseBoard).

/**
 * valid_piece_at(+BaseBoard:baseboard, -Type:p_type, -Side:color, +At:square) is nondet
 *
 * Gets the piece at a given square.
 * Fails to unify if no piece exists on the square
 *
 * @param BaseBoard
 * @param Type
 * @param Side
 * @param At
 */
valid_piece_at(BaseBoard, Type, Side, At) :-
    member(contents(piece(Type, Side), square(At)), BaseBoard).

/**
 * is_empty(+BaseBoard:baseboard, +SquareSet:sqset) is det
 *
 * is_empty/2 succeeds if all squares in SquareSet are empty i.e. have no piece on them.
 *
 * @param BaseBoard
 * @param SquareSet
 */
is_empty(_, []).
is_empty(BaseBoard, [H|T]) :-
    piece_at(BaseBoard, empty, H),
    is_empty(BaseBoard, T).

/**
 * can_attack(+BaseBoard:baseboard, +Square:square, -AttackSquare:square) is nondet
 *
 * Checks if the piece at the given Square can attack another square.
 *
 * @param BaseBoard
 * @param Square
 * @param AttackSquare
 */
can_attack(BaseBoard, Square, AttackSquare) :-
    piece_at(BaseBoard, piece(Type, Side), Square),
    attack_square(Square, Type, Side, AttackSquare),
    findall(Bet, sq_between(Square, AttackSquare, 1, Bet), BetweenSquares), % n^2!
    is_empty(BaseBoard, BetweenSquares).

/**
 * attacks(+BaseBoard:baseboard, +Square:square, -SquareSet:sqset) is nondet
 *
 * Gets the set of attacked squares from the given square.
 * There will be no attacks if the square is empty. Pinned pieces are still considered to be attacking other squares.
 * Assumes no other pieces on board.
 *
 * @param BaseBoard
 * @param Square
 * @param SquareSet
 */
attacks(BaseBoard, Square, SquareSet) :-
    piece_at(BaseBoard, piece(Type, Side), Square),
    attack_squares(Square, Type, Side, SquareSet).

/**
 * is_attacked(+BaseBoard:baseboard, +Square:square, -Piece:piece) is nondet
 *
 * Checks if any piece is attacking the given square.
 *
 * @param 
 */
is_attacked(BaseBoard, Square, Piece) :-
    piece_at(BaseBoard, Piece, At),
    can_attack(BaseBoard, At, Square).

/**
 * remove_piece_at(+BaseBoard:baseboard, +At:square, -NewBaseBoard:baseboard) is det
 *
 * Remove the piece from a given position on the board.
 * Leaves the board in the same state if the given square is empty.
 *
 * @param BaseBoard
 * @param At
 * @param NewBaseBoard
 */
remove_piece_at(BaseBoard, At, NewBaseBoard) :-
    delete(BaseBoard, contents(_, square(At)), NewBaseBoard).

/**
 * set_piece_at(+BaseBoard:baseboard, +Piece:piece, +At:square, -NewBaseBoard) is det
 *
 * Set a piece at a given location on the board.
 *
 * @param BaseBoard
 * @param Piece
 * @param At
 * @param NewBaseBoard
 */
set_piece_at(BaseBoard, Piece, At, NewBaseBoard) :-
    append(BaseBoard, [contents(Piece, square(At))], NewBaseBoard).

/**
 * scan_row(+RowChars:list(char), +Row:int, +Col:int, -RowPreds:list(pred)) is det
 *
 * Converts a row of characters representing the board position of a single rank into their corresponding predicate 
 * representations using contents/2.
 *
 * @param RowChars
 * @param Row The row index [1, 8] of the row to be converted (required for the correct square to be identified)
 * @param Col
 * @param RowPreds
 */
scan_row([], _, _, []).
scan_row([H1|T1], Row, Col, [H2|T2]) :-
    (
        char_type(H1, digit) ->
            atom_number(H1, Incr),
            NewCol is Col + Incr,
            H2 = [],
            scan_row(T1, Row, NewCol, T2)
        ;
            piece_char(H1, Piece),
            coords(Sq, Col, Row),
            NewCol is Col + 1,
            H2 = contents(Piece, square(Sq)),
            scan_row(T1, Row, NewCol, T2)
    ).

/**
 * set_board_contents(+PosStr:str, -BaseBoard:baseboard) is det
 *
 * Converts a string representation of a base board into its predicate-based representation using contents/2.
 *
 * @param PosStr
 * @param BaseBoard
 */
set_board_contents(PosStr, BaseBoard) :-
    split_string(PosStr, "/", "", [Row8, Row7, Row6, Row5, Row4, Row3, Row2, Row1]),
    string_chars(Row1, Row1List),
    string_chars(Row2, Row2List),
    string_chars(Row3, Row3List),
    string_chars(Row4, Row4List),
    string_chars(Row5, Row5List),
    string_chars(Row6, Row6List),
    string_chars(Row7, Row7List),
    string_chars(Row8, Row8List),
    scan_row(Row1List, 1, 1, Row1Preds),
    scan_row(Row2List, 2, 1, Row2Preds),
    scan_row(Row3List, 3, 1, Row3Preds),
    scan_row(Row4List, 4, 1, Row4Preds),
    scan_row(Row5List, 5, 1, Row5Preds),
    scan_row(Row6List, 6, 1, Row6Preds),
    scan_row(Row7List, 7, 1, Row7Preds),
    scan_row(Row8List, 8, 1, Row8Preds),
    flatten([Row1Preds, Row2Preds, Row3Preds, Row4Preds, Row5Preds, Row6Preds, Row7Preds, Row8Preds], BaseBoard).