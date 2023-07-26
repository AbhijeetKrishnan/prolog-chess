# prolog-chess: a chess library for Prolog

`prolog-chess` is a chess library for Prolog, with move generation, move validation, and support for the FEN format.

## Installation

`prolog-chess` is used as a Prolog module. Its source may be obtained by cloning this GitHub repository.

```bash
git clone https://github.com/AbhijeetKrishnan/prolog-chess.git
cd prolog-chess
```

## Usage

`prolog-chess` uses [SWI-Prolog](https://www.swi-prolog.org/). Follow the installation instructions on their website to
obtain it for your chosen platform.

This is Scholar's mate in `prolog-chess`.

```prolog
> swipl
Welcome to SWI-Prolog (threaded, 64 bits, version 9.0.4)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- [load].
true.

?- set_board_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", Board), make_move(Board, [e2, e4], Board1), make_move(Board1, [e7, e5], Board2), make_move(Board2, [d1, h5], Board3), make_move(Board3, [b8, c6], Board4), make_move(Board4, [f1, c4], Board5), make_move(Board5, [g8, f6], Board6), make_move(Board6, [h5, f7], Board7).
Board = [contents(piece(rook,white),square(a1)),contents(piece(knight,white),square(b1)),contents(piece(bishop,white),square(c1)),contents(piece(queen,white),square(d1)),contents(piece(king,white),square(e1)),contents(piece(bishop,white),square(f1)),contents(piece(knight,white),square(g1)),contents(piece(rook,white),square(h1)),contents(piece(pawn,white),square(a2)),contents(piece(pawn,white),square(b2)),contents(piece(pawn,white),square(c2)),contents(piece(pawn,white),square(d2)),contents(piece(pawn,white),square(e2)),contents(piece(pawn,white),square(f2)),contents(piece(pawn,white),square(g2)),contents(piece(pawn,white),square(h2)),contents(piece(pawn,black),square(a7)),contents(piece(pawn,black),square(b7)),contents(piece(pawn,black),square(c7)),contents(piece(pawn,black),square(d7)),contents(piece(pawn,black),square(e7)),contents(piece(pawn,black),square(f7)),contents(piece(pawn,black),square(g7)),contents(piece(pawn,black),square(h7)),contents(piece(rook,black),square(a8)),contents(piece(knight,black),square(b8)),contents(piece(bishop,black),square(c8)),contents(piece(queen,black),square(d8)),contents(piece(king,black),square(e8)),contents(piece(bishop,black),square(f8)),contents(piece(knight,black),square(g8)),contents(piece(rook,black),square(h8)),turn(white),kingside_castle(white),queenside_castle(white),kingside_castle(black),queenside_castle(black),halfmove_clock(0),fullmove(1)],
Board1 = [contents(piece(rook,white),square(a1)),contents(piece(knight,white),square(b1)),contents(piece(bishop,white),square(c1)),contents(piece(queen,white),square(d1)),contents(piece(king,white),square(e1)),contents(piece(bishop,white),square(f1)),contents(piece(knight,white),square(g1)),contents(piece(rook,white),square(h1)),contents(piece(pawn,white),square(a2)),contents(piece(pawn,white),square(b2)),contents(piece(pawn,white),square(c2)),contents(piece(pawn,white),square(d2)),contents(piece(pawn,white),square(f2)),contents(piece(pawn,white),square(g2)),contents(piece(pawn,white),square(h2)),contents(piece(pawn,black),square(a7)),contents(piece(pawn,black),square(b7)),contents(piece(pawn,black),square(c7)),contents(piece(pawn,black),square(d7)),contents(piece(pawn,black),square(e7)),contents(piece(pawn,black),square(f7)),contents(piece(pawn,black),square(g7)),contents(piece(pawn,black),square(h7)),contents(piece(rook,black),square(a8)),contents(piece(knight,black),square(b8)),contents(piece(bishop,black),square(c8)),contents(piece(queen,black),square(d8)),contents(piece(king,black),square(e8)),contents(piece(bishop,black),square(f8)),contents(piece(knight,black),square(g8)),contents(piece(rook,black),square(h8)),kingside_castle(white),queenside_castle(white),kingside_castle(black),queenside_castle(black),fullmove(1),halfmove_clock(0),en_passant(e3),contents(piece(pawn,white),square(e4)),turn(black)],
Board2 = [contents(piece(rook,white),square(a1)),contents(piece(knight,white),square(b1)),contents(piece(bishop,white),square(c1)),contents(piece(queen,white),square(d1)),contents(piece(king,white),square(e1)),contents(piece(bishop,white),square(f1)),contents(piece(knight,white),square(g1)),contents(piece(rook,white),square(h1)),contents(piece(pawn,white),square(a2)),contents(piece(pawn,white),square(b2)),contents(piece(pawn,white),square(c2)),contents(piece(pawn,white),square(d2)),contents(piece(pawn,white),square(f2)),contents(piece(pawn,white),square(g2)),contents(piece(pawn,white),square(h2)),contents(piece(pawn,black),square(a7)),contents(piece(pawn,black),square(b7)),contents(piece(pawn,black),square(c7)),contents(piece(pawn,black),square(d7)),contents(piece(pawn,black),square(f7)),contents(piece(pawn,black),square(g7)),contents(piece(pawn,black),square(h7)),contents(piece(rook,black),square(a8)),contents(piece(knight,black),square(b8)),contents(piece(bishop,black),square(c8)),contents(piece(queen,black),square(d8)),contents(piece(king,black),square(e8)),contents(piece(bishop,black),square(f8)),contents(piece(knight,black),square(g8)),contents(piece(rook,black),square(h8)),kingside_castle(white),queenside_castle(white),kingside_castle(black),queenside_castle(black),contents(piece(pawn,white),square(e4)),fullmove(2),halfmove_clock(0),en_passant(e6),contents(piece(pawn,black),square(e5)),turn(white)],
Board3 = [contents(piece(rook,white),square(a1)),contents(piece(knight,white),square(b1)),contents(piece(bishop,white),square(c1)),contents(piece(king,white),square(e1)),contents(piece(bishop,white),square(f1)),contents(piece(knight,white),square(g1)),contents(piece(rook,white),square(h1)),contents(piece(pawn,white),square(a2)),contents(piece(pawn,white),square(b2)),contents(piece(pawn,white),square(c2)),contents(piece(pawn,white),square(d2)),contents(piece(pawn,white),square(f2)),contents(piece(pawn,white),square(g2)),contents(piece(pawn,white),square(h2)),contents(piece(pawn,black),square(a7)),contents(piece(pawn,black),square(b7)),contents(piece(pawn,black),square(c7)),contents(piece(pawn,black),square(d7)),contents(piece(pawn,black),square(f7)),contents(piece(pawn,black),square(g7)),contents(piece(pawn,black),square(h7)),contents(piece(rook,black),square(a8)),contents(piece(knight,black),square(b8)),contents(piece(bishop,black),square(c8)),contents(piece(queen,black),square(d8)),contents(piece(king,black),square(e8)),contents(piece(bishop,black),square(f8)),contents(piece(knight,black),square(g8)),contents(piece(rook,black),square(h8)),kingside_castle(white),queenside_castle(white),kingside_castle(black),queenside_castle(black),contents(piece(pawn,white),square(e4)),fullmove(2),contents(piece(pawn,black),square(e5)),halfmove_clock(1),contents(piece(queen,white),square(h5)),turn(black)],
Board4 = [contents(piece(rook,white),square(a1)),contents(piece(knight,white),square(b1)),contents(piece(bishop,white),square(c1)),contents(piece(king,white),square(e1)),contents(piece(bishop,white),square(f1)),contents(piece(knight,white),square(g1)),contents(piece(rook,white),square(h1)),contents(piece(pawn,white),square(a2)),contents(piece(pawn,white),square(b2)),contents(piece(pawn,white),square(c2)),contents(piece(pawn,white),square(d2)),contents(piece(pawn,white),square(f2)),contents(piece(pawn,white),square(g2)),contents(piece(pawn,white),square(h2)),contents(piece(pawn,black),square(a7)),contents(piece(pawn,black),square(b7)),contents(piece(pawn,black),square(c7)),contents(piece(pawn,black),square(d7)),contents(piece(pawn,black),square(f7)),contents(piece(pawn,black),square(g7)),contents(piece(pawn,black),square(h7)),contents(piece(rook,black),square(a8)),contents(piece(bishop,black),square(c8)),contents(piece(queen,black),square(d8)),contents(piece(king,black),square(e8)),contents(piece(bishop,black),square(f8)),contents(piece(knight,black),square(g8)),contents(piece(rook,black),square(h8)),kingside_castle(white),queenside_castle(white),kingside_castle(black),queenside_castle(black),contents(piece(pawn,white),square(e4)),contents(piece(pawn,black),square(e5)),contents(piece(queen,white),square(h5)),fullmove(3),halfmove_clock(2),contents(piece(knight,black),square(c6)),turn(white)],
Board5 = [contents(piece(rook,white),square(a1)),contents(piece(knight,white),square(b1)),contents(piece(bishop,white),square(c1)),contents(piece(king,white),square(e1)),contents(piece(knight,white),square(g1)),contents(piece(rook,white),square(h1)),contents(piece(pawn,white),square(a2)),contents(piece(pawn,white),square(b2)),contents(piece(pawn,white),square(c2)),contents(piece(pawn,white),square(d2)),contents(piece(pawn,white),square(f2)),contents(piece(pawn,white),square(g2)),contents(piece(pawn,white),square(h2)),contents(piece(pawn,black),square(a7)),contents(piece(pawn,black),square(b7)),contents(piece(pawn,black),square(c7)),contents(piece(pawn,black),square(d7)),contents(piece(pawn,black),square(f7)),contents(piece(pawn,black),square(g7)),contents(piece(pawn,black),square(h7)),contents(piece(rook,black),square(a8)),contents(piece(bishop,black),square(c8)),contents(piece(queen,black),square(d8)),contents(piece(king,black),square(e8)),contents(piece(bishop,black),square(f8)),contents(piece(knight,black),square(g8)),contents(piece(rook,black),square(h8)),kingside_castle(white),queenside_castle(white),kingside_castle(black),queenside_castle(black),contents(piece(pawn,white),square(e4)),contents(piece(pawn,black),square(e5)),contents(piece(queen,white),square(h5)),fullmove(3),contents(piece(knight,black),square(c6)),halfmove_clock(3),contents(piece(bishop,white),square(c4)),turn(black)],
Board6 = [contents(piece(rook,white),square(a1)),contents(piece(knight,white),square(b1)),contents(piece(bishop,white),square(c1)),contents(piece(king,white),square(e1)),contents(piece(knight,white),square(g1)),contents(piece(rook,white),square(h1)),contents(piece(pawn,white),square(a2)),contents(piece(pawn,white),square(b2)),contents(piece(pawn,white),square(c2)),contents(piece(pawn,white),square(d2)),contents(piece(pawn,white),square(f2)),contents(piece(pawn,white),square(g2)),contents(piece(pawn,white),square(h2)),contents(piece(pawn,black),square(a7)),contents(piece(pawn,black),square(b7)),contents(piece(pawn,black),square(c7)),contents(piece(pawn,black),square(d7)),contents(piece(pawn,black),square(f7)),contents(piece(pawn,black),square(g7)),contents(piece(pawn,black),square(h7)),contents(piece(rook,black),square(a8)),contents(piece(bishop,black),square(c8)),contents(piece(queen,black),square(d8)),contents(piece(king,black),square(e8)),contents(piece(bishop,black),square(f8)),contents(piece(rook,black),square(h8)),kingside_castle(white),queenside_castle(white),kingside_castle(black),queenside_castle(black),contents(piece(pawn,white),square(e4)),contents(piece(pawn,black),square(e5)),contents(piece(queen,white),square(h5)),contents(piece(knight,black),square(c6)),contents(piece(bishop,white),square(c4)),fullmove(4),halfmove_clock(4),contents(piece(knight,black),square(f6)),turn(white)],
Board7 = [contents(piece(rook,white),square(a1)),contents(piece(knight,white),square(b1)),contents(piece(bishop,white),square(c1)),contents(piece(king,white),square(e1)),contents(piece(knight,white),square(g1)),contents(piece(rook,white),square(h1)),contents(piece(pawn,white),square(a2)),contents(piece(pawn,white),square(b2)),contents(piece(pawn,white),square(c2)),contents(piece(pawn,white),square(d2)),contents(piece(pawn,white),square(f2)),contents(piece(pawn,white),square(g2)),contents(piece(pawn,white),square(h2)),contents(piece(pawn,black),square(a7)),contents(piece(pawn,black),square(b7)),contents(piece(pawn,black),square(c7)),contents(piece(pawn,black),square(d7)),contents(piece(pawn,black),square(g7)),contents(piece(pawn,black),square(h7)),contents(piece(rook,black),square(a8)),contents(piece(bishop,black),square(c8)),contents(piece(queen,black),square(d8)),contents(piece(king,black),square(e8)),contents(piece(bishop,black),square(f8)),contents(piece(rook,black),square(h8)),kingside_castle(white),queenside_castle(white),kingside_castle(black),queenside_castle(black),contents(piece(pawn,white),square(e4)),contents(piece(pawn,black),square(e5)),contents(piece(knight,black),square(c6)),contents(piece(bishop,white),square(c4)),fullmove(4),contents(piece(knight,black),square(f6)),halfmove_clock(0),contents(piece(queen,white),square(f7)),turn(black)].
```

## Documentation

The [PlDoc](https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/pldoc.html%27)) documentation server can be started with -

```prolog
?- [debug].
% Started server at http://localhost:4000/pldoc/
true.
```
## Tests

A Python script has been provided to compare results between `prolog-chess` and [`python-chess`](https://python-chess.readthedocs.io/en/latest/) across
a variety of positions loaded from a custom PGN file. The below example uses the 2013 - January archive (17.8 MB) provided by the [Lichess
games database](https://database.lichess.org/).

```bash
wget https://database.lichess.org/standard/lichess_db_standard_rated_2013-01.pgn.zst
pzstd -d lichess_db_standard_rated_2013-01.pgn.zst
```

Create a [conda](https://docs.conda.io/en/latest/) environment to install the packages necessary to run the test script.

```bash
conda env create -f conda_env.yml
conda activate prolog-chess
python3 test_legal_move.py
```

This takes a while to finish.