# Hess

Hess is a Haskell Chess library. It's an approximate port of https://github.com/doismellburning/chess

## Usage

(Please note, until https://github.com/doismellburning/hess/issues/1 is fixed, the below code examples aren't automatically tested. The doctests in the source _are_ though.)

Starting a game (and getting it as a String in FEN):

    >>> toFEN newGame
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
