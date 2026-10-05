# lisp_chess_engine

A chess engine in Common Lisp (SBCL). 8x8 mailbox board, full legal move
generation, alpha-beta search with quiescence, and a UCI interface.

## Quick start

```sh
make test        # run the test suite (SBCL required)
make build       # build ./lisp-chess-engine binary
./lisp-chess-engine --uci   # UCI mode (works with cutechess, Arena, etc.)
./lisp-chess-engine        # interactive REPL (play with `e2e4`, `go`, `help`)
```

Requires SBCL (`apt install sbcl`). No third-party dependencies.

## Layout

```
lisp-chess-engine.asd
src/
  package.lisp  board.lisp   state.lisp    # position representation, FEN, check
  movegen.lisp  legal.lisp                 # pseudo-legal gen, make/unmake, perft
  eval.lisp     search.lisp                # material+PST, negamax+AB+quiescence+ID
  uci.lisp      main.lisp                  # UCI loop, REPL, binary entry
tests/
  test-framework.lisp  test-board.lisp  test-state.lisp
  test-movegen.lisp    test-search.lisp
```

Coordinates: row 0 = rank 8, col 0 = file a. White moves toward
decreasing row.

## Strength / limits (v0.1.0)

- Search: iterative deepening, MVV-LVA + killer + history ordering,
  quiescence on captures/promotions (12-ply cap) with check evasions,
  50-move + stalemate + insufficient material draws. No transposition
  table or null-move pruning yet: depth 4 after 1.e4 e5 takes ~150ms
  (~75k nps, 8x8 mailbox in SBCL), mate-in-1 found at depth 2.
- No threefold-repetition detection yet (planned).
- Eval: material + piece-square tables, no pawn structure / king safety.

## Verification

- `perft` matches known counts: startpos d1-d4
  (20/400/8902/197281), Kiwipete d1-d3 (48/2039/97862).
- Mate-in-1 positions solved at depth 2.

## UCI options

Supports `position [startpos|fen ...] [moves ...]` and
`go [depth N|movetime MS|wtime btime winc binc|infinite]`.
Extra debug commands: `d` (print board), `perft N`.
