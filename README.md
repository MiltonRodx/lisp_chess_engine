# ♟️ lisp-chess-engine

[![SBCL](https://img.shields.io/badge/Common_Lisp-SBCL-3fb68b)](https://www.sbcl.org/)
[![Tests](https://img.shields.io/badge/tests-355_passing-brightgreen)](#verification)
[![UCI](https://img.shields.io/badge/protocol-UCI-blue)](https://backscattering.de/chess-uci/)
[![Deps](https://img.shields.io/badge/dependencies-zero-orange)](#quick-start)
[![License](https://img.shields.io/badge/license-MIT-lightgrey)](#license)

*A from-scratch chess engine in Common Lisp. No libraries, no bitboards,
no magic — just an 8×8 mailbox, exact move rules, and an alpha-beta brain.*

```
  a b c d e f g h
8 r . b q k b . r
7 p p p . p p p p
6 . . n . . n . .
5 . . . . P . . .
4 . . . p . . . .
3 . . N . . N . .
2 P P P P . P P P
1 R . B Q K B . R
Turn: WHITE | Castling: KQkq | EP: - | Half: 0 Full: 5
```

*Actual engine output — self-play after 1.Nf3 Nf6 2.Nc3 Nc6 3.e4 d5 4.e5 d4.*

## ✨ Highlights

| | |
|---|---|
| 🧠 **Real search** | Iterative deepening + alpha-beta + quiescence + MVV-LVA/killer/history ordering |
| 📏 **Provably legal** | Perft matches official node counts to depth 4 (197,281 nodes) |
| 🔌 **Plays anywhere** | Native UCI — plug into XBoard, Scid, Arena, or any GUI |
| 🧪 **355 tests green** | Checkmates, stalemates, pins, en passant, castling edge cases |
| 📦 **Zero dependencies** | SBCL only. One `make build` → one 35 MB binary |

## 🚀 Quick start

```sh
# Prerequisite: SBCL
sudo apt install sbcl

make test    # run the full suite (34 tests, 355 assertions)
make build   # build the ./lisp-chess-engine binary

# Play in the terminal
./lisp-chess-engine
# > e2e4        (you move)
# > go          (engine replies, e.g. "engine plays c7c5 (score 30, nodes 8421)")
# > help        (all commands: undo, fen, perft, moves, new, quit)

# Talk UCI (GUIs, scripts)
./lisp-chess-engine --uci
```

<details>
<summary><b>🖥️ Play against it in a GUI (XBoard)</b></summary>

```sh
sudo apt install xboard
```

1. **Engine → Load New Engine…**
2. Name: `lisp-chess-engine`
3. Command: `/path/to/lisp-chess-engine`, Arguments: `--uci`, tick the **UCI** checkbox *(Register name/code stay empty — that's for commercial engines)*
4. Just move pieces — you're playing the engine. Set a clock under **Options → Time Control**; it understands `wtime`/`btime`/`winc`/`binc` and budgets its thinking.

Scid vs PC works too: **Tools → Analysis Engine… → New** with the same binary + `--uci`.

</details>

## 🧩 How it works

```
GUI ──UCI──▶ uci.lisp ──▶ search.lisp ◀── eval.lisp (material + piece-square tables)
                  │            │
                  │            ▼
                  │       movegen.lisp (pseudo-legal moves)
                  │            │
                  │            ▼
                  │       legal.lisp (make/unmake, king-safety filter, perft)
                  │            │
                  └───── state.lisp + board.lisp (8×8 grid, FEN, check detection)
```

A move's journey: the GUI sends `position startpos moves e2e4 e7e5` + `go depth 4`
→ the board is built and the moves replayed → every legal reply is imagined 4 deep
(~11,000 positions, ~0.15 s) → leaves are scored → `bestmove b1c3` is printed.

Coordinates: row 0 = rank 8, White marches toward row 0.

```
src/
  package.lisp  board.lisp   state.lisp    # position, FEN, check detection
  movegen.lisp  legal.lisp                 # pseudo-legal gen, make/unmake, perft
  eval.lisp     search.lisp                # material+PST, negamax+AB+quiescence+ID
  uci.lisp      main.lisp                  # UCI loop, REPL, binary entry
tests/                                     # dependency-free harness + 4 suites
```

## ✅ Verification

Perft (exact legal-move counts — the gold standard for rule correctness):

| Position | d1 | d2 | d3 | d4 |
|---|---|---|---|---|
| Startpos | 20 | 400 | 8,902 | **197,281** ✓ |
| Kiwipete (castling/promotions) | 48 | 2,039 | **97,862** ✓ | — |
| Endgame (en passant/pins) | 14 | 191 | 2,812 ✓ | — |

Plus: Scholar's-mate-in-1 solved at depth 2 (946 nodes), Fool's-mate/stalemate scored exactly,
make→unmake restores positions byte-for-byte, FEN round-trips on 6 field variants.

## ⌨️ UCI reference

```
position [startpos | fen <6 fields>] [moves e2e4 e7e8q ...]
go [depth N | movetime MS | wtime W btime B winc I binc J | infinite]
isready / ucinewgame / stop / quit
d            # print board (debug extra)
perft N      # count nodes (debug extra)
```

## 🗺️ Roadmap

- [x] Legal movegen + perft
- [x] Alpha-beta + quiescence + iterative deepening + UCI
- [ ] Transposition table (Zobrist) + null-move pruning → target depth 6–7
- [ ] Threefold-repetition detection
- [ ] Opening book + richer eval (pawn structure, king safety)
- [ ] Rating the engine (CCRL-style gauntlet vs. Fairy-Stockfish levels)

## 🛠️ Dev

```sh
make test    # SBCL --non-interactive --load run-tests.lisp (exit 1 on failure)
make repl    # drop into a live Lisp with the engine loaded
make perft   # needs ./lisp-chess-engine built
```

## License

MIT — see `lisp-chess-engine.asd`. Built with SBCL, tested on 2.5.2 (Debian).
