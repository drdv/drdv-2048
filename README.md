# drdv-2048

A simple implementation of the [2048 game](http://gabrielecirulli.github.io/2048).

![An example game with 974 moves stored in `scores/score-2048.json`](images/score-2048.png)

The image is from a game with 974 moves stored in `scores/score-2048.json`. The file `count_moves.py`
reports:

```
move 'U': 288 times
move 'D': 209 times
move 'L':   2 times
move 'R': 475 times
```

# Installation:

Ensure that `drdv-2048.el` is in a directory on your load-path, and add `(require 'drdv-2048)`
to your `~/.emacs` or `~/.emacs.d/init.el`.

# Notes:

1. Given that there is [2048-game.el](https://bitbucket.org/zck/2048.el),
   I wrote this because I simply wanted to have some practice using Lisp,
   to have undo (requested by my daughter) and to be able to replay saved
   games (for no good reason).

2. Start a game using `M-x drdv-2048-play`. Use arrows to move, `u` to undo, and `w` to save.

3. Load a saved game using `M-x drdv-2048-replay`.
