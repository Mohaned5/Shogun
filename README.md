# Shogun Board Game

Shogun is a strategic board game played by two players on an 8x8 chess board, blending elements of chess and checkers. Each player controls a set of pieces, including a king and seven pawns. What sets Shogun apart is the introduction of energy, representing the movement capabilities of each piece.

## Features

Piece Energy: Pawns have energy ranging from 1 to 4, while kings have energy between 1 and 2. The energy determines how far a piece can move, and it can change during the game.
Dynamic Movement: Pieces move in straight lines (up, down, left, right) or in L-shape moves, with a single 90°-turn. Diagonal moves are not allowed, and pieces cannot go forward and then backward in a single move.
Capture and Block: Pieces cannot jump over others or stack on their own kind. Capturing is allowed by moving to an occupied field, removing the opponent's piece from the board.

## Implementation

This repository contains an implementation of Shogun's piece movement functions. It includes data structures for positions, colors (red or white), and pieces (pawn or king). Functions for incrementing and decrementing coordinates are provided in the template file.

## Usage
To use the Shogun Board Game program, create an initial board configuration using the Board class and the provided piece instances. For example:

    val b_init = Board(Set(King(2,Wht,(4,1)), King(1,Red,(5,8)),
                           Pawn(4,Wht,(1,1)), Pawn(4,Red,(1,8)),
                           // ... (additional piece configurations)
                           Pawn(2,Wht,(8,1)), Pawn(3,Red,(8,8))))
Then, find all legal moves for a specific piece using the legal_moves function. For instance:

    val pw1 = Pawn(4, Wht, (4,6))
    legal_moves(pw1, b_init)
Explore the repository to understand and leverage the Shogun board game's strategic nuances, combining energy management with tactical movements to outsmart your opponent.
