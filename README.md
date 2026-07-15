# Functional Mancala Game Engine

A Haskell implementation of the Mancala board game demonstrating functional programming concepts, recursive data structures, game tree search, and custom state handling abstractions.

The project focuses on modeling a complete game environment using functional programming techniques and includes an AI opponent based on the minimax algorithm.

## Features

### Mancala Game Implementation

The project implements a complete Mancala game engine, including:

- Board representation and game state management
- Validation of legal moves
- Seed distribution and movement rules
- Capture mechanics
- Game termination detection
- Winner evaluation

The game can be played interactively against an AI opponent.

---

## Minimax AI

An AI player is implemented using minimax search over generated game states.

Features include:

- Automatic generation of possible future game states
- Recursive exploration of move trees
- Board evaluation
- Selection of optimal moves based on search depth

The game state exploration is represented using a generic multi-way tree structure.

---

## Rose Tree Data Structure

A reusable Rose tree implementation is included to represent hierarchical data.

Implemented operations:

- Tree size calculation
- Tree height calculation
- Leaf extraction
- Retrieving elements at a specific depth
- Generic tree transformations using Functor
- Folding operations over tree structures

The tree structure is used for representing possible game states during AI search.

---

## Functional State Management

The project implements custom state-handling abstractions inspired by Haskell's State monad.

Implemented features:

- Stateful computation composition
- Functor, Applicative, and Monad instances
- Sequential execution of game actions
- Automatic state propagation between operations

---

## Game History Tracking

An extended state abstraction is implemented to record the sequence of game states during execution.

This enables:

- Tracking complete move history
- Replaying games
- Analyzing state transitions

---

## Concepts Demonstrated

- Functional programming
- Recursive data structures
- Algebraic data types
- Higher-order functions
- Functor, Applicative, and Monad abstractions
- Game tree search
- Minimax algorithm
- State modeling

---

## Technologies

- Haskell
- Functional Programming
- Recursive Algorithms
- Artificial Intelligence Search Algorithms

---

## Possible Improvements

- Alpha-beta pruning for minimax optimization
- More advanced board evaluation heuristics
- Graphical user interface
- Stronger AI strategies
