# Clojure Tap Inspector

A `tap>` receiver for Emacs that collects tapped values and displays them in CIDER's inspector for drill-down into data structures.

## Prerequisites

- CIDER with an active nREPL connection to your Clojure project.

## Setup

Require the package in your Emacs config:

```elisp
(require 'clojure-tap-inspector)
```

## Usage

### 1. Start the tap inspector

With a running CIDER REPL, run:

```
M-x clj-tap-start
```

This registers a `tap>` handler in the JVM that collects tapped values with their timestamps and types.

### 2. Tap values from your Clojure code

Use `tap>` anywhere in your Clojure code:

```clojure
(tap> {:name "example" :data [1 2 3]})
(tap> (my-query db))
```

### 3. View tapped values

```
M-x clj-tap-show
```

This opens the entire tap collection in CIDER's inspector. Each tap is a map with `:value`, `:timestamp`, and `:type` keys. You can drill down into any entry using CIDER's standard inspector navigation.

### 4. Clear or stop

- `M-x clj-tap-clear` — clear all collected taps
- `M-x clj-tap-stop` — remove the tap handler and clean up

## Commands

| Command          | Action                                              |
|------------------|-----------------------------------------------------|
| `clj-tap-start`  | Start the tap handler                               |
| `clj-tap-show`   | Open all taps in CIDER's inspector                  |
| `clj-tap-clear`  | Clear all collected taps                             |
| `clj-tap-stop`   | Stop the tap handler and clean up                   |

## How It Works

The inspector injects a small namespace (`jjg.tap-inspector`) into your REPL that:

1. Creates an atom to store tapped values
2. Registers a function via `add-tap` that captures each value along with its type and timestamp
3. Uses CIDER's inspector (`cider-inspect-expr`) to display the full tap collection with drill-down support
