# mode-visual-bell.el

A small Emacs module that provides unmissable visual feedback when a minor mode
is active. It combines two signals — a colored header-line banner across the top
of the buffer and a change to the cursor's color and shape — so you always know
at a glance that you're in a special editing state.

## Motivation

The standard way Emacs communicates minor-mode state is a short string tucked
into the mode line. This is easy to overlook, especially with many active modes.
For modes where awareness matters (overwrite-mode, a custom review mode, etc.),
a more aggressive visual cue is useful.

## How it works

### Registration

You call `mode-visual-bell-register` once per minor mode, at init time:

```elisp
(mode-visual-bell-register 'overwrite-mode
  :label "OVERWRITE"
  :bg "#4a0020"
  :fg "#ff6699"
  :cursor-color "#ff6699"
  :cursor-type 'box)
```

This does one thing: it adds a hook function to that mode's hook variable (e.g.
`overwrite-mode-hook`). The hook fires every time the mode is toggled on or off.

### Activation (mode toggled ON)

When the registered minor mode activates, the hook function:

1. **Saves current state** — The buffer's existing `header-line-format`, the
   frame's current cursor color, and the buffer's `cursor-type` are all stashed
   in buffer-local variables so they can be restored later.

2. **Sets the header line** — `header-line-format` is set to a propertized
   string with the configured label, background color, foreground color, and
   bold weight. This renders as a colored bar spanning the full width of the
   window, directly above the buffer content.

3. **Changes the cursor** — The `cursor` face's background attribute is set to
   the configured color on the current frame, and `cursor-type` is set
   buffer-locally to the configured shape (e.g. `box`, `hollow`, `bar`).

### Deactivation (mode toggled OFF)

When the minor mode deactivates, the same hook function runs again but takes the
else branch:

1. **Restores header line** — `header-line-format` is reset to whatever it was
   before activation (usually `nil`).

2. **Restores cursor** — The `cursor` face background and `cursor-type` are
   reverted to their saved values.

3. **Clears tracking state** — The `mode-visual-bell--active-mode` variable is
   set to nil.

### State storage

All saved state is kept in buffer-local variables (`defvar-local`), so
activating a mode in one buffer does not interfere with another buffer's saved
state.

| Variable | Purpose |
|---|---|
| `mode-visual-bell--saved-header-line` | Prior `header-line-format` value |
| `mode-visual-bell--saved-cursor-color` | Prior cursor face background color |
| `mode-visual-bell--saved-cursor-type` | Prior `cursor-type` value |
| `mode-visual-bell--active-mode` | Symbol of the mode currently driving the indicator |

## Configuration options

All options are passed as keyword arguments to `mode-visual-bell-register`:

| Key | Default | Description |
|---|---|---|
| `:label` | Mode name, uppercased, with `-mode` suffix stripped | Text displayed in the header-line banner |
| `:bg` | `"#4a0020"` | Header-line background color |
| `:fg` | `"#ff6699"` | Header-line foreground (text) color |
| `:cursor-color` | Same as `:fg` | Cursor color while mode is active |
| `:cursor-type` | `box` | Cursor shape while mode is active (`box`, `hollow`, `bar`, `hbar`) |

## Loading

The file is loaded from `init.el` alongside other local modules:

```elisp
(load (locate-user-emacs-file "mode-visual-bell.el") nil :nomessage)
```

## Known limitations

- **Cursor color is frame-global.** Emacs does not support per-buffer cursor
  colors natively. If you have two windows visible and only one has the mode
  active, both windows will show the changed cursor color. Solving this would
  require a `buffer-list-update-hook` that swaps the cursor face on every window
  focus change.

- **Single mode per buffer.** If two registered modes are active simultaneously
  in the same buffer, the second activation overwrites the first's saved state.
  The header line shows whichever activated last, and deactivating either one
  restores to the pre-first-activation state (which may not be correct). For
  most practical uses this is fine — the feature targets modes where only one
  special state is active at a time.
