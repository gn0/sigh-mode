# Sigh Mode

Sigh Mode is an Emacs minor mode for **s**entence h**igh**lighting.
With it, you can navigate the buffer sentence-by-sentence rather than character-by-character, and dynamically highlight the current sentence.

![Sigh Mode activated in Emacs 30.1](https://raw.githubusercontent.com/gn0/sigh-mode/main/screenshot.png)

## Installation

Emacs 29.1 and later versions ship with [`use-package.el`](https://www.gnu.org/software/emacs/manual/html_node/use-package/index.html), and this is the recommended method of installing Sigh Mode.
If your Emacs configuration is in `~/.emacs.d`, then clone or download this GitHub repository into the `~/.emacs.d/sigh-mode` directory, and add the following to your Emacs configuration file:

```elisp
(use-package sigh-mode
  :load-path "~/.emacs.d/sigh-mode"
  :commands (sigh-mode))
```

Alternatively, starting with Emacs 30.1, you can also install Sigh Mode directly from this GitHub repository:

```elisp
(use-package sigh-mode
  :vc (:url "https://github.com/gn0/sigh-mode.git"
       :branch "main")
  :commands (sigh-mode))
```

## Usage

Activate Sigh Mode by entering `M-x sigh-mode RET`.
The following keybindings are available:

| binding     | action                    |
|-------------|---------------------------|
| `h`/`left`  | move to previous sentence |
| `l`/`right` | move to next sentence     |
| `k`/`up`    | move to previous line     |
| `j`/`down`  | move to next line         |

You can deactivate Sigh Mode by entering `M-x sigh-mode RET` again.

## Known issues

If Sigh Mode is invoked in Evil, then `h`/`left` and `l`/`right` move the cursor character-by-character instead of sentence-by-sentence.
With native compilation turned off, this misbehavior is fixed by switching to Emacs state and back, by pressing `C-z` twice.
With native compilation turned on, the misbehavior is resolved after Emacs is restarted and Sigh Mode is loaded a second time.

