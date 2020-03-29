# zettel-mode

`zettel-mode` is an Emacs major mode derived from [`markdown-mode`](https://jblevins.org/projects/markdown-mode/) 
to edit notes using the [`neuron`](https://neuron.srid.ca/) Zettelkasten manager.

## Installation

Please note that I'm new to emacs and even newer to writing emacs packages,
please open an issue if you notice something wrong. In particular I'm not
entirely sure how to manage dependencies but if you do know how here they are:
- [`ivy`/`counsel`](https://github.com/abo-abo/swiper)
- [`markdown-mode`](https://github.com/jrblevin/markdown-mode)
- [`ripgrep`](https://github.com/BurntSushi/ripgrep)

# Setup

`zettel-mode` stores your notes in a directory defined by the `ZETTELPATH`
environment variable that you should set somewhere to e.g. `~/.zettel`

## Features

|              Command             |    Binding    |                                       Description                                |
|:---------------------------------|:--------------|:---------------------------------------------------------------------------------|
| `neuron-new-zettel`              | `C-c C-z`     | Create a new zettel and open it for edition                                      |
| `neuron-edit-zettel`             | `C-c C-e`     | Select an existing zettel and open it for edition                                |
| `neuron-rib-open-current-zettel` | `C-c C-r`     | Open the current zettel in the web application (assuming an instance is running) |
| `neuron-insert-zettel-link`      | `C-c C-l`     | Search a zettel by content and insert the link at point                          |
| `neuron-insert-new-zettel`       | `C-c C-S-L`   | Combine `neuron-new-zettel` and `neuron-insert-zettel-link`                      |
| `neuron-follow-thing-at-point`   | `C-c C-o`     | Override `markdown-follow-thing-at-point` to handle zettel URI protocol          |
| `neuron-select-zettelkasten`     | N/A           | Set the current active Zettelkasten in which all of these command occur          |
| `neuron-rib-serve`               | N/A           | Start the wep application on `localhost:8080`                                    |
| `neuron-rib-open-z-index`        | N/A           | Open the web application at `localhost:8080/z-index.html`                        |
| `neuron-rib-open-zettel`         | N/A           | Select and open a zettel note in the web appliation                              |
| `neuron-rib-kill`                | N/A           | Kill the web application process                                                 |

## doom-emacs configuration

doom-emacs+evil users can use this configuration to work with `zettel-mode`
using evil bindings:

```elisp
(map! :after zettel-neuron
      :leader
      (:prefix ("z" . "zettel")
        "z"  #'neuron-new-zettel
        "e"  #'neuron-edit-zettel
        "s"  #'neuron-select-zettelkasten
        (:prefix ("r" . "rib")
          "s" #'neuron-rib-serve
          "o" #'neuron-rib-open-zettel
          "z" #'neuron-rib-open-z-index
          "k" #'neuron-rib-kill
          )
        )
      )

(map! :after zettel-mode
      :map zettel-mode-map
      :localleader
      "o"      #'neuron-follow-thing-at-point
      (:prefix ("z" . "zettel")
        "z"    #'neuron-new-zettel
        "e"    #'neuron-edit-zettel
        "o"    #'neuron-rib-open-current-zettel
        "l"    #'neuron-insert-zettel-link
        "L"    #'neuron-insert-new-zettel
        )
      )

```
