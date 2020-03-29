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

## Features

|              Command             |    Binding    |                                       Description                                |
|:---------------------------------|:--------------|:---------------------------------------------------------------------------------|
| `neuron-new-zettel`              | `C-c C-z`     | Create a new zettel and open it for edition                                      |
| `neuron-rib-open-current-zettel` | `C-c C-r`     | Open the current zettel in the web application (assuming an instance is running) |
| `neuron-insert-zettel-link`      | `C-c C-l`     | Search a zettel by content and insert the link at point                          |
| `neuron-insert-new-zettel`       | `C-c C-L`     | Combine `neuron-new-zettel` and `neuron-insert-zettel-link`                      |
| `neuron-follow-thing-at-point`   | `C-c C-o`     | Override `markdown-follow-thing-at-point` to handle zettel URI protocol          |





