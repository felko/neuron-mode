[![MELPA](https://melpa.org/packages/neuron-mode-badge.svg)](https://melpa.org/#/neuron-mode)

# neuron-mode

`neuron-mode` is an Emacs major mode derived from [`markdown-mode`](https://jblevins.org/projects/markdown-mode/)
to edit notes using the [`neuron`](https://neuron.zettel.page/) Zettelkasten manager.

## Installation

1. Install [neuron](https://neuron.zettel.page/2011501.html) and
   make sure that the `neuron` command is in your path.

2. Install neuron-mode, either via MELPA (recommended) or
   manually by cloning this repository.

In addition, doom-emacs users can use [this configuration](#appendix-doom-emacs-configuration)
to include neuron-mode into their private config.

## Features

| Command                          | Binding     | Description                                                             |
| :------------------------------- | :---------- | :---------------------------------------------------------------------- |
| `neuron-new-zettel`              | `C-c C-z`   | Create a new zettel and open it for edition                             |
| `neuron-edit-zettel`             | `C-c C-e`   | Select an existing zettel and open it for edition                       |
| `neuron-insert-tag`              | `C-c C-t`   | Select and insert a tag                                                 |
| `neuron-query-tag`               | `C-c C-S-t` | Select a tag and then a zettel with that tag                            |
| `neuron-rib-open-current-zettel` | `C-c C-r`   | Open the current zettel's generated HTML file in the browser            |
| `neuron-insert-zettel-link`      | `C-c C-l`   | Search a zettel by content and insert the link at point                 |
| `neuron-insert-new-zettel`       | `C-c C-S-L` | Combine `neuron-new-zettel` and `neuron-insert-zettel-link`             |
| `neuron-follow-thing-at-point`   | `C-c C-o`   | Override `markdown-follow-thing-at-point` to handle zettel URI protocol |
| `neuron-select-zettelkasten`     | N/A         | Set the current active Zettelkasten in which all of these command occur |
| `neuron-rib-generate`            | N/A         | Perform a one-off generation of the site                                |
| `neuron-rib-serve`               | N/A         | Start the wep application on `localhost:8080`                           |
| `neuron-rib-open-z-index`        | N/A         | Open the web application at `localhost:8080/z-index.html`               |
| `neuron-rib-open-zettel`         | N/A         | Select and open a zettel note in the web appliation                     |
| `neuron-rib-kill`                | N/A         | Kill the web application process                                        |

## Customization

- `neuron-default-zettelkasten-directory` (default: `~/zettelkasten`)
  Defines the default active zettelkasten when `neuron-select-zettelkasten`
  wasn't called yet.
- `neuron-generate-on-save` (default: `nil`)
  Generates the site when saving a note. Opens a compilation buffer
  (`neuron-rib-generate`)
- `neuron-use-short-links` (default: `t`)
  Controls whether inserted zettel links are in the form `[ID](z:/)` or
  `<ID>`.

## Appendix: doom-emacs configuration

doom-emacs users can use this configuration to work with `neuron-mode`:

<https://gist.github.com/felko/cdb3fc19b3a60db27eb3c5bd319fc479>

(include those files in `.doom.d/modules/tools/neuron`)

This creates a private module that can then be enabled by
inserting `neuron` under the `:tools` section of your `doom!`
block (inside your `init.el`).
