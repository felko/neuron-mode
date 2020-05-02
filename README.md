[![MELPA](https://melpa.org/packages/neuron-mode-badge.svg)](https://melpa.org/#/neuron-mode)

# neuron-mode

neuron-mode is an Emacs major mode derived from [markdown-mode](https://jblevins.org/projects/markdown-mode/)
to edit notes using the [neuron](https://neuron.zettel.page/) Zettelkasten
manager.

## Installation

1. Install [neuron](https://neuron.zettel.page/2011501.html) and
   make sure that the `neuron` command is in your path.

2. Install neuron-mode, either via MELPA (recommended) or
   manually by cloning this repository.

In addition, doom-emacs users can use [this configuration](#appendix-doom-emacs-configuration)
to include neuron-mode into their private config.

## Features

All commands are executed in an active zettelkasten which can be selected with
`neuron-select-zettelkasten`. neuron-mode will then cache the zettels and
regenerate it when needed (typically when creating a new zettel). Sometimes,
when the zettelkasten is modified externally to neuron-mode, you might need to
invalidate the cache and rebuild it manually, which is done with the
`neuron-refresh-buffer` command. This will also reload the titles displayed
next to zettel links (see next paragraph).

#### Reading

neuron-mode allows you to browse your zettelkasten directly from Emacs.
Most importantly, neuron links are shown together with their titles so that
you don't need to read the zettel from the associated HTML file anymore.

#### Navigating

neuron links can also be followed using `neuron-follow-thing-at-point`.
For queries, it will prompt you to select a zettel that match the query
under the point.

#### Searching

The `neuron-edit-zettel` will prompt you with the list of zettels, where
you can search by title, by tag or by ID using ivy.

projectile can also be useful asa complement to neuron-mode since it allows
you to search the zettelkasten by content.

#### Editing

You can create new zettels from Emacs, neuron-mode will take care of creating
the file with a generated hash in the current active zettelkasten.

Zettel links can be inserted using `neuron-insert-zettel-link` which will
prompt you to select an existing zettel in the active zettelkasten. Zettels can
also be linked and created on the fly by using `neuron-insert-new-zettel`.

To avoid typos in tags which might mess up the organization of your
zettelkasten, use `neuron-insert-tag` which will allow you to select from
already existing tags.

#### Using rib

Rib commands can be executed from Emacs. Running `neuron-rib-generate` will
compile your zettelkasten to HTML. This can also be done automatically using
`neuron-rib-watch` which will generate every time you save a zettel.

The resulting HTML files can then be accessed either from neuron-mode with
`neuron-open-current-zettel` which will open the HTML that is associated with
the zettel being edited, or globally with `neuron-open-zettel`.

Alternatively, you can use the rib server to read your zettelkasten from your
browser. The server can be started using `neuron-rib-serve` and stopped using
`neuron-rib-kill`. Similar functions are provided to open the zettels in
the web app rather than local files (respectively
`neuron-rib-open-current-zettel` and `neuron-rib-open-zettel`).

### Function list

| Command                        | Binding     | Description                                                             |
| :----------------------------- | :---------- | :---------------------------------------------------------------------- |
| `neuron-new-zettel`            | `C-c C-z`   | Create a new zettel and open it for edition                             |
| `neuron-edit-zettel`           | `C-c C-e`   | Select an existing zettel and open it for edition                       |
| `neuron-insert-tag`            | `C-c C-t`   | Select and insert a tag                                                 |
| `neuron-query-tag`             | `C-c C-S-t` | Select a tag and then a zettel with that tag                            |
| `neuron-open-current-zettel`   | `C-c C-r`   | Open the current zettel's generated HTML file in the browser            |
| `neuron-insert-zettel-link`    | `C-c C-l`   | Search a zettel by content and insert the link at point                 |
| `neuron-insert-new-zettel`     | `C-c C-S-l` | Combine `neuron-new-zettel` and `neuron-insert-zettel-link`             |
| `neuron-refresh-buffer`        | `C-c C-S-r` | Regenerate the zettel cache and title overlays in the current buffer    |
| `neuron-follow-thing-at-point` | `C-c C-o`   | Override `markdown-follow-thing-at-point` to handle zettel URI protocol |
| `neuron-select-zettelkasten`   | N/A         | Set the current active Zettelkasten in which all of these command occur |
| `neuron-rib-generate`          | N/A         | Perform a one-off generation of the site                                |
| `neuron-rib-serve`             | N/A         | Start the wep application on `localhost:8080`                           |
| `neuron-rib-open-z-index`      | N/A         | Open the web application at `localhost:8080/z-index.html`               |
| `neuron-rib-open-zettel`       | N/A         | Select and open a zettel note in the web appliation                     |
| `neuron-rib-kill`              | N/A         | Kill the web application process                                        |

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
