[![MELPA](https://melpa.org/packages/neuron-mode-badge.svg)](https://melpa.org/#/neuron-mode)

# neuron-mode

neuron-mode is an Emacs major mode derived from
[markdown-mode](https://jblevins.org/projects/markdown-mode/) to edit notes
using the [neuron](https://neuron.zettel.page/) Zettelkasten manager.

<p align="center"><a href="https://asciinema.org/a/329911"><img src="https://asciinema.org/a/329911.svg" alt="drawing" width="600"/></a></br>neuron-mode demo in <a href="https://github.com/hlissner/doom-emacs">doom emacs</a></p>

## Installation

1. Install [neuron](https://neuron.zettel.page/2011501.html) and make sure that
   the `neuron` command is in your path.

2. Install neuron-mode, either via MELPA (recommended) or manually by cloning
   this repository.

In addition, doom-emacs users can use
[this configuration](#appendix-doom-emacs-configuration) to include neuron-mode
into their private config.

## Features

All commands are executed in the active zettelkasten which is either detected by
traversing the directory hierarchy upwards until a `neuron.dhall` file is met
(see [neuron configuration](https://neuron.zettel.page/2011701.html)). Although
`neuron.dhall` files are not mandatory to use neuron itself, this is what
enables neuron-mode automatically when opening a markdown file. Otherwise,
neuron-mode will take the default zettelkasten, defined by the
`neuron-default-zettelkasten-directory`. neuron-mode will then cache the zettels
and regenerate it when needed (typically when creating a new zettel). Sometimes,
when the zettelkasten is modified externally to neuron-mode, you might need to
invalidate the cache and rebuild it manually, which is done with the
`neuron-refresh` command. This will also reload the titles displayed next to
zettel links (see next paragraph).

#### Reading

neuron-mode allows you to browse your zettelkasten directly from Emacs. Most
importantly, neuron links are shown together with their titles so that you don't
need to read the zettel from the associated HTML file anymore.

#### Navigating

neuron links can also be followed using `neuron-follow-thing-at-point`. For
queries, it will prompt you to select a zettel that match the query under the
point.

Navigating "upwards" is also possible, using the `neuron-edit-uplink` function.

#### Searching

The `neuron-edit-zettel` will prompt you with the list of zettels, where you can
search by title, by tag or by ID using ivy.

projectile can also be useful as a complement to neuron-mode since it allows you
to search the zettelkasten by content.

#### Editing

You can create new zettels from Emacs, neuron-mode will take care of creating
the file with a generated hash in the current active zettelkasten.

Zettel links can be inserted using `neuron-insert-zettel-link` which will prompt
you to select an existing zettel in the active zettelkasten. Zettels can also be
linked and created on the fly by using `neuron-insert-new-zettel`.

To avoid typos in tags which might mess up the organization of your
zettelkasten, use `neuron-add-tag`/`neuron-add-tags` which will allow you to
select from already existing tags.

#### Using rib

Rib commands can be executed from Emacs. Running `neuron-rib-generate` will
compile your zettelkasten to HTML. This can also be done automatically using
`neuron-rib-watch` which will generate every time you save a zettel.

The resulting HTML files can then be accessed either from neuron-mode with
`neuron-open-current-zettel` which will open the HTML that is associated with
the zettel being edited, or globally with `neuron-open-zettel`.

Alternatively, you can use the rib server to read your zettelkasten from your
browser. The server can be started using `neuron-rib-serve` and stopped using
`neuron-rib-kill`. Similar functions are provided to open the zettels in the web
app rather than local files (respectively `neuron-rib-open-current-zettel` and
`neuron-rib-open-zettel`).

### Function list

| Command                                  | Default binding | Description                                                               |
| :--------------------------------------- | :-------------- | :------------------------------------------------------------------------ |
| `neuron-new-zettel`                      | `C-c C-z`       | Create a new zettel and open it for edition                               |
| `neuron-edit-zettel`                     | `C-c C-e`       | Select an existing zettel and open it for editing                         |
| `neuron-edit-uplink`                     | N/A             | Select and edit a zettel that links to the current one                    |
| `neuron-add-tag`                         | `C-c C-t`       | Select and insert a tag in the current zettel's YAML metadata block       |
| `neuron-add-tags`                        | `C-c C-S-t`     | Same as `neuron-add-tag` but adds multiple tags simultaneously            |
| `neuron-query-tag`                       | N/A             | Select a tag and then a zettel with that tag                              |
| `neuron-replace-tag`                     | N/A             | Replace a tag by another across the entire zettelkasten                   |
| `neuron-open-current-zettel`             | `C-c C-r`       | Open the current zettel's generated HTML file in the browser              |
| `neuron-insert-zettel-link`              | `C-c C-l`       | Search a zettel by content and insert the link at point                   |
| `neuron-insert-new-zettel`               | `C-c C-S-l`     | Combine `neuron-new-zettel` and `neuron-insert-zettel-link`               |
| `neuron-create-and-insert-zettel-link`   | N/A             | Like `neuron-insert-zettel-link` but can also create new zettels          |
| `neuron-create-zettel-from-selection`    | N/A             | Transform the selected text into a new zettel                             |
| `neuron-insert-static-link`              | `C-c C-s`       | Insert a link to a selected file in the static directory                  |
| `neuron-toggle-connection-type`          | N/A             | Toggle the connection type under point, between cf and folgezettel        |
| `neuron-open-daily-notes`                | N/A             | Open today's notes if it exists, or create it otherwise                   |
| `neuron-refresh`                         | `C-c C-S-r`     | Regenerate the zettel cache and title overlays in all neuron-mode buffers |
| `neuron-toggle-id-visibility`            | N/A             | Toggle between showing the titles next to the IDs or over them            |
| `neuron-follow-thing-at-point`           | `C-c C-o`       | Override `markdown-follow-thing-at-point` to handle zettel URI protocol   |
| `neuron-edit-zettelkasten-configuration` | N/A             | Open the neuron.dhall file at the root of the current zettelkasten        |
| `neuron-rib-generate`                    | N/A             | Perform a one-off generation of the site                                  |
| `neuron-rib-serve`                       | N/A             | Start the wep application on `localhost:8080`                             |
| `neuron-rib-open-z-index`                | N/A             | Open the web application at `localhost:8080/z-index.html`                 |
| `neuron-rib-open-zettel`                 | N/A             | Select and open a zettel note in the web appliation                       |
| `neuron-rib-kill`                        | N/A             | Kill the web application process                                          |

## Configuration

### Customize settings

- `neuron-default-zettelkasten-directory` (default: `"~/zettelkasten`) \
  Defines the fallback zettelkasten when the current directory is not located in
  a zettelkasten.
- `neuron-executable` (default: `neuron`) \
  Path or wrapper around the neuron executable. \
  Example: this can be useful when using Emacs from Windows, while having neuron
  installed on WSL, in which case you can set `neuron-executable` to
  `wsl neuron`.
- `neuron-generate-on-save` (default: `nil`) \
  Generates the site when saving a note. Opens a compilation buffer (`neuron-rib-generate`)
- `neuron-id-format` (default: `'hash`) \
  Controls the default ID format used when creating new notes. \
  It can be set to:
  - `'hash`: generates an unique 32 bit UUID (default behavior of neuron)
  - `'date`: generates an ID based on the date of creation
  - `'prompt`: asks for the ID when creating a new zettel
- `neuron-default-tags` (default: `nil`) \
  A list of tags to add to zettels when they are created. This does not affect daily
  notes. \
  Example: `stub` (to mimic
  [Wikipedia's stubs](https://en.wikipedia.org/wiki/Wikipedia:Stub))
- `neuron-tag-specific-title-faces` (default: `nil`) \
  An alist that associates tags to faces, which will appear in the title overlay
  when a link points to a zettel that has the corresponding tag. Example: `'(("stub" hl-todo))`
- `neuron-daily-note-id-format` (default: `"%Y-%m-%d"`) \
  Format that controls the filenames of newly created daily notes
- `neuron-daily-note-title-format` (default: `"%x"`) \
  The format of titles for new daily notes
- `neuron-daily-note-tags` (default: `(list "journal/daily")`) \
  The default tag of daily notes.
- `neuron-rib-server-host` (default: `localhost`) \
- `neuron-rib-server-port` (default: `8080`) \
  The host and port on which the rib server will run

### Other variables

- `neuron-make-title` (default: capitalizes the first word) \
  A function that is called by `neuron-create-zettel-from-selection` to transform
  the text into the actual title of the new zettel.
- `neuron-show-ids` (default: `nil`) \
  Whether to show the IDs next to zettel titles, rather than having the titles completely
  hide them. Can be toggled using `neuron-toggle-id-visibility`.

## Appendix: doom-emacs configuration

doom-emacs users can use this configuration to work with `neuron-mode`:

<https://gist.github.com/felko/cdb3fc19b3a60db27eb3c5bd319fc479>

(include those files in `.doom.d/modules/tools/neuron`)

This creates a private module that can then be enabled by inserting `neuron`
under the `:tools` section of your `doom!` block (inside your `init.el`).
