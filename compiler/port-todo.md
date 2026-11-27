# Self-hosted compiler to-do

Known problems in the self-hosted modules here that are not fixed yet.
Each fix goes into koka-community/compiler as well; remove the entry when it lands in both.

## `pretty/pprint`: rendering is exponential in the number of groups

`render-pretty` builds both layouts of every `Union` in full and only then picks one: `nicest` receives the two rendered `simpleDoc`s, and the rest of the document is rendered inside each of them.
`PPrint.hs` gets away with the same shape because Haskell is lazy, so `fits` only forces the first line of the flat alternative.
In Koka both alternatives are built eagerly, so each `group` doubles the work.

Measured by timing `show` (which renders at the default width) on `n` sequential groups, `list(1,n).map(fn(i) (text("x" ++ i.show) |-| text("y")).group).hsep`, one run each on macOS arm64:

| groups | time |
| --- | --- |
| 8 | 0.000186 s |
| 12 | 0.0024 s |
| 16 | 0.030 s |
| 18 | 0.080 s |
| 20 | 0.286 s |

Fix: decide each `Union` before rendering it. Check whether the flat alternative fits on the current line by walking only that alternative and the pending document stream up to the next line break (or until the width runs out), then render the chosen alternative alone. Add a test that renders a document with many groups and checks both the output and that it finishes quickly.

`render-pretty-b`, used by `put-doc` and `write-doc`, has its own `nicest` that always takes the first alternative and so ignores the width. It should share the fixed `render-pretty` logic.
