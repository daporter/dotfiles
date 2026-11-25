# readest

`settings.json` is stowed as a symlink; Readest writes through it, so
the repo copy updates live.

Readest saves it as one minified line. A git clean filter pretty-prints
it on staging (see the repo's `.gitattributes`), but filter config isn't
versioned, so after a fresh clone run:

```sh
git config filter.json-pretty.clean 'jq --indent 2 .'
```

Without it, `git add` stores the minified line and diffs become
unreadable.

Expect frequent no-op churn: `lastOpenBooks` changes whenever a book
is opened.
