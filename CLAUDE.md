# RCodingSupport — project notes

Companion R package for the **R Coding Support Sessions** module. Ships the
synthetic Republic of Amani malaria datasets that the weekly problem sets use.

Conventions for the wider folder live in the top-level `Work/CLAUDE.md`. This
file covers only what is specific to releasing this package.

## The year pin is a published interface

The teaching materials install the package by tag, not from `main`:

```r
remotes::install_github("mrc-ide/RCodingSupport@teaching-2026")
```

**`teaching-2026` is the pin for the 2026/27 teaching year.** Thirteen files in
the module materials reference it by name, which is why the tag is moved rather
than the materials edited. Treat the tag as part of the package's public
surface: anything that changes what it resolves to changes what every student
gets.

## Before 2026-10-06 — the tag may be force-moved

Nobody has installed from the tag yet, so re-pointing it costs nothing:

```bash
git tag -f -a teaching-2026 -m "Package state for the 2026/27 teaching year" origin/main
git push --force origin teaching-2026
```

Push `main` first, since that command reads `origin/main`.

## From 2026-10-06 — the tag is frozen

The first teaching session is **2026-10-06**. From that date, do not move
`teaching-2026`, and do not force-push it.

Students install once and will not reinstall unless told to. A moving tag would
leave different students holding different data under the same pin, with nothing
to signal that anything differs — two people would get different answers to the
same exercise and have no way to see why.

**If a package fix becomes unavoidable during term:**

1. Bump `Version` in `DESCRIPTION`.
2. Cut a **new** tag `teaching-2026.1`. Do not move `teaching-2026`.
3. Update the pin only in the weeks **not yet released**.
4. Tell students to reinstall.

## Version tracks the data, not just the code

**Bump `Version` in `DESCRIPTION` whenever the shipped data change**, not only
when code changes. `packageVersion("RCodingSupport")` is what a student reads
back when answers disagree in a session, so it has to distinguish two states
that hold different data. It failed to do this once already: 0.2.0 was published
twice with different `case_linelist` row counts (119,757, then 114,110).

Current state: **0.3.0**, `teaching-2026` → `b2b6466`, `case_linelist` 114,110 rows.

## Regenerating data is a release event

`data-raw/` rebuilds every object in `data/` from seed, and `03_build_package_data.R`
writes straight to `data/`. Running the pipeline is therefore never a routine
step — it changes what students receive. Before running it, check the freeze
date above; after running it, bump the version.

The pipeline is seeded, so a rebuild with no parameter change reproduces the same
objects. The `.rda` bytes may still differ (bzip2 metadata) while the contents
are identical — compare loaded objects, not file hashes, before concluding
anything changed.
