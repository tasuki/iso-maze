# Set up

- Read the `Makefile` for how to run this.

# Coding

- Try to keep the logic in Elm, use `main.js` only for rendering.
- Try to avoid duplicating logic, if you can create a useful abstraction, do it!
- You're good at naming things, pay attention to naming.
- Do NOT conform to the Elm code style, instead:
    - Primarily, DO NOT mess with the formatting I already have: it's intentional.
    - Try to match my formatting for any new code you write. Specifically:
        - No line breaks after `=`, unless it's a long line
        - Prefer minimizing vertical white space (but do not remove it completely)
        - Try to put if/else blocks on two lines: one for `if`, one for `else` (unless long)
        - Similar for pattern matching: if the right side is short, keep it on one line and no empty lines
    - Again, if there's vertical white space somewhere, there's a reason for it: do not remove it.
    - If something is formatted a certain way, there's a reason for it: keep the formatting!

# Git & versioning

- Keep branch names short and simple: no `feat/` or other prefixes, start branch names with the verb describing the new behavior or subject, no generic `add` or `implement`.
- Do not include the various dev files in a commit. Verify the commit is minimal.
- Git commits: DO NOT prefix with `feat` or `fix`: Linus doesn't either! Capitalize the first letter.
