# Research: Issue #8 -- Handle `<` and `>` as Brackets

## The Issue

GitHub issue #8 (opened 2015-06-26, still open): In C/C++ includes, C++ templates,
Java generics, etc., `<` and `>` are used as brackets rather than comparison operators.
Electric-operator currently either:
- Spaces them as operators (`x < y`) when they should be unspaced brackets (`vector<int>`)
- Disables them entirely (C++ mode sets `<` and `>` to `nil`)

Neither is correct in all contexts.

## How Electric-Operator Works

Electric-operator is a single-file Emacs minor mode (`electric-operator.el`, ~1600 lines).
It automatically inserts spaces around operators as you type.

### Architecture

1. **Hook-based triggering**: `electric-operator-mode` adds
   `electric-operator-post-self-insert-function` to `post-self-insert-hook`. Every
   character insertion triggers the function.

2. **Rule storage**: Rules are stored in a trie (per major-mode) in the hash table
   `electric-operator--mode-rules-table`. The trie is keyed by reversed character
   sequences of the operator (stripped of whitespace). This enables longest-match
   lookup from the buffer text before point.

3. **Rule matching**: `electric-operator-longest-matching-rule` grabs up to 20
   characters before point and walks the trie to find the longest matching operator.
   The trie's `longest-match` on-fail mode means if there's no exact match it falls
   back to the longest prefix that has a value.

4. **Rule actions**: Each rule maps an operator string to an action:
   - **String**: The replacement text (e.g., `" + "` for `+`)
   - **Function**: Called at the operator's position, returns a string or nil (nil = do nothing)
   - **`electric-operator-comment-prefix` symbol**: Special action that fires even inside
     comments, producing `" operator "` spacing
   - **`nil`**: Disables the operator entirely (no spacing applied)

5. **Replacement logic** (`electric-operator-post-self-insert-function`):
   - Find the longest matching rule
   - If inside a comment/string, only allow `electric-operator-comment-prefix` actions
   - Use `looking-back` with the rule's regex to find the operator boundaries
   - Delete the matched region and insert the spaced replacement
   - Preserve leading indentation if operator is first on a line

6. **Mode inheritance**: Mode-specific rules inherit from parent modes. E.g., `c-mode`
   copies all `prog-mode` rules then overrides specific operators. `c++-mode` copies
   `c-mode` rules and adds more.

### Key Context-Detection Helpers

- `electric-operator-in-docs?` -- checks `syntax-ppss` for string/comment context
- `electric-operator-enclosing-paren` -- returns the char of the enclosing paren (or nil)
- `electric-operator-probably-unary-operator?` -- heuristic based on preceding chars
- `electric-operator-c-after-type?` -- checks if preceding text is a C type name
- `electric-operator-c-is-function-or-class-definition?` -- uses cc-mode's `c-guess-basic-syntax`
- `electric-operator-c-mode-include-line?` -- checks for `#include` context
- `electric-operator-looking-back-locally` -- `looking-back` limited to 2 lines for performance

### Current Handling of `<` and `>` by Mode

| Mode | `<` | `>` | Notes |
|------|-----|-----|-------|
| prog-mode | `" < "` | `" > "` | Always spaced as comparison |
| c-mode | function | function | Special-cased for `#include` and function/class defs |
| c++-mode | `nil` | `nil` | **Disabled entirely** -- too hard to disambiguate |
| java-mode | `nil` | `nil` | Disabled (generics) |
| rust-mode | `nil` | `nil` | Disabled (generics) |
| typescript-mode | `nil` | `nil` | Disabled (generics) |
| perl-mode | `nil` | `nil` | Disabled (`<>`, `<STDIN>`) |

The c-mode functions (`electric-operator-c-mode-<` and `electric-operator-c-mode->`) use
`electric-operator-c-mode-include-line?` and `electric-operator-c-is-function-or-class-definition?`
to decide. But even c-mode gets it wrong sometimes.

### Known Test Failures (`:expected-result :failed`)

From `test/c++-mode-test.el`:
- `c++-greater-than-still-works` -- `bool x = 0 > 1` inside `main()`
- `c++-less-than-still-works` -- `bool x = 0 < 1` inside `main()`
- `c++->>-still-works` -- `std::cin >> x;` inside `main()`
- `c++-nested-template-type-definition` -- `MyType<Template<double>> x` inside `main()`
- `c++-move-constructor-inside-class` -- `A(A&&a)` inside a struct body

These are all disabled because `<` and `>` are set to `nil` in c++-mode. The
templates-in-function-defs tests *do* pass because `c-guess-basic-syntax` correctly
identifies function/class definition contexts at the top level.

## How Real Compilers Disambiguate `<`/`>` -- And What Electric-Operator Can Learn

### The Fundamental Problem

`<` and `>` are genuinely ambiguous when used for both comparison and generics/templates.
The expression `a<b,c>d` could parse as either:
- A template/generic: `a` instantiated with args `b, c`, followed by `d`
- Two comparisons with comma operator: `(a < b), (c > d)`

Every language that uses angle brackets for generics has had to develop workarounds.

### C++ -- Context-Sensitive Parsing via Name Lookup

C++ has the hardest version of this problem. The compiler **cannot parse `<` without
knowing whether the preceding name is a template**. This requires name lookup during
parsing, making C++ fundamentally context-sensitive.

Key mechanisms:
- **Name lookup feeds into parsing**: `foo<bar>` is a template only if `foo` is known
  to be a template name. Otherwise `<` is comparison.
- **`typename` and `template` keywords**: For dependent names inside templates,
  `T::template foo<int>()` explicitly marks `<` as starting a template argument list.
- **The `>>` fix (C++11)**: Pre-C++11, `vector<vector<int>>` was a parse error because
  `>>` was lexed as right-shift. C++11 added a rule: inside a template argument list,
  `>>` is reinterpreted as two closing `>` tokens.
- **The `<::` digraph fix**: Special rule prevents `<::` from being lexed as `[:`.

**Relevance to EO**: Full name lookup is impossible in an editor without a compiler
backend. But some of cc-mode's syntactic analysis (used by `c-guess-basic-syntax`)
can approximate it.

### Java -- Grammar Position Restrictions

Java's approach is simpler because generics were designed with parsing in mind:
- Generic brackets only appear after type names in specific grammar positions
  (declarations, after `new`, with dot-prefixed method type args like
  `Collections.<String>sort()`)
- `>>` and `>>>` are reinterpreted as multiple `>` in type argument context
- The grammar was carefully designed so the parser can always tell from position
  whether `<` starts a type argument

**Relevance to EO**: Position-based heuristics are feasible. E.g., `<` after an
identifier that looks like a type name (capitalized in Java) is probably generic.

### Rust -- The Turbofish `::<>`

Rust took the cleanest approach: in expression context, generic arguments **must** use
`::` before `<`: `foo::<i32>()`. In type context, bare `<>` is fine because the grammar
already expects types. This eliminates all ambiguity without name lookup or backtracking.

The famous "Bastion of the Turbofish" test proves this is necessary:
```rust
let (the, guardian, stands, resolute) = ("the", "Turbofish", "remains", "undefeated");
let _: (bool, bool) = (the<guardian, stands>(resolute));
```
Without turbofish, this would be ambiguous.

**Relevance to EO**: Rust already disables `<` and `>` in EO. The turbofish `::` prefix
could be detected to avoid spacing `<` after it.

### TypeScript -- Trial Parsing with Bounded Lookahead

TypeScript uses bounded lookahead in its hand-written recursive descent parser:
- After `<`, examine subsequent tokens for generic indicators (`extends`, `,`, `=`,
  or `>` followed by `(`)
- `.ts` vs `.tsx` files have different rules (`.tsx` bans `<Type>expr` cast syntax)
- Arrow function generics in TSX require workarounds like `<T,>` or `<T extends unknown>`

**Relevance to EO**: TypeScript already disables `<` and `>` in EO. Lookahead is harder
to do character-by-character during typing.

## Potential Approaches for Electric-Operator

### Approach 1: Heuristic Detection (Most Feasible)

Use context clues available at typing time to guess whether `<`/`>` are brackets:

**Signals that `<` is a bracket opener:**
- Preceded by a known type name or identifier matching type patterns (e.g., capitalized
  in Java, ending in `_t` in C)
- Preceded by `template` keyword
- On a `#include` line (already handled)
- Inside a function/class definition context (already partially handled via `c-guess-basic-syntax`)
- Preceded by `::` (Rust turbofish, C++ qualified names)
- After `new` keyword (Java/TypeScript)

**Signals that `<` is a comparison operator:**
- Preceded by a number literal (`0 < 1`)
- Preceded by `)` (e.g., `f() < g()`)
- Inside a function body, after `=` or `return` (expression context)
- After another operator (`+ <`, `= <`)

**Signals that `>` is a bracket closer:**
- A matching `<` bracket was recently opened (requires tracking state)
- Preceded by an identifier or `>` (nested templates)

**Signals that `>` is a comparison:**
- No unmatched `<` bracket in scope
- Preceded by a number

### Approach 2: Retroactive Correction (Post-Typing)

Since EO fires on `post-self-insert-hook`, it could:
1. When `<` is typed: tentatively space it as comparison
2. When `>` is typed: look back for a matching `<`, and if found, retroactively
   remove the spaces around both

This is appealing but complex -- the buffer has already been modified and the user has
continued typing between `<` and `>`.

### Approach 3: Defer to Subsequent Character (Limited Lookahead)

After inserting `<`, wait for the next character:
- If it's a letter/identifier start: probably a bracket (template argument)
- If it's a space or operator: probably a comparison

But EO operates character-by-character, so this would need to buffer the decision.
The existing approach where functions return `nil` to "do nothing" could be extended.

### Approach 4: Use Emacs Facilities

- **cc-mode's `c-guess-basic-syntax`**: Already used for function/class defs. Could be
  extended to more contexts.
- **Tree-sitter**: Modern Emacs (29+) has built-in tree-sitter support. A tree-sitter
  grammar already parses the full syntax tree, so angle bracket disambiguation is
  already solved. EO could query the tree-sitter parse tree to determine whether `<`/`>`
  at point are brackets or operators.
- **`syntax-ppss`**: Limited -- Emacs syntax tables don't natively support `<`/`>` as
  paired delimiters except in special modes.

### Approach 5: Andreas-Roehler's Suggestion

From issue #8 comments: "EO takes action only if no whitespace is before." This would
mean `x<y` gets spaced to `x < y` but `vector<int>` is left alone because the user
typed it without a leading space. This is too conservative -- EO's whole purpose is to
add spaces where there are none.

## Recommendation

**Approach 1 (heuristics) is the most practical**, potentially combined with tree-sitter
(Approach 4) for Emacs 29+ users. A tiered strategy:

1. **If tree-sitter is available**: Query the AST node at point to determine if `<`/`>`
   are template brackets or operators. This is the most reliable approach.

2. **Fallback heuristics** (for non-tree-sitter modes):
   - Use `c-guess-basic-syntax` for C/C++ (already partially done)
   - Check for preceding type-name patterns (capitalized identifiers, `_t` suffix,
     known type keywords)
   - Check for preceding number literals (strong signal for comparison)
   - Track a "bracket depth" counter for matching `<`/`>` pairs within a statement

3. **When uncertain, do nothing** (`nil`): This is the current behavior for C++, and
   it's better than getting it wrong. The heuristics should only fire when confidence
   is high.

The key insight is that **perfect disambiguation is impossible without full semantic
analysis** (as C++ compilers demonstrate). The goal should be to handle the common cases
correctly and fall back to "do nothing" for ambiguous cases. Even handling `#include`
lines and obvious function/class definition contexts (already done for c-mode) covers
a large fraction of real-world usage.
