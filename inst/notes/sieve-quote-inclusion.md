# The sieve's ceiling is quote inclusion, not seed vocabulary

Found 2026-08-09, during the v2/v3 codebook seed revision.

## What happened

The 42k-post corpus flagged seven codes as under-recalling — the model coded
them far more often than the seeds fired. Adding evidence-derived terms fixed
five of them, but two swung past the target into *over*-firing:

| code | v1 agreement | after widening |
|---|---|---|
| `agriculture_alr` | 3.06 (under) | **0.56 (over)** |
| `economic_dev` | 5.84 (under) | 0.77 (over) |

Agreement is model-coded posts ÷ sieve-caught posts; 1.0 means they agree.

The obvious diagnosis was sloppy regexes — bare `\bcull\b`, `\bflock\b`,
`\bbusinesses\b` are generic English. Tightening them helped `economic_dev`
but `agriculture_alr` did not move at all (0.56 → 0.56), which ruled that
explanation out.

## The actual cause

Reading the posts the sieve caught and the model rejected: they are
**quote-reply chains inside the ostrich-cull megathread**. Castanet's phpBB
includes the quoted parent inline, so a one-line reply

> I'm keeping my guns thanks.

arrives carrying its entire quoted ancestry, which contains "ostrich" and
"cull". The sieve reads the concatenated text and sees agriculture. The model
reads the same text, understands the comment is a gun-rights remark, and codes
it `none`.

Of 483 such posts, the model's own labels are:

| model code | n |
|---|---|
| none | 483 |
| national_partisan | 78 |
| policing_bylaw | 35 |
| federal_fiscal | 34 |
| governance_process | 33 |

## Why this matters beyond one code

**No seed vocabulary can fix this.** The trigger word is genuinely present in
the text; it just belongs to somebody else's sentence. Any term distinctive
enough to catch real ostrich-cull posts will also catch every reply quoting
one. This is a hard ceiling on keyword-sieve precision for a quote-inclusive
forum, and it is structural, not a tuning failure.

It also explains the shape of the whole flag table: the corpus is dominated by
a few large megathreads, so any topical seed inherits that thread's entire
reply tree.

## Consequences

1. **Do not keep tuning seeds against agreement ≈ 1.0.** For codes whose
   discourse concentrates in megathreads, the achievable floor is well below
   1.0 and chasing it will destroy recall elsewhere. Two codes
   (`federal_fiscal` 2.95, `crown_corps` 2.43) are deliberately left flagged
   for the opposite reason: their missing terms are `government`, `money`,
   `canada`, `taxes` — adding those would fire on nearly every post.

2. **This is a strong argument against `OKCP_GATE=scoped`.** Gating model
   calls on the sieve would now both drop real content (under-recall) and
   spend on quoted noise (over-recall). The gate stays `all`.

3. **The real fix is upstream: strip quoted blocks before sieving.**
   `parse_thread()` already resolves `blockquote cite` to build reply edges, so
   the quoted span is identifiable at parse time. Sieving the comment's OWN
   text — while keeping the full text for the model, which uses the quote as
   context — would remove the artifact at its source. Not attempted here
   because it changes `body_local` semantics for 42k existing posts and
   deserves its own migration.

4. **The classifier is doing exactly what it should.** It disagreed with the
   sieve 483 times and was right each time inspected. That is the two-tier
   design working: a cheap recall filter, adjudicated by a model that reads for
   meaning.
