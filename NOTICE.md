# Scope of the licence

The CC BY 4.0 licence in [LICENSE](LICENSE) covers **the code and the codebook
in this repository**.

This notice is a separate file on purpose: appending text to `LICENSE` breaks
GitHub's automatic licence detection, which reported `NOASSERTION` when the
scope note lived inside the licence body.

## What the licence cannot cover

It does not, and cannot, grant any rights in the source material this pipeline
reads. None of that material is contained in this repository.

**Castanet forum comments** are the property of their authors and Castanet.
Castanet's Terms of Use prohibit redistribution. Comment text lives only in a
local, git-ignored DuckDB file; every published artifact is de-texted.

**City of Kelowna council records** are public records published by the
municipality under the City's own terms. Council minutes name identifiable
residents who spoke at public hearings. Those names are stored locally only,
and `actor_key` is a hash of the name so repeat speakers can be linked across
hearings without the name ever appearing in an export.

## If you run this pipeline

You are responsible for your own compliance with the terms of every source you
collect from, and for the privacy obligations attaching to identifiable
individuals in that material. Access postures were verified on the dates
recorded in the `sources` registry and change without notice — re-check before
any new crawl.

Two constraints worth reading before you extend it:

- **Reddit** is deliberately not collected. Their Responsible Builder Policy
  requires research to go through the Reddit for Researchers programme and
  prohibits inferring political affiliation about users — which actor-level
  stance modelling amounts to. The collector exists but is disabled.
- **Commercial use changes the analysis.** This is personal research. BC's
  Personal Information Protection Act, and campaign-finance rules if candidates
  are involved, apply to commercial use in ways they do not here.
