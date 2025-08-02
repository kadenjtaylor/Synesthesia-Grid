# Synesthesia DB

A big grid of synesthesia-type associations between various concepts and sensory descriptors.

## Why?

It was a hackathon project idea that I never built.

## What's it for?

Data science practice, helping robots write metaphors, etc.

## Data Model

A big 3d grid where:
- rows are concepts/nouns
- columns are sensory descriptors/adjectives
- layers are different users' responses (a subjective measure of intensity from 0-9).

So each cell in the grid represents a particular user's answer to the question "How much does C describe R?"

```
Ex: "How [spicy] is [the sun]? => 8"
```