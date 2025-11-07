# Natural language processing of tasting panel comments

Contains sensory data and analysis code for new hop variety registration. NLP analysis of tasting panel comments characterizes aroma and flavor in dried hops and finished beer.

## Dataset

**File:** `../Datasets/Vera Sensory Comments.csv`

268 sensory comments from professional tasting events:

- **Dried Hops:** 181 comments
- **Beer:** 87 comments

### Tasting events

- Hopsource 2022-2024 (Washington and Idaho)
- Craft Brewers Conference (CBC) 2022, 2024
- Hop Research Council (HRC) Winter Meeting 2024 (multiple breweries)
- Arbeiter Brewing Company Experimental Hop Rub 2023

### Data structure

```text
Event,Sample,Comments
Hopsource 2022 - Washington,Dried Hops,Complex and clean
Hopsource 2022 - Washington,Dried Hops,Vinous grape
...
```

## Analysis workflow

### 1. Data cleaning (`1_clean_data.ipynb`)

Preprocess raw sensory comments for text analysis

**Steps:**

- Acronym expansion (GO/O/G/ONG → "onion garlic", DMTS → "dimethyl trisulfide", etc.)
- Text standardization and cleaning
- Data reshaping to JSON format
- Summary statistics on comment length distribution

### 2. Keywords and phrases analysis (`2_keywords_phrases.ipynb`)

Extract and analyze key sensory descriptors and linguistic patterns

**Steps:**

- **N-gram analysis:** Most frequent words, word pairs, and three-word phrases
- **Part-of-speech tagging:** Extraction of adjectives and adverbs
- **Categorical analysis:** Prevalence of different flavor categories
- **Off-flavor analysis:** Quantification of onion/garlic descriptors

## Findings

### Word frequency

Most frequent words, bigrams, and trigrams across comments. Numbers in parentheses are raw counts.

|                             | Dried Hops                                                                                                                              | Beer                                                                                        |
| --------------------------- | --------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------- |
| **Top word**                | *fruit* (29)                                                                                                                            | *fruit* (23)                                                                                |
| **High‑frequency words**    | *tropical* (27), *citrus* (25), *peach* (16), *garlic* (15), *onion* (15), *mango* (14), *pineapple* (13), *fruity* (11), *bright* (11) | *hop* (15), *citrus* (11), *sweet* (10), *pleasant* (10), *bitterness* (10), *tropical* (9) |
| **High-frequency bigrams**  | *onion garlic* (11), *stone fruit* (7), *passion fruit* (4)                                                                             | *stone fruit* (5), *hop flavor* (5), *sweet fruit* (4)                                      |
| **High-frequency trigrams** | *tropical stone fruit* (3), *slight onion garlic* (3)                                                                                   |                                                                                             |
| **Top adjectives**          | *tropical* (25), *bright* (11), *sweet* (9)                                                                                             | *sweet* (10), *tropical* (9) *clean* (8)                                                    |

### Flavor prevalence

Prevalence of flavor expressions based on grouped keyword patterns (e.g., "tropical" captures tropical, pineapple, mango, passion, etc.).

| Term            | Dried hops n | Dried hops % | Beer n | Beer % |
| --------------- | ------------ | ------------ | ------ | ------ |
| Tropical fruits | 48           | 27%          | 21     | 24%    |
| Citrus fruits   | 45           | 25%          | 18     | 22%    |
| Fruit           | 39           | 22%          | 21     | 24%    |
| Stone fruits    | 23           | 13%          | 9      | 10%    |
| Onion garlic    | 18           | 10%          | 5      | 6%     |
| Melons          | 8            | 4%           | 5      | 6%     |
| Berries         | 7            | 4%           | 0      | 0%     |

## Publication

Analysis supports new hop variety registration by profiling sensory attributes and demonstrating distinct flavor profile and market potential.

**Citation:** Registration of 'USDA-ARS Vera': A new public hop cultivar with tropical, stone-fruit aroma and powdery mildew resistance. *Journal of Plant Registrations*, 2025. <https://doi.org/10.1002/plr2.70033>
