---
date: "2020-03-17T00:00:00Z"
draft: false
lastmod: "2020-03-17T00:00:00Z"
linktitle: Data Dictionary
menu:
  resources:
    name: Dictionary
    weight: 40
summary: Data dictionary -  Human readable dictionary of data contents
title:  Data dictionary -  Human readable dictionary of data contents
toc: false
type: docs
weight: 1
---

A data dictionary provide human readable description of the data, providing context on the nature and structure of the data. This helps someone not familiar with the data understand, and use the data. At a minimum they should contain the following pieces of information about the data:

- **variable names**
- **variable labels**
- **variable codes**, and
- special values for **missing data**.

![Data Dictionary](https://www.helsinki.fi/sites/default/files/styles/large_content_image/public/thumbnails/image/data_dictionary_osf_2.png)

An example data dictionary table from [incarceration trends repository](https://github.com/vera-institute/incarceration_trends/). This includes information on the variable, its class (type), and a longer description.

|Variable        |Class          |Description                                                     |
|:---------------|:--------------|:---------------------------------------------------------------|
|year            |integer (date) |Year                                                            |
|urbanicity      |character      |County-type (urban, suburban, small/mid, rural)                 |
|pop_category    |character      |Category for population - either race, gender, or Total         |
|rate_per_100000 |double         |Rate within a category for prison population per 100,000 people |

Note: __Every data dictionary should also be provided in its raw form (e.g., a CSV) in the repository__

Some links to understand more on creating and maintaining data dictionaries:

- [The code book](https://github.com/jtleek/datasharing#the-code-book)
- [How to code variables](https://github.com/jtleek/datasharing#how-to-code-variables)
- [Data Dictionary: a how to and best practices](https://medium.com/@leapingllamas/data-dictionary-a-how-to-and-best-practices-a09a685dcd61)
- [Create a data dictionary](https://kbroman.org/dataorg/pages/dictionary.html)
- [How to Make a Data Dictionary](https://help.osf.io/hc/en-us/articles/360019739054-How-to-Make-a-Data-Dictionary)