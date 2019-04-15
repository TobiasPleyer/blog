---
title:  "Parser nomenclature"
date: 2017-07-14
tags: parser
category: Programming
authors: Tobias Pleyer
summary: "If you read about parser, some terms are a must to know"
---

Parser nomenclature
===================

My systematic way to parser theory
----------------------------------

Parsers and compilers have always fascinated me. In the very beginning
they were just buzzwords standing for the magical black boxes that turn
the code of your favourite programming language into something that your
computer can execute.

Generating machine executable code sure is amongst the most important
use cases for parsers and compilers. But this is not the whole story.
Basically every occasion very structured input of some sort has to be
transformed into structured output, a parser/compiler setup is needed.

At first I was willing to accept the black box view and didn't bother
too much to dig deeper. As of lately I feel more and more interested in
the topic. Over the last months I caught myself reading some articles or
tutorials about parsers. The frequency of those occasions is increasing.
In the next time I want to devote a couple of posts to parser. I'll
focus on parsers. Compilers are an even more complicated topic and I
feel that it would overwhelm me to tackle it now.

I plan to have a more systematic approach to the topic. A first step is
to gain an overview of the terminology landscape. When you read about
parsers certain words will come up over and over again and it is
mandatory to know them. In the following I'll list some of them, mainly
as a reference for myself.

Terminology
-----------

### General

  Word            Meaning
  --------------- ---------------------------------------------------------------------------
  Backtracking    The parser moves back (rewinds) the token stream
  LL(k) grammar   If a LL(k) parser exists that can parse this grammar without backtracking

### Parser types

  Acronym                    Description
  -------------------------- ----------------------------------------------------------------------------------------------------------
  LL(k) parser               Left to right parser with leftmost derivation that uses *k* tokens for lookahead
  LR(k) parser               Left to right parser with righmost derivation in reverse with *k* peek tokens
  Recursive descent parser   A parser that is made out of mutually recursive procedures calling each other as they consume the tokens
  Top-down parser            Parser starting with the smallest details first
  Bottom-up parser           Parser starting with the overal structure first
  Shift reduce parsers       Table-driven bottom-up parsing
  Precedence parsers         Bottom-up parser for context-free grammars that can be used only by simple precedence grammars
  Deterministic parser       A parser that does not require backtracking

### Grammars

  Word                   Meaning
  ---------------------- -----------------------------------------------------------------------------------------
  Formal grammar         A set of production rules for strings in a formal language
  Context free grammar   A set of production rules that describe all possible strings in a given formal language

Closing words
-------------

I know that this article is stil fairly incomplete. The area of parser
and their theory is a pretty vast one. As a first go I was throwing
everything in that I have heart about or I stumbled upon while reading
about parsers. I plan to come back to hear and update this article while
I learn more about parsers.