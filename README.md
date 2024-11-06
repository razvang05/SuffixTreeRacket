# SuffixTreeRacket
Construction and manipulation of suffix trees
Introduction

Welcome to the Racket Suffix Tree Assignment! This project aims to deepen your understanding of suffix trees (ST) by implementing them in Racket, a valuable data structure in string processing. Throughout this assignment, you’ll develop a library to create and manipulate suffix trees, which have powerful applications in text analysis.

Assignment Outline

The assignment is divided into two primary stages:

Stage 1: Fundamental Suffix Tree Operations

In this first stage, you’ll get to know the structure and organization of suffix trees. You’ll implement a variety of functions to work with suffix trees, including defining their structure, extracting branches, and searching for patterns within the tree.

Stage 2: Constructing the Suffix Tree

In the second stage, the focus will shift to building suffix trees from a provided text. You’ll develop constructors to create both atomic and compact suffix trees by extracting suffixes and incrementally building the tree structure.

Tasks

Here is a summary of the tasks you’ll complete for each stage:

Stage 1: Fundamental Suffix Tree Operations

Represent a suffix tree as a list of branches.
Implement functions like first-branch, other-branches, get-branch-label, get-branch-subtree, get-ch-branch, longest-common-prefix, longest-common-prefix-of-list, match-pattern-with-label, and st-has-pattern?.
Stage 2: Building the Suffix Tree

Extract all suffixes from a given text using the get-suffixes function.
Construct the atomic suffix tree with the ast-func function.
Build the compact suffix tree using the cst-func function.
