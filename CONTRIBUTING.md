# Contributing to `etstats`

## Adding a citation

To track where Epiverse-TRACE is used or mentioned, we use `BibTex`. All citation information is stored under `data/citations`.

### Getting the BibTex itself

An easy way to get the BibTex information is to get the DOI of the article and run it through <https://www.doi2bib.org>. A Pubmed Central ID or an arXiv ID will also work.

You should have something that looks like this:

```bib
@article{charniga2024best,
      title={Best practices for estimating and reporting epidemiological delay distributions of infectious diseases using public health surveillance and healthcare data},
      author={Kelly Charniga and Sang Woo Park and Andrei R Akhmetzhanov and Anne Cori and Jonathan Dushoff and Sebastian Funk and Katelyn M Gostic and Natalie M Linton and Adrian Lison and Christopher E Overton and Juliet R C Pulliam and Thomas Ward and Simon Cauchemez and Sam Abbott},
      year={2024},
      journal={arXiv preprint},
      doi={10.48550/arXiv.2405.08841}
}
```

### Adding it to the relevant `.bib` file

If your reference is specific to a package, add it to the `.bib` file for that package. For example, if the reference is for **cfr** use `cfr.bib`.

If it is for Epiverse-TRACE more generally, add it to the `epiverse.bib` file
