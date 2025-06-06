# Executable Environment for OSF Project [przvy](https://osf.io/przvy/)

This repository was automatically generated as part of a project to test the reproducibility of open science projects hosted on the Open Science Framework (OSF).

**Project Title:** Grammatical gender in French: its impact on the interpretation of social gender in generic hybrid nouns

**Project Description:**
> There is a growing interest among researchers working on French in the way grammatical gender in this language affects speakers' mental representations about social gender (e.g. Gygax et al. 2013, Gygax et al 2019, Richy &amp; Burnett 2021). Grammatical gender refers to the classification of nouns in two or more classes, based on the agreement patterns involving the noun and its syntactic dependents. For instance, in French, the noun 'pain' "bread" is masculine, as manifested by the fact that it requires the masculine form for dependents such as determiners and adjectives (e.g. un bon pain "a good bread"). Feminine nouns such as baguette "baguette" require the feminine form for their dependents (e.g. une bonne baguette "a good baguette"). Systems of grammatical gender are often sex-based, namely the grammatical gender of human-denoting nouns correlates with the biological sex/social gender of the corresponding referents (Corbett 2013). For instance, in French, the noun referring to men is masculine ("un homme") whereas the noun referring to women is feminine ("une femme"). However, there can be mismatches between the grammatical gender of human-denoting nouns and the social gender of corresponding referents. One famous case of mismatch in French is masculine generics. In their generic use, masculine nouns can refer to both male and female individuals (e.g. "un artiste, ça doit s'entraîner tous les jours"). Hence, the masculine gender is traditionnally considered by linguists as a neutral gender in this specific context. However, experimental studies have consistently shown that masculine generics bias speakers toward a masculine interpretation, even after controlling for the effect of gender stereotypes associated to nouns (e.g. Sato et al 2013, Richy &amp; Burnett 2021). This suggests that grammatical gender is not semantically vacuous but consistently contributes to the inferences speakers make about nouns' referents, with potential psychological consequences on speakers' attitudes in the world (e.g. Chatard et al 2005). These findings have been used as an argument for gender-neutral language, namely a set of strategies designed to avoid using masculine forms when the meaning intended does not include only male referents.

This study focuses on another case of mismatch between grammatical gender and social gender that has not received as much attention in the literature on French: hybrid nouns with a generic interpretation. Hybrid nouns are nouns that have a grammatical gender that does not match the social gender of their referents (Corbett 2015). A famous case is German "Mädchen": this noun is grammatically neuter ("das Mädchen") but refers to girls. French has a set of hybrid words that are either masculine (e.g. individu "individual") or feminine grammatically (e.g. personne "person") but are semantically generic, namely can refer to both male and female individuals. To our knowledge, a single experimental study by  Brauer and Landry (2008) has investigated whether grammatical gender biases the interpretation of generic hybrid nouns, focusing on a single pair (individu/personne). In Study 3,  Brauer and Landry (2008) found a larger proportion of male interpretations for the masculine noun "individu" than for the feminine noun "personne", in line with the general hypothesis that grammatical gender affects semantic interpretation. The present study aims to replicate this study using a larger set of hybrid nouns. 

Gygax et al (2019) argued that hybrid nouns are not relevant to explore the semantic effect of grammatical gender on the basis that these words are exceptions within gender systems. Indeed, hybrid nouns belong to a closed lexical class in French whereas the more extensively studied masculine generic is morphologically productive. We think that generic hybrid nouns are nonetheless relevant because, contrary to masculine generics, they make it possible to test the interpretative effect of both grammatical gender values (masculine and feminine). Generic hybrid nouns are found among both masculine nouns (e.g. "individu") and feminine nouns (e.g. "personne") whereas masculine generics are by definition only masculine and therefore only allow to test a male bias in the interpretation of grammatical gender (and not a potential corresponding female bias induced by feminine forms used generically).

**Original OSF Page:** [https://osf.io/przvy/](https://osf.io/przvy/)

---

**Important Note:** The contents of the `przvy_src` folder were cloned from the OSF project on **12-03-2025**. Any changes made to the original OSF project after this date will not be reflected in this repository.

The `DESCRIPTION` file was automatically added to make this project Binder-ready. For more information on how R-based OSF projects are containerized, please refer to the `osf-to-binder` GitHub repository: [https://github.com/Code-Inspect/osf-to-binder](https://github.com/Code-Inspect/osf-to-binder)

## flowR Integration

This version of the repository has the **[flowR Addin](https://github.com/flowr-analysis/rstudio-addin-flowr)** preinstalled. flowR allows visual design and execution of data analysis workflows within RStudio, supporting better reproducibility and modular analysis pipelines.

To use flowR, open the project in RStudio and go to `Addins` > `flowR`.

## How to Launch:

**Launch in your Browser:**

🚀 **MyBinder:** [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/code-inspect-binder/osf_przvy-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment in your web browser.
   * Please note that Binder may take a few minutes to build the environment.

🚀 **NFDI JupyterHub:** [![NFDI](https://nfdi-jupyter.de/images/nfdi_badge.svg)](https://hub.nfdi-jupyter.de/r2d/gh/code-inspect-binder/osf_przvy-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment on the NFDI JupyterHub platform.

**Access Downloaded Data:**
The downloaded data from the OSF project is located in the `przvy_src` folder.

## Run via Docker for Long-Term Reproducibility

In addition to launching this project using Binder or NFDI JupyterHub, you can reproduce the environment locally using Docker. This is especially useful for long-term access, offline use, or high-performance computing environments.

### Pull the Docker Image

```bash
docker pull meet261/repo2docker-przvy-f:latest
```

### Launch RStudio Server

Run the container (with a name, e.g. `rstudio-dev`):
```bash
docker run -it --name rstudio-dev --platform linux/amd64 -p 8888:8787 --user root meet261/repo2docker-przvy-f bash
```

Inside the container, start RStudio Server with no authentication:
```bash
/usr/lib/rstudio-server/bin/rserver --www-port 8787 --auth-none=1
```

Then, open your browser and go to: [http://localhost:8888](http://localhost:8888)

> **Note:** If you're running the container on a remote server (e.g., via SSH), replace `localhost` with your server's IP address.
> For example: `http://<your-server-ip>:8888`

## Looking for the Base Version?

For the original Binder-ready repository **without flowR**, visit:
[osf_przvy](https://github.com/code-inspect-binder/osf_przvy)

