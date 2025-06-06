# Executable Environment for OSF Project [m6pb2](https://osf.io/m6pb2/)

This repository was automatically generated as part of a project to test the reproducibility of open science projects hosted on the Open Science Framework (OSF).

**Project Title:** Is Accurate, Positive, or Inflated Self-Perception Most Advantageous for Psychological Adjustment? A Competitive Test of Key Hypotheses

**Project Description:**
> 
Here, we provide you with additional materials (including data and R codes) to the following article: 

Humberg, S., Dufner, M., Schönbrodt, F. D., Geukes, K., Hutteman, R., Küfner, A. C. P., van Zalk, M. H. W., Denissen, J. J. A., Nestler, S., &amp; Back, M. D. (2019). Is accurate, positive, or inflated self-perception most advantageous for psychological adjustment? A competitive test of key hypotheses. Journal of Personality and Social Psychology, 116(5), 835–859. doi:10.1037/pspp0000204

The folder "Additional OSF materials" contains 
- detailed result tables for the model comparison analyses (OSF Material 1), 
- detailed description of the assessment of all variables of interest in the five studies (OSF Material 2), 
- descriptive statistics of all variables used in the analyses and internal consistencies of the outcome categories (OSF Material 3),
- the description of additional analyses for achievement outcomes (OSF Material 4),
- information on the nesting of the models in the initial model set (OSF Material 5),
- the results of model comparison analyses conducted within the single studies (OSF Material 6), 
- the parameter estimates and graphs of the full polynomial models for all analyses (OSF Material 7), 
- a comparison of the results obtained with AIC model evaluation to respective analyses using BIC (OSF Material 8; additional analysis due to a reviewer's question), 
- a comparison of results obtained with the strict information-theoretic approach to analyses using AIC confidence intervals (OSF Material 9; additional analysis due to a reviewer's question), 
- and the results of model comparison analyses applying a more conservative outlier treatment (OSF Material 10; additional analysis due to a reviewer's question).

The folder "Open data, open code, research transparency" contains 
- all data and R code needed to reproduce the analyses reported in the manuscript; Researchers interested in reproducing our results are adviced to first consider the file "0_README_Rcode.txt", which contains an overview of the data and R code files.
- additional information on the five studies that were used; Researchers interested in additional information on the studies or in information required for the 21 word solution are adviced to start with the file "0_Open_Science_Table.xlsx".

**Original OSF Page:** [https://osf.io/m6pb2/](https://osf.io/m6pb2/)

---

**Important Note:** The contents of the `m6pb2_src` folder were cloned from the OSF project on **12-03-2025**. Any changes made to the original OSF project after this date will not be reflected in this repository.

The `DESCRIPTION` file was automatically added to make this project Binder-ready. For more information on how R-based OSF projects are containerized, please refer to the `osf-to-binder` GitHub repository: [https://github.com/Code-Inspect/osf-to-binder](https://github.com/Code-Inspect/osf-to-binder)

## flowR Integration

This version of the repository has the **[flowR Addin](https://github.com/flowr-analysis/rstudio-addin-flowr)** preinstalled. flowR allows visual design and execution of data analysis workflows within RStudio, supporting better reproducibility and modular analysis pipelines.

To use flowR, open the project in RStudio and go to `Addins` > `flowR`.

## How to Launch:

**Launch in your Browser:**

🚀 **MyBinder:** [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/code-inspect-binder/osf_m6pb2-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment in your web browser.
   * Please note that Binder may take a few minutes to build the environment.

🚀 **NFDI JupyterHub:** [![NFDI](https://nfdi-jupyter.de/images/nfdi_badge.svg)](https://hub.nfdi-jupyter.de/r2d/gh/code-inspect-binder/osf_m6pb2-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment on the NFDI JupyterHub platform.

**Access Downloaded Data:**
The downloaded data from the OSF project is located in the `m6pb2_src` folder.

## Run via Docker for Long-Term Reproducibility

In addition to launching this project using Binder or NFDI JupyterHub, you can reproduce the environment locally using Docker. This is especially useful for long-term access, offline use, or high-performance computing environments.

### Pull the Docker Image

```bash
docker pull meet261/repo2docker-m6pb2-f:latest
```

### Launch RStudio Server

Run the container (with a name, e.g. `rstudio-dev`):
```bash
docker run -it --name rstudio-dev --platform linux/amd64 -p 8888:8787 --user root meet261/repo2docker-m6pb2-f bash
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
[osf_m6pb2](https://github.com/code-inspect-binder/osf_m6pb2)

