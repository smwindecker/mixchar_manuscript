## mixchar: An R package for the deconvolution of thermal decay curves

This repository contains all the code used in the manuscript associated with the R packaged [mixchar](https://github.com/smwindecker/mixchar). 

## Running the code

All analyses were done in `R`, and the paper is written in LaTeX. All code needed to reproduce the submitted products is included in this repository. To reproduce this paper, run the code contained in the `analysis.R` file. The paper and supplementary materials will be created in the directory `ms`.

If you are reproducing these results on your own machine, first download the code and then install the required packages, listed under `Depends` in the `DESCRIPTION` file. This can be achieved by opening the Rstudio project and running:

```{r}
#install.packages("remotes")
remotes::install_deps()
```

You can access an interactive RStudio session with the required software pre-installed by opening a container hosted by [Binder](http://mybinder.org): 

[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/smwindecker/mixchar_manuscript/master?urlpath=rstudio)

To ensure long-term [computational reproducibility](https://www.britishecologicalsociety.org/wp-content/uploads/2017/12/guide-to-reproducible-code.pdf) of this work, we have created a [Docker](http://dockerhub.com) image to enable others to reproduce these results on their local machines using the same software and versions we used to conduct the original analysis. Instructions for reproducing this work using the docker image are available at the bottom of the page. 

## Material included in the repository include:

- `data/`: Raw data for demonstration
- `my_functions.R`: File with functions for the analysis and figures in the manuscript
- `ms/`: directory containing manuscript in LaTeX and accompanying style files 
- `DESCRIPTION`: A machine-readable [compendium]() file containing key metadata and dependencies 
- `LICENSE`: License for the materials
- `Dockerfile` & `.binder/Dockerfile`: files used to generate docker containers for long-term reproducibility

## Running via Docker

If you have Docker installed, you can recreate the computing environment as follows in the terminal. 

From the directory you'd like this repo saved in, clone the repository:

```
git clone https://github.com/smwindecker/mixchar_manuscript.git
```

Then fetch the container:

```
docker pull smwindecker/mixchar_manuscript
```

Navigate to the downloaded repo, then launch the container using the following code (it will map your current working directory inside the docker container): 

```
docker run --user root -v $(pwd):/home/rstudio/ -p 8787:8787 -e DISABLE_AUTH=true smwindecker/mixchar_manuscript
```

The code above initialises a docker container, which runs an RStudio session accessed by pointing your browser to [localhost:8787](http://localhost:8787). For more instructions on running docker, see the info from [rocker](https://hub.docker.com/r/rocker/rstudio).

### NOTE: Building the docker image

For posterity, the docker image was built off [`rocker/verse:3.5.1` container](https://hub.docker.com/r/rocker/verse) via the following command, in a terminal contained within the downloaded repo:

```
docker build -t smwindecker/mixchar_manuscript .
```

and was then pushed to [dockerhub](https://cloud.docker.com/u/traitecoevo/repository/docker/smwindecker/mixchar_manuscript). The image used by binder builds off this container, adding extra features needed by binder, as described in [rocker/binder](https://hub.docker.com/r/rocker/binder/dockerfile).

## Problems?

If you have any problems getting the workflow to run please create an [issue](https://github.com/smwindecker/mixchar_manuscript/issues) and I will endevour to remedy it ASAP.

Special thank you to [James Camac](https://github.com/jscamac) and [Daniel Falster](https://github.com/dfalster) for assistance in setting up this reproducible workflow. 
