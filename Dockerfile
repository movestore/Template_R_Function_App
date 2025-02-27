########################################################################################################################
# MoveApps R SDK
########################################################################################################################

FROM rocker/geospatial:4.4.2

LABEL maintainer = "couchbits GmbH <us@couchbits.com>"

# Security Aspects
# group `staff` b/c of:
# When running rocker with a non-root user the docker user is still able to install packages.
# The user docker is member of the group staff and could write to /usr/local/lib/R/site-library.
# https://github.com/rocker-org/rocker/wiki/managing-users-in-docker
RUN useradd --create-home --shell /bin/bash moveapps --groups staff
USER moveapps:staff

WORKDIR /home/moveapps/co-pilot-r

# copy the SDK
COPY --chown=moveapps:staff src/ ./src/
COPY --chown=moveapps:staff data/ ./data/
COPY --chown=moveapps:staff sdk.R RFunction.R .env app-configuration.json start-process.sh ./

# restore the current snapshot via renv
COPY --chown=moveapps:staff renv.lock .Rprofile ./
COPY --chown=moveapps:staff renv/activate.R renv/settings.dcf ./renv/
RUN R -e 'renv::restore()'

ENTRYPOINT ["/bin/bash"]