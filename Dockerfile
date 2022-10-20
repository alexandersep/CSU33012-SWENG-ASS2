FROM haskell:8.6.5

WORKDIR /CSU33012-SWENG-ASS2
ADD . /CSU33012-SWENG-ASS2

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# unless the .cabal stack file .yml changes
# In case path is not updated automatically
RUN stack setup
RUN stack config set system-ghc --global true
RUN stack build --only-dependencies

# Add and install Application code
RUN stack install

CMD ["CSU33012-SWENG-ASS2-exe"]
