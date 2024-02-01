FROM haskell:9.4.8

WORKDIR /opt/tictactoe

RUN cabal update

# Add just the .cabal file to capture dependencies
COPY ./tictactoe.cabal /opt/tictactoe/tictactoe.cabal

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal build --only-dependencies -j4

# Add and Install Application Code
COPY . /opt/tictactoe
RUN cabal install
#RUN cabal test

EXPOSE 8080

CMD ["tictactoe"]
