FROM haskell
MAINTAINER jckdrpr <sherub.thakur@gmail.com>
WORKDIR /app

CMD ["stack", "exec", "app-search-exe"]

RUN stack --resolver lts-8.6 setup && \
stack install aeson bytestring containers either parsec time bloodhound text \
http-client http-types scotty transformers wai wai-cors wai-extra warp

COPY . /app

RUN stack install && stack build
