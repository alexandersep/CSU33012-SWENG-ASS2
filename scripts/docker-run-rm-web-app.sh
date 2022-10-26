#!/bin/sh
# Run dockerhub pulled image to access web on localhost:8080
docker run --expose 8080 -p 8080:8080 -ti --rm asepelenco/interactive-calculator:v2.0.0
