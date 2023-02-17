#!/usr/bin/env bash

set -euo pipefail

pushd "$(dirname "$(readlink -f "$0")")"

sbt proguard
version=$(grep version build.sbt | cut -d '"' -f2)
mv -f "target/scala-2.13/proguard/textworldexpress_2.13-${version}.jar" ../textworld_express/textworld-express.jar

popd
