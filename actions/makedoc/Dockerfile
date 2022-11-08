# Dockerfile — Docker image to build documentation

# Atelier (https://github.com/melusina-org/cl-atelier)
# This file is part of Atelier.
#
# Copyright © 2017–2022 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

FROM debian:11

RUN env DEBIAN_FRONTEND=noninteractive apt update
RUN env DEBIAN_FRONTEND=noninteractive apt install -y\
 sbcl cl-quicklisp texinfo texlive
ADD describe /opt/local/bin/describe
