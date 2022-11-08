# github_actions.sh — GitHub Actions Support

# Atelier (https://github.com/melusina-org/cl-atelier)
# This file is part of Atelier.
#
# Copyright © 2017–2022 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

# with_group_presentation GROUP-NAME COMMAND [ARG1…]
#  Run COMMAND and present its output under GROUP-NAME.
#
# The COMMAND is run in a separated shell, so that it cannot 
   
with_group_presentation()
(
    trap "printf '::endgroup::\n'" INT TERM EXIT 
    printf '::group::%s\n' "$1"
    shift
    "$@"    
)

docker_info()
{
    docker_build_details "$@"
    github_context_info
}

github_context_info()
{
    cat <<EOF
::group::Context info
eventName: ${GITHUB_EVENT_NAME}
sha: ${GITHUB_SHA}
ref: ${GITHUB_REF}
workflow: ${GITHUB_WORKFLOW}
action: ${GITHUB_ACTION}
actor: ${GITHUB_ACTOR}
runNumber: ${GITHUB_RUN_NUMBER}
runId: ${GITHUB_RUN_ID}
::endgroup::
EOF
}

docker_registry()
{
    if [ -n "${docker_registry}" ]; then
	printf '%s' "${docker_registry}"
    else
	printf 'ghcr.io'
    fi
}

docker_repository()
{
    if [ -n "${docker_repository}" ]; then
	printf '%s' "${docker_repository}"
    else
	printf '%s' "${GITHUB_REPOSITORY}"
    fi
}

docker_image_version()
{
    if [ -n "${docker_image_version}" ]; then
	printf '%s' "${docker_image_version}"
    elif [ -n "${GITHUB_REF_NAME}" ]; then
	printf '%s' "${GITHUB_REF_NAME}"
    else
	git rev-parse --abbrev-ref HEAD
    fi
}

# docker_labels TITLE DESCRIPTION LICENSE
#  Print the docker labels for TITLE, DESCRIPTION and LICENSE

docker_labels()
{
    local title description license
    title="$1"
    description=$(printf '%s' "$2" | tr '\n' ' ')
    license="$3"
    grep -v -F ':undefined' <<EOF
org.opencontainers.image.title=${title:=:undefined}
org.opencontainers.image.description=${description:=:undefined}
org.opencontainers.image.url=${GITHUB_SERVER_URL}/${GITHUB_REPOSITORY}
org.opencontainers.image.source=${GITHUB_SERVER_URL}/${GITHUB_REPOSITORY}
org.opencontainers.image.version=$(docker_image_version)
org.opencontainers.image.created=$(timestamp)
org.opencontainers.image.revision=${GITHUB_SHA}
org.opencontainers.image.licenses=${license:=:undefined}
EOF
}

# End of file `github_actions.sh'
