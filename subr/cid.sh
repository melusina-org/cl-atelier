# cid.sh — A standard library for shell programming

# Atelier (https://github.com/melusina-org/cl-atelier)
# This file is part of Atelier.
#
# Copyright © 2017–2023 Michaël Le Barbier
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
    printf '==> %s\n' "$1"
    shift
    "$@"
)

gprintf()
{
    printf "$@" | awk -vOFS=':' -F': ' '
$2 ~ /,/ {
  $2 = "," $2
  gsub(",", "\n  - ", $2)
}

{ print }
'
}


docker_file()
{
    printf '%s/%s/Dockerfile' "${dockerdir}" "$1"
}

docker_registry()
{
    if [ -n "${docker_registry}" ]; then
	printf '%s' "${docker_registry}"
    else
	printf ''
    fi
}

docker_repository()
{
    if [ -n "${docker_repository}" ]; then
	printf '%s' "${docker_repository}"
    else
	printf ''
    fi
}


docker_image_version()
{
    if [ -n "${docker_image_version}" ]; then
	printf '%s' "${docker_image_version}"
    else
	git rev-parse --abbrev-ref HEAD
    fi
}

# docker_tags IMAGE-NAME
#  Print the docker tags for IMAGE-NAME

docker_tags()
{
    local registry image_version
    registry="$(docker_registry)"
    repository="$(docker_repository)"
    image_version="$(docker_image_version)"

    printf '%s%s%s%s%s:%s'\
	   "${registry}" "${registry:+/}"\
	   "${repository}" "${repository:+/}"\
	   "$1"\
	   "${image_version}"
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
org.opencontainers.image.title=${title}
org.opencontainers.image.description=${description:=:undefined}
org.opencontainers.image.version=$(docker_image_version)
org.opencontainers.image.created=$(timestamp)
org.opencontainers.image.revision=$(git rev-parse HEAD)
org.opencontainers.image.licenses=${license:=:undefined}
EOF
}

docker_info()
{
    docker_build_details "$@"
}

docker_build_details()
{
    with_group_presentation 'Build Details'\
        gprintf '%s: %s\n'\
	tag "$(docker_tags $1)"\
	label "$(docker_labels $1 | fold_lines)"\
	file "$(docker_file $1 | fold_lines)"
}

if [ -r "${subrdir}/github_actions.sh" -a "${GITHUB_ACTIONS}" = 'true' ]; then
    . "${subrdir}/github_actions.sh"
fi

# End of file `cid.sh'
